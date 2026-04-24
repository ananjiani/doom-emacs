;;; my-org-roam-weekly-log.el --- Weekly work log generator from org-roam  -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a weekly work log from org-roam clock notes.
;; Groups all clocked tasks by their top-level project heading.
;; Uses day-of-week abbreviations instead of full dates.
;;
;; Usage: M-x my/org-roam-generate-weekly-log
;;
;; Input: clock notes ("Note taken on ...") in LOGBOOK drawers of org-roam files.
;; Output: an .org file in work/weekly-logs/ with one section per project.

;;; Code:

(require 'org-ql)
(require 'org-id)

;;; --- Helpers ---

(defun my/org-roam--week-bounds (&optional date)
  "Return (START . END) timestamps for the Monday–Friday work week containing DATE.
If DATE is Monday before noon, return the previous week."
  (let* ((target (or date (current-time)))
         (decoded (decode-time target))
         (dow (nth 6 decoded))    ; 0=Sun, 1=Mon, ...
         (hour (nth 2 decoded)))
    (when (and (= dow 1) (< hour 12))
      (setq target (time-subtract target (days-to-time 7))
            decoded (decode-time target)
            dow (nth 6 decoded)))
    (let* ((monday-offset (if (= dow 0) -6 (- 1 dow)))
           (monday (time-add target (days-to-time monday-offset)))
           (friday (time-add monday (days-to-time 4)))
           (start (encode-time 0 0 0
                               (nth 3 (decode-time monday))
                               (nth 4 (decode-time monday))
                               (nth 5 (decode-time monday))))
           (end (encode-time 59 59 23
                             (nth 3 (decode-time friday))
                             (nth 4 (decode-time friday))
                             (nth 5 (decode-time friday)))))
      (cons start end))))

(defun my/org-roam--all-org-files ()
  "Return all .org files under `org-roam-directory', excluding archives and weekly-logs."
  (seq-remove (lambda (f)
                (or (string-suffix-p ".org_archive" f)
                    (string-match-p "/weekly-logs/" f)))
              (directory-files-recursively org-roam-directory "\\.org$")))

(defun my/org-roam--loogbook-text (marker)
  "Return raw text inside the LOGBOOK drawer at MARKER."
  (when (markerp marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((element (org-element-at-point))
               (headline-end (org-element-property :end element)))
          (when (re-search-forward ":LOGBOOK:" headline-end t)
            (let ((drawer-beg (line-beginning-position 2))
                  (drawer-end (save-excursion
                                (when (re-search-forward "^ *:END:" headline-end t)
                                  (line-beginning-position)))))
              (when drawer-end
                (string-trim (buffer-substring-no-properties drawer-beg drawer-end))))))))))

(defun my/org-roam--top-level-heading (marker)
  "Return the text of the top-level (* ) ancestor heading at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (while (org-up-heading-safe))
      (org-get-heading t t t t))))

(defun my/org-roam--date-to-day-abbrev (date-str)
  "Convert DATE-STR (YYYY-MM-DD) to short day name like Mon, Tue, etc."
  (let* ((parsed (parse-time-string (concat date-str " 00:00")))
         (time (encode-time 0 0 0 (nth 3 parsed) (nth 4 parsed) (nth 5 parsed))))
    (format-time-string "%a" time)))

(defun my/org-roam--extract-loogbook-notes (loogbook-text)
  "Extract CLOSING NOTEs and clock-out notes (with continuation lines) from LOGBOOK-TEXT."
  (let ((notes '()))
    (when loogbook-text
      (with-temp-buffer
        (insert loogbook-text)
        (goto-char (point-min))
        ;; CLOSING NOTEs (with continuation lines)
        (while (re-search-forward "^- CLOSING NOTE\\(.*\\)" nil t)
          (let ((note (string-trim (match-string 0))))
            (forward-line 1)
            (while (looking-at "^[ \t]+\\S-")
              (setq note (concat note "\n" (string-trim (buffer-substring-no-properties
                                                       (line-beginning-position) (line-end-position)))))
              (forward-line 1))
            (push note notes)))
        ;; Clock-out notes (with continuation lines)
        (goto-char (point-min))
        (while (re-search-forward "^- Note taken on\\(.*\\)" nil t)
          (let ((note (string-trim (match-string 0))))
            (forward-line 1)
            (while (looking-at "^[ \t]+\\S-")
              (setq note (concat note "\n" (string-trim (buffer-substring-no-properties
                                                       (line-beginning-position) (line-end-position)))))
              (forward-line 1))
            (push note notes)))))
    (nreverse notes)))

(defun my/org-roam--note-date (note)
  "Extract the YYYY-MM-DD date string from NOTE, or nil."
  (cond
   ((string-match "- Note taken on \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" note)
    (match-string 1 note))
   ((string-match "- CLOSING NOTE \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" note)
    (match-string 1 note))
   (t nil)))

(defun my/org-roam--clean-note-lines (note)
  "Parse NOTE into a list of bulleted strings.
First line gets [Day] prefix, continuation lines become separate bullets.
Returns nil if NOTE has no content after stripping the prefix."
  (let ((date-str (my/org-roam--note-date note))
        (body nil)
        (lines '()))
    ;; Strip prefix and extract body
    (with-temp-buffer
      (insert note)
      (goto-char (point-min))
      (cond
       ((re-search-forward "^- Note taken on \\[[^]]+\\][ \t]*\\\\?[ \t]*\n?" nil t)
        (delete-region (point-min) (point)))
       ((re-search-forward "^- CLOSING NOTE \\[[^]]+\\][ \t]*" nil t)
        (delete-region (point-min) (point))))
      ;; Remove stray backslash-only lines left by org continuation markers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\\\[ \t]*$" nil t)
        (replace-match ""))
      (setq body (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Split body into lines
    (when (not (string-empty-p body))
      (let ((first t))
        (dolist (line (split-string body "\n" t "[ \t]*"))
          (setq line (string-trim line))
          (when (not (string-empty-p line))
            (push (if (and date-str first)
                      (format "[%s] %s" (my/org-roam--date-to-day-abbrev date-str) line)
                    line)
                  lines)
            (setq first nil)))
        (nreverse lines)))))

;;; --- Main ---

(defun my/org-roam-generate-weekly-log ()
  "Generate a weekly work log from org-roam clock notes.
Defaults to the previous week when invoked on Monday before noon.
Groups tasks by their top-level project heading."
  (interactive)
  (let* ((week-bounds (my/org-roam--week-bounds))
         (start-ts (car week-bounds))
         (end-ts (cdr week-bounds))
         (start-str (format-time-string "%Y-%m-%d" start-ts))
         (end-str (format-time-string "%Y-%m-%d" end-ts))
         (week-label (format "%s–%s"
                             (format-time-string "%B %d" start-ts)
                             (format-time-string "%B %d, %Y" end-ts)))
         (filename (expand-file-name
                    (format "work/weekly-logs/%s-to-%s-work-log.org"
                            start-str end-str)
                    org-roam-directory))
         (all-org-files (my/org-roam--all-org-files))
         ;; Query ALL tasks with clock entries in the date range
         (clocked-tasks (org-ql-query
                          :select (lambda ()
                                    (list (point-marker)))
                          :from all-org-files
                          :where `(and (category "work")
                                       (clocked :from ,start-str :to ,end-str))))
         ;; Collect: project-name -> list of (date-str . lines)
         (projects (make-hash-table :test 'equal)))

    ;; Group notes by project
    (dolist (task clocked-tasks)
      (let* ((marker (nth 0 task))
             (project (my/org-roam--top-level-heading marker))
             (lb (my/org-roam--loogbook-text marker))
             (notes (my/org-roam--extract-loogbook-notes lb)))
        (dolist (note notes)
          (let ((note-date (my/org-roam--note-date note)))
            ;; Only include notes from this week
            (when (or (null note-date)
                      (and (not (string< note-date start-str))
                           (not (string< end-str note-date))))
              (let ((cleaned (my/org-roam--clean-note-lines note)))
                (when cleaned
                  (puthash project
                           (append (gethash project projects '())
                                   (list (cons (or note-date "9999-99-99") cleaned)))
                           projects))))))))

    ;; Build report
    (with-current-buffer (get-buffer-create "*Weekly Work Log*")
      (erase-buffer)
      (org-mode)
      (insert (format "#+title: Work Log: %s\n" week-label))
      (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" (org-id-new)))
      (insert "#+filetags: :work:weekly-log:\n\n")

      ;; Sort projects alphabetically and output
      (let ((project-names '()))
        (maphash (lambda (name _) (push name project-names)) projects)
        (dolist (project (sort project-names #'string<))
          (insert (format "* %s\n" project))
          (let ((entries (gethash project projects)))
            ;; Sort entries by date
            (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
            (dolist (entry entries)
              (dolist (line (cdr entry))
                (insert (format "  - %s\n" line)))))
          (insert "\n")))

      ;; Save and register with org-roam
      (write-file filename)
      (when (fboundp 'org-roam-db-update-file)
        (org-roam-db-update-file filename))
      (message "Weekly log saved to %s" filename)
      (pop-to-buffer (current-buffer)))))

(provide 'my-org-roam-weekly-log)
;;; my-org-roam-weekly-log.el ends here
