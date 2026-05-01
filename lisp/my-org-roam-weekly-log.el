;;; my-org-roam-weekly-log.el --- Weekly work log generator from org-roam  -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a weekly work log from org-roam clock notes.
;; Groups all clocked tasks by their top-level project heading.
;; Uses day-of-week abbreviations instead of full dates.
;;
;; Usage: M-x my/org-roam-generate-weekly-log
;;
;; Input: clock notes, CLOSING NOTEs, and plain notes in LOGBOOK drawers of
;;        org-roam files.  Also captures CLOSING NOTEs outside the LOGBOOK
;;        drawer and plain "- text" notes after CLOCK lines.
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
  "Return .org files under `org-roam-directory' with #+FILETAGS containing `agenda',
excluding archives and weekly-logs."
  (seq-filter
   (lambda (f)
     (with-temp-buffer
       (insert-file-contents-literally f nil 0 1024)
       (goto-char (point-min))
       (re-search-forward "^#\\+FILETAGS:.*:agenda:" nil t)))
   (seq-remove (lambda (f)
                 (or (string-suffix-p ".org_archive" f)
                     (string-match-p "/weekly-logs/" f)))
               (directory-files-recursively org-roam-directory "\\.org$"))))

(defun my/org-roam--heading-text (marker)
  "Return all LOGBOOK content plus CLOSING NOTEs outside the drawer for heading at MARKER."
  (when (markerp marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((element (org-element-at-point))
               (headline-end (org-element-property :end element))
               (result ""))
          ;; Grab LOGBOOK drawer content
          (when (re-search-forward ":LOGBOOK:" headline-end t)
            (let ((drawer-beg (line-beginning-position 2))
                  (drawer-end (save-excursion
                                (when (re-search-forward "^ *:END:" headline-end t)
                                  (line-beginning-position)))))
              (when (and drawer-beg drawer-end)
                (setq result (buffer-substring-no-properties drawer-beg drawer-end)))))
          ;; Grab CLOSING NOTEs outside the LOGBOOK drawer
          ;; Only scan direct body — stop at subheadings to avoid duplicates
          (goto-char marker)
          (forward-line 1) ; skip the heading line itself
          (let ((in-drawer nil))
            (while (< (point) headline-end)
              (cond
               ((looking-at "^\\*") (goto-char headline-end)) ; stop at subheadings
               ((looking-at ":LOGBOOK:") (setq in-drawer t) (forward-line 1))
               ((and in-drawer (looking-at ":END:")) (setq in-drawer nil) (forward-line 1))
               ((and (not in-drawer) (looking-at "^- CLOSING NOTE"))
                (let ((note-text (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position))))
                  (forward-line 1)
                  (while (looking-at "^[ \t]+\\S-")
                    (setq note-text (concat note-text "\n"
                                            (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position))))
                    (forward-line 1))
                  (setq result (concat result "\n" note-text))))
               (t (forward-line 1)))))
          (string-trim result))))))

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

(defun my/org-roam--collect-continuation-lines ()
  "Collect indented continuation lines from point, return as trimmed string list.
Point must be on the first line after the note line.  Advances point past them."
  (let ((cont-lines '()))
    (while (looking-at "^[ \t]+\\S-")
      (push (string-trim (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
            cont-lines)
      (forward-line 1))
    (nreverse cont-lines)))

(defun my/org-roam--extract-loogbook-notes (loogbook-text)
  "Extract CLOSING NOTEs, Note-taken-on notes, and plain notes after CLOCK lines from text.
Plain notes inherit their date from the CLOCK line above them."
  (let ((notes '()))
    (when loogbook-text
      (with-temp-buffer
        (insert loogbook-text)
        (goto-char (point-min))

        ;; Pass 1: CLOSING NOTEs (with continuation lines)
        (while (re-search-forward "^- CLOSING NOTE\\(.*\\)" nil t)
          (let ((note (string-trim (match-string 0))))
            (forward-line 1)
            (dolist (cl (my/org-roam--collect-continuation-lines))
              (setq note (concat note "\n" cl)))
            (push note notes)))

        ;; Pass 2: Note-taken-on notes (with continuation lines)
        (goto-char (point-min))
        (while (re-search-forward "^- Note taken on\\(.*\\)" nil t)
          (let ((note (string-trim (match-string 0))))
            (forward-line 1)
            (dolist (cl (my/org-roam--collect-continuation-lines))
              (setq note (concat note "\n" cl)))
            (push note notes)))

        ;; Pass 3: Plain notes after CLOCK lines — inherit date from the CLOCK timestamp
        (goto-char (point-min))
        (while (re-search-forward "^CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
          (let ((clock-date (match-string 1)))
            (forward-line 1)
            ;; Collect all consecutive plain "- " list items after this CLOCK line
            (while (looking-at "^\\s-*- \\(.+\\)")
              (let ((raw (string-trim (match-string 0))))
                (if (or (string-match-p "^- Note taken on" raw)
                        (string-match-p "^- CLOSING NOTE" raw))
                    ;; Skip already-captured formats, but advance point
                    (forward-line 1)
                  ;; Build a synthetic note with the clock's date
                  (let ((note (format "- Plain note [%s] %s"
                                      clock-date
                                      (string-trim (match-string 1)))))
                    (forward-line 1)
                    (dolist (cl (my/org-roam--collect-continuation-lines))
                      (setq note (concat note "\n" cl)))
                    (push note notes)))))))))
    (nreverse notes)))

(defun my/org-roam--note-date (note)
  "Extract the YYYY-MM-DD date string from NOTE, or nil."
  (cond
   ((string-match "- Note taken on \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" note)
    (match-string 1 note))
   ((string-match "- CLOSING NOTE \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" note)
    (match-string 1 note))
   ((string-match "- Plain note \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\]" note)
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
        (delete-region (point-min) (point)))
       ((re-search-forward "^- Plain note \\[[^]]+\\][ \t]*" nil t)
        (delete-region (point-min) (point))))
      ;; Remove stray backslash-only lines left by org continuation markers
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\\\+[ \t]*$" nil t)
        (replace-match ""))
      (setq body (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Split body into lines
    (when (not (string-empty-p body))
      (dolist (line (split-string body "\n" t "[ \t]*"))
        (setq line (string-trim line))
        (when (not (string-empty-p line))
          (push (if date-str
                    (format "[%s] %s" (my/org-roam--date-to-day-abbrev date-str) line)
                  line)
                lines))))
    (nreverse lines)))

(defun my/org-roam--find-closed-this-week (files start-str end-str)
  "Scan FILES for headings closed between START-STR and END-STR (YYYY-MM-DD).
Return a list of markers to those headings."
  (let ((markers '())
        (closed-re "^CLOSED: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"))
    (dolist (f files)
      (with-current-buffer (find-file-noselect f)
        (goto-char (point-min))
        (while (re-search-forward closed-re nil t)
          (let ((closed-date (match-string 1)))
            (when (and (not (string< closed-date start-str))
                       (not (string< end-str closed-date)))
              (save-excursion
                (org-back-to-heading t)
                (push (point-marker) markers)))))))
    (delete-dups markers)))

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
         ;; Query tasks with clock entries in the date range
         (clocked-tasks (org-ql-query
                          :select (lambda ()
                                    (list (point-marker)))
                          :from all-org-files
                          :where `(and (category "work")
                                       (clocked :from ,start-str :to ,end-str))))
         ;; Find tasks closed this week (may overlap with clocked)
         (closed-markers (my/org-roam--find-closed-this-week
                          all-org-files start-str end-str))
         ;; Collect: project-name -> list of (date-str . lines)
         (projects (make-hash-table :test 'equal))
         (seen-markers (make-hash-table :test 'equal)))

    ;; Helper to process a task marker
    (cl-flet ((process-marker (marker)
                (when (and (markerp marker)
                           (not (gethash (cons (marker-buffer marker) (marker-position marker))
                                           seen-markers)))
                  (puthash (cons (marker-buffer marker) (marker-position marker))
                           t seen-markers)
                  (let* ((project (my/org-roam--top-level-heading marker))
                         (text (my/org-roam--heading-text marker))
                         (notes (my/org-roam--extract-loogbook-notes text)))
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
                                       projects))))))))))

      ;; Process clocked tasks
      (dolist (task clocked-tasks)
        (process-marker (nth 0 task)))
      ;; Process closed tasks (deduped by seen-markers)
      (dolist (m closed-markers)
        (process-marker m)))

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

(defun my/org-note-to-active-clock ()
  "Add a note to the currently clocked task."
  (interactive)
  (let ((marker org-clock-hd-marker))
    (unless (and marker (marker-buffer marker))
      (user-error "No active clock"))
    (save-window-excursion
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (org-add-note)))))

(provide 'my-org-roam-weekly-log)
;;; my-org-roam-weekly-log.el ends here
