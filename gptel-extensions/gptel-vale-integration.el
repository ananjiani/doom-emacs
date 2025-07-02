;;; gptel-vale-integration.el --- Integration between gptel and vale-ls -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides integration between gptel-rewrite and vale-ls,
;; allowing gptel to use vale-ls diagnostics as context when rewriting text.

;;; Code:

(require 'lsp-mode)
(require 'gptel-rewrite)

(defun my/get-vale-diagnostics-in-region (start end)
  "Get vale-ls diagnostics within the specified region START to END.
Returns a list of diagnostics with their messages and positions."
  (when (and lsp-mode (lsp-workspaces))
    (let ((diagnostics (lsp--get-buffer-diagnostics))
          (region-diagnostics '()))
      (when diagnostics
        (seq-do
         (lambda (diagnostic)
           (-let* (((&Diagnostic :range (&Range :start (&Position :line start-line :character start-char)
                                                :end (&Position :line end-line :character end-char))
                                 :message
                                 :severity?
                                 :source?
                                 :code?) diagnostic)
                   (diag-start (save-excursion
                                 (goto-char (point-min))
                                 (forward-line start-line)
                                 (forward-char start-char)
                                 (point)))
                   (diag-end (save-excursion
                               (goto-char (point-min))
                               (forward-line end-line)
                               (forward-char end-char)
                               (point))))
             ;; Check if diagnostic is within our region
             (when (and (>= diag-start start)
                        (<= diag-start end)
                        (string= source? "vale-ls"))
               (push (list :message message
                           :severity severity?
                           :code code?
                           :start diag-start
                           :end diag-end
                           :start-char start-char
                           :end-char end-char
                           :line (1+ start-line))  ; Convert to 1-based for display
                     region-diagnostics))))
         diagnostics))
      (nreverse region-diagnostics))))

(defun my/format-vale-diagnostics-for-prompt (diagnostics)
  "Format DIAGNOSTICS into a string suitable for including in a gptel prompt."
  (if (null diagnostics)
      ""
    (concat "Vale issues: "
            (mapconcat
             (lambda (diag)
               (format "Characters %d-%d: %s"
                       (plist-get diag :start-char)
                       (plist-get diag :end-char)
                       (plist-get diag :message)))
             diagnostics
             "; ")
            "\n\nRewrite following William Zinsser's principles of clarity, simplicity, brevity, and humanity. Return ONLY the corrected text:")))

(defun my/gptel-rewrite-with-vale ()
  "Rewrite selected region using gptel, with vale-ls errors as context."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  
  (let* ((start (region-beginning))
         (end (region-end))
         (vale-diagnostics (my/get-vale-diagnostics-in-region start end))
         (vale-context (my/format-vale-diagnostics-for-prompt vale-diagnostics))
         ;; Set rewrite message to vale context only
         (gptel--rewrite-message vale-context))
    
    ;; Debug: Show what we're sending
    (message "Vale context being added: %s" vale-context)
    (message "Full rewrite message: %s" gptel--rewrite-message)
    
    ;; Show a message if no vale diagnostics were found
    (when (null vale-diagnostics)
      (message "No vale-ls diagnostics found in region. Proceeding with standard rewrite."))
    
    ;; Call gptel-rewrite with our enhanced message
    (call-interactively 'gptel-rewrite)))

(provide 'gptel-vale-integration)
;;; gptel-vale-integration.el ends here
