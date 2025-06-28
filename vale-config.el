;;; vale-config.el --- Vale LSP configuration for Doom Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides Vale LSP integration with custom code actions
;; for adding words to Vale's vocabulary and AI-powered text improvements.

;;; Code:

(defun my/vale-add-word-at-point ()
  "Add the word at point to Vale's accept.txt vocabulary."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (when word
      (my/vale-add-word-at-point-with-arg word))))

(defun my/vale-add-word-at-point-with-arg (word)
  "Add WORD to Vale's accept.txt vocabulary."
  (let ((vale-vocab-file (expand-file-name "~/Documents/org-roam/.vale/config/vocabularies/OrgRoam/accept.txt")))
    (message "Adding '%s' to Vale vocabulary" word)
    ;; Append word to file
    (with-temp-buffer
      (insert word "\n")
      (append-to-file (point-min) (point-max) vale-vocab-file))
    (message "Successfully added '%s' to Vale vocabulary" word)
    ;; Restart LSP to reload Vale configuration
    (when (bound-and-true-p lsp-mode)
      (lsp-workspace-restart (lsp--read-workspace)))))

(defun my/vale-get-paragraph-bounds ()
  "Get the bounds of the paragraph at point."
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (cons start end))))

(defun my/vale-get-extended-paragraph-bounds ()
  "Get the bounds including previous, current, and next paragraphs."
  (save-excursion
    (let* ((current-bounds (my/vale-get-paragraph-bounds))
           (start (progn
                   (goto-char (car current-bounds))
                   (backward-paragraph)
                   (point)))
           (end (progn
                 (goto-char (cdr current-bounds))
                 (forward-paragraph)
                 (point))))
      (cons start end))))

(defun my/vale-ai-rewrite-for-diagnostic (diagnostic)
  "Use gptel-rewrite to fix text based on Vale DIAGNOSTIC."
  (let* ((range (gethash "range" diagnostic))
         (start (gethash "start" range))
         (end (gethash "end" range))
         (start-point (lsp--position-to-point
                      (lsp-make-position :line (gethash "line" start)
                                       :character (gethash "character" start))))
         (end-point (lsp--position-to-point
                    (lsp-make-position :line (gethash "line" end)
                                     :character (gethash "character" end))))
         (message (gethash "message" diagnostic))
         (code (gethash "code" diagnostic))
         (extended-bounds (save-excursion
                           (goto-char start-point)
                           (my/vale-get-extended-paragraph-bounds)))
         ;; Get the problematic text
         (problem-text (buffer-substring-no-properties start-point end-point)))
    ;; Select the extended region (3 paragraphs)
    (goto-char (car extended-bounds))
    (push-mark (cdr extended-bounds) t t)
    ;; Call gptel-rewrite with Vale context
    (let ((gptel--rewrite-message (format "Rewrite to fix: %s (Vale: %s)\nProblem location: <<%s>>\nMaintain tone and meaning."
                                         message code problem-text)))
      (message "Vale AI: Requesting fix for '%s' - %s" problem-text message)
      (call-interactively 'gptel-rewrite))))

(defun my/vale-ai-rewrite-handler (params)
  "Handle AI rewrite action for Vale diagnostics."
  (let* ((arguments (cond
                     ((or (vectorp params) (and (listp params) (not (hash-table-p params))))
                      params)
                     ((hash-table-p params)
                      (gethash "arguments" params))
                     (t params)))
         (diagnostic (when arguments
                      (cond
                       ((vectorp arguments) (aref arguments 0))
                       ((listp arguments) (car arguments))
                       (t arguments)))))
    (when diagnostic
      (my/vale-ai-rewrite-for-diagnostic diagnostic))))

(defun my/vale-action-handler (params)
  "Handle custom Vale actions."
  (let ((arguments (cond
                    ;; If params is already the arguments vector/list
                    ((or (vectorp params) (and (listp params) (not (hash-table-p params))))
                     params)
                    ;; If params is a hash table with arguments
                    ((hash-table-p params)
                     (gethash "arguments" params))
                    ;; Otherwise, assume it's the word directly
                    (t params))))
    (when arguments
      (let ((word (cond
                   ((vectorp arguments) (aref arguments 0))
                   ((listp arguments) (car arguments))
                   ((stringp arguments) arguments)
                   (t (error "Unknown argument type: %S" arguments)))))
        (my/vale-add-word-at-point-with-arg word)))))

(defun my/vale-code-actions-advice (orig-fun &optional kind)
  "Add Vale vocabulary and AI rewrite code actions to standard LSP code actions."
  (let ((actions (funcall orig-fun kind))
        (diagnostics (lsp-cur-line-diagnostics))
        (seen-words (make-hash-table :test 'equal))
        (seen-diagnostics (make-hash-table :test 'equal)))
    (when (and diagnostics (vectorp diagnostics))
      (seq-do (lambda (diagnostic)
                (when (equal (gethash "source" diagnostic) "vale-ls")
                  ;; Add AI rewrite action for all Vale diagnostics
                  (let ((diagnostic-key (format "%s-%s"
                                              (gethash "code" diagnostic)
                                              (gethash "message" diagnostic))))
                    (unless (gethash diagnostic-key seen-diagnostics)
                      (puthash diagnostic-key t seen-diagnostics)
                      (push (lsp-make-code-action
                             :title "AI: Suggest improvement"
                             :kind "quickfix"
                             :command? (lsp-make-command
                                        :title "AI: Suggest improvement"
                                        :command "vale.aiRewrite"
                                        :arguments? (vector diagnostic)))
                            actions)))
                  
                  ;; Add vocabulary action for spelling errors
                  (when (string-match-p "Spelling" (or (gethash "code" diagnostic) ""))
                    (let* ((range (gethash "range" diagnostic))
                           (start (gethash "start" range))
                           (end (gethash "end" range))
                           (start-char (gethash "character" start))
                           (end-char (gethash "character" end))
                           (line (gethash "line" start))
                           (word (save-excursion
                                   (goto-char (lsp--position-to-point 
                                              (lsp-make-position :line line :character start-char)))
                                   (buffer-substring-no-properties
                                    (point)
                                    (lsp--position-to-point 
                                     (lsp-make-position :line line :character end-char))))))
                      ;; Only add action if we haven't seen this word yet
                      (unless (gethash word seen-words)
                        (puthash word t seen-words)
                        (push (lsp-make-code-action
                               :title (format "Add '%s' to Vale vocabulary" word)
                               :kind "quickfix"
                               :command? (lsp-make-command
                                          :title (format "Add '%s' to Vale vocabulary" word)
                                          :command "vale.addToVocabulary"
                                          :arguments? (vector word)))
                              actions))))))
              diagnostics))
    actions))

(defun my/setup-vale-lsp ()
  "Set up Vale LSP with custom code actions."
  (add-to-list 'lsp-language-id-configuration '(org-mode . "org"))
  
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "vale-ls")
                        :activation-fn (lsp-activate-on "org")
                        :server-id 'vale-ls
                        :initialization-options (lambda ()
                                                  '(:installVale :json-false
                                                    :syncOnStartup :json-false))
                        :action-handlers (ht ("vale.addToVocabulary" #'my/vale-action-handler)
                                           ("vale.aiRewrite" #'my/vale-ai-rewrite-handler))))
  
  ;; Bind to a convenient key
  (map! :map org-mode-map
        :localleader
        :desc "Add word to Vale vocabulary"
        "v a" #'my/vale-add-word-at-point)
  
  ;; Add advice for code actions
  (advice-add 'lsp-code-actions-at-point :around #'my/vale-code-actions-advice)
  
  ;; Register in default handlers
  (with-eval-after-load 'lsp-mode
    (puthash "vale.addToVocabulary" #'my/vale-action-handler lsp--default-action-handlers)
    (puthash "vale.aiRewrite" #'my/vale-ai-rewrite-handler lsp--default-action-handlers)))

(provide 'vale-config)
;;; vale-config.el ends here