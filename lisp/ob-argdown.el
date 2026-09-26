;;; ob-argdown.el --- Org Babel support for Argdown -*- lexical-binding: t; -*-

(require 'ob)
(require 'markdown-mode)
(require 'subr-x)

(defgroup ob-argdown nil
  "Org Babel support for Argdown."
  :group 'org-babel)

(defcustom ob-argdown-cli-path nil
  "Path to the Argdown command-line program.
When nil, find `argdown' in `exec-path'."
  :group 'ob-argdown
  :type '(choice (const :tag "Find argdown in PATH" nil) file))

(defvar org-babel-default-header-args:argdown
  '((:results . "file") (:exports . "results"))
  "Default arguments for Argdown source blocks.")

(defconst argdown-font-lock-keywords
  '(("^\\s-*\\(([^)\n]+)\\)" 1 font-lock-constant-face)
    ("\\(\\[[^]\n]+\\]\\)" 1 font-lock-variable-name-face)
    ("\\(<[^>\n]+>\\)" 1 font-lock-function-name-face)
    ("^\\s-*\\([-+<>][+><-]*\\)\\s-+" 1 font-lock-builtin-face)
    ("^\\s-*\\(-\\{2,\\}\\)\\s-*$" 1 font-lock-keyword-face)
    ("\\(?:^\\|\\s-\\)\\(#[[:alnum:]_-]+\\)" 1 font-lock-preprocessor-face))
  "Extra font-lock rules for Argdown syntax.")

;;;###autoload
(define-derived-mode argdown-mode markdown-mode "Argdown"
  "Major mode for Argdown argument maps."
  (font-lock-add-keywords nil argdown-font-lock-keywords 'append)
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.argdown\\'" . argdown-mode))
(add-to-list 'org-src-lang-modes '("argdown" . argdown))
(add-to-list 'org-babel-tangle-lang-exts '("argdown" . "argdown"))

(defun ob-argdown--cli ()
  "Return the Argdown executable or report a useful error."
  (let ((cli (or ob-argdown-cli-path (executable-find "argdown"))))
    (unless (and cli (file-executable-p cli))
      (error "Cannot find the Argdown CLI in exec-path"))
    cli))

(defun org-babel-execute:argdown (body params)
  "Render Argdown BODY according to PARAMS.
The :file header is required.  SVG, PDF, and DOT output are supported."
  (let* ((out-file (or (cdr (assq :file params))
                       (error "Argdown requires a \":file\" header argument")))
         (out-file (expand-file-name out-file))
         (format (downcase (or (file-name-extension out-file) "")))
         (in-file (org-babel-temp-file "argdown-" ".argdown"))
         (out-dir (make-temp-file "ob-argdown-" t))
         (generated-file
          (expand-file-name
           (concat (file-name-base in-file) "." format)
           out-dir)))
    (unless (member format '("svg" "pdf" "dot"))
      (error "Argdown output must use an .svg, .pdf, or .dot file"))
    (unwind-protect
        (progn
          (with-temp-file in-file
            (insert body))
          (with-temp-buffer
            (let ((status (call-process (ob-argdown--cli) nil t nil
                                        "map" in-file out-dir
                                        "--format" format)))
              (unless (zerop status)
                (error "Argdown failed: %s" (string-trim (buffer-string))))))
          (unless (file-exists-p generated-file)
            (error "Argdown did not create %s" generated-file))
          (make-directory (file-name-directory out-file) t)
          (copy-file generated-file out-file t)
          nil)
      (ignore-errors (delete-file in-file))
      (ignore-errors (delete-directory out-dir t)))))

(provide 'ob-argdown)
;;; ob-argdown.el ends here
