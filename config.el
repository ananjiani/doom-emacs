;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq doom-font (font-spec :family "Hack" :size 20))
(setq doom-variable-pitch-font (font-spec :family "Alegreya" :size 24))
(after! vterm
  (evil-set-initial-state 'vterm-mode 'emacs)
  (setq vterm-timer-delay 0.01
        vterm-shell "fish"
        vterm-buffer-name-string "vterm %s"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Documents/org-roam")
(setq org-roam-directory "~/Documents/org-roam")

(after! org
  (font-lock-add-keywords
   'org-mode
   '(;; Inline margin notes [mn::text]
     ("\\[mn::\\([^]]+\\)?\\]"
      (0 'org-footnote t))
     ;; Referenced margin notes [mn:label]
     ("\\[mn:\\([^]:]+\\)\\]"
      (0 'org-footnote t))))
  (setq org-startup-folded 'fold)
  (setq org-log-done 'note)
  ;; (setq org-agenda-remove-tags t)
  (setq org-agenda-hide-tags-regexp "meeting\\|agenda\\|@ammar\\|daily\\|naarpr")
  (setq org-agenda-prefix-format '(
                                   (agenda  . " %i %?-12t% s%e ") ;; file name + org-agenda-entry-type
                                   (timeline  . "  % s")
                                   (todo  . " %i %e ")
                                   (tags  . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; (setq org-agenda-hide-tags-regexp ".*")
  (setq org-agenda-category-icon-alist
        `(("tinker" ,(list (nerd-icons-faicon "nf-fa-cogs")) nil nil :ascent center)
          ("rare" ,(list (nerd-icons-faicon "nf-fa-pencil")) nil nil :ascent center)
          ("organizing" ,(list (nerd-icons-faicon "nf-fa-hand_rock_o")) nil nil :ascent center)
          ("naarpr" ,(list (nerd-icons-faicon "nf-fa-renren")) nil nil :ascent center)
          ("unit" ,(list (nerd-icons-faicon "nf-fa-rebel")) nil nil :ascent center)
          ("igf" ,(list (nerd-icons-faicon "nf-fae-dice")) nil nil :ascent center)
          ("ha" ,(list (nerd-icons-faicon "nf-fa-home")) nil nil :ascent center)
          ("personal" ,(list (nerd-icons-mdicon "nf-md-human")) nil nil :ascent center)
          ("work" ,(list (nerd-icons-faicon "nf-fa-graduation_cap")) nil nil :ascent center)))
  (setq org-agenda-tags-column -125)

  (setq org-agenda-custom-commands
        '(("n" "NAARPR Dallas"
           ((org-ql-block '(and (todo "TODO")
                                (category "naarpr"))
                          ((org-ql-block-header "Tasks")))
            (org-ql-block '(and (todo)
                                (not (todo "TODO"))
                                (category "naarpr"))
                          ((org-ql-block-header "Backlog")))))
          ("u" "Unit"
           ((org-ql-block '(and (todo "TODO")
                                (category "unit"))
                          ((org-ql-block-header "Tasks")))
            (org-ql-block '(and (todo)
                                (not (todo "TODO"))
                                (category "unit"))
                          ((org-ql-block-header "Backlog")))))
          ("w" "Work"
           ((org-ql-block '(and (category "work")
                                (todo "TODO" "PROJ"))
                          ((org-ql-block-header "Tasks")))

            (org-ql-block '(and (category "work")
                                (todo)
                                (not (todo "TODO" "PROJ")))

                          ((org-ql-block-header "Backlog")))))
          ("p" "Priority"
           ((org-ql-block '(and (todo "TODO" "PROJ") (priority >= "C")))))))


  ;; (setq org-agenda-todo-keyword-format "")
  (setq org-capture-templates `(
                                ("i" "Inbox" entry (file "inbox.org") "* TODO %?\n/Entered on/ %U")
                                ("w" "Work" entry (file "~/Documents/org-roam/work/work-projects.org") "* TODO %?\n/Entered on/ %U")
                                ("p" "Personal" entry (file+headline "~/Documents/org-roam/projects.org" "Personal") "* TODO %?\n/Entered on/ %U")
                                ("r" "Red Reading List" item (file"~/Documents/org-roam/red-notes/red-reading-list.org") "- %?")
                                ("n" "NAARPR Dallas Meeting Agenda Item" item (file+headline "~/Documents/org-roam/naarpr-dallas-notes/meeting-notes.org" "Next Meeting") "- %?")
                                ("u" "Unit Meeting Agenda Item" item (file+headline "~/Documents/org-roam/unit-notes/pc-meeting-notes.org" "Next Meeting") "- %?")
                                )))
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("f" "fleeting" plain
           "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("z" "zettelkasten" plain
           "%?"
           :if-new (file+head "zk/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n- tags :: ")
           :unnarrowed t)
          ("?" "question - zk" plain
           "%?"
           :if-new (file+head "zk/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: question\n\n- tags :: ")
           :unnarrowed t)
          ("o" "organizing" plain
           "%?"
           :if-new (file+head "red-notes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("w" "work" plain
           "%?"
           :if-new (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "literature note" plain (file "roam-templates/literature-note.org")
           :if-new
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/literature/${citar-citekey}.org"
            "#+title: ${note-title} (${citar-date})\n#+created: %U\n#+last_modified: %U")
           :unnarrowed t))))






;; (org-roam-db-autosync-mode)
;; (setq org-roam-database-connector 'emacsql-sqlite-builtin)
(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        ;; Set order of multiple groups at once
        (:order-multi (99 (:name "Unit (team)" :and (:category "unit" :not (:tag "@ammar")))
                          (:name "NAARPR Dallas (team)" :and (:category "naarpr" :not (:tag "@ammar")))))
        (:name "Reschedule"
         :scheduled past
         :face 'error)
        (:name "Habits"
         :tag "habit"
         :face 'warning)
        (:name "❗ Overdue"
         :deadline past
         :face 'error)
        (:name "📅 Today"
         :date today
         :scheduled today
         :deadline today
         :face 'warning)
        (:name "Important"
         :priority "A"
         )
        (:auto-category t)))

;; After the last group, the agenda will display items that didn't
;; match any of these groups, with the default order position of 99

(setq org-super-agenda-header-map (make-sparse-keymap))

(org-super-agenda-mode t)
;; (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "agenda")))

;; Build the agenda list the first time for the session
(defun org-agenda-open-hook()
  (my/org-roam-refresh-agenda-list)
  (toggle-truncate-lines 1)
  (olivetti-mode)
  (olivetti-set-width 150)
  )

(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)
;; (with-eval-after-load 'org (global-org-modern-mode))

;; Set up the built-in Emacs tools
(after! claude-code-ide
  (claude-code-ide-emacs-tools-setup))

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode))

(use-package! consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

(map! :leader
      (:prefix ("nr")
               (:desc "backlinks" "b" #'consult-org-roam-backlinks
                :desc "backlinks recursive" "B" #'consult-org-roam-backlinks-recursive
                :desc "forward links" "l" #'consult-org-roam-forward-links
                :desc "search" "S" #'consult-org-roam-search)))

(defun org-roam-subtree-aware-preview-function ()
  "Same as `org-roam-preview-default-function', but gets entire subtree in specific buffers."
  (if (--> (org-roam-node-at-point)
           (org-roam-node-file it)
           (or (member it
                       ;; This is a list of buffers where I want to see preview of subtree
                       org-roam-subtree-aware-preview-buffers)
               (f-ancestor-of-p bibtex-completion-notes-path it)))
      (let ((beg (progn (org-roam-end-of-meta-data t)
                        (point)))
            (end (progn (org-previous-visible-heading 1)
                        (org-end-of-subtree)
                        (point))))
        (-reduce
         (lambda (str el)
           ;; remove properties not interested. If prop drawer is empty at the end, remove drawer itself
           (s-replace-regexp (format "\n *:%s:.*$" el) "" str))
         ;; remove links
         (list (s-replace-regexp "\\[id:\\([a-z]\\|[0-9]\\)\\{8\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{4\\}-\\([a-z]\\|[0-9]\\)\\{12\\}\\]"
                                 ""
                                 (string-trim (buffer-substring-no-properties beg end)))
               "INTERLEAVE_PAGE_NOTE" "BRAIN_CHILDREN" okm-parent-property-name "PROPERTIES:\n *:END")))
    (org-roam-preview-default-function)))

(defun bhankas-org-age-encrypt-and-replace ()
  "Replace current org file with age-encrypted version"
  (interactive)
  (let* ((current-file-name (buffer-file-name))
         (encr-file-name (-> (buffer-file-name)
                             (string-trim)
                             (concat ".age")))
         (encr-file-exists nil))
    (when (string-suffix-p ".org" current-file-name)
      (if (file-exists-p encr-file-name)
          (progn
            (message "Using existing encrypted version instead of overwriting")
            (setq encr-file-exists t))
        (progn
          (message "Encrypting file %s" current-file-name)
          (shell-command (concat "rage -R " age-default-recipient " -e " current-file-name " -o " encr-file-name))
          (when (file-exists-p encr-file-name)
            (setq encr-file-exists t))))
      (when encr-file-exists
        (doom/delete-this-file current-file-name t)
        (find-file encr-file-name)))))

(use-package! org-roam
  :after (org age org-roam-dailies)
  :init
  (add-to-list 'auto-mode-alist '("\\.org\\.age" . org-mode)))

(use-package! age
  :after (org)
  :commands (age-file-enable)
  :init
  (setq! age-program "rage"
         age-default-identity "~/.dotfiles/secrets/emacs/emacs"
         age-default-recipient "~/.dotfiles/secrets/emacs/emacs.pub")
  (age-file-enable))

;; (use-package! websocket
;;   :after org-roam)

;; (use-package! org-roam-ui
;;   :after org-roam ;; or :after org
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   ;;  :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

(after! lsp-mode
  (setq lsp-pylsp-plugins-ruff-enabled t
        lsp-pylsp-plugins-mypy-enabled t
        lsp-nix-nixd-server-path "nixd")

  ;; Ensure org-mode is recognized by LSP
  (add-to-list 'lsp-language-id-configuration '(org-mode . "org"))
  
  ;; Register Vale LSP client directly
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "vale-ls")
    :activation-fn (lsp-activate-on "org")
    :server-id 'vale-ls
    :initialization-options (lambda ()
                              '(:installVale :json-false
                                :syncOnStartup :json-false))))

  )


(after! dap-mode
  (setq dap-python-debugger 'debugpy))

;; (map! :map dap-mode-map
;;       :leader
;;       :prefix ("d" . "dap")
;;       ;; basics
;;       :desc "dap next"          "n" #'dap-next
;;       :desc "dap step in"       "i" #'dap-step-in
;;       :desc "dap step out"      "o" #'dap-step-out
;;       :desc "dap continue"      "c" #'dap-continue
;;       :desc "dap hydra"         "h" #'dap-hydra
;;       :desc "dap debug restart" "r" #'dap-debug-restart
;;       :desc "dap debug"         "s" #'dap-debug

;;       ;; debug
;;       :prefix ("dd" . "Debug")
;;       :desc "dap debug recent"  "r" #'dap-debug-recent
;;       :desc "dap debug last"    "l" #'dap-debug-last

;;       ;; eval
;;       :prefix ("de" . "Eval")
;;       :desc "eval"                "e" #'dap-eval
;;       :desc "eval region"         "r" #'dap-eval-region
;;       :desc "eval thing at point" "s" #'dap-eval-thing-at-point
;;       :desc "add expression"      "a" #'dap-ui-expressions-add
;;       :desc "remove expression"   "d" #'dap-ui-expressions-remove

;;       :prefix ("db" . "Breakpoint")
;;       :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
;;       :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
;;       :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
;;       :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)
;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (:map copilot-completion-map
;;              ("<tab>" . 'copilot-accept-completion)
;;              ("TAB" . 'copilot-accept-completion)
;;              ("C-TAB" . 'copilot-accept-completion-by-word)
;;              ("C-<tab>" . 'copilot-accept-completion-by-word)))
;; (setq copilot-indent-offset-warning-disable t)

(setq yas-snippet-dirs (append yas-snippet-dirs '("~/Documents/org-roam/red-notes/.snippets")))

(after! citar
  (setq citar-bibliography '("~/Documents/org-roam/literature/references.bib"))
  (setq citar-library-paths '("~/Documents/org-roam/library"))
  (setq citar-notes-paths '("~/Documents/org-roam/literature"))
  (setq citar-org-roam-capture-template-key "n"))

(after! org-noter
  (setq org-noter-highlight-selected-text t
        org-noter-always-create-frame nil))

(setq browse-url-browser-function 'browse-url-firefox)

(after! gptel
  (setq gptel-model 'deepseek-reasoner
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key gptel-api-key)
        gptel-default-mode 'org-mode)
  )

;; Remove bold from links in zen mode
(defun my/zen-mode-unbold-links ()
  "Remove bold weight from links when entering zen mode."
  (when (bound-and-true-p writeroom-mode)
    (set-face-attribute 'link nil :weight 'normal)
    (set-face-attribute 'org-link nil :weight 'normal)))

(defun my/zen-mode-restore-links ()
  "Restore link faces when exiting zen mode."
  (unless (bound-and-true-p writeroom-mode)
    (set-face-attribute 'link nil :weight 'unspecified)
    (set-face-attribute 'org-link nil :weight 'unspecified)))

(add-hook 'writeroom-mode-hook #'my/zen-mode-unbold-links)
(add-hook 'writeroom-mode-hook #'my/zen-mode-restore-links)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(require 'acp)
(require 'agent-shell)
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

;; Load gptel extensions
(load! "gptel-extensions/gptel-vale-integration")

;; Mermaid diagram support
(use-package! ob-mermaid
  :after org
  :config
  (setq ob-mermaid-cli-path "mmdc")
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))

;; Keybindings for gptel-vale integration
(map! :leader
      :desc "Rewrite with vale errors" "r v" #'my/gptel-rewrite-with-vale)

;; Customize LSP diagnostics to show vale rule names
(after! lsp-mode
  (setq lsp-diagnostic-filter
        (lambda (params workspace)
          (let ((diagnostics (gethash "diagnostics" params)))
            (puthash "diagnostics"
                     (vconcat
                      (mapcar (lambda (diagnostic)
                                (let ((message (gethash "message" diagnostic))
                                      (code (gethash "code" diagnostic))
                                      (source (gethash "source" diagnostic)))
                                  (when (and code (string= source "vale-ls"))
                                    (puthash "message"
                                             (format "[%s] %s" code message)
                                             diagnostic))
                                  diagnostic))
                              diagnostics))
                     params)
            params))))
