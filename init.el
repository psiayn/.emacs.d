(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Enable vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :straight t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
;; consult
(use-package consult
  :straight t
  :bind
  ("C-s" . consult-line))

;; themes
(use-package yoshi-theme
  :straight t
  :config
  (load-theme 'yoshi t))

;; tree-sitter
(use-package tree-sitter
  :straight t)
(use-package tree-sitter-langs
  :straight t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; hydra
(use-package hydra
  :straight t)

;; which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; ace-window
(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window))

;; exec path from shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;                            lsp-mode                            ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; completion engine
(use-package company
  :straight t
  :config
  (global-company-mode))

;; errors
(use-package flycheck
  :straight t
  :config (global-flycheck-mode))

;; magit
(use-package magit
  :straight t)

;; projectile
(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; treemacs
(use-package treemacs
  :straight t)
(use-package treemacs-projectile
  :straight t)
(use-package treemacs-magit
  :straight t)

;; lsp-ui
(use-package lsp-ui
  :straight t)

;; lsp-treemacs
(use-package lsp-treemacs
  :straight t)

;; dap
(use-package dap-mode
  :straight t)

;;;;;;;; editorconfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;;;;;;;; languages

;;;; golang

;;; hooks
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;;; installing go-mode
(use-package go-mode
  :straight t
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
