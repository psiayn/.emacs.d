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

(use-package lsp-mode
  :straight t
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;;;;;;;; editorconfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; yasnippets
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

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

;;;; rust
(use-package rust-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook #'lsp-deferred))


;;;; haskell
(use-package haskell-mode
  :straight t)
(use-package lsp-haskell
  :straight t
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred))
(defun org-babel-execute:runhaskell (body params)
  (org-babel-eval "runhaskell"
                  (org-babel-expand-body:generic body params)))
(add-to-list 'org-src-lang-modes '("runhaskell" . haskell))

;;;; nix
(use-package nix-mode
  :straight t)

;; ruby
(use-package rvm
  :straight t
  :config
  (rvm-use-default))


;; clojure
(use-package cider
  :straight t)
