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
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
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

(load "~/.emacs.d/config/evil.el")

;; org-mode go brr
(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; themes
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-moonlight t))

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

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


(load "~/.emacs.d/config/lsp.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("2d70bca08b194d0becf19a1df2c54fcb78daeeebc880042de47c735a5c837af0" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "a66b97790776954a16911f901355beb2cf1607eb8682e6d8a9654aac2a0da902" "94f9c204b7fdcef8fcc6029e10b77dea1387fe4c2380cd270d196a7f59bdb1eb" "4e12f047c0ee29f5d5c1c70855d382838dc97cda06fa962d78e407ea1384ecf3" default))
 '(global-display-line-numbers-mode t)
 '(hl-sexp-background-color "#efebe9")
 '(ispell-dictionary nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "mononoki NF" :foundry "outline" :slant normal :weight normal :height 139 :width normal)))))
