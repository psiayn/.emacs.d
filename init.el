(set-frame-font "Iosevka Nerd Font 15" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font 15"))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(setq backup-directory-alist 
  '(("." . "~/.emacs.d/file-backups")))
;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
;; (add-to-list 'default-frame-alist '(alpha 90 90))
(global-display-fill-column-indicator-mode 1)
(show-paren-mode 1)
;; i hate the splash screen
(setq inhibit-splash-screen t)


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
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(use-package swiper
  :straight t)
(use-package counsel
  :straight t)

(use-package ivy
  :straight t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-vibrant t))

(use-package tree-sitter
  :straight t)
(use-package tree-sitter-langs
  :straight t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package magit
  :straight t)

(use-package company
  :straight t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :straight t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))  
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package flycheck
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package nix-mode
  :straight t
  :hook
  (nix-mode . lsp-deferred))

(use-package rust-mode
  :straight t
  :hook (rust-mode . lsp-deferred))

(use-package rustic
  :straight t)

(use-package speed-type
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package envrc
  :straight t
  :config
  (envrc-global-mode))

(use-package go-mode
  :straight t
  :config 
  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package typescript-mode
  :straight t
  :config
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))


(defun spawn-eshell ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
(setq split-width-threshold nil)
(global-set-key (kbd "C-c t") 'spawn-eshell)
