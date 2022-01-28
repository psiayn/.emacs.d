(use-package undo-fu
  :straight t)

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  )

(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode))

(use-package evil-args
  :straight t
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package vimish-fold
  :straight t)
(use-package evil-vimish-fold
  :straight t
  :config
  (global-evil-vimish-fold-mode 1))
