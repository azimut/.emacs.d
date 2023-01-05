;; NOTE: need a compositor
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun disable-trailing-highlight ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'Buffer-menu-mode-hook #'disable-trailing-highlight)
(add-hook 'shell-mode-hook #'disable-trailing-highlight)
(add-hook 'vterm-mode-hook #'disable-trailing-highlight)
(add-hook 'help-mode-hook #'disable-trailing-highlight)
(add-hook 'eww-mode-hook #'disable-trailing-highlight)
(add-hook 'sly-autodoc-mode-hook #'disable-trailing-highlight)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face empty))

(use-package neotree
  :bind ("C-0" . neotree-toggle)
  :custom
  (neo-theme              'arrow)
  (neo-hidden-regexp-list '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$" "\\.beam$" "\\.meta$")))

(use-package imenu-list
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize t))

(use-package which-key
  :init (which-key-mode +1))

(use-package vertico
  :init (vertico-mode +1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; (use-package yascroll
;;   :custom (yascroll:disabled-modes '(image-mode prog-mode))
;;   :init (global-yascroll-bar-mode +1))

(use-package nyan-mode
  :init (nyan-mode +1)
  :custom
  (nyan-minimum-window-width  32)
  (nyan-bar-length            16)
  (nyan-animate-nyancat      nil))

(lexical-let ((color-flag t))
  (defun toggle-color ()
    (interactive)
    (setf color-flag (not color-flag))
    (if color-flag
        (set-face-foreground 'font-lock-comment-face "orange")
      (set-face-foreground 'font-lock-comment-face "#696969"))))

(set-cursor-color "#FF00FF")
(setq-default left-fringe-width 1 right-fringe-width 8
              left-margin-width 1 right-margin-width 0)
(set-fringe-style (quote (20 . 10)))

(global-set-key (kbd "M-m") 'delete-other-windows)
