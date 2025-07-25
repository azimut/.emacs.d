;; NOTE: need a compositor
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Better, only background and NOT text transparency...
;; https://kristofferbalintona.me/posts/202206071000/
;; https://github.com/emacs-mirror/emacs/commit/5c87d826201a5cae242ce5887a0aa7e24ad6f5ee
(defun kb/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75))
    (pcase (frame-parameter nil 'alpha-background)
      (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(defun disable-trailing-highlight ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'Buffer-menu-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook       #'hl-line-mode)
(add-hook 'dape-repl-mode-hook   #'disable-trailing-highlight)
(add-hook 'dape-info-scope-mode-hook #'disable-trailing-highlight)
(add-hook 'dape-info-stack-mode-hook #'disable-trailing-highlight)
(add-hook 'Buffer-menu-mode-hook #'disable-trailing-highlight)
(add-hook 'sqlite-mode-hook      #'disable-trailing-highlight)
(add-hook 'shell-mode-hook       #'disable-trailing-highlight)
(add-hook 'vterm-mode-hook       #'disable-trailing-highlight)
(add-hook 'help-mode-hook        #'disable-trailing-highlight)
(add-hook 'eww-mode-hook         #'disable-trailing-highlight)
;;(add-hook 'sly-autodoc-mode-hook #'disable-trailing-highlight)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t
      completion-ignore-case                t)

;; (set-face-attribute 'mode-line nil :height 100)
;; (set-face-attribute 'mode-line-inactive nil :height 100)


;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tool-bar-mode   -1)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'whitespace)
(global-whitespace-mode 1)

(setq whitespace-style '(face empty))
(use-package neotree
  :bind ("C-0" . neotree-toggle)
  ;;  neo-theme
  :custom
  (neo-theme (if (display-graphic-p) 'icons))
  (neo-show-hidden-files  t)
  (neo-hidden-regexp-list '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$" "\\.beam$" "\\.meta$")))

(use-package imenu-list
  :bind (("C-'" . imenu-list-show)
         :map imenu-list-major-mode-map
         ("C-'" . imenu-list-quit-window))
  :custom
  (imenu-list-auto-resize t))

(use-package which-key
  :init (which-key-mode +1))

(use-package vertico
  :init (vertico-mode +1))

(use-package vertico-posframe
  :init (vertico-posframe-mode +1))

(use-package marginalia
  :init (marginalia-mode +1)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  ;; Persist cycles
  (advice-add
   #'marginalia-cycle
   :after
   (lambda ()
     (let ((inhibit-message t))
       (customize-save-variable
        'marginalia-annotator-registry
        marginalia-annotator-registry)))))

(use-package doom-modeline
  :custom
  (doom-modeline-height    1)
  (doom-modeline-bar-width 0)
  (doom-modeline-icon    nil)
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nyan-mode
  :init (nyan-mode +1)
  :custom
  (nyan-minimum-window-width  32)
  (nyan-bar-length            16)
  (nyan-animate-nyancat      nil))

(use-package focus)

(set-face-foreground 'font-lock-comment-face "orange")
;; (lexical-let ((color-flag))
;;   (defun toggle-color ()
;;     (interactive)
;;     (setf color-flag (not color-flag))
;;     (if color-flag
;;         (set-face-foreground 'font-lock-comment-face "orange")
;;       (set-face-foreground 'font-lock-comment-face "#696969"))))

;;(set-cursor-color "#FF00FF")
(set-cursor-color "#00FF00")

(setq-default left-fringe-width 8 right-fringe-width 8
              left-margin-width 1 right-margin-width 0)

(set-fringe-style (quote (20 . 10)))
(global-set-key (kbd "M-m") 'delete-other-windows)
(dap-mode)
(save-place-mode +1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'text-mode-hook (lambda () (setq-local mode-require-final-newline nil)))
(add-hook 'package-menu-mode-hook (lambda () (hl-line-mode +1)))

(setq display-line-numbers-type t)
(setq display-line-numbers-grow-only t)

(add-hook 'ibuffer-mode-hook (lambda () (hl-line-mode +1)))

(column-number-mode +1)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package indent-bars)

(use-package doc-view
  :ensure nil
  :bind (:map
         doc-view-mode-map
         ("k" . doc-view-previous-line-or-previous-page)
         ("j" . doc-view-next-line-or-next-page)))

;; NOTE: needed to enforce it when needed, because some themes just wash it out
(set-face-attribute 'whitespace-tab nil :inverse-video t)
