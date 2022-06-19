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

;;
;;(add-hook 'after-init-hook (lambda () (load-theme 'kaolin-galaxy t)))

(add-hook
 'Buffer-menu-mode-hook
 (lambda () (setq-local show-trailing-whitespace nil)))

(add-hook
 'shell-mode-hook
 (lambda () (setq-local show-trailing-whitespace nil)))

(add-hook
 'vterm-mode-hook
 (lambda () (setq-local show-trailing-whitespace nil)))

;; https://github.com/syl20bnr/spacemacs/issues/12535
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; other WORKAROUND:
;;(setq package-check-signature nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; disable menu-bar
;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

;; https://www.emacswiki.org/emacs/ShowParenMode
;; parens match
(show-paren-mode 1)

;; Fuck tilde!
(setq make-backup-files nil)

;; I want spaces for indentation
(setq-default indent-tabs-mode nil)

(put 'erase-buffer 'disabled nil)

;;(grep-apply-setting 'grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-command
      (quote
       ("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49)))
(setq grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-template
      "find <D> <X> -type f <F> -exec grep <C> --exclude='*.svn-base' -nH --null -e <R> \\{\\} +")

;; https://stackoverflow.com/questions/40060220/how-to-prevent-emacs-from-scrolling-with-the-mouse-past-the-end-of-the-buffer
;; Avoids SOME mouse scrolling after file ended, to mid screen last line
(setq scroll-conservatively 101)
(setq next-line-add-newlines nil)

;;--------------------------------------------------

;; Emacs Tutorial 9 - Switching windows in a smart way!
;; https://www.youtube.com/watch?v=dppr_js-U0s&t=474s
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; kill this
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; pretty lambda
(global-prettify-symbols-mode 1)

;; window resize
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;--------------------------------------------------

;; NOTE: need a compositor
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;;--------------------------------------------------
;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;--------------------------------------------------

(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face empty))

(setq dired-listing-switches "-lh")
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|*.fasl"))
(setq use-package-always-ensure t)

(use-package smartparens)
(use-package phi-search
  :config (require 'phi-search)
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward))

(use-package spaceline
  :config (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package imenu-list
  :bind ("C-'" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t))

(use-package company
  :bind (:map
         company-active-map
         ("<tab>" . company-complete-selection))
  :config
  (setq company-show-numbers              nil
        company-minimum-prefix-length     2
        company-tooltip-limit             30
        company-tooltip-align-annotations t
        company-idle-delay                0.1
        company-begin-commands '(self-insert-command))
  (setq company-auto-complete t)

  ;;(setq company-tooltip-align-annotations nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous))

(use-package aggressive-indent)

(use-package string-inflection
  :bind ("C-c j" . string-inflection-toggle))

(setq browse-url-firefox-program "/snap/bin/firefox")
(use-package w3m
  :ensure nil
  :config
  (defun browse-url-other (url &rest args)
    (interactive)
    (split-window)
    (balance-windows)
    (other-window 1)
    (w3m-browse-url url))
  (setq browse-url-browser-function #'browse-url-other)
  (add-hook 'w3m-mode-hook
            (lambda ()
              (local-set-key "\C-n"   'w3m-next-anchor)
              (local-set-key "\C-p"   'w3m-previous-anchor)
              (local-set-key '[up]    'previous-line)
              (local-set-key '[down]  'next-line)
              (local-set-key '[left]  'backward-char)
              (local-set-key '[right] 'forward-char))))

(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config (yas-global-mode +1)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB")   nil)
  ;; Bind `SPC' to `yas-expand' when snippet expansion available (it
  ;; will still call `self-insert-command' otherwise).
  (define-key yas-minor-mode-map (kbd "M-SPC") yas-maybe-expand)
  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)
  (add-hook 'snippet-mode-hook
            (lambda ()
              (set (make-local-variable require-final-newline) nil))))

(use-package neotree
  :bind ("C-0" . neotree-toggle)
  :config
  (setq neo-theme 'arrow)
  (setq neo-hidden-regexp-list
        '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$" "\\.beam$" "\\.meta$")))

(use-package ggtags)

;; compile-mode
;; https://github.com/fsharp/zarchive-fsharpbinding/issues/246
(add-hook 'compilation-mode-hook
          (lambda ()
            ;;(setq compilation-auto-jump-to-first-error t)
            (setq compilation-scroll-output t)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-git-executable "/usr/bin/git"))

(use-package magit-todos
  :config (magit-todos-mode +1))

(use-package forge
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq forge-topic-list-limit '(100 . 0)))

(use-package gitignore-templates)
(use-package gitignore-mode)
(use-package git-timemachine)

(use-package dash)

(use-package helm-dash
  :config
  (setq dash-docs-browser-func 'eww)
  (add-hook
   'eww-mode-hook
   (lambda ()
     (setq dash-docs-enable-debugging nil)
     (setq-local show-trailing-whitespace nil))))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package lice)
(use-package systemd)
(use-package vterm)
(use-package ag
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window     t))

(use-package flycheck)
(use-package flycheck-inline
  :init (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package lsp-mode
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)
         ("M-e" . flycheck-list-errors)))

(add-hook
 'makefile-mode
 (lambda () (setq-local whitespace-style '(face tabs empty))))

(use-package lorem-ipsum)

(use-package evil-numbers
  ;; (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  ;; (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
  ;; (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
  ;; (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)
         ("C-c C-+" . evil-numbers/inc-at-pt-incremental)
         ("C-c C--" . evil-numbers/dec-at-pt-incremental)))

;; https://stackoverflow.com/questions/25521897/how-to-never-expand-yasnippets-in-comments-and-strings
(defun yas-no-expand-in-comment/string ()
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))

(use-package vertico
  :init (vertico-mode +1))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recently-active))

;;(add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)
;;-------------------------------------------------
(load-file "~/.emacs.d/lang/shell.el")
(load-file "~/.emacs.d/lang/elisp.el")
;;(load-file "~/.emacs.d/lang/erlang.el")
(load-file "~/.emacs.d/lang/lisp.el")
(load-file "~/.emacs.d/lang/clojure.el")
(load-file "~/.emacs.d/lang/go.el")
(load-file "~/.emacs.d/lang/rust.el")
(load-file "~/.emacs.d/lang/markup.el")
;; (load-file "~/.emacs.d/lang/elixir.el")
(load-file "~/.emacs.d/lang/lua.el")
(load-file "~/.emacs.d/lang/glsl.el")
;;(load-file "~/.emacs.d/lang/livecoding.el")
(load-file "~/.emacs.d/lang/c.el")
(load-file "~/.emacs.d/lang/haskell.el")
;;(load-file "~/.emacs.d/lang/chuck.el")
(load-file "~/.emacs.d/lang/ocaml.el")

(load-file "~/.emacs.d/lang/html.el")
(load-file "~/.emacs.d/lang/javascript.el")

;; NOSY comments
(set-face-foreground 'font-lock-comment-face "orange")
;; (set-cursor-color "#FFC0CB")
(set-cursor-color "#FF00FF")
