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

;; I do NOT want white spaces to be highlighted
(require 'whitespace)
(global-whitespace-mode 0)

;; disable menu-bar
;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; powerline
;; (require 'powerline)
;; (powerline-default-theme)
(require 'spaceline-config)
(spaceline-emacs-theme)

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

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

;; window resize
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle))

(use-package company
  :ensure t
  :config
  ;; #'company-complete is set per minor mode as needed
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous))

(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda ()
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (aggressive-indent-mode +1)))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs\\'"   . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'"   . glsl-mode)))

;; w3m
(use-package w3m
  :ensure nil
  :config
  (defun browse-url-other (url &rest args)
    (interactive)
    (split-window)
    (balance-windows)
    (other-window 1)
    (w3m-browse-url url))
  (setq browse-url-browser-function #'browse-url-other))

(use-package dockerfile-mode
  :ensure t)

;; jupyter elpy
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(add-hook 'yaml-mode-hook (lambda () (ansible 1)))
(add-hook 'python-mode-hook (lambda () (elpy-mode)))

(use-package go-eldoc   :ensure t)
(use-package gotest    :ensure t)
(use-package flymake-go :ensure t)
(use-package go-guru
  :ensure t
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier))
(use-package go-dlv
  :ensure t
  :commands (dlv-current-func dlv)
  :init
  (setq go-guru-command "/usr/lib/go/bin/guru"))
(use-package company-go
  :ensure t
  :config
  (setq company-go-show-annotation t)
  (define-key go-mode-map (kbd "M-TAB") #'company-complete)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-go company-yasnippet))))))
(use-package go-mode
  :ensure t
  :init
  ;; (use-package go-autocomplete
  ;;   :ensure t)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq-local prettify-symbols-alist '(("func" .  955)))
              ;;
              (flycheck-mode +1)
              (define-key go-mode-map (kbd "M-p") #'flycheck-previous-error)
              (define-key go-mode-map (kbd "M-n") #'flycheck-next-error)
              ;;
              (go-eldoc-setup)
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (company-mode +1)
              (company-quickhelp-mode +1)
              (add-hook 'before-save-hook
                        #'gofmt-before-save
                        nil :local)))
  :config
  ;; (define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
  ;; (define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
  ;; (define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
  ;; (define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
  (define-key go-mode-map (kbd "M-.")     #'godef-jump)
  (define-key go-mode-map (kbd "C-c C-k") #'go-run)
  (define-key go-mode-map (kbd "C-c C-d") #'godoc-at-point))


;;--------------------------------------------------
;; Common-lisp

(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(("lambda" . 955) ;; λ
          ;;("->" . 8594)    ; →
          ("=>" . 8658)                 ; ⇒
          (":->" . 8594)                ;
          )))

; pretty lambda
(global-prettify-symbols-mode 1)

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            ;; (projectile-mode +1)
	    ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
	    ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (aggressive-indent-mode +1)
            (yas-minor-mode +1)
	    (yas-reload-all)
            (my-add-pretty-lambda)))

;; FIX lisp ident
(put :default-initargs 'common-lisp-indent-function '(&rest))
(require 'cl-indent)
;; (define-common-lisp-style "asdf"
;;   (:inherit "modern")
;;   (:indentation
;;    (define-package  (as defpackage))
;;    (define-constant (as defconstant))))
;; (custom-set-variables '(common-lisp-style-default "asdf"))
;; (put 'if 'lisp-indent-function nil)
;; (put 'when 'lisp-indent-function 1)
;; (put 'unless 'lisp-indent-function 1)
;; (put 'do 'lisp-indent-function 2)
;; (put 'do* 'lisp-indent-function 2)

;;--------------------------------------------------
;; Common Lisp - Slime

;; cbaggers/varjo
(defun slime-vari-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((pkg (slime-current-package)))
    (slime-eval-describe
     `(vari.cl::vari-describe ,symbol-name nil ,pkg))))

(define-key lisp-mode-map (kbd "C-c C-v C-v")
  'slime-vari-describe-symbol)

;;; concurrent hints
;; https://www.reddit.com/r/lisp/comments/72v6p3/pushing_pixels_with_lisp_episode_18_shadow/
(defun slime-enable-concurrent-hints ()
  (interactive)
  (setq slime-inhibit-pipelining nil))

;; paredit on repl
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (paredit-mode +1)
            (define-key slime-repl-mode-map (kbd "C-c C-d C-d")
              #'slime-describe-symbol)))

;; (setq sly-complete-symbol-function 'sly-simple-completions
;;       inferior-lisp-program "/usr/bin/sbcl")
;; (add-hook 'sly-mode-hook (lambda () (paredit-mode +1)))
;; (setq sly-lisp-implementations
;;       '((sbcl ("set_rlimits" "sbcl"))))

;; sbcl real
(setq slime-lisp-implementations
      '((sbcl ("set_rlimits" "sbcl"))))
;;--------------------------------------------------
;; Shell
(add-hook 'sh-mode-hook
          (lambda ()
            (aggressive-indent-mode +1)
            (smartparens-strict-mode +1)
            (sp-use-paredit-bindings)))

;;--------------------------------------------------
;; Emacs
(use-package elisp-mode
  :ensure nil
  :init
  ;; Default *scratch* buffer to lexical binding.
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (equal (buffer-name) "*scratch*")
                (setq-local lexical-binding t))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (paredit-mode +1)
              (aggressive-indent-mode +1)
              (eldoc-mode +1)
              ;; SLIME like keybinding instead of "C-h f"
              (define-key emacs-lisp-mode-map (kbd "C-c C-d d")
                (lambda ()
                  (interactive)
                  (describe-symbol
                   (symbol-at-point))))
              (define-key emacs-lisp-mode-map (kbd "C-c C-d C-d")
                (lambda ()
                  (interactive)
                  (describe-symbol
                   (symbol-at-point))))
              (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'compile-defun))))
;;--------------------------------------------------

;; https://www.emacswiki.org/emacs/ShowParenMode
;; parens match
(show-paren-mode 1)

;; Fuck tilde!
(setq make-backup-files nil)

;; I want spaces for indentation
(setq-default indent-tabs-mode nil)

;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

(put 'erase-buffer 'disabled nil)

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-0") 'neotree-toggle)
  (setq neo-theme 'arrow)
  (setq neo-hidden-regexp-list
        '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$" "\\.beam$")))

;;
;; Erlang
;;
(use-package erlang
  :ensure t
  :mode
  ("\\.erl\\'" . erlang-mode)
  ("\\.hrl\\'" . erlang-mode)
  ("\\.xrl\\'" . erlang-mode)
  :init
  ;;
  (add-hook 'erlang-mode-hook
            (lambda ()
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (aggressive-indent-mode +1)
              (define-key erlang-mode-map (kbd "M-p") #'flycheck-previous-error)
              (define-key erlang-mode-map (kbd "M-n") #'flycheck-next-error)
              (flycheck-mode +1)
              ;; pretty needs to happen on init
              (setq-local prettify-symbols-alist '(("fun" .  955)
                                                   ("->"  . 8594)))
              (require 'ivy-erlang-complete)
              (add-hook 'erlang-mode-hook
                        (lambda ()
                          (ivy-erlang-complete-init)
                          (define-key erlang-mode-map (kbd "M-TAB")
                            #'ivy-erlang-complete)
                          (define-key erlang-mode-map (kbd "C-c C-d h")
                            #'ivy-erlang-complete-show-doc-at-point)))
              (add-hook 'erlang-mode-hook
                        (lambda ()
                          (add-hook 'after-save-hook
                                    #'ivy-erlang-complete-reparse
                                    nil :local)))
              (setq ivy-erlang-complete-use-default-keys t)
              ))
  :config
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-name" "emacs@sabayon"))
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (setq flycheck-display-errors-function nil)
  ;; ivy should run after flycheck...
  )


(defun hide-erlang-shell ()
  (interactive)
  (other-window 1)
  (delete-other-windows))

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (smartparens-strict-mode +1)
            (sp-use-paredit-bindings)
            (define-key erlang-shell-mode-map (kbd "C-c C-z") #'hide-erlang-shell)))

;; Elixir

;; (add-to-list 'elixir-mode-hook
;;              (defun auto-activate-ruby-end-mode-for-elixir-mode ()
;;                (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;;                     "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;;                (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;;                (ruby-end-mode +1)))

;; C / C++
(defun my-cmode-hook ()
  (setq imenu-list-auto-resize t)
  (setq zeal-at-point-docset "C")
  ;; (local-set-key (kbd "C-c C-d d")
  ;;                (lambda () (interactive) (manual-entry (current-word))))
  (ggtags-mode))
(add-hook 'c-mode-hook #'my-cmode-hook)

;; compile-mode
;; https://github.com/fsharp/zarchive-fsharpbinding/issues/246
(add-hook 'compilation-mode-hook
          (lambda ()
            ;;(setq compilation-auto-jump-to-first-error t)
            (setq compilation-scroll-output t)))

(global-set-key (kbd "C-x g") 'magit-status)

(magit-todos-mode)

;;  https://github.com/jaypei/emacs-neotree/issues/56
;;(magithub-feature-autoinject t)
;;(setq magithub-clone-default-directory "/home/sendai/quicklisp/local-projects/")
