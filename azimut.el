; If you used option 4 above and installed your own version of the
; files and not in the site-lisp/ directory, uncomment this line and
; replace LOC below with the directory you put the files in
; (setq load-path (cons "LOC" load-path))

; I do NOT want white spaces to be highlighted
(require 'whitespace)
(global-whitespace-mode 0)

; disable menu-bar
; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Yasnippet
(yas-reload-all)

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;; FIX lisp ident
(put :default-initargs 'common-lisp-indent-function '(&rest))
(require 'slime-cl-indent)
(define-common-lisp-style "asdf"
  (:inherit "modern")
  (:indentation
   (define-package (as defpackage))
   (define-constant (as defconstant))))
(custom-set-variables '(common-lisp-style-default "asdf"))
;; (put 'if 'lisp-indent-function nil)
;; (put 'when 'lisp-indent-function 1)
;; (put 'unless 'lisp-indent-function 1)
;; (put 'do 'lisp-indent-function 2)
;; (put 'do* 'lisp-indent-function 2)

;; kill this
(global-set-key (kbd "C-x k") 'kill-this-buffer)
; window resize
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; imenu-list
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)

;; jupyter elpy
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")


; pretty lambda
(global-prettify-symbols-mode 1)
(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)            ; λ
          ;;          ("->" . 8594)    ; →
          ("=>" . 8658)                 ; ⇒
          (":->" . 8594)                ;
          )))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'sly-mrepl-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (yas-minor-mode)
            ;;(whitespace-mode 0)
            (my-add-pretty-lambda)))

(add-hook 'yaml-mode-hook (lambda () (ansible 1)))
(add-hook 'python-mode-hook (lambda () (elpy-mode)))
(add-hook 'go-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends) '(company-go))
        (company-mode)))

;; Some hacky wip emacs intergration.. still feels good though :)
;; requires slime

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

; melpa
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

; https://www.emacswiki.org/emacs/ShowParenMode
; parens match
(show-paren-mode 1)

; Fuck tilde!
(setq make-backup-files nil)

;; sbcl real
(setq slime-lisp-implementations
      '((sbcl ("set_rlimits" "sbcl")) ...))

; I want spaces for indentation
(setq-default indent-tabs-mode nil)

;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

;; Neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'arrow)
(setq neo-hidden-regexp-list
      '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$"))
(put 'erase-buffer 'disabled nil)

;; Flycheck - Erlang
(setq flycheck-check-syntax-automatically
      '(save idle-change new-line mode-enabled))

(defun my-erlang-hook ()
  "Setup for erlang."
;;  (require 'wrangler)
  (ivy-erlang-complete-init)
  (smartparens-mode)
  (flycheck-mode)
;;  (defvar erlang-extended-mode-map)
  (setq ivy-erlang-complete-use-default-keys t)
  (define-key erlang-mode-map
    (kbd "M-TAB")
    'ivy-erlang-complete)
  (define-key erlang-mode-map
    (kbd "C-c C-d h")
    'ivy-erlang-complete-show-doc-at-point)
;;  (define-key erlang-extended-mode-map (kbd "M-.") nil)
;;  (define-key erlang-extended-mode-map (kbd "M-,") nil)
;;  (define-key erlang-extended-mode-map (kbd "M-?") nil)
;;  (define-key erlang-extended-mode-map (kbd "(") nil)
  )
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)

;; Emacs
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (eldoc-mode +1)
            (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'compile-defun)))

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
          '(lambda()
;;             (setq compilation-auto-jump-to-first-error t)
             (setq compilation-scroll-output t)))


;;  https://github.com/jaypei/emacs-neotree/issues/56
