(use-package redshank
  :ensure t)

;; lisp-mode-hook
;; sly-editing-mode
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (paredit-mode +1)
;;             ;; (projectile-mode +1)
;;             ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;             ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;             (redshank-mode +1)
;;             (aggressive-indent-mode +1)
;;             (yas-minor-mode +1)
;;             ;;(yas-reload-all)
;;             (setq prettify-symbols-alist
;;                   '(("lambda" . 955) ; λ
;;                     ;;("->"  . 8594) ; →
;;                     ;;("=>"  . 8658) ; ⇒
;;                     ;;(":->" . 8594) ;
;;                     ))))

;; FIX lisp ident
;; (put :default-initargs 'common-lisp-indent-function '(&rest))
;; (put 'defstruct-g 'common-lisp-indent-function '(as defstruct))

;;(require 'slime-autoloads)
;;(require 'slime-cl-indent)
;;(require 'sly-cl-indent)
;; (define-common-lisp-style "asdf"
;;   (:inherit "modern")
;;   (:indentation
;;    (define-package  (as defpackage))
;;    (define-constant (as defconstant))))

;; (put 'if 'lisp-indent-function nil)
;; (put 'when 'lisp-indent-function 1)
;; (put 'unless 'lisp-indent-function 1)
;; (put 'do 'lisp-indent-function 2)
;; (put 'do* 'lisp-indent-function 2)

;;--------------------------------------------------
;; Common Lisp - Slime

;; cbaggers/varjo
;; (defun slime-vari-describe-symbol (symbol-name)
;;   "Describe the symbol at point."
;;   (interactive (list (slime-read-symbol-name "Describe symbol: ")))
;;   (when (not symbol-name)
;;     (error "No symbol given"))
;;   (let ((pkg (slime-current-package)))
;;     (slime-eval-describe
;;      `(vari.cl::vari-describe ,symbol-name nil ,pkg))))

;; (define-key lisp-mode-map (kbd "C-c C-v C-v")
;;   'slime-vari-describe-symbol)
;; (define-key lisp-mode-map (kbd "C-c C-a")
;;   'redshank-align-forms-as-columns)

;;; concurrent hints
;; https://www.reddit.com/r/lisp/comments/72v6p3/pushing_pixels_with_lisp_episode_18_shadow/
;; (defun slime-enable-concurrent-hints ()
;;   (interactive)
;;   (setq slime-inhibit-pipelining nil))

;; paredit on repl
;; 'slime-repl-mode-hook

(use-package sly
  :ensure t
  :init
  ;; (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))
  ;;       inferior-lisp-program "/usr/local/bin/sbcl"
  ;;       slime-contribs '(slime-fancy))
  (setq sly-lisp-implementations     '((sbcl  ("/usr/local/bin/sbcl")))
        sly-complete-symbol-function 'sly-flex-completions
        inferior-lisp-program        "/usr/local/bin/sbcl")
  ;; "modern" style
  (setq lisp-lambda-list-keyword-alignment t
        lisp-lambda-list-keyword-parameter-alignment t
        lisp-lambda-list-keyword-parameter-indentation 0
        lisp-loop-indent-subclauses nil)
  :config
  ;;(setq sly-contribs '(sly-fancy sly-cl-indent))
  ;;(require 'sly-cl-indent)
  ;; sly-mode-hook
  (add-hook 'sly-mode-hook
            (lambda ()
              ;;(display-line-numbers-mode +1)
              (paredit-mode +1)
              (yas-minor-mode +1)
              (aggressive-indent-mode +1)
              ;; Enable sly-cl-indent
              ;;(setq-local display-line-numbers t)
              (setq-local lisp-indent-function 'common-lisp-indent-function))))

(defun sly-enable-concurrent-hints ()
  (interactive)
  (setq sly-inhibit-pipelining nil))

(add-hook
 'sly-mrepl-hook
 (lambda ()
   (paredit-mode +1)
   ;;(define-key slime-repl-mode-map (kbd "C-c C-d C-d") #'slime-describe-symbol)
   ))
