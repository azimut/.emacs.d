(use-package redshank)

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

(use-package sly
  :ensure
  :init
  ;; (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))
  ;;       inferior-lisp-program "/usr/local/bin/sbcl"
  ;;       slime-contribs '(slime-fancy))
  (setq sly-lisp-implementations     '((sbcl  ("/usr/local/bin/sbcl"
                                               ;;"--dynamic-space-size"
                                               ;;"1024"
                                               )))
        sly-complete-symbol-function 'sly-simple-completions
        inferior-lisp-program        "/usr/local/bin/sbcl"
        sly-contribs                 '(sly-fancy sly-macrostep)
        sly-inhibit-pipelining       nil
        sly-load-failed-fasl         'always
        sly-description-autofocus    t)
  ;; "modern" style
  :config
  (sly-setup)
  (add-hook 'sly--completion-display-mode-hook
            (lambda () (setq-local show-trailing-whitespace nil)))
  (add-hook
   'sly-mode-hook
   (lambda ()
     (setq-local lisp-indent-function 'common-lisp-indent-function)
     (setq lisp-lambda-list-keyword-alignment             t
           lisp-lambda-list-keyword-parameter-alignment   t
           lisp-lambda-list-keyword-parameter-indentation 0
           lisp-loop-indent-forms-like-keywords           t
           lisp-loop-indent-subclauses                    nil)
     (setq redshank-align-slot-forms-list
           '("defclass" "define-condition"
             "define-subject"
             "define-shader-subject" "define-shader-entity" "define-shader-pass"))
     (paredit-mode +1)
     (yas-minor-mode +1)
     (aggressive-indent-mode +1)
     )))

(add-hook
 'sly-mrepl-hook
 (lambda ()
   (paredit-mode +1)
   (aggressive-indent-mode -1)
   (setq-local show-trailing-whitespace nil)
   ;;(define-key slime-repl-mode-map (kbd "C-c C-d C-d") #'slime-describe-symbol)
   ))
