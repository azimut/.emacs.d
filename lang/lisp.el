(use-package redshank)
(require 'cl-font-lock)

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
(put 'mapping 'common-lisp-indent-function '(as let))
(put 'collect 'common-lisp-indent-function '(as progn))
;;--------------------------------------------------
;; Common Lisp - Slime

;; cbaggers/varjo
(defun sly-vari-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (sly-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((pkg (sly-current-package)))
    (sly-eval-describe
     `(vari.cl::vari-describe ,symbol-name nil ,pkg))))

;; (define-key lisp-mode-map (kbd "C-c C-v C-v")
;;   'slime-vari-describe-symbol)
;; (define-key lisp-mode-map (kbd "C-c C-a")
;;   'redshank-align-forms-as-columns)

(use-package sly
    ;;:init
    ;; (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))
    ;;       inferior-lisp-program "/usr/local/bin/sbcl"
    ;;       slime-contribs '(slime-fancy))
    ;; "modern" style
    :hook (sly-mode . paredit-mode)
    :hook (sly-mode . aggressive-indent-mode)
    :hook (sly-mode . rainbow-delimiters-mode)
    :bind (:map paredit-mode-map
                ("C-M-p" . move-text-up)
                ("RET" . nil)
                ("C-j" . paredit-newline))
    :config
    (setq sly-lisp-implementations     '((sbcl  ("/usr/local/bin/sbcl"
                                                 ;;"--dynamic-space-size"
                                                 ;;"1024"
                                                 )))
          ;;sly-complete-symbol-function 'sly-flex-completions;;'sly-simple-completions
          inferior-lisp-program        "/usr/local/bin/sbcl"
          ;;sly-contribs                 '(sly-fancy sly-macrostep)
          sly-inhibit-pipelining       nil
          sly-load-failed-fasl         'always
          sly-description-autofocus    t)
    (sly-setup)
    ;; (add-hook 'sly--completion-display-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
    (add-hook
     'sly-mode-hook
     (lambda ()
       (setq sly-lisp-lambda-list-keyword-alignment             t
             sly-lisp-lambda-list-keyword-parameter-alignment   t
             sly-lisp-lambda-list-keyword-parameter-indentation 2
             sly-lisp-loop-indent-forms-like-keywords           t
             sly-lisp-loop-indent-subclauses                    nil)
       ;; (setq redshank-align-slot-forms-list
       ;;       '("defclass" "define-condition"
       ;;         "define-subject"
       ;;         "define-shader-subject" "define-shader-entity" "define-shader-pass"))
       (display-fill-column-indicator-mode +1)
       ))
    (add-hook
     'sly-mrepl-hook
     (lambda ()
       (setq-local mode-line-format nil)
       ;; CEPL crashes if receives it, and i tend to do it as a tick...
       (define-key sly-mrepl-mode-map (kbd "C-c C-c") nil)
       (define-key sly-mrepl-mode-map (kbd "C-<escape>") #'sly-interrupt)
       ;; (setq-local show-trailing-whitespace nil)
       ;;(define-key slime-repl-mode-map (kbd "C-c C-d C-d") #'slime-describe-symbol)
       ))  )
