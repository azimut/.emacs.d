(use-package go-eldoc)
(use-package gotest)
(use-package flymake-go)
(use-package go-guru
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier))
(use-package go-dlv
  :commands (dlv-current-func dlv)
  ;;:init
  ;;(setq go-guru-command "/usr/lib/go/bin/guru")
  )
(use-package auto-complete)
(use-package go-autocomplete)
;;(use-package company-go :ensure t)
(use-package gorepl-mode :after go-mode
  :config
  (add-hook
   'gorepl-mode-hook
   (lambda ()
     (add-to-list 'ac-modes 'gorepl-mode)
     (add-hook 'gorepl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-go)))
     (smartparens-strict-mode +1)
     (sp-use-paredit-bindings))))
;; (custom-set-faces
;;  '(company-preview
;;    ((t (:foreground "darkgray" :underline t))))
;;  '(company-preview-common
;;    ((t (:inherit company-preview))))
;;  '(company-tooltip
;;    ((t (:background "lightgray" :foreground "black"))))
;;  '(company-tooltip-selection
;;    ((t (:background "steelblue" :foreground "white"))))
;;  '(company-tooltip-common
;;    ((((type x)) (:inherit company-tooltip :weight bold))
;;     (t (:inherit company-tooltip))))
;;  '(company-tooltip-common-selection
;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
;;     (t (:inherit company-tooltip-selection)))))
(use-package go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq-local prettify-symbols-alist '(("func" . 955)
                                                   ("<-"   . ?←)))
              ;;
              (yas-minor-mode +1)
              (flycheck-mode +1)
              (define-key go-mode-map (kbd "M-p") #'flycheck-previous-error)
              (define-key go-mode-map (kbd "M-n") #'flycheck-next-error)
              ;;
              (require 'go-autocomplete)
              (require 'auto-complete-config)
              (ac-config-default)
              ;;
              ;; (set (make-local-variable 'company-backends) '(company-go))
              ;; (company-mode)
              ;;
              (go-eldoc-setup)
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (setq gofmt-command "goimports")
              (setq godoc-reuse-buffer t)
              (add-hook 'before-save-hook #'gofmt-before-save)))
  :config
  ;; (setq company-tooltip-limit 20)                      ; bigger popup window
  ;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  ;; (setq company-echo-delay 0)                          ; remove annoying blinking
  ;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  ;; (define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
  ;; (define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
  ;; (define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
  ;; (define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
  (define-key go-mode-map (kbd "M-.")     #'godef-jump)
  (define-key go-mode-map (kbd "C-c C-k") #'go-run)
  (define-key go-mode-map (kbd "C-c C-d") #'godoc-at-point))
