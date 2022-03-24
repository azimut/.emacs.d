;; OS:
;; GO111MODULE=on go get -v golang.org/x/tools/gopls@latest
;; go get -u github.com/motemen/gore/cmd/gore
;; go get -u github.com/stamblerre/gocode
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/segmentio/golines
(use-package go-mode
  :config
  ;; (define-key go-mode-map (kbd "M-p") #'flycheck-previous-error)
  ;; (define-key go-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key go-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point))

(use-package ob-go)    ;; Org-mode Go support

(use-package godoctor) ;; Refactor
;; (use-package go-guru
;;   :init
;;   (add-hook 'go-mode-hook #'go-guru-hl-identifier))
(add-hook 'go-mode-hook 'gofly-setup t)
;;(use-package flycheck-golangci-lint)
(defun gofly-setup ()
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-errcheck
                                     go-staticcheck
                                     go-unconvert))
  (flycheck-golangci-lint-setup)
  (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
  (flycheck-add-next-checker 'go-test  '(warning . golangci-lint) t))

(defun go-config ()
  (smartparens-strict-mode +1)
  (sp-use-paredit-bindings)
  (setq-local prettify-symbols-alist '(("func" . 955) ("<-"   . ?←)))
  (setq-local tab-width 4)
  (setq-local lsp-diagnostics-provider :none)
  (setq-local lsp-register-custom-settings '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t)))
  (setq-local company-auto-complete-chars nil);; has to be something better
  ;; (setq company-insertion-triggers nil)
  (setq company-auto-complete nil)
  (setq company-go-show-annotation t)
  (set (make-local-variable 'company-backends) '(company-capf))
  (flycheck-mode +1)
  ;;(company-mode +1)
  )
(add-hook 'go-mode-hook #'go-config)

(add-hook 'go-mode-hook #'lsp-deferred)

(defun lsp-go-install-save-hooks ()
  (setq gofmt-command "golines")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; dap-go ???

;; (use-package gotest)
;; (use-package gorepl-mode :after go-mode
;;   :config
;;   (add-hook
;;    'gorepl-mode-hook
;;    (lambda ()
;;      (add-to-list 'ac-modes 'gorepl-mode)
;;      (add-hook 'gorepl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-go))))))

;; (use-package go-mode
;;   :config
;;   ;; (setq company-tooltip-limit 20)                      ; bigger popup window
;;   ;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
;;   ;; (setq company-echo-delay 0)                          ; remove annoying blinking
;;   ;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;;   (define-key go-mode-map (kbd "C-c C-k") #'go-run)
;;   )
