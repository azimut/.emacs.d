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

;; (defvar-local flycheck-local-checkers nil)
;; (defun +flycheck-checker-get(fn checker property)
;;   (or (alist-get property (alist-get checker flycheck-local-checkers))
;;       (funcall fn checker property)))
;; (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(defun go-config ()
  ;;(set (make-local-variable 'company-backends) '(company-capf))
  ;;(setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))
  ;;(setq-local flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  ;;(setq-local lsp-diagnostics-provider :none)
  ;;(flycheck-mode +1)
  (setq-local prettify-symbols-alist '(("func" . 955) ("<-"   . ?←)))
  (setq-local tab-width 4)
  (setq-local lsp-register-custom-settings '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t))))

(defun lsp-go-install-save-hooks ()
  ;;(add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :hook (go-mode . go-config)
  :hook (go-mode . lsp)
  :hook (go-mode . smartparens-strict-mode)
  :bind (:map go-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ;;("C-j" . newline)
              )
  :custom
  (gofmt-command "golines")
  (lsp-go-use-placeholders t)
  :config
  (sp-local-pair 'go-mode "{" nil :post-handlers '((radian-enter-and-indent-sexp "C-j")))
  (sp-use-paredit-bindings))

(use-package ob-go)    ;; Org-mode Go support
(use-package godoctor) ;; Refactor
;; (use-package go-guru
;;   :init
;;   (add-hook 'go-mode-hook #'go-guru-hl-identifier))
;; (use-package flycheck-golangci-lint)
;; (defun gofly-setup ()
;;   (setq flycheck-disabled-checkers
;;         '(go-gofmt
;;           go-golint
;;           go-vet
;;           go-errcheck
;;           go-staticcheck
;;           go-unconvert))
;;   (flycheck-golangci-lint-setup)
;;   (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
;;   (flycheck-add-next-checker 'go-test  '(warning . golangci-lint) t))
;;(add-hook 'go-mode-hook 'gofly-setup t)


;; dap-go ???
;; (use-package gotest)
;; (use-package gorepl-mode :after go-mode
;;   :config
;;   (add-hook
;;    'gorepl-mode-hook
;;    (lambda ()
;;      (add-to-list 'ac-modes 'gorepl-mode)
;;      (add-hook 'gorepl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-go))))))
