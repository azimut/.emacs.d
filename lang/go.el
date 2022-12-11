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

(defun go-config ()
  (set (make-local-variable 'company-backends) '(company-capf))
  (setq-local company-tooltip-align-annotations nil)
  (setq-local prettify-symbols-alist '(("func" . 955) ("<-"   . ?←)))
  (setq-local tab-width 4)
  (setq-local lsp-register-custom-settings '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t))))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'gofmt-before-save)
  ;;(add-hook 'before-save-hook #'lsp-format-buffer t t);; disabled when golines
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :hook (go-mode . go-config)
  :hook (go-mode . smartparens-strict-mode)
  :hook (go-mode . lsp)
  :bind (:map go-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-c" . go-run)
              ("C-j"     . newline))
  :custom
  (gofmt-command "golines")
  (lsp-go-use-placeholders t)
  :config
  (sp-use-paredit-bindings))

(sp-local-pair 'go-mode "{" nil :post-handlers '((radian-enter-and-indent-sexp "C-j")))

(use-package ob-go)    ;; Org-mode Go support
(use-package dap-mode)
(use-package godoctor) ;; Refactor
;; (use-package go-guru
;; (use-package gotest)
;;   :init
;;   (add-hook 'go-mode-hook #'go-guru-hl-identifier))

;; (use-package gorepl-mode :after go-mode
;;   :config
;;   (add-hook
;;    'gorepl-mode-hook
;;    (lambda ()
;;      (add-to-list 'ac-modes 'gorepl-mode)
;;      (add-hook 'gorepl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-go))))))
