;; OS:
;; GO111MODULE=on go get -v golang.org/x/tools/gopls@latest
;; go get -u -v github.com/godoctor/godoctor
;; go get -u -v golang.org/x/tools/cmd/guru

(use-package go-mode
  :config
  (define-key go-mode-map (kbd "M-p") #'flycheck-previous-error)
  (define-key go-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key go-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point))

(use-package ob-go)    ;; Org-mode Go support

(use-package godoctor) ;; Refactor
(use-package go-guru
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier))

(use-package company-go)
(use-package flycheck-golangci-lint)

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
(add-hook 'go-mode-hook 'gofly-setup t)

(defun go-config ()
  (company-mode +1)
  (smartparens-strict-mode +1)
  (sp-use-paredit-bindings)
  (setq-local prettify-symbols-alist '(("func" . 955)
                                       ("<-"   . ?←)))
  (setq-local tab-width 4)
  (setq-local lsp-diagnostics-provider :none)
  (set (make-local-variable 'company-backends) '(company-capf)))
(add-hook 'go-mode-hook #'go-config)

(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
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
