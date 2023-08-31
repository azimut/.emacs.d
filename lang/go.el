;; OS:
;; $ go install github.com/go-delve/delve/cmd/dlv@latest
;; - https://github.com/go-delve/delve/tree/master/Documentation/installation
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
  (setq-local compile-command
              (cond ((file-exists-p "../../go.mod")
                     "cd ../../ && go run ...")
                    ((file-exists-p "go.mod")
                     "go run .")
                    (t (concat
                        "go run "
                        buffer-file-name))))
  (setq-local dap-auto-configure-features '(locals repl expressions))
  (setq-local treemacs-width 50)
  (setq-local dap-ui-buffer-configurations
              `(("*dap-ui-locals*"      . ((side . right)  (slot . 1) (window-width . 0.4) (window-height . 0.4)))
                ;; ("*dap-ui-breakpoints*" . ((side . right)  (slot . 2) (window-width . 0.4) (window-height . 0.15)))
                ("*dap-ui-expressions*" . ((side . right)  (slot . 2) (window-width . 0.4) (window-height . 0.3)))
                ("*dap-ui-repl*"        . ((side . right)  (slot . 3) (window-width . 0.4) (window-height . 0.3)))
                ;; ("*dap-ui-sessions*"    . ((side . right)  (slot . 3) (window-width . 0.4) (window-height . 0.3)))
                ;; ("*debug-window*"       . ((side . right) (slot . 4) (window-width . 0.15)))
                ))
  (setq-local dap-auto-show-output nil)
  (setq-local company-tooltip-align-annotations nil)
  (setq-local dap-ui-default-fetch-count  25)
  (setq-local prettify-symbols-alist '(("func" . 955) ("<-"   . ?←)))
  (setq-local tab-width 4)
  (setq-local lsp-register-custom-settings '(("gopls.completeUnimported" t t)
                                             ("gopls.staticcheck" t t))))

(defun adjust-lsp-treemacs-symbols-window ()
  (let* ((buf (get-buffer "*LSP Symbols List*"))
         (win (get-buffer-window buf))
         (width (window-width win))
         (window-size-fixed))
    (window-resize win (- sidebar-width width) t)))
(advice-add 'lsp-treemacs-symbols :after #'adjust-lsp-treemacs-symbols-window)

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
  :hook (go-mode . company-mode)
  :bind (:map go-mode-map
              ("M-n"     . move-text-down)
              ("M-p"     . move-text-up)
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-c" . recompile)
              ("C-c C-k" . go-test-current-file)
              ("C-j"     . nil))
  :custom
  (gofmt-command "golines")
  (lsp-go-use-placeholders t)
  :config
  (require 'dap-dlv-go))

(use-package ob-go)    ;; Org-mode Go support
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
