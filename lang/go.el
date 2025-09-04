;; $ go install golang.org/x/tools/cmd/godoc@latest golang.org/x/tools/cmd/goimports@latest golang.org/x/tools/cmd/gorename@latest golang.org/x/tools/cmd/guru@latest golang.org/x/tools/gopls@latest
;;
;; $ go install github.com/x-motemen/gore/cmd/gore@latest
;; $ go install github.com/go-delve/delve/cmd/dlv@latest
;; $ go install github.com/mdempsky/gocode@latest
;; $ go install github.com/fatih/gomodifytags@latest
;; $ go install github.com/segmentio/golines@latest

(defun go-config ()
  (setq-local compile-command
              (cond ((file-exists-p "../../go.mod")
                     "cd ../../ && go run ...")
                    ((file-exists-p "go.mod")
                     "go run .")
                    (t (concat
                        "go run "
                        buffer-file-name))))
  (setq-local treemacs-width 50)
  (setq-local company-tooltip-align-annotations nil)
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
  (require 'smartparens-go))

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
