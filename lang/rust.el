;; OS: install "rls"

(defun rust-config ()
  (rust-enable-format-on-save)
  (smartparens-strict-mode +1)
  (sp-use-paredit-bindings)
  ;; This fixes company behevior one use::
  ;; Default: (32 41 46)
  (setq-local company-auto-complete-chars nil))

(use-package toml-mode)
(use-package rust-mode
  :hook (rust-mode . lsp)
  :init
  (add-hook #'rust-mode-hook
            #'rust-config))
