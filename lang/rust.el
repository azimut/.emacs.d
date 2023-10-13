(defun rust-config ()
  ;; This fixes company behevior one use::
  ;; Default: (32 41 46)
  (setq-local company-auto-complete-chars nil))

(use-package toml-mode)
(use-package rust-mode
  :hook (rust-mode . smartparens-strict-mode)
  :hook (rust-mode . lsp)
  :hook (rust-mode . rust-config)
  :config
  (rust-enable-format-on-save))
