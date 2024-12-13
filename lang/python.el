(use-package python
  :ensure nil
  :hook (python-mode . eglot-ensure)
  :hook (python-mode . corfu-mode)
  :hook (python-mode . indent-bars-mode)
  :hook (python-mode . smartparens-strict-mode)
  :hook (python-mode . python-config)
  :hook (inferior-python-mode . corfu-mode)
  :init
  (defun python-config ()
    ;;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (setq-local sp-hybrid-kill-excessive-whitespace nil)))

;; (add-hook 'python-mode-hook (lambda () (elpy-mode)))
