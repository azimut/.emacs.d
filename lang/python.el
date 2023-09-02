;; pip install --user python-lsp-server

(defun python-config ()
  (setq-local sp-hybrid-kill-excessive-whitespace nil)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package python
  :custom
  (lsp-pylsp-plugins-autopep8-enabled t)
  :ensure nil
  :hook (python-mode . lsp-mode)
  :hook (python-mode . company-mode)
  :hook (python-mode . smartparens-strict-mode)
  :hook (python-mode . python-config))

(use-package lsp-pyright)

;; jupyter elpy
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")
;; (add-hook 'python-mode-hook (lambda () (elpy-mode)))
