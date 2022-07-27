;; pip install --user python-lsp-server

(use-package python
  :ensure nil
  :hook (python-mode . lsp)
  :hook (python-mode . smartparens-mode)
  :config
  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p)))

(use-package lsp-pyright)

;; jupyter elpy
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")
(add-hook 'python-mode-hook (lambda () (elpy-mode)))
