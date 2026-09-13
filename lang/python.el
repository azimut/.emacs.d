;; sudo ln -s /usr/bin/python3 /usr/bin/python
;; pip3 install --user debugpy
(use-package python
  :ensure nil
  ;;:hook (python-mode . eglot-ensure)
  :hook (python-mode . corfu-mode)
  :hook (python-mode . indent-bars-mode)
  :hook (python-mode . display-line-numbers-mode)
  :hook (python-mode . smartparens-strict-mode)
  :hook (python-mode . python-config)
  :hook (inferior-python-mode . corfu-mode)
  :bind (:map
         python-mode-map
         ("C-c C-k" . recompile))
  :init
  (defun python-config ()
    (prettify-symbols-mode -1)
    (setq-local sp-hybrid-kill-excessive-whitespace nil)))

;; (add-hook 'python-mode-hook (lambda () (elpy-mode)))
