(use-package futhark-mode
  :hook (futhark-mode . futhark-config)
  :hook (futhark-mode . smartparens-strict-mode)
  :config
  (defun futhark-config ()
    (add-hook 'before-save-hook #'futhark-fmt-buffer t t)
    (eglot-ensure)))
