(use-package futhark-mode
  :hook (futhark-mode . futhark-config)
  :config
  (defun futhark-config ()
    (add-hook 'before-save-hook #'futhark-fmt-buffer t t)
    (eglot-ensure)))
