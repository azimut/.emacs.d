(use-package cpp-auto-include
  :config
  (add-hook #'c++-mode-hook
            (lambda () (add-hook #'before-save-hook #'cpp-auto-include nil t))))
