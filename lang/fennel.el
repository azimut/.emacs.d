(use-package fennel-mode
  :hook (fennel-mode . paredit-mode)
  :hook (fennel-mode . aggressive-indent-mode)
  :hook (fennel-mode . rainbow-delimiters-mode))
