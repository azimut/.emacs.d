(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'electric-pair-mode))

(use-package format-all
  :init
  (add-hook 'haskell-mode-hook #'format-all-mode))

(use-package haskell-cabal-mode
  :ensure nil
  :bind ((:map haskell-cabal-mode-map ("C-c M-o" . haskell-interactive-mode-clear))))

