;; OS: hlint

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server")
  (lsp-haskell-server-args '("-d" "-l" "/tmp/hls.log" "-j2")))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'electric-pair-mode))

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
;; (use-package haskell-cabal-mode
;;   :ensure nil
;;   :bind ((:map haskell-cabal-mode-map ("C-c M-o" . haskell-interactive-mode-clear))))

