;; OS: hlint

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server")
  (lsp-haskell-server-args '("-d" "-l" "/tmp/hls.log" "-j2"))) ;; -j1 hangs

(use-package haskell-mode
  :bind (:map
         haskell-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c C-k" . haskell-process-load-file)
         ;; haskell-interactive-mode-map
         ;; ("C-c M-o" . haskell-interactive-mode-clear)
         )
  :init
  (add-hook 'haskell-mode-hook #'electric-pair-local-mode)
  :config
  (setq-local company-auto-complete-chars nil))

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(defun lsp-haskell-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'haskell-mode-hook #'lsp-haskell-install-save-hooks)

;; (use-package haskell-cabal-mode
;;   :ensure nil
;;   :bind ((:map haskell-cabal-mode-map ("C-c M-o" . haskell-interactive-mode-clear))))

