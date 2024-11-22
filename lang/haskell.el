;; OS: hlint
;; (haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-server-args '("-j2"))) ;; -j1 hangs

(use-package haskell-interactive-mode
  :ensure nil
  :bind (:map
         haskell-interactive-mode-map
         ;; ("C-c M-o" . haskell-interactive-mode-clear)
         ("C-c C-c" . recompile))
  :config
  (unbind-key "C-c C-c" haskell-interactive-mode-map))

(use-package haskell-mode
  :bind (:map
         haskell-mode-map
         ("C-j" . haskell-indentation-newline-and-indent)
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c C-k" . haskell-process-load-file))
  :hook (haskell-mode . smartparens-strict-mode)
  :hook (haskell-mode . haskell-config)
  :hook (haskell-mode . lsp)
  :config
  (ligature-set-ligatures
   'haskell-mode
   '(">>=" "=<<" "=>" "->" "<-" "-<" ">-" "++" "==" "/=" "<>" "||"))
  :init
  (defun haskell-config ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)))

(add-hook 'haskell-literate-mode-hook #'lsp)

;; (use-package haskell-cabal-mode
;;   :ensure nil
;;   :bind ((:map haskell-cabal-mode-map ("C-c M-o" . haskell-interactive-mode-clear))))
