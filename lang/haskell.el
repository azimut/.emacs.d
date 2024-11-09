;; OS: hlint

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-server-args '("-d" "--log-file" "/tmp/hls.log" "-j2"))) ;; -j1 hangs

(use-package haskell-interactive-mode
  :ensure nil
  :bind (:map
         haskell-interactive-mode-map
         ;; ("C-c M-o" . haskell-interactive-mode-clear)
         ("C-c C-c" . recompile))
  :config
  (unbind-key "C-c C-c" haskell-interactive-mode-map))


(defun haskell-config ()
  ;; Pretty symbols that are as wider as thier characters more or less...
  ;; https://gist.github.com/m-renaud/2c085d453b1263f1a6ed52d0c90688de
  ;; https://github.com/pjones/emacsrc/blob/e6207555f1ab04fd056777e7e918bc4df47db593/modes/haskell-mode-conf.el#L269
  (setq-local prettify-symbols-alist
              '((">>=" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                               (Bl . Bl) ?> (Bc . Bc) ?>
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("=<<" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?<
                               (Bl . Bl) ?= (Br . Br) ?<))
                ("=>"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                               (Br . Br) ?>))
                ("->"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?-
                               (Bc . Bl) ?- (Br . Br) ?>))
                ("<-"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?< (Bc . Br) ?- (Bc . Bc) ?-
                               (Bc . Bl) ?- (Br . Br) ?-))
                ("-<"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?- (Bc . Bc) ?- (Br . Br) ?<))
                (">-"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?> (Bc . Bc) ?- (Br . Br) ?-))
                ("++"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?+ (Bc . Br) ?+ (Bc . Bc) ?-
                               (Bc . Bl) ?+ (Br . Br) ?+))
                ("=="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("/="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?/
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("<>"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?< (Bc . Br) ?<
                               (Bc . Bl) ?> (Br . Br) ?>))))
  (setq-local company-auto-complete-chars nil))

(use-package haskell-mode
  :bind (:map
         haskell-mode-map
         ("C-j" . haskell-indentation-newline-and-indent)
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c C-k" . haskell-process-load-file))
  :hook (haskell-mode . smartparens-strict-mode)
  :hook (haskell-mode . prettify-symbols-mode)
  :hook (haskell-mode . haskell-config)
  :hook (haskell-mode . lsp))

(add-hook 'haskell-literate-mode-hook #'lsp)

(defun lsp-haskell-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'haskell-mode-hook #'lsp-haskell-install-save-hooks)

;; (use-package haskell-cabal-mode
;;   :ensure nil
;;   :bind ((:map haskell-cabal-mode-map ("C-c M-o" . haskell-interactive-mode-clear))))
