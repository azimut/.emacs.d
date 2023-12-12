(defun elm-config ()
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq-local lsp-lens-enable nil) ; hide "exposed|references"
  (setq-local lsp-completion-show-kind nil) ; hide "(Function)"
  (setq-local prettify-symbols-alist
              '(("->"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?-
                               (Bc . Bl) ?- (Br . Br) ?>))
                ("<-"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?< (Bc . Br) ?- (Bc . Bc) ?-
                               (Bc . Bl) ?- (Br . Br) ?-))
                ("++"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?+ (Bc . Br) ?+ (Bc . Bc) ?-
                               (Bc . Bl) ?+ (Br . Br) ?+))
                ("=="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("/="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?/
                               (Bc . Bl) ?= (Br . Br) ?=)))))

(use-package elm-mode
  :hook (elm-mode . elm-config))
