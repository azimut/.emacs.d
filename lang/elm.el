(defun elm-config ()
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
