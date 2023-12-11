(use-package awk-yasnippets)
(require 'awk-yasnippets)
(eval-after-load 'yasnippet '(awk-yasnippets-initialize))

(defun awk-config ()
  (setq-local prettify-symbols-alist
              '(("=="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?=
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("/="  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?= (Bc . Br) ?= (Bc . Bc) ?/
                               (Bc . Bl) ?= (Br . Br) ?=))
                ("++"  .  (?\s (Br . Bl) ?\s
                               (Bl . Bl) ?+ (Bc . Br) ?+ (Bc . Bc) ?-
                               (Bc . Bl) ?+ (Br . Br) ?+))))
  (setq-local compile-command (concat "time gawk -f " buffer-file-name)))

;; https://github.com/Beaglefoot/awk-language-server
;; $ npm install -g awk-language-server
(use-package awk-mode
  :ensure nil
  :bind (:map
         awk-mode-map
         ("C-c C-c" . recompile))
  :hook (awk-mode . lsp-mode)
  :hook (awk-mode . aggressive-indent-mode)
  :hook (awk-mode . smartparens-strict-mode)
  :hook (awk-mode . awk-config))
