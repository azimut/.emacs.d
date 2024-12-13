;; https://github.com/Beaglefoot/awk-language-server
;; $ npm install -g awk-language-server
;; $ npm install -g prettier@^2 prettier-plugin-awk
;; M-x treesit-install-language-grammar awk

(use-package awk-yasnippets)
(require 'awk-yasnippets)
(eval-after-load 'yasnippet '(awk-yasnippets-initialize))

(use-package awk-mode
  :ensure nil
  :hook (awk-mode . lsp)
  :hook (awk-mode . aggressive-indent-mode)
  :hook (awk-mode . smartparens-strict-mode)
  ;;:hook (awk-mode . prettier-mode);; NOTE: faster than after-save-hook (?
  :hook (awk-mode . awk-config)
  :init
  (setenv "AWKPATH" "/usr/share/doc/gawk/examples/lib"); for LSP @include to work
  (ligature-set-ligatures
   'awk-mode
   '("<=" ">=" "==" "!=" "&&" "||"))
  (defun awk-config ()
    ;; NOTE: doesn't work on :bind
    (define-key awk-mode-map "\M-h" 'mark-defun)
    (define-key awk-mode-map "\C-c\C-c" 'recompile)
    (setq-local
     compile-command
     (concat "gawk -f " buffer-file-name))))
