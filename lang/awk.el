;; https://github.com/Beaglefoot/awk-language-server
;; $ npm install -g awk-language-server
;; M-x treesit-install-language-grammar awk

(use-package awk-yasnippets)
(require 'awk-yasnippets)
(eval-after-load 'yasnippet '(awk-yasnippets-initialize))

(use-package awk-ts-mode
  :mode (("\\.awk\\'" . awk-ts-mode))
  :bind (:map
         awk-ts-mode-map
         ("C-c C-c" . recompile))
  :hook (awk-ts-mode . eglot-ensure)
  :hook (awk-ts-mode . aggressive-indent-mode)
  :hook (awk-ts-mode . smartparens-strict-mode)
  :hook (awk-ts-mode . awk-config)
  :config
  (ligature-set-ligatures
   'awk-ts-mode
   '("<=" ">=" "==" "!=" "++" "&&" "||"))
  :init
  ;; NOTE: because is not added by default, but took this from lsp server github
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(awk-ts-mode . ("awk-language-server"))))
  (defun awk-config ()
    (setq-local
     compile-command
     (concat "time gawk -f " buffer-file-name))))
