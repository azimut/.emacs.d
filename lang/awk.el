;; https://github.com/Beaglefoot/awk-language-server
;; $ npm install -g awk-language-server
;; M-x treesi-install-language-grammar awk

(use-package awk-yasnippets)
(require 'awk-yasnippets)

(use-package awk-ts-mode
  :mode (("\\.awk\\'" . awk-ts-mode))
  :bind (:map
         awk-ts-mode-map
         ("C-c C-c" . recompile))
  :hook (awk-ts-mode . lsp-mode)
  ;; :hook (awk-mode . awk-yasnippets-initialize)
  :hook (awk-ts-mode . aggressive-indent-mode)
  :hook (awk-ts-mode . smartparens-strict-mode)
  :hook (awk-ts-mode . awk-config)
  :config
  (ligature-set-ligatures
   'awk-ts-mode
   '("<=" ">=" "==" "!=" "++" "&&" "||"))
  (defun awk-config ()
    (setq-local
     compile-command
     (concat "time gawk -f " buffer-file-name))))
