;; https://github.com/Beaglefoot/awk-language-server
;; $ npm install -g awk-language-server
;; M-x treesit-install-language-grammar awk

(use-package awk-yasnippets)
(require 'awk-yasnippets)
(eval-after-load 'yasnippet '(awk-yasnippets-initialize))

(use-package awk-mode
  :ensure nil
  :bind (:map
         awk-mode-map
         ("C-c C-c" . recompile))
  :hook (awk-mode . eglot-ensure)
  :hook (awk-mode . aggressive-indent-mode)
  :hook (awk-mode . smartparens-strict-mode)
  :hook (awk-mode . awk-config)
  :config
  (ligature-set-ligatures
   'awk-mode
   '("<=" ">=" "==" "!=" "++" "&&" "||"))
  :init
  ;; NOTE: because is not added by default, but took this from lsp server github
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(awk-mode . ("awk-language-server"))))
  (defun awk-config ()
    (setq-local eglot-stay-out-of '(eldoc))
    (setq-local
     compile-command
     (concat "time gawk -f " buffer-file-name))))
