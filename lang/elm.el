;; npm install -g elm elm-test elm-format elm-review @elm-tooling/elm-language-server

(use-package elm-mode
  :hook (elm-mode . elm-config)
  :config
  (ligature-set-ligatures
   'elm-mode
   '("<=" ">=" "==" "/=" "++" "&&" "||"
     "<-" "->" "|>" "<|" ">>" "::"))
  (defun elm-config ()
    (eglot-ensure)
    (corfu-mode +1)
    (add-hook 'before-save-hook #'eglot-format-buffer t t)))
