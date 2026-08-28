;; npm install -g elm elm-test elm-format elm-review @elm-tooling/elm-language-server

(use-package elm-mode
  :hook (elm-mode . elm-config)
  :hook (elm-mode . lsp)
  :config
  (ligature-set-ligatures
   'elm-mode
   '("<=" ">=" "==" "/=" "++" "&&" "||"
     "<-" "->" "|>" "<|" ">>" "::")))
