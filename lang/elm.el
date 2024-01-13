;; npm install -g elm elm-test elm-format elm-review

(defun elm-config ()
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq-local lsp-lens-enable nil)          ; hide "exposed|references"
  (setq-local lsp-completion-show-kind nil) ; hide "(Function)"
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package elm-mode
  :hook (elm-mode . elm-config)
  :config
  (ligature-set-ligatures 'elm-mode '("<=" ">=" "==" "/=" "++" "&&" "||" "<-" "->" "|>" "<|" ">>" "::")))
