(defun gleam-config ()
  (setq-local create-lockfiles nil)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos)
  :hook (gleam-ts-mode . gleam-config)
  :hook (gleam-ts-mode . lsp)
  :config
  (ligature-set-ligatures
   'gleam-ts-mode
   '("==" "!=" ">=" "<=" "&&" "||" "->" "|>")))
