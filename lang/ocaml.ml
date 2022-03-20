;; > opam install merlin ocamlformat ocaml-lsp-server utop

(use-package utop)
(use-package tuareg
  :bind (:map
         tuareg-mode-map
         ("C-c C-c" . utop-eval-phrase)
         ("C-c C-k" . utop-eval-buffer))
  :config
  (setq tuareg-prettify-symbols-full nil)
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp")))
  (put 'tuareg-mode 'eglot-language-id "ocaml")
  ;; tuareg-prettify-symbols-basic-alist
  )
(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config (setq merlin-completion-with-doc t))


