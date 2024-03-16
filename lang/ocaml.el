;; > opam install merlin ocamlformat ocaml-lsp-server utop
;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/ocaml/packages.el
;; https://github.com/doomemacs/doomemacs/tree/develop/modules/lang/ocaml
;; https://github.com/ocaml/merlin
;; https://github.com/emacs-lsp/lsp-ui

(use-package dune)
(use-package utop
  :hook (utop-mode . scroll-bar-mode)
  :config
  (add-hook 'utop-mode-hook (lambda () (setq-local mode-line-format nil))))

(use-package ocamlformat)
(defun tuareg-config ()
  (setq lsp-completion-provider :none) ;; have lsp-mode do not mess with -backends
  (set (make-local-variable 'company-backends)
       '((company-yasnippet :with company-capf company-files))
       ;;'(company-yasnippet company-files)
       ))

(use-package tuareg
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs")
  :bind (
         :map
         tuareg-mode-map
         ("C-c C-z" . utop-switch-to-repl)
         ("C-c C-c" . utop-eval-phrase)
         ("C-c C-k" . utop-eval-buffer))
  :hook (tuareg-mode . tuareg-config)
  :hook (tuareg-mode . smartparens-mode)
  :hook (tuareg-mode . company-mode)
  :hook (tuareg-mode . company-box-mode)
  :hook (tuareg-mode . utop-minor-mode)
  :hook (tuareg-mode . lsp)
  :init
  (add-hook 'before-save-hook #'ocamlformat-before-save)
  :config
  (ligature-set-ligatures 'tuareg-mode '("<=" ">=" "==" "<>" "&&" "||" "<-" "->" "|>"))
  (require 'smartparens-ml)
  ;; (setq tuareg-prettify-symbols-full nil)
  ;; (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp")))
  ;; (put 'tuareg-mode 'eglot-language-id "ocaml")
  ;; tuareg-prettify-symbols-basic-alist
  )

(use-package merlin
  ;; :bind (:map
  ;;        merlin-mode-map
  ;;        ("C-c C-d" . nil))
  ;;:hook (tuareg-mode . merlin-mode)
  :config (setq merlin-completion-with-doc nil))
