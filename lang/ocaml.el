;; > opam install merlin ocamlformat ocaml-lsp-server utop
;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/ocaml/packages.el
;; https://github.com/doomemacs/doomemacs/tree/develop/modules/lang/ocaml
;; https://github.com/ocaml/merlin
;; https://github.com/emacs-lsp/lsp-ui

(use-package dune)
(use-package utop)

(use-package ocamlformat)
(defun tuareg-config ()
  (setq lsp-completion-provider :none);; have lsp-mode do not mess with -backends
  (set (make-local-variable 'company-backends)
       '((company-yasnippet :with company-capf company-files))
       ;;'(company-yasnippet company-files)
       ))

(use-package tuareg
  :custom
  (flycheck-highlighting-style '(conditional 4 (delimiters "" "«") (delimiters "" "«")))
  (flycheck-indication-mode 'right-fringe)
  :bind (
         :map
         tuareg-mode-map
         ("C-c C-c" . utop-eval-phrase)
         ("C-c C-k" . utop-eval-buffer))
  :hook (tuareg-mode . tuareg-config)
  :hook (tuareg-mode . smartparens-mode)
  :hook (tuareg-mode . lsp)
  :hook (tuareg-mode . company-mode)
  :hook (tuareg-mode . company-box-mode)
  :init
  (add-hook 'before-save-hook #'ocamlformat-before-save)
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "`" nil :actions nil)
  ;;:config
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
  :config (setq merlin-completion-with-doc t))
