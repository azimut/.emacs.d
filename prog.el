(use-package flycheck)
(use-package flycheck-inline
  :init (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package company
  :bind (:map
         company-active-map
         ("<tab>" . company-complete-selection)
         ("M-n"   . nil)
         ("M-p"   . nil)
         ("C-n"   . company-select-next)
         ("C-p"   . company-select-previous)
         ("C-n"   . company-select-next)
         ("C-p"   . company-select-previous))
  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-insertion-on-trigger      nil)
  (company-show-numbers              nil)
  (company-minimum-prefix-length     1)
  (company-tooltip-limit             30)
  (company-tooltip-align-annotations t)
  (company-idle-delay                0.1)
  (company-begin-commands '(self-insert-command))
  ;;(setq company-tooltip-align-annotations nil)
  )

(use-package company-box :custom (company-box-doc-delay 0.2))
(use-package company-quickhelp :custom
  (company-quickhelp-delay 0.2))

(use-package lsp-mode
  :custom
  (lsp-disabled-clients '((web-mode . eslint) (web-tsx-mode . nil)))
  (lsp-headerline-breadcrumb-enable nil)
  :bind (:map
         lsp-mode-map
         ("M-n"     . flycheck-next-error)
         ("M-p"     . flycheck-previous-error)
         ("M-e"     . flycheck-list-errors)
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c d"   . lsp-describe-thing-at-point)))

(use-package aggressive-indent)
(use-package ggtags)

(defun yasnippet-config ()
  (set (make-local-variable require-final-newline) nil))

(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets
  :custom (yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  :hook (snippet-mode . yasnippet-config)
  :bind (:map
         yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB"   . nil)
         ("M-SPC" . yas-maybe-expand)
         ("C-c y" . yas-expand)))

;; https://stackoverflow.com/questions/25521897/how-to-never-expand-yasnippets-in-comments-and-strings
(defun yas-no-expand-in-comment/string ()
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))
;;(add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)

(use-package smartparens)
(defun radian-enter-and-indent-sexp (&rest _ignored)
  "Insert an extra newline after point, and reindent.
   https://github.com/Fuco1/smartparens/issues/80"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
