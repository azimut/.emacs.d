(use-package flycheck
  :hook (flycheck-mode . display-line-numbers-mode)
  :bind (:map
         flycheck-error-list-mode-map
         ("M-e" . spacemacs/toggle-flycheck-error-list)
         :map
         flycheck-mode-map
         ("M-e"     . spacemacs/goto-flycheck-error-list)
         ("M-n"     . flycheck-next-error)
         ("M-p"     . flycheck-previous-error))
  :custom
  (flycheck-highlighting-style '(conditional 4 nil (delimiters "" "«")))
  (flycheck-indication-mode    'left-margin)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
  (defun spacemacs/goto-flycheck-error-list ()
    "Open and go to the error list buffer."
    (interactive)
    (if (flycheck-get-error-list-window)
        (switch-to-buffer flycheck-error-list-buffer)
      (progn
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer))))

  (defun spacemacs/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window)
      (flycheck-list-errors)))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;; (use-package flycheck-inline
;;   :init (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

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

(use-package company-prescient)
(use-package company-box :custom (company-box-doc-delay 0.2))
(use-package company-quickhelp :custom
  (company-quickhelp-delay 0.2))

(use-package lsp-mode
  :custom
  (lsp-disabled-clients '((web-mode . eslint) (web-tsx-mode . nil)))
  (lsp-headerline-breadcrumb-enable nil)
  :bind (:map
         lsp-mode-map
         ("C-j"     . newline);; override electric-mode-newline
         ("C-c C-d" . lsp-describe-thing-at-point)
         ("C-c d"   . lsp-describe-thing-at-point)))

(use-package aggressive-indent)
(use-package ggtags)

(defun yasnippet-config ()
  (set (make-local-variable require-final-newline) nil))

(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :init (yas-global-mode +1)
  :custom (yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  :hook (snippet-mode . yasnippet-config)
  ;; :bind (         ;; ("<tab>" . nil)
  ;;        ;; ("TAB"   . nil)
  ;;        ("M-SPC" . #'yas-maybe-expand)
  ;;        ("C-c y" . yas-expand))
  )

;; https://stackoverflow.com/questions/25521897/how-to-never-expand-yasnippets-in-comments-and-strings
(defun yas-no-expand-in-comment/string ()
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))
;;(add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)

(use-package smartparens
  :custom (sp-base-key-bindings 'paredit))
(defun radian-enter-and-indent-sexp (&rest _ignored)
  "Insert an extra newline after point, and reindent.
   https://github.com/Fuco1/smartparens/issues/80"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package dap-mode
  :bind (:map
         dap-mode-map
         ("<f12>" . dap-ui-locals)
         ("<f10>" . dap-continue)
         ("<f9>"  . dap-next)
         ("<f8>"  . dap-step-out)
         ("<f7>"  . dap-step-in))
  :custom
  (dap-output-window-min-height 5)
  (dap-output-window-max-height 5)
  (dap-ui-locals-expand-depth   1)
  (dap-ui-default-fetch-count  25)
  (dap-debug-restart-keep-session nil)
  (dap-auto-configure-features '(sessions locals breakpoints expressions))
  :config
  (custom-set-faces
   '(dap-ui-pending-breakpoint-face ((t (:background "dark gray" :foreground "black"))))
   '(dap-ui-verified-breakpoint-face ((t (:background "green" :foreground "black")))))
  (define-key dap-mode-map (kbd "C-c C-s") #'dap-breakpoint-toggle))

(use-package cheat-sh)
(use-package tree-sitter
  :config
  (global-tree-sitter-mode +1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
