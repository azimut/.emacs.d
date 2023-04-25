(use-package electric
  :ensure nil
  :custom (electric-pair-delete-adjacent-pairs nil))

(use-package flycheck
  :hook (flycheck-mode . display-line-numbers-mode)
  :bind (:map
         flycheck-error-list-mode-map
         ("M-e" . spacemacs/toggle-flycheck-error-list)
         :map
         flycheck-mode-map
         ("M-e"      . spacemacs/goto-flycheck-error-list)
         ("<C-down>" . flycheck-next-error)
         ("<C-up>"   . flycheck-previous-error))
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

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

;; CHAT-GPT :) ... plus hacky stuff
(defun my-company-or-yas-next ()
  "Complete with company if active, otherwise go to next yasnippet field."
  (interactive)
  (cond
   ((yas-expand-from-trigger-key) nil)
   ((and (boundp 'yas--active-field-overlay)
         (not (numberp (yas--field-start (overlay-get yas--active-field-overlay 'yas--field)))))
    (yas-next-field))
   (t (company-complete-common-or-cycle))))

(use-package company
  :bind (:map
         company-active-map
         ("<backtab>" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
         ("<tab>"   . my-company-or-yas-next)
         ("M-n"   . nil); Deprecated bindings by upstream
         ("M-p"   . nil); Deprecated bindings by upstream
         ("C-n"   . company-select-next)
         ("C-p"   . company-select-previous))
  :config
  (setq company-backends
        '((company-capf :with company-yasnippet)
          company-files
          company-dabbrev-code))
  :custom
  (company-echo-truncate-lines nil)
  ;; (company-echo-delay nil)
  (company-begin-commands '(self-insert-command))
  (company-idle-delay              0.1)
  (company-insertion-on-trigger    nil)
  (company-minimum-prefix-length     1)
  (company-selection-wrap-around     t)
  (company-show-numbers            nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit            20)
  (company-transformers '(company-sort-by-backend-importance)))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))
(use-package company-box
  :config
  (setq company-auto-update-doc nil)
  ;; (setq company-box-frame-behavior 'point)
  :custom
  (company-box-max-candidates 20)
  (company-box-doc-delay 0.2)
  (company-box-show-single-candidate 'never)
  ;; :hook (company-mode . company-box-mode)
  )

(use-package company-quickhelp
  :custom (company-quickhelp-delay 0.2))

(use-package move-text
  ;; NOTE: here to avoid paredit conflicts
  :bind (:map prog-mode-map ("M-n" . move-text-down) ("M-p" . move-text-up)))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  ;; (lsp-enable-snippet t)
  (lsp-disabled-clients '((web-mode . eslint) (web-tsx-mode . nil)))
  (lsp-headerline-breadcrumb-enable nil)
  :bind (:map
         lsp-mode-map
         ("C-j"      . newline);; override electric-mode-newline
         ("M-n"     . move-text-down) ;; NOTE: override lsp-signature-previous
         ("M-p"     . move-text-up)
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
  ;; :bind (
  ;;        ;; ("<tab>" . nil)
  ;;        ;; ("TAB"   . nil)
  ;;        ("M-SPC" . yas-maybe-expand)
  ;;        ("C-c y" . yas-expand)
  ;;        )
  )

;; https://stackoverflow.com/questions/25521897/how-to-never-expand-yasnippets-in-comments-and-strings
(defun yas-no-expand-in-comment/string ()
  (setq yas-buffer-local-condition
        '(if (nth 8 (syntax-ppss)) ;; non-nil if in a string or comment
             '(require-snippet-condition . force-in-comment)
           t)))
;;(add-hook 'prog-mode-hook 'yas-no-expand-in-comment/string)

(use-package smartparens
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-hybrid-kill-excessive-whitespace t))

(use-package dap-mode
  :bind (:map
         dap-mode-map
         ("<f7>"  . dap-step-in)
         ("<f8>"  . dap-step-out)
         ("<f9>"  . dap-next)
         ("<f10>" . dap-continue)
         ("<f12>" . dap-ui-locals))
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

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key
        (lambda () (auth-source-pick-first-password :host "api.openai.com"))))
