;; lsp-install-server >> html-ls

(use-package impatient-mode)

(defun +web/indent-or-yas-or-emmet-expand ()
  "Do-what-I-mean on TAB.
Invokes `indent-for-tab-command' if at or before text bol, `yas-expand' if on a
snippet, or `emmet-expand-yas'/`emmet-expand-line', depending on whether
`yas-minor-mode' is enabled or not."
  (interactive)
  (call-interactively
   (cond ((or (<= (current-column) (current-indentation))
              (not (eolp))
              (not (or (memq (char-after) (list ?\n ?\s ?\t))
                       (eobp))))
          #'indent-for-tab-command)
         (#'emmet-expand-yas))))

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("<tab>" . +web/indent-or-yas-or-emmet-expand)
              ("<C-return>" . nil)
              ("C-j" . newline)))

(defun +web-is-auto-close-style-3 (_id action _context)
  (and (eq action 'insert)
       (eq web-mode-auto-close-style 3)))

(defun web-config ()
  (setq-local lsp-disabled-clients '(eslint))
  (setq-local lsp-eldoc-enable-hover nil) ;; Too busy
  (setq-local company-insertion-on-trigger nil)
  (setq-local company-insertion-triggers '(?\  ?\)))
  (smartparens-mode +1)
  (emmet-mode +1))

(defun radian-enter-and-indent-sexp (&rest _ignored)
  "Insert an extra newline after point, and reindent.
   https://github.com/Fuco1/smartparens/issues/80"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :bind (:map
         web-mode-map
         ("C-M-t" . web-mode-element-transpose)
         ("C-c C-d" . lsp-describe-thing-at-point))
  :custom
  (lsp-html-format-enable                      nil "BUG?: hangs up <style> editing for 2 seconds")
  (web-mode-block-padding                      2)
  (web-mode-code-indent-offset                 2)
  (web-mode-css-indent-offset                  2)
  (web-mode-markup-indent-offset               2)
  (web-mode-comment-style                      2)
  (web-mode-auto-close-style                   1)
  (web-mode-enable-css-colorization            nil "LSP already has colors")
  (web-mode-enable-current-element-highlight   nil "LSP already has it")
  (web-mode-enable-html-entities-fontification t)
  :init
  (add-hook #'web-mode-hook #'web-config)
  :config
  (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))
  (sp-local-pair 'web-mode "{" nil :post-handlers '((radian-enter-and-indent-sexp "C-j"))))
