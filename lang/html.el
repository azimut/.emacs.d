;; lsp-install-server >> html-ls
;; lsp-install-server >> tailwindcss
;; npm install -g prettier

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
         ((bound-and-true-p yas-global-mode)
          (require 'yasnippet)
          (if (yas--templates-for-key-at-point)
              #'yas-expand
            #'emmet-expand-yas))
         (#'emmet-expand-line))))

(defun +web-is-auto-close-style-3 (_id action _context)
  (and (eq action 'insert)
       (eq web-mode-auto-close-style 3)))

(defun web-config ()
  (setq-local lsp-eldoc-enable-hover       nil) ;; Too busy
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-insertion-on-trigger nil))

(defun tsx-config ()
  (set (make-local-variable 'company-backends) '(company-capf company-files))
  (setq-local create-lockfiles             nil)
  (setq-local emmet-jsx-className-braces?  t)
  (setq-local lsp-enable-indentation       nil)
  (setq-local lsp-eldoc-enable-hover       t)
  (setq-local company-insertion-on-trigger nil))

(use-package web-mode
  :mode (("\\.[px]?html?\\'" . web-mode)
         ("\\.tsx\\'" . web-tsx-mode)
         ("\\.jsx\\'" . web-tsx-mode))
  :hook (web-tsx-mode . tsx-config)
  :hook (web-mode . lsp)
  :hook (web-mode . smartparens-mode)
  :hook (web-mode . web-config)
  :hook (web-mode . electric-pair-mode)
  :bind (:map
         web-mode-map
         ("<tab>" . +web/indent-or-yas-or-emmet-expand)
         ("C-M-t" . web-mode-element-transpose))
  :custom
  (lsp-html-format-enable                      nil "BUG?: hangs up <style> editing for 2 seconds")
  (web-mode-enable-auto-quoting                nil "let smartparens handle this")
  (web-mode-enable-auto-pairing                t   "let smartparens handle this")
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
  (define-derived-mode web-tsx-mode web-mode "tsx"))

(sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

(use-package json-mode
  :hook (json-mode . lsp)
  :hook (json-mode . smartparens-strict-mode))

(use-package emmet-mode
  :hook (web-mode web-tsx-mode)
  :bind (:map emmet-mode-keymap
              ("<C-return>" . nil)
              ("C-j"        . newline)))

(use-package css-mode
  :bind (:map css-mode-map
              ("C-j" . newline))
  :ensure nil
  :hook (css-mode . smartparens-strict-mode)
  :hook (css-mode . lsp)
  :custom
  (css-smie-rules)
  (css-fontify-colors nil))

(sp-local-pair 'css-mode "{" nil :post-handlers '((radian-enter-and-indent-sexp "C-j")))

(use-package prettier
  :config (setq prettier-mode-sync-config-flag nil)
  :hook ((web-mode web-tsx-mode css-mode json-mode typescript-mode)
         . prettier-mode))

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t))
