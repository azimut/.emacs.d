;; lsp-install-server >> html-ls
;; https://github.com/doomemacs/doomemacs/blob/develop/modules/lang/web/
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/html
;;
;; web-beautify  : npm install -g js-beautify
;; format-all    : tidy/stylelint(css) http://www.html-tidy.org/

(use-package impatient-mode)
;;(use-package company-web)

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

;; https://github.com/smihica/emmet-mode#html-abbreviations
(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("<tab>" . +web/indent-or-yas-or-emmet-expand)
              ("<C-return>" . nil)
              ("C-j" . newline)))

(defun +web-is-auto-close-style-3 (_id action _context)
  (and (eq action 'insert)
       (eq web-mode-auto-close-style 3)))

(defun web-config ()
  (smartparens-mode +1)
  (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))
  ;; (company-mode +1)
  ;; (set (make-local-variable 'company-backends) '(company-css company-web-html))
  (setq-local company-insertion-on-trigger t)
  (setq-local company-insertion-triggers '(?\  ?\)))
  (emmet-mode +1))

;; https://github.com/Fuco1/smartparens/issues/80
(defun radian-enter-and-indent-sexp (&rest _ignored)
  "Insert an extra newline after point, and reindent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; https://web-mode.org/
(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :bind (:map
         web-mode-map
         ("C-M-t" . web-mode-element-transpose)
         ("C-c C-d" . lsp-describe-thing-at-point))
  :config
  (setq lsp-html-format-enable nil) ; BUG?: hangs up <style> editing for 2 seconds
  (setq-local lsp-eldoc-enable-hover nil)     ;; Too busy
  (setq web-mode-enable-css-colorization nil) ;; LSP already has colors
  (setq web-mode-enable-current-element-highlight nil) ;; LSP already has it
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    2
        web-mode-code-indent-offset   2
        web-mode-block-padding        2
        web-mode-comment-style        2)
  (add-hook #'web-mode-hook #'web-config))

(sp-local-pair 'web-mode "{" nil :post-handlers '((radian-enter-and-indent-sexp "C-j")))
