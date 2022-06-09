;; https://github.com/doomemacs/doomemacs/blob/develop/modules/lang/web/
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/html
;;
;; web-beautify  : npm install -g js-beautify
;; format-all    : tidy/stylelint(css) http://www.html-tidy.org/

(use-package impatient-mode)
(use-package company-web)

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
  (company-mode +1)
  (set (make-local-variable 'company-backends) '(company-css company-web-html))
  (emmet-mode +1))

;; https://web-mode.org/
(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)
  (add-hook #'web-mode-hook #'web-config))
