;; https://github.com/doomemacs/doomemacs/blob/develop/modules/lang/web/
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/html
;;
;; web-beautify  : npm install -g js-beautify
;; format-all    : tidy/stylelint(css) http://www.html-tidy.org/

(use-package impatient-mode)
(use-package company-web)

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("<tab>" . emmet-expand-yas)
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
