;; https://github.com/doomemacs/doomemacs/blob/develop/modules/lang/web/
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/html
;;
;; impatient-mode:
;; web-beautify  : npm install -g js-beautify

(defun +web-is-auto-close-style-3 (_id action _context)
  (and (eq action 'insert)
       (eq web-mode-auto-close-style 3)))

(defun web-config ()
  (smartparens-mode +1)
  (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))
  (company-mode +1)
  (set (make-local-variable 'company-backends) '(company-css company-web-html)))

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1)
  (add-hook #'web-mode-hook #'web-config))

(use-package company-web)
