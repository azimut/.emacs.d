(defun cobol-config ()
  (display-fill-column-indicator-mode +1)
  ;;(highlight-indent-guides-mode +1)
  (setq-local whitespace-style
              '(face empty identation)))

(use-package cobol-mode
  :mode (("\\.cob\\'" . cobol-mode)
         ("\\.cbl\\'" . cobol-mode))
  :hook (cobol-mode . cobol-config))
