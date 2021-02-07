;; Not in MELPA
(load-file "/usr/share/emacs/site-lisp/chuck.el")

(defvar chuck-electric-pairs '((?\< . ?\>))
  "Electric pairs for chuck-mode.")

(defun chuck-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs chuck-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(use-package chuck-mode
  :ensure nil
  :init
  (add-hook 'chuck-mode-hook
            (lambda ()
              (setq-local prettify-symbols-alist '(("=>" . 8658)))
              (aggressive-indent-mode +1)
              (electric-pair-mode +1)
              (chuck-add-electric-pairs))))
