;; Emacs
(use-package elisp-mode
  :ensure nil
  :init
  ;; Default *scratch* buffer to lexical binding.
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (equal (buffer-name) "*scratch*")
                (setq-local lexical-binding t))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (paredit-mode +1)
              (rainbow-delimiters-mode +1)
              (aggressive-indent-mode +1)
              (eldoc-mode +1)
              ;; SLIME like keybinding instead of "C-h f"
              (define-key emacs-lisp-mode-map (kbd "C-c C-d")
                (lambda ()
                  (interactive)
                  (describe-symbol
                   (symbol-at-point))))
              (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'compile-defun))))
