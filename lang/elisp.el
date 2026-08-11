(use-package emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . paredit-mode)
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . #'compile-defun)
              ("C-c C-d" . #'emacs-lisp-describe-at-point))
  :init
  (defun emacs-lisp-describe-at-point ()
    (interactive)
    (describe-symbol
     (symbol-at-point)))
  ;; Default *scratch* buffer to lexical binding.
  ;; (add-hook 'lisp-interaction-mode-hook
  ;;           (lambda ()
  ;;             (when (equal (buffer-name) "*scratch*")
  ;;               (setq-local lexical-binding t))))
  )
