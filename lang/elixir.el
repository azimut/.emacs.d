;; Elixir

(use-package elixir-mode
  :config
  (define-key elixir-mode-map (kbd "SPC") #'yas-maybe-expand))

;; (add-to-list 'elixir-mode-hook
;;              (defun auto-activate-ruby-end-mode-for-elixir-mode ()
;;                (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;;                     "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;;                (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;;                (ruby-end-mode +1)))

