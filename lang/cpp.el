;; C / C++
(defun my-cmode-hook ()
  (setq imenu-list-auto-resize t)
  (setq-local zeal-at-point-docset '("C" "gl4"))
  (setq-local helm-dash-docsets '("C" "OpenGL4"))
  ;; (local-set-key (kbd "C-c C-d d")
  ;;                (lambda () (interactive) (manual-entry (current-word))))
  (ggtags-mode +1))
(add-hook 'c-mode-hook #'my-cmode-hook)
(use-package cc-mode
  :ensure nil
  :init
  (add-hook #'c++-mode-hook
            (lambda () (ggtags-mode +1)))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local zeal-at-point-docset '("gl4" "cpp"))
  (setq-local helm-dash-docsets '("OpenGL4" "cpp")))
