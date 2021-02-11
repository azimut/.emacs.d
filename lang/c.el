;; Os: install "ccls"

(use-package ccls
  :init
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comments 2) :completion (:detailedLabel t))))

(defun c-config ()
  (smartparens-strict-mode +1)
  (sp-use-paredit-bindings)
  (require 'ccls)
  (lsp-mode +1)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(add-hook #'c-mode-hook #'c-config)
;; (add-hook #'c++-mode-hook   #'my-cmode-hook)

;; (defun my-cmode-hook ()
;;   ;;(setq-local zeal-at-point-docset '("C" "gl4"))
;;   (setq-local dash-docs-common-docsets '("C" "OpenGL4"))
;;   ;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")
;;   ;;(add-to-list 'company-backends 'company-c-headers)
;;   ;; (local-set-key (kbd "C-c C-d d")
;;   ;;                (lambda () (interactive) (manual-entry (current-word))))
;;   )


