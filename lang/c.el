;; Os: install "ccls"
;; Enable it per buffer with M-lsp

;; (require 'dap-lldb)
(require 'dap-codelldb)
(require 'dap-gdb-lldb)

(use-package ccls
  :init
  (setq ccls-executable "/snap/bin/ccls")
  (setq ccls-initialization-options
        '(:index
          (:comments 2 :trackDependecy 1 :threads 2)
          :completion
          (:detailedLabel t)))
  ;; :config
  ;; (add-to-list 'ccls-root-files ".ccls")
  ;; (add-to-list 'ccls-root-files "compile_commands.json")
  )

(defun c-config ()
  (smartparens-strict-mode +1)
  (sp-use-paredit-bindings)
  (require 'ccls)
  (lsp-mode +1)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (setq-local company-auto-commit-chars nil)
  (display-fill-column-indicator-mode +1))

(define-key c-mode-map (kbd "C-c C-h") #'helm-dash-at-point)

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

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))
