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
  (require 'ccls)
  (smartparens-strict-mode +1)
  (company-yasnippet +1)
  (display-fill-column-indicator-mode +1)
  (lsp-mode +1)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (ligature-set-ligatures 'c-mode '("<=" ">=" "==" "!=" "&&" "||" "->"))
  (setq-local lsp-enable-snippet nil) ; the quality is really poor for C
  (setq-local company-auto-commit-chars nil)
  (setq-local compile-command
              (cond ((file-exists-p "Makefile")
                     "make -k")
                    (t (concat
                        "gcc -Wall -Wextra -pedantic -ggdb "
                        buffer-file-name
                        " -o out && ./out")))))

(define-key c-mode-map (kbd "C-c C-h") #'helm-dash-at-point)
(define-key c-mode-map (kbd "C-c C-c") #'recompile)

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
