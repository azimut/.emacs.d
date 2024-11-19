;; Os: install "ccls"

(setq gdb-many-windows t
      gdb-debuginfod-enable-setting nil)

(defun c-config ()
  (smartparens-strict-mode +1)
  ;;(company-yasnippet +1)
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  (eglot-ensure)
  (corfu-mode +1)
  (ligature-set-ligatures 'c-mode '("<=" ">=" "==" "!=" "&&" "||" "->"))
  ;; (setq-local lsp-enable-snippet nil) ; the quality is really poor for C
  (setq-local compile-command
              (cond ((file-exists-p "Makefile")
                     "make -k")
                    (t (concat
                        "gcc -Wall -Wextra -pedantic -ggdb "
                        buffer-file-name
                        " -o out && ./out")))))

;; (define-key c-mode-map (kbd "C-c C-h") #'helm-dash-at-point)
;; (define-key c-mode-map (kbd "C-c C-c") #'recompile)

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
