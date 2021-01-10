;; company-irony-c-headers????
;; flycheck-irony

(defun my-cmode-hook ()
  ;;(setq-local zeal-at-point-docset '("C" "gl4"))
  (setq-local dash-docs-common-docsets '("C" "OpenGL4"))
  (irony-mode +1)
  (irony-eldoc +1)
  ;;(company-mode +1)
  ;;(aggressive-indent-mode +1)
  ;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")
  ;;(add-to-list 'company-backends 'company-c-headers)
  ;;(add-to-list 'company-backends 'company-irony)
  ;; (local-set-key (kbd "C-c C-d d")
  ;;                (lambda () (interactive) (manual-entry (current-word))))
  ;;(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;;(ggtags-mode +1)
  ;; (setq c-default-style "linux" ;; set style to "linux"
  ;;       gdb-many-windows t ;; use gdb-many-windows by default
  ;;       gdb-show-main t)
  )

(use-package irony
  :config
  ;;(add-hook #'irony-mode-hook #'company-arduino-turn-on)
  (add-hook #'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc)

(use-package cpp-auto-include
  :config
  (add-hook #'c++-mode-hook
            (lambda () (add-hook #'before-save-hook #'cpp-auto-include nil t))))

(use-package rtags
  :hook (c-mode . rtags-start-process-unless-running)
  :bind ((:map c-mode-map ("M-." . rtags-find-symbol-at-point))
         (:map c-mode-map ("M-," . rtags-location-stack-back)))
  :config
  (setq rtags-autostart-diagnostics nil
        rtags-display-result-backend 'default
        rtags-use-bookmarks nil
        rtags-completions-enabled nil
        rtags-results-buffer-other-window nil
        rtags-jump-to-first-match nil))

;; (use-package google-c-style
;;   :hook (c-mode-common . google-set-c-style))

;; (use-package company-irony
;;   :config (add-to-list 'company-backends 'company-irony))
;; (use-package company-c-headers
;;   :config (add-to-list 'company-backends 'company-c-headers))

(use-package clang-format
  :config
  (add-hook #'c-mode-hook
            (lambda () (add-hook #'before-save-hook #'clang-format-buffer nil t))))

;; (defun company-abort-and-insert-space ()
;;   (interactive)
;;   (company-abort)
;;   (insert " "))
(use-package cc-mode
  ;; :bind ((:map c-mode-map ("M-SPC" . company-complete))
  ;;        (:map c-mode-map ("SPC"   . company-abort-and-insert-space)))
  :ensure nil)

(add-hook #'c-mode-hook     #'my-cmode-hook)
;; (add-hook #'c++-mode-hook   #'my-cmode-hook)
