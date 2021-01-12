;; eldoc mode for rtags: https://github.com/Andersbakken/rtags/issues/987
(require 'subr-x)
(defun remove-spaces (source)
  (let ((newstring nil)
        (lastplace 0))
    (dolist (char (string-to-list source))
      (if (or (and (= lastplace 32)
                   (not (= char 32))))
          (push 32 newstring))
      (if (not (= char 32))
          (push char newstring))
      (setq lastplace char))
    (concat (reverse newstring))))
(defun fontify-string (str mode)
  "Return STR fontified according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil)
    (buffer-string)))
(defun rtags-eldoc-function ()
  (let ((summary (rtags-get-summary-text)))
    (and summary
         (fontify-string
          (remove-spaces
           (replace-regexp-in-string
            "{[^}]*$" ""
            (mapconcat
             (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
             (split-string summary "\r?\n")
             " ")))
          major-mode))))
(defun rtags-eldoc-mode ()
  (interactive)
  (setq-local eldoc-documentation-function #'rtags-eldoc-function)
  (eldoc-mode 1))

(use-package company-rtags
  :config
  (add-hook #'c-mode-hook
            (lambda ()
              (rtags-eldoc-mode)
              (company-mode +1)
              (push 'company-rtags company-backends))))

(use-package rtags
  :hook (c-mode . rtags-start-process-unless-running)
  :bind ((:map c-mode-map ("M-." . rtags-find-symbol-at-point))
         (:map c-mode-map ("M-," . rtags-location-stack-back)))
  :init
  (setq rtags-autostart-diagnostics       nil
        rtags-use-bookmarks               nil
        rtags-completions-enabled         nil
        rtags-display-result-backend     'default
        rtags-results-buffer-other-window t
        rtags-jump-to-first-match         nil))

(use-package clang-format
  :config
  (add-hook #'c-mode-hook
            (lambda () (add-hook #'before-save-hook #'clang-format-buffer nil t))))

(defun my-cmode-hook ()
  ;;(setq-local zeal-at-point-docset '("C" "gl4"))
  (setq-local dash-docs-common-docsets '("C" "OpenGL4"))
  ;; (irony-mode +1)
  ;; (irony-eldoc +1)
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
