;; Shell - bashate
;; (use-package flycheck-bashate
;;   :ensure t
;;   :config)

;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

(setq flycheck-disabled-checkers '(sh-posix-bash))
(setq-default flycheck-shellcheck-excluded-warnings '("SC2086"))

(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode +1)
            (define-key sh-mode-map (kbd "M-p") #'flycheck-previous-error)
            (define-key sh-mode-map (kbd "M-n") #'flycheck-next-error)
            (aggressive-indent-mode +1)
            (electric-pair-mode +1)
            (sp-use-paredit-bindings)))

;; https://nistara.net/post/emacs-send-line-or-region-to-shell/
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      (setq comint-scroll-to-bottom-on-output t))
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))

(define-key sh-mode-map (kbd "C-c C-c") #'sh-send-line-or-region-and-step)
