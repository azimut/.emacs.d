;; Shell - bashate
;; (use-package flycheck-bashate
;;   :ensure t
;;   :config)

;; go install mvdan.cc/sh/v3/cmd/shfmt@latest
(use-package shfmt)

;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

(setq flycheck-disabled-checkers '(sh-posix-bash))
(setq-default flycheck-shellcheck-excluded-warnings '("SC2086"))

(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode +1)
            (setq-local tab-width 4)
            (setq-local compile-command (concat "sh " buffer-file-name))
            (shfmt-on-save-mode)
            (define-key sh-mode-map (kbd "C-c C-c") #'recompile)
            ;;(define-key sh-mode-map (kbd "C-c C-c") #'sh-send-line-or-region-and-step)
            (electric-pair-local-mode +1)))

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

(use-package jq-mode
  :hook (jq-mode . aggressive-indent-mode)
  :hook (jq-mode . smartparens-strict-mode)
  :mode (("\\.jq\\'" . jq-mode)))
