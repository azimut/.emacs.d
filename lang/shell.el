;; OS npm i -g bash-language-server
;; OS go install mvdan.cc/sh/v3/cmd/shfmt@latest

(use-package shfmt
  :custom
  (shfmt-arguments '("-i" "4")))

(use-package sh-mode
  :ensure nil
  :custom
  ;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
  (sh-basic-offset   4)
  (sh-indentation    4)
  (smie-indent-basic 4)
  :bind (:map
         sh-mode-map
         ("C-c C-c" . recompile))
  :hook (sh-mode . eglot-ensure)
  :hook (sh-mode . electric-pair-local-mode)
  :hook (sh-mode . rainbow-delimiters-mode)
  :hook (sh-mode . sh-config)
  :init
  (defun sh-config ()
    (shfmt-on-save-mode)
    (ligature-set-ligatures ;; NOTE: for some reason this didn't worked on :config
     'sh-mode
     '("==" "!=" "<=" ">=" "&&" "||" "--" "++" "**"))
    (setq-local tab-width 4)
    (setq-local whitespace-style '(face tabs empty))
    (setq-local compile-command (concat "sh " buffer-file-name)))  )

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
  :hook (jq-mode . (lambda () (rainbow-delimiters-mode -1)))
  :mode (("\\.jq\\'" . jq-mode))
  :config
  (ligature-set-ligatures 'jq-mode '("==" "!=" ">=" "<=")))

(use-package sed-mode)
