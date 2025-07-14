(use-package clojure-mode
  :custom (cider-repl-display-help-banner . nil)
  :hook (clojure-mode . paredit-mode)
  :hook (clojure-mode . aggressive-indent-mode)
  :hook (clojure-mode . cider-mode)
  :bind
  (:map
   paredit-mode-map
   ("C-M-p" . nil)
   ("C-j"   . nil))
  :config
  (add-hook 'eldoc-documentation-functions #'cider-eldoc nil t))

(use-package cider
  :hook (cider-repl-mode . electric-pair-local-mode)
  :hook (cider-repl-mode . corfu-disable-margin)
  :hook (cider-repl-mode . corfu-mode)
  :hook (cider-mode      . corfu-disable-margin)
  :hook (cider-mode      . corfu-mode)
  :config
  (setq cider-infer-remote-nrepl-ports t)
  :bind
  (:map
   cider-repl-mode-map
   ("C-c M-o"     . cider-repl-clear-buffer)
   ("C-c C-d C-h" . cider-clojuredocs)
   :map
   clojure-mode-map
   ("C-x C-e"     . cider-eval-last-sexp)
   ("C-c C-c"     . cider-eval-defun-at-point)
   ("C-c C-d C-h" . cider-clojuredocs)
   ("C-c ~"       . cider-switch-and-set))
  :init
  (defun corfu-disable-margin ()
    (setq-local corfu-margin-formatters nil))
  (defun cider-switch-and-set ()
    (interactive)
    (cider-switch-to-repl-buffer t)))
