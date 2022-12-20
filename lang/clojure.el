(use-package clojure-mode
  :custom (cider-repl-display-help-banner . nil)
  :hook (clojure-mode . paredit-mode)
  :hook (clojure-mode . flycheck-mode)
  :hook (clojure-mode . aggressive-indent-mode)
  :hook (clojure-mode . cider-mode)
  :config
  (add-hook 'eldoc-documentation-functions #'cider-eldoc nil t)
  )

(use-package flycheck-clj-kondo)
(use-package cider
  :hook (cider-repl-mode . electric-pair-local-mode)
  :bind
  (:map
   cider-repl-mode-map
   ("C-c M-o" . cider-repl-clear-buffer)
   :map
   clojure-mode-map
   ("C-x C-e"     . cider-eval-last-sexp)
   ("C-c C-c"     . cider-eval-defun-at-point)
   ("C-c C-d C-h" . cider-clojuredocs)
   ("C-c ~"       . cider-repl-set-ns)))
