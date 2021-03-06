;;--------------------------------------------------
;; Clojure
;;--------------------------------------------------

(use-package clojure-mode
  ;; :bind
  ;; (("C-c C-d C-h" . cider-clojuredocs)
  ;;  ("C-c ~"       . cider-repl-set-ns))
  :config
  (define-key clojure-mode-map (kbd "C-c C-d C-h") #'cider-clojuredocs)
  (define-key clojure-mode-map (kbd "C-c ~") #'cider-repl-set-ns)
  (define-key clojure-mode-map (kbd "C-x C-e") #'cider-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c C-c") #'cider-eval-defun-at-point)
  (setq cider-repl-display-help-banner nil)
  (add-hook 'eldoc-documentation-functions #'cider-eldoc nil t)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode +1)
              (aggressive-indent-mode +1)
              (flycheck-mode +1))))

(use-package flycheck-clj-kondo)
(use-package cider
  :config
  (define-key cider-repl-mode-map
    (kbd "C-c M-o") #'cider-repl-clear-buffer)
  (add-hook 'cider-repl-mode-hook
            (lambda () (electric-pair-mode +1))))

