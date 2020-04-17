;;--------------------------------------------------
;; Clojure
;;--------------------------------------------------
(use-package cider
  :ensure t
  :config
  (define-key cider-repl-mode-map
    (kbd "C-c M-o") #'cider-repl-clear-buffer)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (paredit-mode +1))))

(use-package clojure-mode
  :ensure t
  ;; :bind
  ;; (("C-c C-d C-h" . cider-clojuredocs)
  ;;  ("C-c ~"       . cider-repl-set-ns))
  :config
  (define-key clojure-mode-map
    (kbd "C-c C-d C-h") #'cider-clojuredocs)
  (define-key clojure-mode-map
    (kbd "C-c ~") #'cider-repl-set-ns)
  (setq cider-repl-display-help-banner nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode +1)
              (aggressive-indent-mode +1))))
