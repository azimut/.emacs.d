(use-package sqlite3)
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-git-executable "/usr/bin/git"))

(use-package magit-todos
  :init (magit-todos-mode +1))

(use-package forge
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (forge-topic-list-limit '(100 . 0)))

(use-package gitignore-templates)
(use-package git-timemachine)
(use-package lice)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recently-active))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

;; NOTE: -fringe needed (? crt.o on dnf gcc-x86_64-linux-gnu.x86_64
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

