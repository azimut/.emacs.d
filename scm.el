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
(use-package gitignore-mode)
(use-package git-timemachine)
(use-package lice)
