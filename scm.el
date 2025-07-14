(use-package sqlite3)
(use-package magit
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :custom
  (magit-git-executable "/usr/bin/git")
  :init
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  ;; This code makes magit-status run alone in the frame, and then
  ;; restores the old window configuration when you quit out of magit.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package magit-todos
  :init (magit-todos-mode +1))

(use-package forge
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (forge-topic-list-limit '(100 . 0)))

(use-package gitignore-templates)
(use-package git-timemachine)
(use-package lice)

(use-package git-gutter
  :hook (yaml-mode . git-gutter-mode)
  :hook (web-mode . git-gutter-mode)
  :hook (css-mode . git-gutter-mode)
  :hook (prog-mode . git-gutter-mode))

;; NOTE: -fringe needed (? crt.o on dnf gcc-x86_64-linux-gnu.x86_64
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
