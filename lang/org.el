(use-package org
  ;;  :hook (good-scroll)
  :ensure nil
  :custom
  (org-startup-folded 'showeverything)
  (org-display-remote-inline-images 'cache)
  (org-image-actual-width '(300))
  (org-startup-with-inline-images t)
  (org-confirm-babel-evaluate nil)
  :init
  ;; Description: Adds fill color for transparent images
  ;; Source: https://emacs.stackexchange.com/questions/20574/default-inline-image-background-in-org-mode
  ;; Source: https://kimi.im/2022-04-29-background-color-of-inline-image-for-orgmode
  (defun org--create-inline-image-advice (img)
    (nconc img (list :background "#f8f8f8")))
  :config
  (advice-add
   'create-image ; here it will affect remote images too
   :filter-return #'org--create-inline-image-advice)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (gnuplot . t)
     (dot . t)))
  (plist-put org-format-latex-options :scale 2.0)
  :bind (:map org-mode-map
              ("C-'" . imenu-list-show)
              ("C-j" . org-return)
              ("M-n" . org-metadown)
              ("M-p" . org-metaup)))

(use-package org-remoteimg
  :straight (org-remoteimg
             :type git
             :host github
             :repo "hubisan/org-remoteimg"))

(use-package org-modern)
