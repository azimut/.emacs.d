(use-package sqlite-mode
  :ensure nil
  :hook (sqlite-mode . hl-line-mode)
  :bind (:map
         sqlite-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("n" . next-line)
         ("p" . previous-line)))
