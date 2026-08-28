(use-package uxntal-mode
  :bind (:map
         uxntal-mode-map
         ("C-c C-d" . uxntal-explain-word)))

(use-package ps-mode
  :ensure nil
  :custom (ps-mode-tab 2))
