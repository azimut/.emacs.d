;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/java
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/java/README.org
;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :bind (:map java-mode-map ("C-c C-c" . recompile))
  :hook (java-mode . rainbow-delimiters-mode))
