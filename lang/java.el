;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Blang/java
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/java/README.org
;; https://github.com/emacs-lsp/lsp-java

(defun java-config ()
  (setq-local compile-command
              (cond ((file-exists-p "pom.xml") ;; FIXME: parent path
                     "mvn compile")
                    (t (concat
                        "javac "
                        buffer-file-name
                        " && java "
                        (file-name-sans-extension
                         (file-name-nondirectory
                          buffer-file-name)))))))

(use-package java-mode
  :ensure nil
  :hook (java-mode . smartparens-strict-mode)
  :hook (java-mode . rainbow-delimiters-mode)
  :hook (java-mode . java-config))

(use-package lsp-java
  :bind (:map java-mode-map ("C-c C-c" . recompile)))
