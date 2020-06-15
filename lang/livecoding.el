;;
;; Tidal
;;
(use-package tidal
  :config
  (setq tidal-interpreter "/home/sendai/.ghcup/bin/ghci")
  (setq tidal-boot-script-path "/home/sendai/.cabal/share/x86_64-linux-ghc-8.6.5/tidal-1.0.14/BootTidal.hs"))

(use-package csound-mode
  :mode (("\\.csd\\'" . csound-mode)
  	 ("\\.orc\\'" . csound-mode)
  	 ("\\.sco\\'" . csound-mode)
         ("\\.udo\\'" . csound-mode))
  :config
  (define-key csound-mode-map (kbd "C-c C-d")
    (lambda ()
      (interactive)
      (browse-url (concat ;;"http://www.csounds.com/manual/html/"
                   "https://csound.com/docs/manual/"
                   (symbol-name (symbol-at-point))
                   ".html")))))
