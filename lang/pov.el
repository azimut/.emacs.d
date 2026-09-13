(use-package pov-mode
  :hook (pov-mode . smartparens-strict-mode)
  :hook (pov-mode . font-lock-mode)
  :hook (pov-mode . pov-config)
  :hook (pov-mode . corfu-mode)
  :hook (pov-mode . git-gutter-mode)
  :custom
  (pov-run-default             "+p +i%s")
  (pov-run-test                "+p res120 +i%s -Q3")
  (pov-run-low                 "+p res320 +i%s")
  (pov-run-mid                 "+p res640 +i%s")
  (pov-run-high                "+p res800 +i%s")
  (pov-run-highest             "+p res1k  +i%s +A0.3")
  (pov-external-viewer-command "nsxiv")
  :init
  (defun pov-completion-at-point ()
    "Provide POV-Ray completions through the standard Emacs CAPF interface."
    (let ((end (point))
          (start (save-excursion
                   (skip-chars-backward "a-zA-Z0-9#_")
                   (point))))
      (save-excursion
        (pov-get-scope))
      (list start end
            (completion-table-dynamic
             (lambda (_)
               pov-completion-list)))))
  (defun pov-config ()
    (add-hook 'completion-at-point-functions
              #'pov-completion-at-point nil t))
  :bind (:map pov-mode-map
              ("{"     . nil)
              ("}"     . nil)
              ("TAB"   . nil)
              ("\t"    . nil)
              ("\M-\t" . nil)
              ("\r"    . nil)))
