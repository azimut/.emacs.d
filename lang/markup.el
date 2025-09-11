(setq image-use-external-converter t)

(pixel-scroll-mode +1)

(use-package org
  :hook (good-scroll)
  :ensure nil
  :custom
  (org-startup-folded 'showeverything)
  (org-display-remote-inline-images 'cache)
  (org-image-actual-width '(300))
  (org-startup-with-inline-images t)
  :config
  (plist-put org-format-latex-options :scale 2.0)
  :bind (:map org-mode-map
              ("C-'" . imenu-list-show)
              ("C-j" . org-return)
              ("M-n" . org-metadown)
              ("M-p" . org-metaup)))

(use-package org-remoteimg
  :straight (org-remoteimg :type git :host github :repo "hubisan/org-remoteimg"))

;; Description: Adds fill color for transparent images
;; Source: https://emacs.stackexchange.com/questions/20574/default-inline-image-background-in-org-mode
;; Source: https://kimi.im/2022-04-29-background-color-of-inline-image-for-orgmode
(defun org--create-inline-image-advice (img)
  (nconc img (list :background "#f8f8f8")))
(advice-add
 'create-image ; here it will affect remote images too
 :filter-return #'org--create-inline-image-advice)

(use-package flymake-aspell)

;; (use-package tex
;;   :ensure nil
;;   :init
;;   (setq
;;    TeX-PDF-mode t
;;    TeX-engine 'xetex
;;    TeX-command-default "LaTeXMK"
;;    latex-run-command "LaTeXMK"
;;    LaTeX-command "LaTeXMK"
;;    TeX-auto-save t
;;    TeX-parse-self t
;;    ;;TeX-newline-function 'delete-other-windows

;;    TeX-command-list
;;    '(("LaTeXMK" "latexmk %s" TeX-run-TeX nil t :help "Run latexmk")
;;      ("LuaLaTeX" "%`lualatex%(mode) --synctex=1 --8bit --shell-escape%' %t" TeX-run-TeX nil t :help "Run lualatex")
;;      ("XeLaTeX" "%`xelatex%(mode) -synctex=1 -8bit -shell-escape%' %t" TeX-run-TeX nil t :help "Run xelatex")
;;      ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
;;      ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
;;      ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
;;      ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
;;      ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
;;      )

;;    LaTeX-verbatim-environments '("verbatim" "verbatim*" "Verbatim" "Verbatim*" "lstlisting" "code" "minted" "gascode" "ccode" "pythoncode" "javacode" "bashcode")

;;    TeX-view-program-selection '((output-pdf "PDF Tools") (output-html "xdg-open"))
;;    TeX-view-style nil
;;    bibtex-maintain-sorted-entries t
;;    bibtex-align-at-equal-sign t
;;    )                                    ; end of setq
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;   (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;;   (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeXMK")))
;;   (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "LaTeXMK")))
;;   (setq-default TeX-master nil)
;;   )

;; (use-package outline
;;   :config
;;   (add-hook 'LaTeX-mode-hook (lambda () (outline-minor-mode 1)))
;;   (setq outline-minor-mode-prefix "\C-c\C-o")
;;   :bind (:map LaTeX-mode-map
;;               ("C-c C-n" . outline-next-visible-heading)
;;               ("C-c C-p C-p" . outline-previous-visible-heading))
;;   )

;; (use-package latex
;;   :bind (:map LaTeX-mode-map ("$" . self-insert-command))
;;   )

(use-package reftex
  :init
  (setq
   reftex-plug-into-AUCTeX t
   reftex-bibliography-commands '("addbibresource" "bibliography" "nobibliography")
   ;; reftex-default-bibliography  '("~/lecture_notes/bib/os.bib" "~/lecture_notes/bib/net.bib"
   ;;                                "~/lecture_notes/bib/wikipedia.bib")
   bibtex-dialect 'biblatex
   reftex-use-external-file-finders t
   )
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  )

;; (use-package tex-fold
;;   :init
;;   (setq
;;    TeX-fold-env-spec-list
;;    '(("[frame]" ("frame"))
;;      ("[block]" ("block"))
;;      ("[comment]" ("comment"))
;;      ("[tikzpicture]" ("tikzpicture"))
;;      ("[minted]" ("minted"))
;;      ("[listing]" ("lstlisting"))
;;      ("[example]" ("example" "exampleblock"))
;;      ("[columns]" ("columns")))
;;    )
;;   :config (add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))
;;   )


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :hook (gfm-mode . visual-line-mode)
  ;; Set Github Formatted Markdown Mode for README.md
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  ;;(setq markdown-command "/usr/bin/MultiMarkdown.pl")
  (setq markdown-command
        "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments"))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :init
  (require 'ox-org)
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(use-package org-modern)

(use-package dockerfile-mode)
(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode)
  :mode "\\.mat\\'"); UNITY

(use-package nroff-mode
  :ensure nil
  :mode "\\.mom\\'"
  :mode "\\.me\\'"
  :mode "\\.ms\\'")
