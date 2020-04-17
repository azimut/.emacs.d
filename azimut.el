;; https://github.com/syl20bnr/spacemacs/issues/12535
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(set-cursor-color "#FFC0CB")

;;--------------------------------------------------
;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;--------------------------------------------------

;; I do NOT want white spaces to be highlighted
(require 'whitespace)
(global-whitespace-mode 0)

;; disable menu-bar
;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
;; HIDE lisp .fasl files
(setq dired-omit-files (concat dired-omit-files "\\|*.fasl"))
(setq dired-listing-switches "-lh")
;;(use-package spacemacs-theme
;;  :ensure t)

(use-package smartparens
  :ensure t)

(use-package phi-search
  :ensure t)
(require 'phi-search)
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Emacs Tutorial 9 - Switching windows in a smart way!
;; https://www.youtube.com/watch?v=dppr_js-U0s&t=474s
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; kill this
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; window resize
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle))

(use-package company
  :ensure t
  :config
  ;; #'company-complete is set per minor mode as needed
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous))

(use-package aggressive-indent
  :ensure t)

(use-package string-inflection
  :ensure t
  :config
  (global-set-key (kbd "C-c j") 'string-inflection-toggle)
  ;; BROKEN, chages whole file
  ;; (global-set-key (kbd "C-c _") (lambda ()
  ;;                                 (interactive)
  ;;                                 (goto-char (1+ (point-min)))
  ;;                                 (replace-string "_" "-")))
  )

;; w3m
(use-package w3m
  :ensure nil
  :config
  (defun browse-url-other (url &rest args)
    (interactive)
    (split-window)
    (balance-windows)
    (other-window 1)
    (w3m-browse-url url))
  (setq browse-url-browser-function #'browse-url-other)
  (add-hook 'w3m-mode-hook
            (lambda ()
              (local-set-key "\C-n"   'w3m-next-anchor)
              (local-set-key "\C-p"   'w3m-previous-anchor)
              (local-set-key '[up]    'previous-line)
              (local-set-key '[down]  'next-line)
              (local-set-key '[left]  'backward-char)
              (local-set-key '[right] 'forward-char))))

(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (setq-default yas-prompt-functions
                '(yas-ido-prompt yas-dropdown-prompt))
  (add-hook 'snippet-mode-hook
            (lambda ()
              (set (make-local-variable require-final-newline) nil))))

(use-package dockerfile-mode
  :ensure t)

;; jupyter elpy
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.mat\\'" . yaml-mode)) ; Unity
  :init
  (add-hook 'yaml-mode-hook (lambda () (ansible 1))))

(add-hook 'python-mode-hook (lambda () (elpy-mode)))



;; pretty lambda
(global-prettify-symbols-mode 1)
;; (ac-set-trigger-key "<?\\M-\\t>")
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")
;; (define-key ac-completing-map (kbd "C-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-p") 'ac-previous)


;;--------------------------------------------------

;; https://www.emacswiki.org/emacs/ShowParenMode
;; parens match
(show-paren-mode 1)

;; Fuck tilde!
(setq make-backup-files nil)

;; I want spaces for indentation
(setq-default indent-tabs-mode nil)

;; https://emacs.stackexchange.com/questions/24719/set-indentation-for-shell-script-function
(setq sh-basic-offset 4)
(setq sh-indentation 4)
(setq smie-indent-basic 4)

(put 'erase-buffer 'disabled nil)

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-0") 'neotree-toggle)
  (setq neo-theme 'arrow)
  (setq neo-hidden-regexp-list
        '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$" "\\.beam$" "\\.meta$")))

;; C / C++
(defun my-cmode-hook ()
  (setq imenu-list-auto-resize t)
  (setq-local zeal-at-point-docset '("C" "gl4"))
  (setq-local helm-dash-docsets '("C" "OpenGL4"))
  ;; (local-set-key (kbd "C-c C-d d")
  ;;                (lambda () (interactive) (manual-entry (current-word))))
  (ggtags-mode +1))
(add-hook 'c-mode-hook #'my-cmode-hook)
(use-package ggtags
  :ensure t)
(use-package cc-mode
  :ensure nil
  :init
  (add-hook #'c++-mode-hook
            (lambda () (ggtags-mode +1)))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local zeal-at-point-docset '("gl4" "cpp"))
  (setq-local helm-dash-docsets '("OpenGL4" "cpp")))

;; compile-mode
;; https://github.com/fsharp/zarchive-fsharpbinding/issues/246
(add-hook 'compilation-mode-hook
          (lambda ()
            ;;(setq compilation-auto-jump-to-first-error t)
            (setq compilation-scroll-output t)))

(global-set-key (kbd "C-x g") 'magit-status)

(use-package magit-todos :ensure t)
(magit-todos-mode +1)

(use-package gitignore-templates :ensure t)
(use-package gitignore-mode :ensure t)

(use-package git-timemachine  :ensure t)

;;  https://github.com/jaypei/emacs-neotree/issues/56
;;(magithub-feature-autoinject t)
;;(setq magithub-clone-default-directory "/home/sendai/quicklisp/local-projects/")

(use-package helm-dash
  :ensure t
  :config
  (setq dash-docs-browser-func 'eww))

;;(grep-apply-setting 'grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-command
      (quote
       ("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49)))
(setq grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-template
      "find <D> <X> -type f <F> -exec grep <C> --exclude='*.svn-base' -nH --null -e <R> \\{\\} +")

;;(setq package-check-signature nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  ;; Set Github Formatted Markdown Mode for README.md
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  ;;(setq markdown-command "/usr/bin/MultiMarkdown.pl")
  (setq markdown-command
        "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments"))


;;--------------------------------------------------

;; (use-package dimmer
;;   :ensure t)
;; (require 'dimmer)
;; (dimmer-mode +1)

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
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

;;-------------------------------------------------
(load-file "~/.emacs.d/lang/shell.el")
(load-file "~/.emacs.d/lang/elisp.el")
(load-file "~/.emacs.d/lang/erlang.el")
(load-file "~/.emacs.d/lang/lisp.el")
;; (load-file "~/.emacs.d/lang/clojure.el")
;; (load-file "~/.emacs.d/lang/go.el")
;; (load-file "~/.emacs.d/lang/elixir.el")
;; (load-file "~/.emacs.d/lang/lua.el")
;; (load-file "~/.emacs.d/lang/glsl.el")
;; (load-file "~/.emacs.d/lang/livecoding.el")

