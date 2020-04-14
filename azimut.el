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

;;
;; Lua
;;
(use-package lua-mode
  :ensure t
  :config
  (add-hook
   'lua-mode-hook
   (lambda ()
     (setq-local prettify-symbols-alist '(("function" .  955)))
     (setq lua-default-application "~/.luarocks/bin/rep.lua")
     ;; FIXME: still doesn't work
     (setenv "LUA_PATH" (concat "/usr/share/awesome/lib/?/init.lua;"
                                "/usr/share/awesome/lib/?.lua;"
                                "/usr/share/awesome/lib/?/?.lua;;"))
     (setenv "LUA_REPL_RLWRAP" "sure");?
     (define-key lua-mode-map (kbd "C-c C-d") #'lua-search-documentation)
     (define-key lua-mode-map (kbd "C-c C-k") #'lua-send-buffer)
     (define-key lua-mode-map (kbd "C-c C-c") #'lua-send-region)
     ;;
     (smartparens-strict-mode +1)
     (sp-use-paredit-bindings)
     (aggressive-indent-mode +1))))

(defun azm-renoise-manual ()
  (interactive)
  (dired "~/projects/lua/xrnx/Documentation/")
  (dired-hide-details-mode)
  (rename-buffer "*renoise-manual*"))

;;
;; GLSL
;;

(use-package glsl-mode
  :ensure t
  :config
  (add-hook 'glsl-mode-hook
            (lambda ()
              (interactive)
              (setq-local zeal-at-point-docset '("gl4"))
              (setq-local helm-dash-docsets '("OpenGL4"))
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (aggressive-indent-mode +1)))
  ;; g3d engine
  (add-to-list 'auto-mode-alist '("\\.pix\\'"   . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'"  . glsl-mode))
  ;; Unreal (though is actually hlsl...)
  (add-to-list 'auto-mode-alist '("\\.usf\\'"   . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.ush\\'"   . glsl-mode))
  ;;
  (add-to-list 'auto-mode-alist '("\\.vert\\'"  . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'"  . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'"  . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glsl\\'"  . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs\\'"    . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'"    . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.hlsl\\'"  . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.hlsli\\'" . glsl-mode)))

(use-package shader-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cginc\\'"    . shader-mode)))

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

(use-package csound-mode
  :ensure t
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


;;
;; Go/Golang
;;

(use-package go-eldoc   :ensure t)
(use-package gotest     :ensure t)
(use-package flymake-go :ensure t)
(use-package go-guru    :ensure t
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier))
(use-package go-dlv     :ensure t
  :commands (dlv-current-func dlv)
  :init
  ;;(setq go-guru-command "/usr/lib/go/bin/guru")
  )
(use-package auto-complete   :ensure t)
(use-package go-autocomplete :ensure t)
;;(use-package company-go :ensure t)
(use-package gorepl-mode :ensure t :after go-mode
  :config
  (add-hook
   'gorepl-mode-hook
   (lambda ()
     (add-to-list 'ac-modes 'gorepl-mode)
     (add-hook 'gorepl-mode-hook #'(lambda () (add-to-list 'ac-sources 'ac-source-go)))
     (smartparens-strict-mode +1)
     (sp-use-paredit-bindings))))
;; (custom-set-faces
;;  '(company-preview
;;    ((t (:foreground "darkgray" :underline t))))
;;  '(company-preview-common
;;    ((t (:inherit company-preview))))
;;  '(company-tooltip
;;    ((t (:background "lightgray" :foreground "black"))))
;;  '(company-tooltip-selection
;;    ((t (:background "steelblue" :foreground "white"))))
;;  '(company-tooltip-common
;;    ((((type x)) (:inherit company-tooltip :weight bold))
;;     (t (:inherit company-tooltip))))
;;  '(company-tooltip-common-selection
;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
;;     (t (:inherit company-tooltip-selection)))))
(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq-local prettify-symbols-alist '(("func" . 955)
                                                   ("<-"   . ?←)))
              ;;
              (yas-minor-mode +1)
              (flycheck-mode +1)
              (define-key go-mode-map (kbd "M-p") #'flycheck-previous-error)
              (define-key go-mode-map (kbd "M-n") #'flycheck-next-error)
              ;;
              (require 'go-autocomplete)
              (require 'auto-complete-config)
              (ac-config-default)
              ;;
              ;; (set (make-local-variable 'company-backends) '(company-go))
              ;; (company-mode)
              ;;
              (go-eldoc-setup)
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook #'gofmt-before-save)))
  :config
  ;; (setq company-tooltip-limit 20)                      ; bigger popup window
  ;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  ;; (setq company-echo-delay 0)                          ; remove annoying blinking
  ;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  ;; (define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
  ;; (define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
  ;; (define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
  ;; (define-key go-mode-map (kbd "C-x b") 'go-test-current-benchmark)
  (define-key go-mode-map (kbd "M-.")     #'godef-jump)
  (define-key go-mode-map (kbd "C-c C-k") #'go-run)
  (define-key go-mode-map (kbd "C-c C-d") #'godoc-at-point))

;; pretty lambda
(global-prettify-symbols-mode 1)
;; (ac-set-trigger-key "<?\\M-\\t>")
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")
;; (define-key ac-completing-map (kbd "C-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-p") 'ac-previous)
;;--------------------------------------------------
;; Common-lisp

(use-package redshank
  :ensure t)

;; lisp-mode-hook
;; sly-editing-mode
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (paredit-mode +1)
;;             ;; (projectile-mode +1)
;;             ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;             ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;             (redshank-mode +1)
;;             (aggressive-indent-mode +1)
;;             (yas-minor-mode +1)
;;             ;;(yas-reload-all)
;;             (setq prettify-symbols-alist
;;                   '(("lambda" . 955) ; λ
;;                     ;;("->"  . 8594) ; →
;;                     ;;("=>"  . 8658) ; ⇒
;;                     ;;(":->" . 8594) ;
;;                     ))))

;; FIX lisp ident
;; (put :default-initargs 'common-lisp-indent-function '(&rest))
;; (put 'defstruct-g 'common-lisp-indent-function '(as defstruct))

;;(require 'slime-autoloads)
;;(require 'slime-cl-indent)
;;(require 'sly-cl-indent)
;; (define-common-lisp-style "asdf"
;;   (:inherit "modern")
;;   (:indentation
;;    (define-package  (as defpackage))
;;    (define-constant (as defconstant))))

;; (put 'if 'lisp-indent-function nil)
;; (put 'when 'lisp-indent-function 1)
;; (put 'unless 'lisp-indent-function 1)
;; (put 'do 'lisp-indent-function 2)
;; (put 'do* 'lisp-indent-function 2)

;;--------------------------------------------------
;; Common Lisp - Slime

;; cbaggers/varjo
;; (defun slime-vari-describe-symbol (symbol-name)
;;   "Describe the symbol at point."
;;   (interactive (list (slime-read-symbol-name "Describe symbol: ")))
;;   (when (not symbol-name)
;;     (error "No symbol given"))
;;   (let ((pkg (slime-current-package)))
;;     (slime-eval-describe
;;      `(vari.cl::vari-describe ,symbol-name nil ,pkg))))

;; (define-key lisp-mode-map (kbd "C-c C-v C-v")
;;   'slime-vari-describe-symbol)
;; (define-key lisp-mode-map (kbd "C-c C-a")
;;   'redshank-align-forms-as-columns)

;;; concurrent hints
;; https://www.reddit.com/r/lisp/comments/72v6p3/pushing_pixels_with_lisp_episode_18_shadow/
;; (defun slime-enable-concurrent-hints ()
;;   (interactive)
;;   (setq slime-inhibit-pipelining nil))

;; paredit on repl
;; 'slime-repl-mode-hook

(use-package sly
  :ensure t
  :init
  (setq sly-lisp-implementations     '((sbcl  ("sbcl")))
        sly-complete-symbol-function 'sly-flex-completions
        inferior-lisp-program        "/usr/local/bin/sbcl")
  ;; "modern" style
  (setq lisp-lambda-list-keyword-alignment t
        lisp-lambda-list-keyword-parameter-alignment t
        lisp-lambda-list-keyword-parameter-indentation 0
        lisp-loop-indent-subclauses nil)
  :config
  ;;(setq sly-contribs '(sly-fancy sly-cl-indent))
  ;;(require 'sly-cl-indent)
  (add-hook 'sly-mode-hook
            (lambda ()
              (paredit-mode +1)
              (yas-minor-mode +1)
              (aggressive-indent-mode +1)
              ;; Enable sly-cl-indent
              (setq-local lisp-indent-function 'common-lisp-indent-function))))

(defun sly-enable-concurrent-hints ()
  (interactive)
  (setq sly-inhibit-pipelining nil))

(add-hook
 'sly-mrepl-hook
 (lambda ()
   (paredit-mode +1)
   ;;(define-key slime-repl-mode-map (kbd "C-c C-d C-d") #'slime-describe-symbol)
   ))

;; sbcl real
;; (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))
;;       inferior-lisp-program "/usr/local/bin/sbcl"
;;       slime-contribs '(slime-fancy))

;;--------------------------------------------------
;; Shell - bashate
;; (use-package flycheck-bashate
;;   :ensure t
;;   :config)
(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode +1)
            (define-key sh-mode-map (kbd "M-p") #'flycheck-previous-error)
            (define-key sh-mode-map (kbd "M-n") #'flycheck-next-error)
            (aggressive-indent-mode +1)
            (smartparens-strict-mode +1)
            (sp-use-paredit-bindings)))
(setq flycheck-disabled-checkers '(sh-posix-bash))
(setq-default flycheck-shellcheck-excluded-warnings '("SC2086"))

;; https://nistara.net/post/emacs-send-line-or-region-to-shell/
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      (setq comint-scroll-to-bottom-on-output t))
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))

(define-key sh-mode-map (kbd "C-c C-c") #'sh-send-line-or-region-and-step)

;;--------------------------------------------------
;; Emacs
(use-package elisp-mode
  :ensure nil
  :init
  ;; Default *scratch* buffer to lexical binding.
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (equal (buffer-name) "*scratch*")
                (setq-local lexical-binding t))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (paredit-mode +1)
              (aggressive-indent-mode +1)
              (eldoc-mode +1)
              ;; SLIME like keybinding instead of "C-h f"
              (define-key emacs-lisp-mode-map (kbd "C-c C-d")
                (lambda ()
                  (interactive)
                  (describe-symbol
                   (symbol-at-point))))
              (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'compile-defun))))
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

;;
;; Erlang
;;
(use-package ivy-erlang-complete
  :ensure t)

(use-package erlang
  :load-path ("~/.kerl/builds/22.1/release_22.1/lib/tools-3.2.1/emacs")
  :hook (after-save . ivy-erlang-complete-reparse)
  :mode (("\\.erl\\'"             . erlang-mode)
         ("\\.hrl\\'"             . erlang-mode)
         ("\\.xrl\\'"             . erlang-mode)
         ("rebar\\.config$"       . erlang-mode)
         ("relx\\.config$"        . erlang-mode)
         ("sys\\.config\\.src$"   . erlang-mode)
         ("sys\\.config$"         . erlang-mode)
         ("\\.config\\.src?$"     . erlang-mode)
         ("\\.config\\.script?$"  . erlang-mode)
         ("\\.hrl?$"              . erlang-mode)
         ("\\.app?$"              . erlang-mode)
         ("\\.app.src?$"          . erlang-mode)
         ("\\Emakefile"           . erlang-mode))
  :init
  ;;
  (add-hook 'erlang-mode-hook
            (lambda ()
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (aggressive-indent-mode +1)
              ;;(yas-minor-mode +1)
              ;;
              (define-key erlang-mode-map (kbd "M-p") #'flycheck-previous-error)
              (define-key erlang-mode-map (kbd "M-n") #'flycheck-next-error)
              (flycheck-mode +1)
              ;; pretty needs to happen on init
              ;;(setq-local prettify-symbols-alist '(("fun" .  955) ("->"  . 8594)))
              (setq
               flycheck-erlang-executable "~/.kerl/builds/22.1/release_22.1/bin/erlc"
               flycheck-erlang-include-path (append
                                             (file-expand-wildcards
                                              (concat
                                               (flycheck-rebar3-project-root)
                                               "_build/*/lib/*/include"))
                                             (file-expand-wildcards
                                              (concat
                                               (flycheck-rebar3-project-root)
                                               "_checkouts/*/include")))
               flycheck-erlang-library-path (append
                                             (file-expand-wildcards
                                              (concat
                                               (flycheck-rebar3-project-root)
                                               "_build/*/lib/*/ebin"))
                                             (file-expand-wildcards
                                              (concat
                                               (flycheck-rebar3-project-root)
                                               "_checkouts/*/ebin"))))
              ;;
              (require 'ivy-erlang-complete)
              (define-key erlang-mode-map (kbd "M-TAB") #'ivy-erlang-complete)
              ;;(define-key erlang-mode-map (kbd "C-c C-d h") #'ivy-erlang-complete-show-doc-at-point)
              ;;
              ;;
              ;;(setq load-path (cons "~/.kerl/builds/22.1/lib/tools-2.11.2/emacs" load-path))
              (setq ivy-erlang-complete-erlang-root "~/.kerl/builds/22.1/release_22.1")
              (setq erlang-root-dir "~/.kerl/builds/22.1/release_22.1")
              (setq exec-path (cons "~/.kerl/builds/22.1/release_22.1/bin" exec-path))
              (setq-local ivy-erlang-complete-enable-autosave nil)
              ;;
              (setq ivy-erlang-complete-use-default-keys t
                    ;;ivy-erlang-complete-erlang-root "/usr/lib64/erlang/"
                    )
	      (setq erlang-electric-commands '(erlang-electric-comma
				               erlang-electric-semicolon))
	      (setq erlang-electric-newline-inhibit-list '(erlang-electric-gt))
	      (setq erlang-electric-newline-inhibit t)
              ;; https://github.com/massemanet/dotfiles.nixos/
              (unless (null buffer-file-name)
                (make-local-variable 'compile-command)
                (setq compile-command
                      (cond ((file-exists-p "../rebar.config")
                             "cd .. && rebar3 compile")
                            ((file-exists-p "Makefile")
                             "make -k")
                            ((file-exists-p "../Makefile")
                             "make -kC..")
                            (t (concat
                                "erlc "
                                (if (file-exists-p "../ebin") "-o ../ebin " "")
                                (if (file-exists-p "../include") "-I ../include " "")
                                "+debug_info -W "
                                buffer-file-name))))))
            (ivy-erlang-complete-init))
  :config
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; rebar3 in emacs from:
  ;; https://gist.github.com/maruks/ee19934306bc219bd969ae25aa909f1f
  (setq inferior-erlang-machine "rebar3")
  (setq inferior-erlang-machine-options '("shell"))
  (setq inferior-erlang-shell-type nil)
  ;; NOTE: Prefer to use C-u M-x erlang-shell to use a glob (*) for the deps
  ;; default node name to emacs@localhost
  ;; (setq inferior-erlang-machine-options
  ;;       (list "-name" "emacs@sabayon"
  ;;             "-pa"
  ;;             "../ebin"
  ;;             ;; (file-expand-wildcards
  ;;             ;;  (concat
  ;;             ;;   (flycheck-rebar3-project-root)
  ;;             ;;   "_build/*/lib/*/ebin"))
  ;;             "../_build/default/lib/*/ebin"
  ;;             "../../../_build/default/lib/*/ebin")
  ;;       )

  ;;(setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-function nil)
  ;; ivy should run after flycheck...
  )


(defun hide-erlang-shell ()
  (interactive)
  (other-window 1)
  (delete-other-windows))

(add-hook
 'erlang-shell-mode-hook
 (lambda ()
   (smartparens-strict-mode +1)
   (sp-use-paredit-bindings)
   ;; Bindings set from erlang-mode
   (define-key erlang-shell-mode-map (kbd "M-.")
     #'ivy-erlang-complete-find-definition)
   (define-key erlang-shell-mode-map (kbd "M-,")
     #'xref-pop-marker-stack)
   ;;
   (setq-local ivy-erlang-complete-enable-autosave nil)
   ;;
   ;;(ivy-erlang-complete-autosetup-project-root)
   (setq-local ivy-erlang-complete--eldocs (make-hash-table :test 'equal))
   ;;(ivy-erlang-complete-reparse)
   (set (make-local-variable 'eldoc-documentation-function)
        'ivy-erlang-complete-eldoc)
   ;; Other bindings..
   ;;(define-key erlang-shell-mode-map (kbd "C-c C-d h") #'ivy-erlang-complete-show-doc-at-point)
   (define-key erlang-shell-mode-map (kbd "C-c C-d")   #'erlang-man-function-no-prompt)
   (define-key erlang-shell-mode-map (kbd "M-TAB")     #'ivy-erlang-complete)
   (define-key erlang-shell-mode-map (kbd "TAB")       #'ivy-erlang-complete)
   (define-key erlang-shell-mode-map (kbd "C-c C-z")   #'hide-erlang-shell)))

;; Elixir

;; (add-to-list 'elixir-mode-hook
;;              (defun auto-activate-ruby-end-mode-for-elixir-mode ()
;;                (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;;                     "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;;                (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;;                (ruby-end-mode +1)))

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

;;  https://github.com/jaypei/emacs-neotree/issues/56
;;(magithub-feature-autoinject t)
;;(setq magithub-clone-default-directory "/home/sendai/quicklisp/local-projects/")

;;
;; Tidal
;;
(use-package tidal
  :ensure t
  :config
  (setq tidal-interpreter "/home/sendai/.ghcup/bin/ghci")
  (setq tidal-boot-script-path "/home/sendai/.cabal/share/x86_64-linux-ghc-8.6.5/tidal-1.0.14/BootTidal.hs"))

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
;; Clojure
;;--------------------------------------------------
(use-package cider
  :ensure t
  :config
  (define-key cider-repl-mode-map
    (kbd "C-c M-o") #'cider-repl-clear-buffer)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (paredit-mode +1))))

(use-package clojure-mode
  :ensure t
  ;; :bind
  ;; (("C-c C-d C-h" . cider-clojuredocs)
  ;;  ("C-c ~"       . cider-repl-set-ns))
  :config
  (define-key clojure-mode-map
    (kbd "C-c C-d C-h") #'cider-clojuredocs)
  (define-key clojure-mode-map
    (kbd "C-c ~") #'cider-repl-set-ns)
  (setq cider-repl-display-help-banner nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode +1)
              (aggressive-indent-mode +1))))

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
