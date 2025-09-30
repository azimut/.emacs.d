(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/syl20bnr/spacemacs/issues/12535
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; other WORKAROUND:
;;(setq package-check-signature nil)

;; https://www.emacswiki.org/emacs/ShowParenMode
;; parens match
(show-paren-mode 1)

;; Fuck tilde!
(setq make-backup-files nil)

;; I want spaces for indentation
(setq-default indent-tabs-mode nil)

(put 'erase-buffer 'disabled nil)

;;(grep-apply-setting 'grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-command
      (quote
       ("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49)))
(setq grep-command "grep --color -nHirI -e \"\" *")
(setq grep-find-template
      "find <D> <X> -type f <F> -exec grep <C> --exclude='*.svn-base' -nH --null -e <R> \\{\\} +")

;; https://stackoverflow.com/questions/40060220/how-to-prevent-emacs-from-scrolling-with-the-mouse-past-the-end-of-the-buffer
;; Avoids SOME mouse scrolling after file ended, to mid screen last line
(setq scroll-conservatively 101)
(setq next-line-add-newlines nil)

;;--------------------------------------------------

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
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; pretty lambda
(global-prettify-symbols-mode 1)

;; window resize
(global-set-key (kbd "<C-up>")    'shrink-window)
(global-set-key (kbd "<C-down>")  'enlarge-window)
(global-set-key (kbd "<C-left>")  'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(global-set-key (kbd "C-x }") 'windmove-swap-states-right)
(global-set-key (kbd "C-x {") 'windmove-swap-states-left)

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

(use-package dired
  :ensure nil
  ;; from dired-do-redisplay
  :bind (:map dired-mode-map ("l" . dired-up-directory)))

(setq dired-listing-switches "-lh")
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|*.fasl"))
(setq use-package-always-ensure t)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (dired-sort-toggle-or-edit)))


(use-package phi-search
  :config (require 'phi-search)
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(setq browse-url-firefox-program "/snap/bin/firefox")


(defun browse-url-other (url &rest args)
  (interactive)
  (split-window)
  (balance-windows)
  (other-window 1)
  (w3m-browse-url url))
;;(setq browse-url-browser-function #'browse-url-other)
(setq browse-url-browser-function #'browse-url-firefox)
(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key "\C-n"   'w3m-next-anchor)
            (local-set-key "\C-p"   'w3m-previous-anchor)
            (local-set-key '[up]    'previous-line)
            (local-set-key '[down]  'next-line)
            (local-set-key '[left]  'backward-char)
            (local-set-key '[right] 'forward-char)))

(use-package compile
  :ensure nil
  :custom
  ;; https://github.com/fsharp/zarchive-fsharpbinding/issues/246
  (compilation-auto-jump-to-first-error nil)
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output t))

(use-package dash)
(use-package helm-dash
  :config
  (setq dash-docs-browser-func 'eww)
  (setq dash-docs-enable-debugging nil))

;;(use-package systemd)
(use-package vterm
  :bind ( :map vterm-mode-map ("C-x [" . vterm-copy-mode);; TODO: C-q [
          :map vterm-copy-mode-map ("C-c" . vterm-copy-mode-done) ("M-w" . vterm-copy-mode-done))
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local mode-line-format nil))))

(use-package lorem-ipsum)

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window     t)
  (ag-reuse-buffer     t)
  :bind (:map ag-mode-map ("M-." . compile-goto-error)))

(use-package string-inflection
  :bind ("C-c j" . string-inflection-toggle))

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)
         ("C-c C-+" . evil-numbers/inc-at-pt-incremental)
         ("C-c C--" . evil-numbers/dec-at-pt-incremental)))

(use-package image-mode
  :bind (:map image-mode-map
              ("b" . nil); old frame controls
              ("f" . nil); old frame controls
              ("," . image-previous-frame)
              ("." . image-next-frame)
              ("q" . kill-current-buffer))
  :ensure nil)

(use-package recentf
  :bind ("C-S-t" . recentf-open-most-recent-file)
  :custom
  ;; (recentf-auto-cleanup 'never)
  (recentf-save-file "~/.emacs.d/recentf")
  (recentf-max-menu-items 30)
  (recentf-max-saved-items nil)
  :config
  (defun undo-kill-buffer (arg)
    "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
    (interactive "p")
    (find-file (nth (1- arg) recentf-list))))

(recentf-mode t)
(add-hook 'makefile-mode-hook (lambda () (setq-local whitespace-style '(face tabs empty))))

(defun evince-open-pdf ()
  (interactive)
  (let ((pdf (thing-at-point 'url t)))
    (when (and (string-search "page=" pdf)
               (string-search "file:///" pdf))
      (cl-destructuring-bind (_ page)
          (assoc "page"
                 (url-parse-query-string
                  (cl-subseq pdf (1+ (string-search "#" pdf)))))
        (apply #'start-process
               (concat "/usr/bin/evince" pdf)
               nil
               "/usr/bin/evince"
               (list (format "--page-index=%d" (string-to-number page))
                     pdf))))))

;;-------------------------------------------------

(load-file "~/.emacs.d/scm.el")
(load-file "~/.emacs.d/prog.el")
(load-file "~/.emacs.d/ui.el")

(load-file "~/.emacs.d/lang/awk.el")
(load-file "~/.emacs.d/lang/c.el")
(load-file "~/.emacs.d/lang/clojure.el")
(load-file "~/.emacs.d/lang/elisp.el")
(load-file "~/.emacs.d/lang/elm.el")
(load-file "~/.emacs.d/lang/erlang.el")
(load-file "~/.emacs.d/lang/futhark.el")
(load-file "~/.emacs.d/lang/go.el")
(load-file "~/.emacs.d/lang/haskell.el")
(load-file "~/.emacs.d/lang/html.el")
(load-file "~/.emacs.d/lang/javascript.el")
(load-file "~/.emacs.d/lang/lisp.el")
(load-file "~/.emacs.d/lang/lua.el")
(load-file "~/.emacs.d/lang/markup.el")
(load-file "~/.emacs.d/lang/ocaml.el")
(load-file "~/.emacs.d/lang/python.el")
(load-file "~/.emacs.d/lang/shell.el")

;;(load-file "~/.emacs.d/lang/arduino.el")
;;(load-file "~/.emacs.d/lang/chuck.el")
;;(load-file "~/.emacs.d/lang/cobol.el")
;;(load-file "~/.emacs.d/lang/elixir.el")
;;(load-file "~/.emacs.d/lang/fennel.el")
;;(load-file "~/.emacs.d/lang/glsl.el")
;;(load-file "~/.emacs.d/lang/java.el")
;;(load-file "~/.emacs.d/lang/livecoding.el")
;;(load-file "~/.emacs.d/lang/nim.el")
;;(load-file "~/.emacs.d/lang/pascal.el")
;;(load-file "~/.emacs.d/lang/roc.el")
;;(load-file "~/.emacs.d/lang/rust.el")
