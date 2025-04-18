;; OS: erlang-ls https://github.com/erlang-ls/erlang_ls

(use-package erlang
  :load-path ("~/kerl/27.1/lib/tools-4.1/emacs")
  :hook (erlang-mode . aggressive-indent-mode)
  :hook (erlang-mode . smartparens-strict-mode)
  :hook (erlang-mode . erlang-config)
  :hook (erlang-mode . lsp-mode)
  :mode (("\\.erl\\'"             . erlang-mode)
         ("\\.hrl\\'"             . erlang-mode)
         ("\\.xrl\\'"             . erlang-mode)
         ("\\.escript\\'"         . erlang-mode)
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
  :config
  (ligature-set-ligatures
   'erlang-mode ; todo: gte lte
   '("==" "/=" "++" "&&" "||" "<-" "->" "=>"))
  (require 'smartparens-erlang)
  :init
  (defun erlang-config ()
    ;;(setq load-path (cons "~/.kerl/builds/25.1.2/lib/tools-2.11.2/emacs" load-path))
    (setq erlang-root-dir "~/kerl/27.1")
    (setq exec-path (cons "~/kerl/27.1/bin" exec-path))
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
                      buffer-file-name)))))))

(use-package erlang-shell-mode
  :ensure nil
  :hook (erlang-shell-mode . electric-pair-local-mode)
  :bind (:map erlang-shell-mode-map
              ("C-c C-d" . erlang-man-function-no-prompt)
              ("C-c C-z" . hide-erlang-shell))
  :config
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
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; rebar3 in emacs from:
  ;; https://gist.github.com/maruks/ee19934306bc219bd969ae25aa909f1f
  (setq inferior-erlang-machine "rebar3")
  (setq inferior-erlang-machine-options '("shell"))
  (setq inferior-erlang-shell-type nil)
  :init
  (defun hide-erlang-shell ()
    (interactive)
    (other-window 1)
    (delete-other-windows)))
