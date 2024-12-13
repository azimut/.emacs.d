(use-package lua-mode
  :mode (("\\.nse\\'" . lua-mode))
  :hook (lua-mode . smartparens-strict-mode)
  :hook (lua-mode . aggressive-indent-mode)
  :bind
  (:map lua-mode-map
        ("C-c C-d" . lua-search-documentation)
        ("C-c C-k" . lua-send-buffer)
        ("C-c C-c" . lua-send-region))
  :config
  (ligature-set-ligatures
   'lua-mode
   '("<=" ">=" "==" "!=" "&&" "||"))
  (add-hook
   #'lua-mode-hook
   (lambda ()
     (setq lua-indent-level 2)
     (setq-local prettify-symbols-alist '(("function" .  955)))
     (setq lua-default-application "~/.luarocks/bin/rep.lua")
     ;; FIXME: still doesn't work
     (setenv "LUA_PATH" (concat "/usr/share/awesome/lib/?/init.lua;"
                                "/usr/share/awesome/lib/?.lua;"
                                "/usr/share/awesome/lib/?/?.lua;;"))
     (setenv "LUA_REPL_RLWRAP" "sure");?
     )))

(defun azm-renoise-manual ()
  (interactive)
  (dired "~/projects/lua/xrnx/Documentation/")
  (dired-hide-details-mode)
  (rename-buffer "*renoise-manual*"))
