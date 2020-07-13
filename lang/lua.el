;;
;; Lua
;;
(use-package lua-mode
  :mode (("\\.nse\\'" . lua-mode))
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
