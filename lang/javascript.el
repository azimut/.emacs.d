;; lsp-install-server >> ts-ls
;; lsp-install-server >> eslint
;; lsp-install-server >> json-ls

;; import-js   : npm install -g import-js
;; web-beautify: npm install -g js-beautify
;;
;; tern        : (2020) https://github.com/ternjs/tern https://ternjs.net/
;; tide        : (2022) https://github.com/ananthakumaran/tide
;;               Typescript Interactive Development Environment
;;
;; js2-mode
;; js2-refactor

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(use-package js2-mode)
(use-package rjsx-mode)
(use-package js-react-redux-yasnippets)

(use-package add-node-modules-path
  :hook (web-mode web-tsx-mode))

(defun js-config ()
  (setq-local create-lockfiles             nil)
  (setq-local lsp-enable-indentation       nil)
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-insertion-on-trigger nil))

(use-package js-mode
  :hook (js-mode . lsp)
  :hook (js-mode . smartparens-strict-mode)
  :hook (js-mode . js-config)
  :ensure nil)

(use-package typescript-mode
  :hook (typescript-mode . lsp)
  :hook (typescript-mode . smartparens-strict-mode)
  :hook (typescript-mode . js-config))
