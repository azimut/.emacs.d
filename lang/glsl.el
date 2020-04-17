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
