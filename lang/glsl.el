;;
;; GLSL
;;

(use-package glsl-mode
  :ensure t
  :mode (("\\.pix\\'"   . glsl-mode)
         ("\\.comp\\'"  . glsl-mode)
         ("\\.usf\\'"   . glsl-mode)
         ("\\.ush\\'"   . glsl-mode)
         ("\\.vert\\'"  . glsl-mode)
         ("\\.frag\\'"  . glsl-mode)
         ("\\.geom\\'"  . glsl-mode)
         ("\\.glsl\\'"  . glsl-mode)
         ("\\.vs\\'"    . glsl-mode)
         ("\\.fs\\'"    . glsl-mode)
         ("\\.hlsl\\'"  . glsl-mode)
         ("\\.hlsli\\'" . glsl-mode))
  :config
  (add-hook 'glsl-mode-hook
            (lambda ()
              (interactive)
              (setq-local zeal-at-point-docset '("gl4"))
              (setq-local helm-dash-docsets '("OpenGL4"))
              (smartparens-strict-mode +1)
              (sp-use-paredit-bindings)
              (aggressive-indent-mode +1))))

(use-package shader-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cginc\\'"    . shader-mode)))
