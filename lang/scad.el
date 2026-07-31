(use-package scad-mode
  :hook (scad-mode . flymake-mode)
  :hook (scad-mode . corfu-mode))

(use-package scad-preview-mode
  :after scad-mode
  :ensure nil
  :bind (:map scad-preview-mode-map
              ("C-1" . scad-view-reset)
              ("C-2" . scad-view-top)
              ("C-3" . scad-view-bottom)
              ("C-4" . scad-view-left)
              ("C-5" . scad-view-right))
  :init
  (defun scad-view-reset ()
    (interactive)
    (setq-local scad-preview-camera '(0 0 0  50 0 20 500))
    (scad--preview-render))

  (defun scad-view-top ()
    (interactive)
    (setq-local scad-preview-camera '(0 0 0   0 0  0 500))
    (scad--preview-render))

  (defun scad-view-bottom ()
    (interactive)
    (setq-local scad-preview-camera '(0 0 0 180 0  0 500))
    (scad--preview-render))

  (defun scad-view-left ()
    (interactive)
    (setq-local scad-preview-camera '(0 0 0  90 0 180 500))
    (scad--preview-render))

  (defun scad-view-right ()
    (interactive)
    (setq-local scad-preview-camera '(0 0 0  90 0  0 500))
    (scad--preview-render)))
