(use-package scad-mode
  :hook (scad-mode . flymake-mode)
  :hook (scad-mode . corfu-mode))

(use-package scad-preview-mode
  :after scad-mode
  :ensure nil
  :bind (:map scad-preview-mode-map
              ("1" . scad-view-reset)
              ("2" . scad-view-top)
              ("3" . scad-view-bottom)
              ("4" . scad-view-left)
              ("5" . scad-view-right)
              ("6" . scad-view-front)
              ("7" . scad-view-back))
  :init
  (defun scad-view-reset ()
    (interactive)
    (setq-local scad-preview-camera (copy-tree '(0 0 0  50 0  20 500)))
    (scad--preview-render))

  (defmacro define-scad-view (name camera-args)
    (let ((fun-name (intern (concat "scad-view-" (symbol-name name)))))
      `(defun ,fun-name ()
         (interactive)
         (setq-local scad-preview-old-distance (nth 6 scad-preview-camera))
         (setq-local scad-preview-camera
                     (append ,camera-args (list scad-preview-old-distance)))
         (scad--preview-render))))

  (define-scad-view top    (copy-tree '(0 0 0   0 0   0)))
  (define-scad-view bottom (copy-tree '(0 0 0 180 0   0)))
  (define-scad-view left   (copy-tree '(0 0 0  90 0 180)))
  (define-scad-view right  (copy-tree '(0 0 0  90 0   0)))
  (define-scad-view front  (copy-tree '(0 0 0  90 0  90)))
  (define-scad-view back   (copy-tree '(0 0 0  90 0 270))))
