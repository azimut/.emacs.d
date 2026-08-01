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
  (defmacro define-scad-view (name camera-args)
    (let ((fun-name (intern (concat "scad-view-" (symbol-name name)))))
      `(defun ,fun-name ()
         (interactive)
         (setq-local scad-preview-camera ,camera-args)
         (scad--preview-render))))
  (define-scad-view reset  '(0 0 0  50 0  20 500))
  (define-scad-view top    '(0 0 0   0 0   0 500))
  (define-scad-view bottom '(0 0 0 180 0   0 500))
  (define-scad-view left   '(0 0 0  90 0 180 500))
  (define-scad-view right  '(0 0 0  90 0   0 500))
  (define-scad-view front  '(0 0 0  90 0  90 500))
  (define-scad-view back   '(0 0 0  90 0 270 500)))
