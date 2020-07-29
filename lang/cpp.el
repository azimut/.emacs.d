;; company-irony-c-headers????
;; flycheck-irony

(defun my-cmode-hook ()
  ;;(setq-local zeal-at-point-docset '("C" "gl4"))
  (setq-local helm-dash-docsets '("C" "OpenGL4"))
  (irony-mode +1)
  (company-mode +1)
  (aggressive-indent-mode +1)
  ;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-irony)
  ;; (local-set-key (kbd "C-c C-d d")
  ;;                (lambda () (interactive) (manual-entry (current-word))))
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (ggtags-mode +1)
  (setq c-default-style "linux" ;; set style to "linux"
        gdb-many-windows t ;; use gdb-many-windows by default
        gdb-show-main t)
  )

;; NOTE: some extra alignments
(use-package google-c-style
  :config
  (add-hook #'c-mode-common-hook #'google-set-c-style))

(defun my-company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default '("/usr/include/" "/usr/local/include/"
                   "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/libraries/"
                   "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/include/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/avr/include/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/x86_64-pc-linux-gnu/avr/include/")))
    (company-arduino-append-include-dirs default)))

(use-package company-arduino)

(use-package company-irony)
(use-package company-c-headers)

(use-package irony
  :config
  (add-hook #'irony-mode-hook #'company-arduino-turn-on)
  (add-hook #'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc)

(add-hook #'c-mode-hook     #'my-cmode-hook)
(add-hook #'c++-mode-hook   #'my-cmode-hook)

(use-package arduino-mode
  :config
  (add-hook
   #'arduino-mode-hook
   (lambda () (setq-local helm-dash-docsets '("Arduino"))
     (setq irony-arduino-includes-options
           '("-I/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/libraries/"
             "-I/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/"
             "-I/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/include/"
             "-I/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/avr/include/"
             "-I/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/x86_64-pc-linux-gnu/avr/include/"
             "-include"
             "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/Arduino.h"))
     (setq company-arduino-header
           "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/Arduino.h")
     (setq company-arduino-includes-dirs
           '("/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/libraries/"
             "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/"
             "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/include/"
             "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/avr/include/"
             "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/x86_64-pc-linux-gnu/avr/include/"))
     (setq company-c-headers-path-system #'my-company-c-headers-get-system-path)
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-irony)))
  (add-hook #'arduino-mode-hook #'irony-mode))

(use-package auto-minor-mode)
(use-package arduino-cli-mode
  :init (add-to-list 'auto-minor-mode-alist '("\\.ino\\'" . arduino-cli-mode))
  :config (setq arduino-cli-warnings 'all
                arduino-cli-verify t
                arduino-cli-default-fqbn "arduino:avr:uno"
                arduino-cli-default-port "/dev/ttyACM0"))

