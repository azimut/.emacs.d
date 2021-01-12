(use-package irony
  :config
  ;;(add-hook #'irony-mode-hook #'company-arduino-turn-on)
  (add-hook #'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc)


;; (use-package company-irony
;;   :config (add-to-list 'company-backends 'company-irony))
;; (use-package company-c-headers
;;   :config (add-to-list 'company-backends 'company-c-headers))
;; (use-package google-c-style
;;   :hook (c-mode-common . google-set-c-style))


(use-package company-arduino)
(use-package auto-minor-mode)
(use-package arduino-cli-mode
  :mode "\\.ino\\'"
  :config (setq arduino-cli-warnings 'all
                arduino-cli-verify t
                arduino-cli-default-fqbn "arduino:avr:uno"
                arduino-cli-default-port "/dev/ttyACM0"))

(defun my-company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default '("/usr/include/" "/usr/local/include/"
                   "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/libraries/"
                   "/home/sendai/.arduino15/packages/arduino/hardware/avr/1.8.3/cores/arduino/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/include/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/avr/include/"
                   "/home/sendai/.arduino15/packages/arduino/tools/avr-gcc/7.3.0-atmel3.6.1-arduino7/x86_64-pc-linux-gnu/avr/include/")))
    (company-arduino-append-include-dirs default)))

(use-package arduino-mode
  :config
  (add-hook
   #'arduino-mode-hook
   (lambda () (setq-local dash-docs-common-docsets '("Arduino"))
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
     (irony-eldoc +1)))
  (add-hook #'arduino-mode-hook #'irony-mode))
