;; sudo apt install pry
;; sudo gem install solargraph rubocop

(use-package ruby-mode
  :ensure nil
  :bind (:map
         ruby-mode-map
         ("C-c C-c" . recompile)))

(use-package inf-ruby)
(use-package robe)
