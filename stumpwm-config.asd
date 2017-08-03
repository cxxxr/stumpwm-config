(defsystem "stumpwm-config"
  :depends-on ("trivial-battery" "ttf-fonts" "swank")
  :components ((:file "stumpwm-config")
               (:file "column-util")
               (:file "modeline")
               (:file "quickmenu")))
