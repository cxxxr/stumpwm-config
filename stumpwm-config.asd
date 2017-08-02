(defsystem "stumpwm-config"
  :depends-on ("trivial-battery" "ttf-fonts")
  :components ((:file "stumpwm-config")
               (:file "modeline")
               (:file "quickmenu")))
