(defsystem "stumpwm-config"
  :depends-on ("trivial-battery")
  :components ((:file "stumpwm-config")
               (:file "modeline")
               (:file "quickmenu")))
