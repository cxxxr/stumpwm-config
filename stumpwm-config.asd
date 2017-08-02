(defsystem "stumpwm-config"
  :depends-on ("trivial-battery")
  :components ((:file "main")
               (:file "modeline")
               (:file "quickmenu")))
