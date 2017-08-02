(defpackage :stump-config.quickmenu
  (:use :cl :stumpwm))
(in-package :stump-config.quickmenu)

(defvar *table*
  '(("firefox" "exec firefox")
    ("thunderbird" "exec thunderbird")
    ("terminal" "exec xfce4-terminal")
    ("cl-aip" "exec fbreader ~/Desktop/CL-AIP201706.epub")
    ("pavucontrol" "exec pavucontrol")))

(defcommand stump-quick-menu () ()
  (let ((elt (select-from-menu (current-screen)
                               *table*)))
    (when elt
      (stumpwm::eval-command (cadr elt)))))

(define-key *top-map* (kbd "M-F2") "stump-quick-menu")
