(defpackage :stumpwm-config
  (:use :cl :stumpwm))
(in-package :stumpwm-config)

(setq *input-window-gravity* :center)
(setq *message-window-gravity* :center)
(set-border-color "darkcyan")
(set-msg-border-width 5)
(setq *timeout-wait* 5)

(set-focus-color "orange")
(set-unfocus-color "gray")

(set-prefix-key (kbd "C-3"))


(defcommand shutdown () () (run-shell-command "systemctl poweroff"))
(defcommand reboot () () (run-shell-command "systemctl reboot"))

(define-key *root-map* (kbd "b") "exec firefox")
(define-key *root-map* (kbd "c") "exec xfce4-terminal")
(define-key *root-map* (kbd "C-c") "exec xfce4-terminal")

(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *top-map* (kbd "s-o") "fnext")
(define-key *top-map* (kbd "s-TAB") "fother")

(defcommand group-next () ()
  (gnext)
  (stumpwm::echo-groups (current-screen) stumpwm::*group-format* t))

(defcommand group-prev () ()
  (gprev)
  (stumpwm::echo-groups (current-screen) stumpwm::*group-format* t))

(defcommand group-other () ()
  (gother)
  (stumpwm::echo-groups (current-screen) stumpwm::*group-format* t))

(defcommand groups-verbose () ()
  (stumpwm::echo-groups (current-screen) stumpwm::*group-format* t))

(define-key *top-map* (kbd "s-N") "gnext")
(define-key *top-map* (kbd "s-P") "gprev")
(define-key *top-map* (kbd "s-O") "gother")
(define-key *top-map* (kbd "s-g") "groups-verbose")

(defcommand new-group () ()
  (let ((num-groups (length (screen-groups (current-screen)))))
    (gnew (format nil "group-~d" num-groups))))

(define-key *top-map* (kbd "s-G") "new-group")

(define-key *top-map* (kbd "M-TAB") "windowlist")
(define-key stumpwm::*menu-map* (kbd "TAB") 'stumpwm::menu-down)
(define-key stumpwm::*menu-map* (kbd "M-TAB") 'stumpwm::menu-down)


;(set-font "-gnu-unifont csur-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1")
(set-font (make-instance 'xft:font :family "VL Gothic" :subfamily "regular" :size 12))

(swank:create-server :dont-close t :port 33333)
