(defpackage #:stumpwm-config.modeline
  (:use :cl :stumpwm))
(in-package #:stumpwm-config.modeline)

(defvar *battery-last-update* nil)
(defvar *battery-last-text* nil)

(defun battery-text ()
  (cond ((or (null *battery-last-update*)
             (< 10 (- (get-universal-time) *battery-last-update*)))
         (setf *battery-last-update* (get-universal-time))
         (let* ((alist (trivial-battery:battery-info))
                (percentage (cdr (assoc "percentage" alist :test #'equal)))
                (charging (cdr (assoc "charging" alist :test #'equal))))
           (setf *battery-last-text*
                 (format nil "~D% ~:[BAT~;AC~]" percentage charging))))
        (t
         *battery-last-text*)))

(defun current-date ()
  (multiple-value-bind (second minute hour day month year day-of-week daylight-p zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore second daylight-p zone))
    (format nil "~2,'0D:~2,'0D ~D/~D/~D ~[月~;火~;水~;木~;金~;土~;日~]曜日"
            hour minute year month day day-of-week)))

(defun groups-string ()
  (format nil "~{~A~}"
          (loop :for n :from 1
                :for g :in (stumpwm::sort-groups (current-screen))
                :collect (let ((str (format nil " ~D " n)))
                           (if (eq g (current-group))
                               (stumpwm::fmt-highlight str)
                               str)))))

(defun modeline-string (ml)
  (format nil "^B~A [~A]^b [^B~A^b] ^B~A^b"
          (current-date)
          (battery-text)
          (groups-string)
          (stumpwm::fmt-head-window-list ml)))

(add-screen-mode-line-formatter #\@ 'modeline-string)

(unless (stumpwm::head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head)))

(setf *screen-mode-line-format* "%@")
