(defpackage #:stumpwm-config.modeline
  (:use :cl :stumpwm))
(in-package #:stumpwm-config.modeline)

(defvar *battery-last-update* nil)
(defvar *battery-last-text* nil)

(defun battery-text ()
  (cond ((or (null *battery-last-update*)
             (< 10 (- (get-universal-time) *battery-last-update*)))
         (setf *battery-last-update* (get-universal-time))
         (let ((info (first (trivial-battery:battery-info))))
           (when info
             (let* ((percentage (cdr (assoc "percentage" info :test #'equal)))
                    (charging (cdr (assoc "charging" info :test #'equal))))
               (setf *battery-last-text*
                     (format nil "~D% ~:[BAT~;AC~]" percentage charging))))))
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

(defun adjust-width (str n)
  (let ((width (stumpwm-config.column-util:string-width str)))
    (if (< n width)
        (setf str (concatenate 'string
                               (subseq str 0 (- (stumpwm-config.column-util:wide-index str n) 3))
                               "..."))
        (concatenate 'string str (make-string (- n width) :initial-element #\space)))))

(defun window-list-string (ml)
  (format nil "~{~A~}"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (setf str (adjust-width str 20))
                      (setf str (format nil "[~A]" str))
                      (if (eq w (current-window))
                          (stumpwm::fmt-highlight str)
                          str)))
                  (stumpwm::sort1 (stumpwm::head-windows (stumpwm::mode-line-current-group ml)
                                                         (stumpwm::mode-line-head ml))
                                  #'< :key #'window-number))))

(defun modeline-string (ml)
  (format nil "^B~A [~A]^b ^B~A^b ^B~A^b"
          (current-date)
          (battery-text)
          (groups-string)
          (window-list-string ml)))

(add-screen-mode-line-formatter #\@ 'modeline-string)

(dolist (screen *screen-list*)
  (unless (stumpwm::head-mode-line (current-head))
    (toggle-mode-line screen (current-head))))

(setf *screen-mode-line-format* "%@")
