(defpackage :stumpwm-config.quickmenu
  (:use :cl :stumpwm))
(in-package :stumpwm-config.quickmenu)

(defvar *table*
  '(("firefox"     :command "exec firefox")
    ("mail"        :command "exec thunderbird")
    ("terminal"    :command "exec xfce4-terminal")
    ("cl-aip"      :command "exec fbreader ~/Desktop/CL-AIP201706.epub")
    ("pavucontrol" :command "exec pavucontrol")))

(defun switch-to-window (window)
  (stumpwm::switch-to-group (stumpwm::window-group window))
  (focus-window window))

(defun window-table ()
  (mapcar (lambda (w)
            (list (stumpwm::format-expand *window-formatters* "%t" w)
                  :window w))
          (stumpwm::all-windows)))

(defun otherwise-command (input)
  (run-shell-command (format nil "firefox --private http://ejje.weblio.jp/content/~A" input)))

(defcommand stump-quick-menu () ()
  (multiple-value-bind (elt input)
      (select-from-menu (current-screen)
                        (append *table*
                                '(("--------------------" nil nil))
                                (window-table)))
    (cond (elt
           (ecase (second elt)
             ((:command)
              (stumpwm::eval-command (third elt)))
             ((:window)
              (switch-to-window (third elt)))
             ((nil))))
          ((string/= input "")
           (otherwise-command input)))))

(define-key *top-map* (kbd "M-F2") "stump-quick-menu")


(in-package :stumpwm)

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit 'abort))

(defun select-from-menu (screen table &optional (prompt "Search:")
                                                (initial-selection 0)
                                                extra-keymap
                                                (filter-pred #'menu-item-matches-regexp))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings.
EXTRA-KEYMAP can be a keymap whose bindings will take precedence
over the default bindings.
FILTER-PRED should be a a function returning T when a certain menu
item should be visible to the user.  It should accept arguments
ITEM-STRING (the string shown to the user), ITEM-OBJECT (the object
corresponding to the menu item), and USER-INPUT (the current user
input). The default is MENU-ITEM-MATCHES-REGEXP.
Returns the selected element in TABLE or nil if aborted. "
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)

  (when table
    (let* ((*record-last-msg-override* t)
           (*suppress-echo-timeout* t)
           (menu (make-menu-state
                  :unfiltered-table table
                  :table table
                  :filter-pred filter-pred
                  :prompt prompt
                  :view-start 0
                  :view-end 0
                  :selected initial-selection))
           (keymap (if extra-keymap
                       (list extra-keymap *menu-map*)
                       (list *menu-map*))))
      (bound-check-menu menu)
      (let ((result
              (catch :menu-quit
                (unwind-protect
                     (with-focus (screen-key-window screen)
                       (loop
                         (let* ((sel (menu-state-selected menu))
                                (start (menu-state-view-start menu))
                                (end (menu-state-view-end menu))
                                (len (length (menu-state-table menu)))
                                (prompt-line (when (menu-prompt-visible menu)
                                               (format nil "~@[~A ~]~A"
                                                       prompt (menu-state-current-input menu))))
                                (strings (mapcar #'menu-element-name
                                                 (subseq (menu-state-table menu)
                                                         start end)))
                                (highlight (- sel start)))
                           (unless (zerop start)
                             (setf strings (cons "..." strings))
                             (incf highlight))
                           (unless (= len end)
                             (setf strings (nconc strings '("..."))))
                           (when prompt-line
                             (push prompt-line strings)
                             (incf highlight))
                           (echo-string-list screen strings highlight))
                         (multiple-value-bind (action key-seq) (read-from-keymap keymap)
                           (if action
                               (progn (funcall action menu)
                                      (bound-check-menu menu))
                               (check-menu-complete menu (first key-seq))))))
                  (unmap-all-message-windows)))))
        (if (eq 'abort result)
            (values nil "")
            (values result (menu-state-current-input menu)))))))
