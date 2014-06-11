(in-package #:cl-cli)

(defvar *debug*)
(defvar *dir*)
(defparameter *options*
  '((*debug* nil "Run in debug mode" :alias ("-d"))
    (*dir* "/tmp" "Change to directory" :metavars ("DIR"))))

(defparameter *commands*
  (list
   (cl-cli::defcommand
       ("server" "start")
       ((restart nil "restart instead of start")
	(delay 2 "Second to wait" :metavars ("DELAY")))
       "Start or restart server"
     (when *debug*
       (format t "Delay: ~a Restart: ~a~%" delay restart))
     (format t "Server ~a in ~a~%"
	     (if restart "restarted" "started") *dir*)
     "start")

   (cl-cli::defcommand
       ("server" "stop")
       ((delay 2 "Second to wait" :metavars ("DELAY")))
       "Stop server"
     (when *debug*
       (format t "Delay: ~a~%" delay))
     (format t "Server stopped in ~a~%" *dir*)
     "stop")))

;; (cl-cli::run-command
;;  '("./tool" "--dir" "/path/to/chroot" "--debug"
;;    "server" "start" "--restart" "--delay" "3"
;;    "instance1" "isntance2") *options* *commands*)
