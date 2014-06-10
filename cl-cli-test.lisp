(in-package #:cl-cli)

(defparameter *options*
  '((debug nil "Run in debug mode" :alias ("-d"))
    (dir "/tmp" "Change to directory" :metavars ("DIR"))))

(defparameter *commands*
  '((server-start
     ("server" "start")
     (&optional
      (restart "restart instead of start")
      &key (delay 2 "Second to wait" :metavars ("DELAY")))
     "Start or restart server"
     (progn
       (when *debug*
	 (format t "Delay: ~a Restart: ~a~%" delay restart))
       (format t "Server ~a in ~a~%"
	       (if restart "restarted" "started") *dir*)))

    (server-stop
     ("server" "stop")
     (&key (delay 2 "Second to wait" :metavars ("DELAY")))
     "Stop server"
     (progn
       (when *debug*
	 (format t "Delay: ~a~%" delay))
       (format t "Server stopped in ~a~%" *dir*)))))

(defparameter *cli*
  '("./tool" "--dir" "/path/to/chroot" "--debug"
    "server" "start" "--restart" "--delay" "3"
    "instance1" "isntance2"))


;; call:
;; (cl-cli::run-command cl-cli::*cli* cl-cli::*options* cl-cli::*commands*)
