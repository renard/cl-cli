# CL-CLI

Simple and easy to use modern command line argument parser for common lisp.


## Definition

A typical command line consists of a *program name*, eventually *options*,
*sub-commands*, *sub-command options* and a remaining.

A modern command line would look like:

	./tool --verbose --chroot /path/to/dir server start --restart \
		--delay 10 instance1 instance2

A program get its argument by the mean of list (or a vector) of strings such
as:

	'("./tool" "--verbose" "--chroot" "/path/to/dir" "server" "start"
		"--restart" "--delay" "10" "instance1" "instance2")

It is up to the programmer's responsibility to parse and interpret the
command line. We can split that command line into several parts:


* The *program name* (`CL-CLI:*PROG-NAME*`): `./tool`. This is how the
  program is called.
* Some *global options* (`CL-CLI:*GLOBAL-OPTIONS*`): `--verbose --chroot
  /path/to/dir`. Each option can be a *switch*, such as `--verbose`, which
  is argument less, or can have one (or many) argument: `/path/to/dir` is an
  argument of the `--chroot` option.
* A *sub-command* (`CL-CLI:*SUB-COMMAND*`): `server start`. A sub command
  can be one or many verbs (or terms).
* Some *sub-command options* (`CL-CLI:*SUB-COMMAND-OPTIONS*`): `--restart
  --delay 10`. This is the same as the *global options* but specific to the
  *sub-command*.
* A *rest* (`CL-CLI:*REST*`): `instance1 instance2` its the command line
  remains after parsing process.

## CL-CLI usage


### Basic usage

Think of a global option as a global variable as defined with
`DEFPARAMETER` or `DEFVAR`. For example you would define `*DEBUG*`:

	(defparameter *debug* nil "Run in debug mode")

Or in a more laconic way:

	(defvar *debug*)

Now you only need to defined an option list suitable for `CL-CLI:PARSE-CLI`:

	CL-USER> (defparameter *options*
	'((*debug* nil "Run in debug mode" :alias ("-d"))
	  (*dir* "/tmp" "Change to directory" :params ("DIR"))))


Now you can parse the command line:

	CL-USER> (cl-cli::parse-cli '("./tool" "--dir" "/path/to/chroot" "--debug"
				      "server" "start" "--restart" "--delay" "3"
				      "instance1" "isntance2") *options*)
	(*DEBUG* *DIR*)
	(T "/path/to/chroot")
	NIL
	NIL
	NIL
	CL-USER> 

The first two returned values are the variable list and their values
suitable for `PROGV`. You can now bind these variables and execute code
using the `CL-CLI:WITH-ENVIRONMENT` macro:

	CL-USER> (multiple-value-bind (vars vals)
		     (cl-cli::parse-cli '("./tool" "--dir" "/path/to/chroot" "--debug"
					  "server" "start" "--restart" "--delay" "3"
					  "instance1" "isntance2") *options*)
		   (cl-cli::with-environment vars vals
					     (format t "dir: ~a, debug: ~a~%" *dir* *debug*)))
	dir: /path/to/chroot, debug: T
	NIL


### Advanced usage

A more advanced usage would use sub-commands. A sub-command defined by using
`CL-CLI:DEFCOMMAND` which requires a dispatch verb list, an options list, a
docstring and a body.

	CL-USER> (cl-cli::defcommand
		     ("server" "start")
		     ((restart nil "restart instead of start")
		      (delay 2 "Second to wait" :params ("DELAY")))
		     "Start or restart server"
		   (when *debug*
		     (format t "Delay: ~a Restart: ~a~%" delay restart))
		   (format t "Server ~a in ~a~%"
			   (if restart "restarted" "started") *dir*)
		   'start)
	#S(CL-CLI::SUB-COMMAND
	   :VERBS ("server" "start")
	   :OPTIONS ((RESTART NIL "restart instead of start")
	             (DELAY 2 "Second to wait" :METAVARS ("DELAY")))
	   :DOCSTRING "Start or restart server"
	   :FUNC #<FUNCTION (LAMBDA (&KEY (RESTART ()) (DELAY 2))) {1003B1B4DB}>)






`CL-CLI:DEFCOMMAND` create the function and returns a `CL-CLI:COMMAND`
structure:

	CL-USER> (cl-cli:defcommand server-start
		     (:help "Start server"
		      :verbs ("server" "start")
		      :options ((restart :help "restart instead of start")
				(:name delay :default 2 :params ("DELAY") :help "Seconds to wait")))
	  "Start or restart server"
	  (when *debug*
	    (format t "Delay: ~a Restart: ~a~%" delay restart))
	  (format t "Server ~a in ~a~%" (if restart "restarted" "started") *dir*))
	#S(CL-CLI::SUB-COMMAND
	   :NAME SERVER-START
	   :FUNCTION #<FUNCTION (LAMBDA (&KEY (DELAY 2) (RESTART ()))) {1006330A7B}>
	   :ARGS #<HASH-TABLE :TEST EQUAL :COUNT 2 {1006153A43}>
	   :VERBS ("server" "start")
	   :DOCSTRING "Start or restart server"
	   :HELP "Start server")
	CL-USER> 


A full working example would be something like:

	CL-USER> (let ((options
		(list
		 (cl-cli::defoption '*debug* :default nil)
		 (cl-cli::defoption '*dir* :default "/tmp" :params '("DIR"))))
	      (argv '("./tool" "--dir" "/path/to/chroot" "--debug"
		      "server" "start" "--restart" "--delay" "3"
		      "instance1" "isntance2"))
	      (sub-commands
		(list (cl-cli::defcommand server-start
			  (:help "Start server"
			   :verbs ("server" "start")
			   :options ((restart
				      :help "restart instead of start")
				     (delay :default 2 :params '("DELAY")
				      :help "Seconds to wait")))
			"Start or restart server"
			(when *debug*
			  (format t "Delay: ~a Restart: ~a~%" delay restart))
			(format t "Server ~a in ~a~%"
				(if restart "restarted" "started") *dir*))
		      (cl-cli::defcommand server-stop
			  (:help "Stop server"
			   :verbs ("server" "stop")
			   :options ((:name delay :default 2 :params '("DELAY")
				      :help "Seconds to wait")))
			"Stop server"
			(when *debug*
			  (format t "Delay: ~a~%" delay))
			(format t "Server stopped in ~a~%" *dir*)))))
	  (cl-cli::run (:argv argv :options options :sub-commands sub-commands)))
	
	
	Delay: 3 Restart: T
	Server restarted in /path/to/chroot
	NIL
	CL-USER> 

## API




## Copyright

Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
License: WTFPL, grab your copy here: http://www.wtfpl.net


