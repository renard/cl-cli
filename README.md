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


* The *program name*: `./tool`. This is how the program is called.
* Some *global options*: `--verbose --chroot /path/to/dir`. Each option can
  be a *switch*, such as `--verbose`, which is argument less, or can have
  one (or many) argument: `/path/to/dir` is an argument of the `--chroot`
  option.
* A *sub-command*: `server start`. A sub command can be one or many verbs
  (or terms).
* Some *sub-command options*: `--restart --delay 10`. This is the same as
  the *global options* but specific to the *sub-command*.
* A *rest*: `instance1 instance2` its the command line remains after parsing
  process.

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

The 3rd and 4th values are the sub-command to be run (if any, see below) and
its arguments. The 5th value is the cli rest after parsing.

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

### The options list

This is a list of options you pass to te parser. Each option is defined as:

	(name default help &key long alias params type)

* `name`: the option name, or variable to which the option is bound to.
* `default`: The option default value.
* `help`: a docstring that is used to display help.
* `long`: the option long form generated against `name`. For example
  `*foo-bar*`: will result as `--foo-bar`.
* `alias`: a list of option aliases. This is a good place to define
  shortcuts such as `-f` for `--foo` or `-b` for `--bar`.
* `params`: a list of option parameters that define how many arguments the
  option requires. If `params` is `nil` no option is required. n the other
  hand if you need to defined a chroot diretory you can setup `params` to
  `("dir")`. Thus `--chroot` requires one mandatory argument. The `"dir"`
  string is used in help display.
* `type`: A type to which the option should be converted to.

### The commands list

As for options, the command list defined all known commands the parser
should be aware of. A command is created using the `CL-CLI:DEFCOMMAND`
macro.

	(defcommand verbs options positional docstring func)

* `verbs`: A list of words that triggers the command.
* `options`: a list of command options.
* `positional`: a list of mandatory positional arguments (order
  maters). This is like an option but with no default value.
* `docstring`: a string that is used to display help.
* `func`: a function body that should handle the command.

### Functions

#### parse-cli

	parse-cli (argv &optional options commands)

Parse `ARGV` using `OPTIONS` both and `COMMANDS` directives.

Returns
* options variables
* options values
* matched command dispatch function
* dispatch function keyword arguments
* the rest of the command line argument"

#### with-environment

	with-environment (vars vals &body body)

Run `BODY` after binding `VARS` to `VALS`.

The 2 first values of `parse-cli` can be used to feen `with-environment`.

#### run-command

	run-command (argv &optional options commands)

Run `parse-cli` and execute the matched dispatch function.

#### help

	help (options sub-commands &key prog-name version prolog epilog)

Display help screen.

* `prog-name`: the program name.
* `version`: the program version.
* `prolog`: a text that should be displayed at the beging.
* `epilog`: a text that should be diplayed at the end.




## Copyright

Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
License: WTFPL, grab your copy here: http://www.wtfpl.net


