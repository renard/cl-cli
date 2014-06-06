(defpackage #:cl-cli
  (:use #:cl)
  (:export
   #:*argv*
   #:*argc*
   #:*prog-name*
   #:*global-options*
   #:*sub-command*
   #:*sub-command-options*
   #:*rest*
   #:defcommand
   #:defoption
   #:parse-cli
   #:run
   #:help))


(in-package #:cl-cli)



(defparameter *argv* asdf/image:*command-line-arguments*
  "Whole command line")

(defparameter *prog-name* (car *argv*)
  "The program name")

(defparameter *args* (cdr *argv*)
  "Command line arguments")

(defparameter *global-options* nil
  "Global options list")

(defparameter *sub-command* nil
  "Verbs routing-list")

(defparameter *sub-command-options* nil
  "Options used for `*sub-command*'")

(defparameter *rest* nil
  "Command line rest after parsing process")



(define-condition option-requires-argument (error)
  ((option :initarg :option :reader option-requires-argument-option)
   (nargs :initarg :nargs :reader option-requires-argument-nargs))
  (:report
   (lambda (condition stream)
     (format stream "Option ~S requires ~a argument~:p"
             (option-requires-argument-option condition)
             (option-requires-argument-nargs condition)))))



(defun %symbol-to-option-string (symbol)
  "Convert SYMBOL to its argument string representation by prepending with
  double dash \"--\" and removing fancy chars such as \"*\"."
  (concatenate 'string "--"
	       ;; (remove #\*  string) seems to be less speed-efficient.
	       (loop for c across (string-downcase (symbol-name symbol))
		     unless (char= c #\*)
		       collecting c)))


(defun %arg-to-symbol-or-keyword (arg &optional keyword)
  "Convert ARG to its symbol representation.

If KEYWORD is set return a keyword instead of symbol

Return NIL if ARG does not start with double dash.

Example:  --foo -> FOO
          --foo-bar -> FOO-BAR"
  (when
      (and (<= 3 (length arg))
	   (char= #\- (schar arg 0) (schar arg 1)))
    (let ((arg-symbol (string-upcase (subseq arg 2))))
      (if keyword
	  (intern arg-symbol "KEYWORD")
	  (intern arg-symbol)))))


(defstruct
    (sub-command
     (:conc-name sub-)
     (:constructor make-sub-command (name function args verbs docstring help)))
  name
  function
  args
  verbs
  docstring
  help)

(defstruct
    (option
     (:conc-name opt-)
     (:constructor make-option (name default string alias metavars type help)))
  name
  default
  string
  alias
  metavars
  type
  help)


(defmacro defoption (name &key default alias metavars type help)
  `(make-option ',name
		(if (boundp ',name) (or (symbol-value ,name) ,default) ,default)
		(%symbol-to-option-string ',name)
		,alias
		,metavars
		,type
		(or ,help (documentation ',name 'variable))))

(defmacro defcommand (name spec docstring &body body)
  "Create a function NAME with BODY and DOCSTRING.

SPEC is used to describe the function &key arguments."
  (let (fn-spec
	(fn-opts (make-hash-table :test 'equal)))
    (loop for cmd-spec in (getf spec :options)
	  do (destructuring-bind
		 (&key name default help metavars type alias
		  &allow-other-keys) cmd-spec
	       (push (list (intern (string-upcase name))
			   default)
		     fn-spec)
	       ;; This is not very clean :(
	       ;; Anyway is there an alternate?
	       (setf (gethash (%symbol-to-option-string name) fn-opts nil)
		     (eval
		      `(defoption ,name :default ,default :alias ,alias
					:metavars ,metavars :type ,type
					:help ,help)))))
    `(progn
       ;;(defun ,name (&key ,@fn-spec) ,docstring ,@body)
       (destructuring-bind (&key verbs help &allow-other-keys) ',spec
	 ;;(make-sub-command #',name ',fn-opts verbs ,docstring help)
	 (locally
	     (declare #+sbcl(sb-ext:muffle-conditions style-warning))
	   (handler-bind
	       (#+sbcl(style-warning #'muffle-warning))
	     (make-sub-command ',name #'(lambda (&key ,@fn-spec) ,@body)
			       ',fn-opts verbs ,docstring help)))))))


(defun consume-option(args option)
  "Extract all OPTION arguments from ARGS.
Return both consumed arguments count and the arguments"
  (let ((nargs (or (length (opt-metavars option)) 0)))
    (when (< (length args) nargs)
      (error 'option-requires-argument
	     :option (opt-string option) :nargs nargs))
    (let ((argument
	    (cond
	      ((= 0 nargs) t)
	      ((= 1 nargs) (car args))
	      (t  (subseq args 0 (1- nargs)))))
	  (type (opt-type option)))
      (values nargs
	      (if type
		  (if (atom argument)
		      (coerce (read-from-string argument) type)
		      (loop for a in argument
			    collect (coerce (read-from-string a) type)))
		  argument)))))

(defun %parse-options (argv options &key (keys nil))
  (let ((idx 0)
	options-vars)
    
    (loop for i from idx below (length argv)
    	  for cur-opt = (nth i argv)
    	  for cur-opt-def = (gethash cur-opt options)
    	  while cur-opt-def
    	  do (multiple-value-bind (nargs vals)
		 (consume-option (subseq argv (1+ i)) cur-opt-def)
	       (setf i (+ i nargs))
	       (if keys
		   (progn
		     (push vals options-vars)
		     (push
		      (%arg-to-symbol-or-keyword (opt-string cur-opt-def) keys)
		      options-vars))
		   (push (list (opt-name cur-opt-def) vals) options-vars)))
    	  finally (setf idx i))

    (values options-vars idx)))

(defun parse-cli (&key (argv *argv*) options sub-commands)
  "Parse the command line as specified in ARGV using both OPTIONS and
SUB-COMMANDS specifiers.

Return
- global options as a variables/value list
- the matched sub-command function
- argument key-list for the sub-command function
- the remaining.
"
  (let* ((*argv* argv)
	 (*prog-name* (car *argv*))
	 (*args* (cdr *argv*))
	 *global-options*
	 (bound-option (make-hash-table))
	 *sub-command-options*
	 *sub-command*
	 *rest*
	 (opt-hash (make-hash-table :test 'equal))
	 (idx 0))
    
    ;; Create lookup table for global options
    (loop for option in options
	  do (progn
	       (setf (gethash (opt-string option) opt-hash) option)
	       ;; Set up aliases
	       (loop for alias in (opt-alias option)
		     do (setf (gethash alias opt-hash) option))
	       ;; Make sure all global options are set
	       (setf (gethash (opt-name option) bound-option)
	       	     (opt-default option))))

    (multiple-value-bind (option-vars consumed)
	(%parse-options *args* opt-hash)
      (setf idx (+ idx consumed))
      ;; Update the global options
      (loop for (opt arg) in option-vars
	    do (setf (gethash opt bound-option) arg))
      ;; Finally transpose the global options as a (var value) list.
      (setf *global-options*
	    (loop for opt being the hash-key of bound-option
		  collect (list opt (gethash opt bound-option)))))

    ;; Parse the subcommand
    (loop for sub in sub-commands
    	  for verbs = (sub-verbs sub)
    	  for args-verbs = (subseq *args* idx (+ idx (length verbs)))
    	  when (equal verbs args-verbs)
    	    return (progn
		     (setq idx (+ idx (length verbs)))
		     (multiple-value-bind (option-vars consumed)
		       (%parse-options (subseq *args* idx)
				       (sub-args sub) :keys t)
		       (setf idx (+ idx consumed))
		       (setf *sub-command-options* option-vars))
		     (setf *sub-command* sub)))

    ;; Set the rest
    (setq *rest* (subseq *args* idx))
    
    
    (values
     *global-options*
     (when *sub-command* (sub-function *sub-command*))
     *sub-command-options*
     *rest*)))

(defmacro with-environment (bindings &body body)
  "Bind variables according to BINDINGS then eval BODY.
The value of the last form in BODY is returned.

BINDINGS is a LET-suitable list."
  (let ((varlist (gensym)))
    `((lambda (,varlist)
	(progv
	    (mapcar #'(lambda (x) (if (consp x) (car x) x)) ,varlist)
	    (mapcar #'(lambda (x) (if (consp x) (cadr x) nil)) ,varlist)
	  ,@body))
      ,bindings)))


(defmacro run ((&key (argv *argv*) options sub-commands) &body body)
  "Parse ARGV command line using both OPTIONS and SUB-COMMANDS definitions.

If BODY is defined it is executed using *GLOBAL-OPTIONS* as environment. The
result of BODY is returned.

If BODY is NIL ans if a sub command is matched, it would be evaluated and
its value is returned."
  (let ((argv-def (gensym))
	(opt-def (gensym))
	(cmd-def (gensym)))
    `((lambda (,argv-def ,opt-def ,cmd-def)
	(multiple-value-bind (*global-options* *sub-command*
			      *sub-command-options* *rest*)
	    (parse-cli :argv ,argv-def :options ,opt-def
		       :sub-commands ,cmd-def)
	  (if ',body
	      (with-environment *global-options* ,@body)
	      (if *sub-command*
		  (with-environment *global-options*
		    (apply *sub-command* *sub-command-options*))))))
      ,argv ,options ,sub-commands)))

(defun %print-option(option)
  (format t  "~2T~a~@[,~{~a~^,~}~]~20T~@[ ~{~a~^ ~}~]~35T~{~<~%~35T~0,79:;~a~>~^ ~}~%"
	  (opt-string option)
	  (opt-alias option)
	  (opt-metavars option)
	  (split-sequence:split-sequence
	   #\ (format nil "~a~:[~; (default: ~:*~@a)~]"
		      (or (opt-help option) "")
		      (opt-default option)))
	  ))

  
(defun help (options sub-commands &key (prog-name *prog-name*) version)
  (format t "~@[~a~]~@[ version ~a~]~%~%" prog-name version)

  (format t
	  "~@[~a~]~:[~; [ OPTIONS ]~]~:[~; [ SUB COMMAND [ OPTIONS ] ]~] ...~%~%"
	  prog-name options sub-commands)

  (when options
    (format t "Global options:~%")
    (loop for option in options
	  do (%print-option option)))

  (when sub-commands
    (format t "~%Sub commands:~%")
    (loop for sub-command in sub-commands
	  do (progn
	       (format t "~%~1T~{~a~^ ~}:~20T~a~%"
		       (sub-verbs sub-command)
		       (sub-help sub-command))
	       (loop for option being the hash-values of (sub-args sub-command)
		     do (%print-option option))))))
