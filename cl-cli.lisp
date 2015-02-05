(defpackage #:cl-cli
  (:use #:cl)
  (:export
   #:defcommand
   #:defcommand-to-list
   #:parse-cli
   #:run-command
   #:with-environment
   #:help))


(in-package #:cl-cli)



(define-condition option-requires-argument (error)
  ((option :initarg :option :reader option-requires-argument-option)
   (nargs :initarg :nargs :reader option-requires-argument-nargs))
  (:report
   (lambda (condition stream)
     (format stream "Option ~S requires ~a argument~:p"
             (option-requires-argument-option condition)
             (option-requires-argument-nargs condition)))))

(define-condition bad-argument-type (error)
  ((option :initarg :option :reader bad-argument-type-option)
   (type :initarg :type :reader bad-argument-type-type))
  (:report
   (lambda (condition stream)
     (format stream "Option ~S requires a ~(~a~) argument"
             (bad-argument-type-option condition)
             (bad-argument-type-type condition)))))

(define-condition not-enougth-pos-args (error)
  ((command :initarg :command :reader not-enougth-pos-args-command)
   (count :initarg :count :reader not-enougth-pos-args-count)
   (items :initarg :items :reader not-enougth-pos-args-items))
  (:report
   (lambda (condition stream)
     (format stream "Command \"~{~a~^ ~}\" requires ~a positional argument~:p: ~{~a~^, ~}."
             (not-enougth-pos-args-command condition)
             (not-enougth-pos-args-count condition)
	     (not-enougth-pos-args-items condition)))))



(defun %symbol-to-option-string (symbol)
  "Convert SYMBOL to its argument string representation by prepending with
  double dash \"--\" and removing fancy chars such as \"*\"."
  (concatenate 'string "--"
	       ;; (remove #\*  string) seems to be less speed-efficient.
	       (loop for c across (string-downcase (symbol-name symbol))
		     unless (char= c #\*)
		       collecting c)))


(defun %make-options-list (options)
  "Convert given OPTIONS list into a list of CL-CLI:OPTION"
  (loop for option in options
	for long = (%symbol-to-option-string (car option))
	collect (apply #'make-option (append option `(:long ,long)))))




(defun consume-option (args option)
  "Extract all OPTION arguments from ARGS.
Return both consumed arguments count and the arguments"
  (let ((nargs (or (length (opt-params option)) 0)))
    ;;(format t "   ~a ~a~%" nargs option)
    (when (< (length args) nargs)
      (error 'option-requires-argument
	     :option (opt-long option) :nargs nargs))
    (let ((argument
	    (cond
	      ((= 0 nargs) t)
	      ((= 1 nargs) (car args))
	      (t  (subseq args 0 (1- nargs)))))
	  (type (opt-type option)))
      (values nargs
	      (if type
		  (handler-case
		      (if (atom argument)
			  (coerce (read-from-string argument) type)
			  (loop for a in argument
				collect (coerce (read-from-string a) type)))
		    (simple-type-error ()
		      (error 'bad-argument-type :option (opt-long option)
						:type type)))
		  argument)))))


(defstruct
    (option
     (:conc-name opt-)
     (:constructor make-option (name default help &key long alias params type )))
  name
  default
  help
  long
  alias
  params
  type)



(defun parse-options (argv options)
  (let ((opts (make-hash-table :test 'equal))
	(values (make-hash-table :test 'equal))
	(idx 0) vars vals)

    ;; Setup lookup has
    (loop for option in (%make-options-list options)
	  do (progn
	       (setf (gethash (opt-name option) values) (opt-default option))
	       ;; For long option
	       (setf (gethash (opt-long option) opts) option)
	       ;; For alias as well
	       (loop for alias in (opt-alias option)
		     do (setf (gethash alias opts) option))))

    (loop for i from idx below (length argv)
	  for cur-opt = (nth i argv)
	  for cur-opt-def = (gethash cur-opt opts)
	  ;;do (format t "Option[~a]: ~a~%" i cur-opt-def)
    	  while cur-opt-def
    	  do (progn
	       (multiple-value-bind (nargs vals)
		   (consume-option (subseq argv (1+ i)) cur-opt-def)
		 (setf i (+ i nargs))
		 (setf (gethash (opt-name cur-opt-def) values) vals)))
    	  finally (setf idx i))

    (loop for k being the hash-keys of values
	  do (progn
	       (push k vars)
	       (push (gethash k values) vals)))
	       
    (values (subseq argv idx) opts vars vals)))


(defstruct
    (sub-command
     (:conc-name sub-)
     (:constructor make-sub-command (verbs options positional docstring func)))
  verbs
  options
  positional
  docstring
  func)


(defun parse-command-args (args)
  "Parse ARGS and return 3 lists:

- A function suitable lambda-list
- A list of all positional arguments
- A list of all key arguments.

ARGS is a lambda-list as defined for defcommand."
  (let (positional keys lambda-list)
    (loop for arg in args
	  with in
	  do (cond
	       ((eq '&key arg) (progn
				 (push arg lambda-list)
				 (setf in arg)))
	       ((eq '&key in)
		(progn
		  (push (list (car arg) (nth 1 arg)) lambda-list)
		  (push arg keys)))
	       (t
		(progn
		  (push (car arg) lambda-list)
		  (push (append (list (car arg) nil) (cdr arg)) positional)))))
    (values
     (reverse lambda-list)
     (reverse positional)
     (reverse keys))))

(defmacro defcommand (verbs options docstring &body body)
  "Create a new command defined by VERBS list (dispatch arguments) a list of
OPTIONS, a DOCSTRING and use BODY as dispatch function."
  (multiple-value-bind (lambda-list positional keys)
      (parse-command-args options)
    `(make-sub-command ',verbs ',keys ',positional ,docstring
		       (lambda (,@lambda-list) ,@body))))

(defmacro defcommand-to-list (var verbs options docstring &body body)
  "Call DEFCOMMAND and append result to VAR."
  `(setq ,var
	 (nconc ,var
		(list (defcommand ,verbs ,options ,docstring ,@body)))))


(defun convert-vars-vals-to-keys (vars vals)
  "Convert VARS and VALS lists to a keyword plist.

Example:
  '(VAR1 VAR1) (val1 val2) => (:VAR1 val1 :VAR2 val2)"

  (loop for i below (length vars)
	nconc (list (intern (format nil "~a" (nth i vars)) "KEYWORD")
		    (nth i vals))))

(defun get-positional-args(argv cmd)
  (let ((nargs (length (sub-positional cmd))))
    (when (> nargs 0)
      (when (< (length argv) nargs)
	(error 'not-enougth-pos-args
	       :command (sub-verbs cmd)
	       :count nargs
	       :items (mapcar #'car (sub-positional cmd))))
      (values
       (subseq argv nargs)
       (subseq argv 0 nargs)))))

(defun parse-commands(argv commands)
  (let* ((len-argv (length argv))
	 (cmd (loop for command in commands
		   for verbs = (sub-verbs command)
		    when (and (>= len-argv (length verbs))
			      (equal verbs (subseq argv 0 (length verbs))))
		     return command)))
    (if (not cmd)
	argv
	(multiple-value-bind (argv opts-hash opts-vars opts-values)
	    (parse-options
	     (subseq argv (length (sub-verbs cmd)))
	     (sub-options cmd))

	  (multiple-value-bind (argv positional)
	      (get-positional-args argv cmd)
	
	    (values argv opts-hash
		    (append positional
			    (convert-vars-vals-to-keys opts-vars opts-values))
		    cmd))))))

(defun parse-cli (argv &optional options commands)
  "Parse ARGV using OPTIONS both and COMMANDS directives.

Return:
- options variables
- options values
- matched command dispatch function
- dispatch function keyword arguments
- the rest of the command line argument"
  (let ((argv (cdr argv)))
    (multiple-value-bind (argv opts-hash opts-vars opts-values)
	(parse-options argv options)
      (declare (ignore opts-hash))
      ;;(format t "ARGV: ~S~%" argv)
      (multiple-value-bind (argv sub-opt-hash sub-opts-keys sub)
	  (parse-commands argv commands)
	(declare (ignore sub-opt-hash))
	;;(format t "ARGV: ~S~%" argv)
	(values opts-vars opts-values
		(when sub (sub-func sub)) sub-opts-keys argv)))))


(defun run-command (argv &optional options commands)
  "Parse ARGV using OPTIONS both and COMMANDS directives."
  (multiple-value-bind (opts-vars opts-values sub-func sub-opts argv)
      (parse-cli argv options commands)
    (when sub-func
      (with-environment (append opts-vars '(*argv*))
	(append opts-values (list argv))
	(apply sub-func sub-opts)))))

(defmacro with-environment (vars vals &body body)
  (let ((%vars (gensym))
	(%vals (gensym)))
    `((lambda (,%vars ,%vals)
	(progv ,%vars ,%vals
	  ,@body)) ,vars ,vals)))



(defun %print-option(option)
  (format t  "~2T~@[~{~a,~}~]~a~20T~@[ <~{~(~a~)~^ ~}>~]~
~35T~{~<~%~35T~0,79:;~a~>~^ ~}~%"
	  (opt-alias option)
	  (opt-long option)
	  (opt-params option)
	  (split-sequence:split-sequence
	   #\ (format nil "~a~:[~; (default: ~:*~@a)~]"
		      (or (opt-help option) "")
		      (opt-default option)))))

(defun %print-positional(option)
  (format t "~2T<~(~a~)>~35T~{~<~%~35T~0,79:;~a~>~^ ~}~%"
	  (opt-name option)
	  (split-sequence:split-sequence
	   #\  (or (opt-help option) ""))))

(defun help (options sub-commands &key prog-name version prolog epilog)
  (let ((options (%make-options-list options)))

  
    (format t "~@[~a~]~@[ version ~a~]~%~%" prog-name version)

    (when prolog
      (format t "~{~<~%~0,79:;~a~>~^ ~}~%~%"
	      (split-sequence:split-sequence
	       #\  prolog)))
    
    (if sub-commands
	(loop for sub in sub-commands
	      do (format t
			 "~@[~a~]~:[~; [ OPTIONS ]~] ~{~a~^ ~}~
~:[~; [ OPTIONS ]~] ~{<~(~a~)>~^ ~}~%"
			 prog-name
			 options
			 (sub-verbs sub)
			 (sub-options sub)
			 (mapcar #'car (sub-positional sub))))
	(format t "~@[~a~]~:[~; [ OPTIONS ]~]~%"
		prog-name options))
    
    (when options
      (format t "~%Global options:~%")
      (loop for option in options
	    do (%print-option option)))

    (when sub-commands
      (format t "~%Sub commands:~%")
      (loop for sub-command in sub-commands
	    do (progn
		 (format t "~%~1T~{~a~^ ~}:~20T~{~<~%~20T~0,79:;~a~>~^ ~}~%"
			 (sub-verbs sub-command)
			 (split-sequence:split-sequence
			  #\  (sub-docstring sub-command)))
		 (loop for option in (%make-options-list (sub-options sub-command))
		       do (%print-option option))
		 (loop for option in (%make-options-list (sub-positional sub-command))
		       do (%print-positional option)))))

    (when epilog
      (format t "~%~{~<~%~0,79:;~a~>~^ ~}~%"
	      (split-sequence:split-sequence
	       #\  epilog)))))
