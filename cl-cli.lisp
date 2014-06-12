(defpackage #:cl-cli
  (:use #:cl)
  (:export
   #:defcommand
   #:parse-cli
   #:run-command))


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



(defun consume-option (args option)
  "Extract all OPTION arguments from ARGS.
Return both consumed arguments count and the arguments"
  (let ((nargs (or (length (opt-metavars option)) 0)))
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
     (:constructor make-option (name default help &key long alias metavars type )))
  name
  default
  help
  long
  alias
  metavars
  type)



(defun parse-options (argv options)
  (let ((opts (make-hash-table :test 'equal))
	(values (make-hash-table :test 'equal))
	(idx 0) vars vals)
    ;; Setup lookup hash
    (loop for option in options
	  for long = (%symbol-to-option-string (car option))
	  for opt = (apply #'make-option (append option `(:long ,long)))
	  do (progn
	       (setf (gethash (opt-name opt) values) (opt-default opt))
	       (setf (gethash long opts) opt)))
    
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
  ""
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
    (when cmd

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
      (multiple-value-bind (argv sub-opt-hash sub-opts-keys sub)
	  (parse-commands argv commands)
	(declare (ignore sub-opt-hash))
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
