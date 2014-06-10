(defpackage #:cl-cli
  (:use #:cl)
  (:export
   #:run-command))


(in-package #:cl-cli)




(defun consume-option(args option)
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
	(idx 0) vars values)
    (loop for option in options
	  for long = (%symbol-to-option-string (car option))
	  for opt = (apply #'make-option (nconc option `(:long ,long)))
	  do (setf (gethash long opts) opt))
    (loop for i from idx below (length argv)
	  for cur-opt = (nth i argv)
	  for cur-opt-def = (gethash cur-opt opts)
	  ;;do (format t "Option[~a]: ~a~%" i cur-opt-def)
    	  while cur-opt-def
    	  do (progn
	       (multiple-value-bind (nargs vals)
		   (consume-option (subseq argv (1+ i)) cur-opt-def)
		 ;;(format t " -> ~a ~a ~a~%" cur-opt nargs vals)
		 (setf i (+ i nargs))
		 (push (opt-name cur-opt-def) vars)
		 (push vals values)))
    	  finally (setf idx i))
    (values (subseq argv idx) opts vars values)))



(defstruct
    (sub-command
     (:conc-name sub-)
     (:constructor make-sub-command (name verbs spec docstring body)))
  name
  verbs
  spec
  docstring
  body)

;; (defmacro %create-cmd-fn(spec &body body)
;;   `((lambda(x)  (funcall #'x) ,spec))

(defun parse-commands(argv commands)
  (let ((cmds
	  (loop for (name verbs specs docstring body) in commands
		collect (let*
			    (kind
			     (args-list (loop for spec in specs
					      if (member spec '(&optional &key))
						do (setf kind spec)
					      else
						collect
						(cond
						  ((eq kind '&optional)
						   (apply #'make-option
							  (nconc (list (car spec) nil)
								 (cdr spec))))
						  ((eq kind '&key)
						   (apply #'make-option spec)))))
			     (args (loop for arg in args-list
					 collect (list
						  (intern (format nil "~a"
								  (opt-name arg))
							  "KEYWORD")
						  (opt-default arg)))))
								   
			  (make-sub-command name verbs args docstring body)))))
  

    cmds))

  




(defun run-command (argv &optional options commands)
  (let ((argv (cdr argv))
	(*print-readably* t))
    (multiple-value-bind (argv opts-hash opts-vars opts-values)
	(parse-options argv options)
      (parse-commands argv commands)      )))
