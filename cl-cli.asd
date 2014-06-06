
(asdf:defsystem #:cl-cli
  :author ("Sébastien Gross")
  :maintainer "Sébastien Gross"
  :licence "WTFPL"
  :description "Command line parser"
  :long-description "Simple and easy to use modern command line argument parser for common lisp."
  :depends-on (#:alexandria #:split-sequence)
  :pathname "."
  :components
  ((:file "cl-cli")))
