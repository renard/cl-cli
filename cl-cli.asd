
(asdf:defsystem #:cl-cli
  :author ("Sébastien Gross")
  :version "0.2"
  :maintainer "Sébastien Gross"
  :licence "WTFPL"
  :description "Command line parser"
  :long-description "Simple and easy to use modern command line argument parser for common lisp."
  :depends-on (#:split-sequence)
  :pathname "."
  :components
  ((:file "cl-cli")))
