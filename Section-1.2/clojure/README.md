Section 1.2
=========== 

Contains solutions expressed in the Clojure dialet of Lisp.

To execute the script from the command line:

> java -cp clojure.jar clojure.main sample.clj

To load the script into memory after the interpreter has started:

> (load-file "sample.clj")

Examples of obtaining metadata on a defined function "sample-function" include:

> (meta #'sample-function)

> (:doc (meta #'sample-function))

and so on.