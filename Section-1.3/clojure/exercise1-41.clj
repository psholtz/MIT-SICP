;;
;; Exercise 1.41
;;
;; Define a procedure "double" that takes a procedure of one argument as argument and returns a
;; procedure that applies the original procedure twice. For example, if "inc" is a procedure that
;; adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned
;; by:
;;
;; (((double (double double)) inc) 5)
;;

;;
;; Definition of double.
;;
;; Since "double" is already defined in the Clojure namespace, we will call this procedure "xdouble":
;;
(defn xdouble [g]
  (fn [x] (g (g x))))

;;
;; Evaluation of (((double (double double)) inc) 5):
;;
;; I'm not going to walk through the call graph the way I did for the scheme implementation.
;;
;; Here we will just give the results of the evaluation:
;;
(((xdouble (xdouble xdouble)) inc) 5)
;; ==> 21

;;
;; We could as well define the following procedures:
;;
(def add-two (xdouble inc))
(def add-four ((xdouble xdouble) inc))
(def add-sixteen ((xdouble (xdouble xdouble)) inc))

;;
;; Note that these last two procedures could be rewritten as:
;;
(def add-four (xdouble (xdouble inc)))
(def add-sixteen (xdouble (xdouble (xdouble (xdouble inc)))))

;;
;; So far, we have deduced that:
;;
;; (double inc) ==> adds 2
;; ((double double) inc) ==> adds 4 = (2^2)
;; ((double (double double)) inc) ==> adds 16 = ((2^2)^2)
;;
;; From this, we might surmise that:
;; ((double (double (double double))) inc) ==> adds 256 = (((2^2)^2)^2)
;;
;; Typing this into the interpreter, we obtain the expected expression:
;;
(((xdouble (xdouble (xdouble xdouble))) inc) 5)