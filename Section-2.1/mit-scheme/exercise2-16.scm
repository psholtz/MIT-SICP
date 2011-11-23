;;
;; Exercise 2.16
;;

;;
;; The first thing I did was look around to see if there are other "interval arithmetic" packages
;; in commercial use that may exhibit the same "error" we've just encountered. 
;;
;; Interestingly, the Boost C++ libraries have their own template library "interval" which implements
;; interval arithmetic. 
;;
;; I ran some examples of interval arithmetic through the Boost C++ libraries, and posted the results
;; in the Boost C++ branch of this repository. Indeed, the Boost libraries give the same answers and
;; results that we obtained with the Scheme libraries derived in the SICP text.
;;
;; Specifically, the Boost C++ libraries exhibit the same "error" when computing parallel resistances 
;; that we encountered with the Scheme code.
;;

