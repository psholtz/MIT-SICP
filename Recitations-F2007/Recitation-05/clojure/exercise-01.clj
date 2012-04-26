;;
;; Exercise 1
;;
;; Draw box-and-pointer diagrams for the values of the following expressions.
;; Also give the printed representation.
;;
;; (a) (cons 1 2)
;;
;; (b) (cons 1 (cons 3 (cons 5 '())))
;;
;; (c) (cons (cons (cons 3 2) (cons 1 0)) '())
;;
;; (d) (cons 0 (list 1 2))
;;
;; (e) (list (cons 1 2) (list 4 5) 3)
;;

;;
;; Clojure does not have the same notion of CONS as do other versions of Lisp.
;;
;; Unlike traditional versions of Lisp, in Clojure lists are not the primary data
;; structure. Data structures in Clojure can implement the ISeq interface, and
;; the "seq?" procedure checks whether an object implements ISeq.
;;
;; The ISeq interface is defined as follows:
;;
;; public interface ISeq extends IPersistentCollection, Sequential {
;;
;;   Object first();
;;
;;   ISeq next();
;;
;;   ISeq more();
;;
;;   ISeq cons(Object obj);
;;
;; }
;;
;; Many classes will extends ASeq, which provides an abstract implementation
;; of ISeq:
;;
;; public abstract class ASeq extends Obj implements ISeq, List, Streamable {
;;
;;   public ISeq more() {
;;     ISeq s = next();
;;     if ( s == null ) return LazySeq.EMPTY;
;;     return s; 
;;   }
;;
;;   public ISeq cons(Object o) {
;;     return new Cons(o,this);
;;   }
;;
;;   //
;;   // Additional implementation details
;;   //
;; }
;;
;; Two good examples for seeing how the ISeq interface is implemented are
;; clojure.lang.StringSeq, and clojure.lang.IteratorSeq.
;; 
;; At any rate, the manner in which clojure implements "cons" precludes
;; answering the question as posed in this question.
;;