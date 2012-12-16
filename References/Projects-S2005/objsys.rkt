;;; OBJSYS.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;;
;;; This file provides a basic object system, and 
;;; a clock for objects in a simulation world.  Additional
;;; utility procedures are also provided.

;; Some terminology:
;;
;; "instance" of an object -- each individual object has
;; its own identity. The instance knows its type, and has
;; a message handler associated with it. One can "ask" an
;; object to do something, which will cause the object 
;; to use the message handler to look for a method to 
;; handle the request and then invoke the method on the 
;; arguments.
;;
;; "make" an object message handler -- makes a new message
;; handler to inherit the state information and methods of the 
;; specified class. The message handler is not a full "object
;; instance" in our system; the message handler needs to be
;; part of an instance object (or part of another message 
;; handler that is part of an instance object). All
;; procedures that define class objects should take a self pointer (a
;; pointer to the enclosing instance) as the first argument.
;; 
;; "create" an object -- this does three things: it makes
;; a new instance of the object, it makes and sets the 
;; message handler for that instance, and finally it INSTALL's
;; that new object into the world. 
;;
;; "install" an object -- this is a method in the object, by
;; which the object can initialize itself and insert itself
;; into the world by connecting itself up with other related
;; objects in the world.

;;------------------------------------------------------------
;; Instance

; instance is a tagged data structure which holds the "self" of a normal
; object instance.  It passes all messages along to the handler
; procedure that it contains.
;

(define (make-instance)
  (list 'instance #f))

(define (instance? x)
  (and (pair? x) (eq? (car x) 'instance)))

(define (instance-handler instance) (cadr instance))

(define (set-instance-handler! instance handler)
  (set-car! (cdr instance) handler))

(define (create-instance maker . args)
  (let* ((instance (make-instance))
         (handler (apply maker instance args)))
    (set-instance-handler! instance handler)
    (if (method? (get-method 'INSTALL instance))
        (ask instance 'INSTALL))
    instance))

;;------------------------------------------------------------
;; Handler
; handler is a procedure which responds to messages with methods
; it automatically implements the TYPE and METHODS methods.

(define (make-handler typename methods . super-parts)
  (cond ((not (symbol? typename))    ;check for possible programmer errors
	 (error "bad typename" typename))
	((not (method-list? methods))
	 (error "bad method list" methods))
	((and super-parts (not (filter handler? super-parts)))
	 (error "bad part list" super-parts))
	(else
	 (named-lambda (handler message)
	   (case message
	     ((TYPE)
	      (lambda () (type-extend typename super-parts)))
	     ((METHODS)
	      (lambda ()
		(append (method-names methods)
			(append-map (lambda (x) (ask x 'METHODS))
				    super-parts))))
	     (else
	      (let ((entry (method-lookup message methods)))
		(if entry
		    (cadr entry)
		    (find-method-from-handler-list message super-parts)))))))))

(define (handler? x)
  (and (compound-procedure? x)
       (eq? 'handler (lambda-name (procedure-lambda x)))))

(define (->handler x)
  (cond ((instance? x)
	 (instance-handler x))
	((handler? x)
	 x)
	(else
	 (error "I don't know how to make a handler from" x))))

; builds a list of method (name,proc) pairs suitable as input to make-handler
; note that this puts a label on the methods, as a tagged list

(define (make-methods . args)
  (define (helper lst result)
    (cond ((null? lst) result)

	  ; error catching
	  ((null? (cdr lst))
	   (error "unmatched method (name,proc) pair"))
	  ((not (symbol? (car lst)))
	   (if (procedure? (car lst))
	       (pp (car lst)))
	   (error "invalid method name" (car lst)))
	  ((not (procedure? (cadr lst)))
	   (error "invalid method procedure" (cadr lst)))

	  (else
	   (helper (cddr lst) (cons (list (car lst) (cadr lst)) result)))))
  (cons 'methods (reverse (helper args '()))))

(define (method-list? methods)
  (and (pair? methods) (eq? (car methods) 'methods)))

(define (empty-method-list? methods)
  (null? (cdr methods)))

(define (method-lookup message methods)
  (assq message (cdr methods)))

(define (method-names methods)
  (map car (cdr methods)))

;;------------------------------------------------------------
;; Root Object

; Root object.  It contains the IS-A method.
; All classes should inherit (directly or indirectly) from root.
;
(define (root-object self)
  (make-handler
   'root
   (make-methods
    'IS-A
    (lambda (type)
      (memq type (ask self 'TYPE))))))

;;------------------------------------------------------------
;; Object Interface

; ask
; 
; We "ask" an object to invoke a named method on some arguments.
;
(define (ask object message . args)
  ;; See your Scheme manual to explain `. args' usage
  ;; which enables an arbitrary number of args to ask.
  (let ((method (get-method message object)))
    (cond ((method? method)
           (apply method args))
          (else
           (error "No method for" message 'in 
                  (safe-ask 'UNNAMED-OBJECT
                            object 'NAME))))))

; Safe (doesn't generate errors) method of invoking methods
; on objects.  If the object doesn't have the method,
; simply returns the default-value.  safe-ask should only
; be used in extraordinary circumstances (like error handling).
;
(define (safe-ask default-value obj msg . args)
  (let ((method (get-method msg obj)))
    (if (method? method)
        (apply ask obj msg args)
        default-value)))

;;--------------------
;; Method Interface
;;
;; Objects have methods to handle messages.

; Gets the indicated method from the object or objects.
; This procedure can take one or more objects as 
; arguments, and will return the first method it finds 
; based on the order of the objects.
;
(define (get-method message . objects)
  (find-method-from-handler-list message (map ->handler objects)))

(define (find-method-from-handler-list message objects)
  (if (null? objects)
      (no-method)
      (let ((method ((car objects) message)))
	(if (not (eq? method (no-method)))
	    method
	    (find-method-from-handler-list message (cdr objects))))))

(define (method? x)
  (cond ((procedure? x) #T)
        ((eq? x (no-method)) #F)
        (else (error "Object returned this non-message:" x))))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

; used in make-handler to build the TYPE method for each handler
;
(define (type-extend type parents)
  (cons type 
        (remove-duplicates
         (append-map (lambda (parent) (ask parent 'TYPE))
                     parents))))

;;------------------------------------------------------------
;; Utility procedures

(define (random-number n)
  ;; Generate a random number between 1 and n
  (+ 1 (random n)))

(define (pick-random lst)
  (if (null? lst)
      #F
      (list-ref lst (random (length lst)))))

(define (find-all source type)
  (filter (lambda (x) (ask x 'IS-A type))
          (ask source 'THINGS)))

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) 
                                         (not (eq? x (car lst))))
                                       lst)))))

;; utility for finding all the people in the world
(define (all-people)
  (append-map (lambda (room) (find-all room 'person)) all-rooms)) 


;;------------------------------------------------------------
;; Support for Objects in a Simulation World

;;--------------------
;; Clock
;;
;; A clock is an object with a notion of time, which it
;; imparts to all objects that have asked for it.  It does
;; this by invoking a list of CALLBACKs whenever the TICK
;; method is invoked on the clock.  A CALLBACK is an action to
;; invoke on each tick of the clock, by sending a message to an object

(define (clock self . args)
  (let ((root-part (root-object self))
        (name (if (not (null? args))
                  (car args)
                  'THE-CLOCK))
        (the-time 0)
        (callbacks '())
        (removed-callbacks '()))
    (make-handler
     'clock
     (make-methods
      'INSTALL
      (lambda ()
	;; By default print out clock-ticks
	;; -- note how we are adding a callback
	;;    to a method of the clock object
	(ask self 'ADD-CALLBACK
	     (create-clock-callback 'tick-printer self 'PRINT-TICK)))
      'NAME      (lambda () name)
      'THE-TIME  (lambda () the-time)
      'RESET     (lambda ()
		   (set! the-time 0)
		   (set! callbacks '()))
      'TICK
      (lambda ()
	(set! removed-callbacks '())
	(for-each (lambda (x) 
		    (if (not (memq x removed-callbacks))
			(ask x 'activate)))
		  (reverse callbacks))
	(set! the-time (+ the-time 1)))
      'ADD-CALLBACK
      (lambda (cb)
	(cond ((not (ask cb 'IS-A 'CLOCK-CALLBACK))
	       (error "Non callback provided to ADD-CALLBACK"))
	      ((null? (filter (lambda (x) (ask x 'SAME-AS? cb))
			      callbacks))
	       (set! callbacks (cons cb callbacks))
	       'added)
	      (else
	       'already-present)))
      'REMOVE-CALLBACK
      (lambda (obj cb-name)
	(set! callbacks 
	      (filter (lambda (x) 
			(cond ((and (eq? (ask x 'NAME) cb-name)
				    (eq? (ask x 'OBJECT) obj))
			       (set! removed-callbacks
				     (cons x removed-callbacks))
			       #f)
			      (else #t)))
		      callbacks))
	'removed)
      'PRINT-TICK
      ;; Method suitable for a callback that prints out the tick
      (lambda ()
	(ask screen 'TELL-WORLD
	     (list "---" (ask self 'NAME) "Tick" (ask self 'THE-TIME) "---"))))
     root-part)))

(define (create-clock . args)
  (apply create-instance clock args))

;; Clock callbacks
;;
;; A callback is an object that stores a target object, 
;; message, and arguments.  When activated, it sends the target
;; object the message.  It can be thought of as a button that executes an 
;; action at every tick of the clock.

(define (clock-callback self name object msg . data)
  (let ((root-part (root-object self)))
    (make-handler
     'clock-callback
     (make-methods
      'INSTALL  (lambda () 'INSTALLED)
      'NAME     (lambda () name)
      'OBJECT   (lambda () object)
      'MESSAGE  (lambda () msg)
      'ACTIVATE (lambda () (apply ask object msg data))
      'SAME-AS? (lambda (cb)
		  (and (ask cb 'IS-A 'CLOCK-CALLBACK)
		       (eq? (ask self 'NAME)
			    (ask cb 'NAME))
		       (eq? object (ask cb 'OBJECT)))))
     root-part)))

(define (create-clock-callback name object msg . data)
  (apply create-instance clock-callback name object msg data))

;; Setup global clock object
(define clock (create-clock))

;; Get the current time
(define (current-time)
  (ask clock 'THE-TIME))

;; Advance the clock some number of ticks
(define (run-clock n)
  (cond ((= n 0) 'DONE)
        (else (ask clock 'tick)
              ;; remember that this activates each item in callback list
              (run-clock (- n 1)))))

;; Using the clock:
;;
;; When you want the object to start being aware of the clock
;; (during initialization of autonomous-person, for example),
;; add a callback to the clock which activates a method on the
;; object:
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'thingy self 'DO-THINGY))
;; The first argument is a name or descriptor of the callback.
;; The second argument is the object to which to send the message.
;; The third argument is the message (method-name) to send.
;; Additional arguments can be provided and they are sent to 
;; the object with the message when the callback is activated.
;; In this case, the method do-thingy should be descriptive of
;; the behavior the object will exhibit when time passes.
;; When the object's lifetime expires (sometimes this is taken
;; literally!), it should remove its callback(s) from the clock.
;; This can be done with
;; (ask clock 'REMOVE-CALLBACK
;;      self 'thingy)
;;
;; An example of using callback names and additional arguments:
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'whoopee me 'SAY '("Whoopee!")))
;; (ask clock 'ADD-CALLBACK
;;      (create-clock-callback 'fun me 'SAY '("I am having fun!")))
;; This causes the avatar to say two things every time the clock
;; ticks.

;;-----------
;; screen
;;
;; This is a singleton object (only one object of this type in 
;; existence at any time), which deals with outputting text to 
;; the user.
;;
;; If the screen is in deity-mode, the user will hear every message,
;; regardless of the location of the avatar.  If deity-mode is
;; false, only messages sent to the room which contains the avatar
;; will be heard.
;;
;; network-mode is something set only by the network code.

(define (screen self)
  (let ((deity-mode #t)
        (network-mode #f)
        (me #f)
        (root-part (root-object self)))
    (make-handler
     'screen
     (make-methods
      'TYPE   (lambda () (type-extend 'screen root-part))
      'NAME   (lambda () 'THE-SCREEN)
      'SET-ME (lambda (new-me) (set! me new-me))
      'TELL-ROOM    (lambda (room msg)
		      (if (or deity-mode
			      (eq? room (safe-ask #f me 'location)))
			  (if network-mode
			      (display-net-message msg)
			      (display-message msg))))
      'TELL-WORLD   (lambda (msg)
		      (if network-mode
			  (display-net-message msg)
			  (display-message msg)))
      'DEITY-MODE   (lambda (value) (set! deity-mode value))
      'NETWORK-MODE (lambda (value) (set! network-mode value))
      'DEITY-MODE?  (lambda () deity-mode))
     root-part)))

(define screen
  (create-instance screen))

;;--------------------
;; Utilities for our simulation world 
;;

(define (display-message list-of-stuff)
  (if (not (null? list-of-stuff)) (newline))
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  'MESSAGE-DISPLAYED)

(define (display-net-message list-of-stuff)
  (for-each (lambda (s) (display s server-port) (display " " server-port))
            list-of-stuff)
  (display #\newline server-port)
  (flush-output server-port)
  'MESSAGE-DISPLAYED)

; Grab any kind of thing from avatar's location, 
; given its name.  The thing may be in the possession of
; the place, or in the possession of a person at the place.
; THING-NAMED SHOULD NEVER BE USED IN OBJTYPES OR ANY OBJECT
; YOU CREATE.
(define (thing-named name)
  (let* ((place (ask me 'LOCATION))
         (things (ask place 'THINGS))
         (peek-stuff (ask me 'PEEK-AROUND))
         (my-stuff (ask me 'THINGS))
         (all-things (append things (append my-stuff peek-stuff)))
         (things-named (filter (lambda (x) (eq? name (ask x 'NAME)))
                               all-things)))
    (cond ((null? things-named)
           (error "In here there is nothing named" name))
          ((null? (cdr things-named))   ; just one thing
           (car things-named))
          (else
           (display-message (list "There is more than one thing named"
                                  name "here. Picking one of them."))
           (pick-random things-named)))))



;;--------------------
;; show 
;;
;; Some utilities.
;;
;; Treat these as gifts from the (Scheme) Gods.  
;; Don't try to understand these procedures!

(load-option 'format)

(define (show obj)
  (define (show-guts obj)
    (format #t "INSTANCE ~A~% TYPE: ~A~%" obj (ask obj 'TYPE))
    (show-handler (->handler obj))
    'instance)
  (if (instance? obj)
      (show-guts obj) 
      (show-handler obj)))

(define (show-handler proc)
  (define (show-frame frame depth)
    (define *max-frame-depth* 1)
    (if (global-environment? frame)
        (display (env-name frame))
        (let* ((bindings (environment-bindings frame))
               (parent   (environment-parent frame))
               (names    (cons "Parent frame"
                               (map symbol->string (map car bindings))))
               (values   (cons (env-name parent)
                               (map cadr bindings)))
               (width    (reduce max 0 (map string-length names))))
          (for-each (lambda (n v) (pp-binding n v width depth))
                    names values)
          (if (and (not (global-environment? parent))
                   (< depth *max-frame-depth*))
              (show-frame parent (+ depth 1))))))
  (define (global-environment? frame)
    (environment->package frame))
  (define (env-name env)
    (if (global-environment? env) 'GLOBAL-ENVIRONMENT env))
  (define (pp-binding name value width depth)
    (let ((value* (with-string-output-port
                      (lambda (port)
                        (if (pair? value)
                            (pretty-print value port #F (+ width 2))
                            (display value port))))))
      (display-spaces (* 2 (+ depth 1)))
      (display name) (display ": ")
      (display (make-string (- width (string-length name)) #\Space))
      (if (pair? value)
          (display (substring value* (+ width 2) (string-length value*)))
          (display value*))
      (newline)))
  (define (display-spaces num)
    (if (> num 0) (begin (display " ") (display-spaces (- num 1)))))
  (if (handler? proc)
      (fluid-let ((*unparser-list-depth-limit* 5)
		  (*unparser-list-breadth-limit* 6))
	(let ((methods (environment-lookup (procedure-environment proc)
					   'methods))
	      (parts   (environment-lookup (procedure-environment proc)
					   'super-parts))
	      (type    (environment-lookup (procedure-environment proc)
					   'typename)))
	  (format #t " HANDLER: ~A~%" proc)
	  (format #t " TYPE: ~A~%" type)
	  (format #t "~A~%" (with-output-to-string 
					(lambda () (pretty-print methods))))
	  (if (cdr methods)
	      (show-frame (procedure-environment (cadadr methods)) 0)
	      (format #t " PARENTS: ~A~%" parts))
	  ;(display " HANDLER PROCEDURE:\n")
	  ;(pretty-print (procedure-lambda proc) (current-output-port) #T 2)
	  'handler))
      'not-a-handler))


