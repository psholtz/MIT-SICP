;;; OBJTYPES.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;;
;;; This file defines object types for use in our simulation
;;; world.  The full world is created in setup.scm.

;;--------------------
;; named-object
;; 
;; Named objects are the basic underlying object type in our
;; system. For example, persons, places, and things will all 
;; be kinds of (inherit from) named objects.
;;
;; Behavior (messages) supported by all named objects:
;;  - Has a NAME that it can return
;;  - Handles an INSTALL message
;;  - Handles a DESTROY message

(define (create-named-object name)      ; symbol -> named-object
  (create-instance named-object name))

(define (named-object self name)
  (let ((root-part (root-object self)))
    (make-handler
     'named-object
     (make-methods
      'NAME    (lambda () name)
      'INSTALL (lambda () 'installed)
      'DESTROY (lambda () 'destroyed))
     root-part)))

(define (names-of objects)
  ; Given a list of objects, returns a list of their names.
  (map (lambda (x) (ask x 'NAME)) objects))


;;--------------------
;; container
;;
;; A container holds THINGS.  
;; 
;; This class is not meant for "stand-alone" objects; rather, 
;; it is expected that other classes will inherit from the
;; container class in order to be able to contain things. 
;; For this reason, there is no create-container procedure.

(define (container self)
  (let ((root-part (root-object self))
	(things '()))
    (make-handler
     'container
     (make-methods
      'THINGS      (lambda () things)
      'HAVE-THING? (lambda (thing)
		     (not (null? (memq thing things))))
      'ADD-THING   (lambda (thing)
		     (if (not (ask self 'HAVE-THING? thing))
			 (set! things (cons thing things)))
		     'DONE)
      'DEL-THING   (lambda (thing)
		     (set! things (delq thing things))
		     'DONE))
     root-part)))


;;--------------------
;; thing
;;
;; A thing is a named-object that has a LOCATION
;;
;; Note that there is a non-trivial INSTALL here.  What does it do?

(define (create-thing name location)    ; symbol, location -> thing
  (create-instance thing name location))

(define (thing self name location)
  (let ((named-part (named-object self name)))
    (make-handler
     'thing
     (make-methods
      'INSTALL  (lambda ()
		  (ask named-part 'INSTALL)
		  (ask (ask self 'LOCATION) 'ADD-THING self))
      'LOCATION (lambda () location)
      'DESTROY  (lambda ()
		  (ask (ask self 'LOCATION) 'DEL-THING self))
      'EMIT     (lambda (text)
		  (ask screen 'TELL-ROOM (ask self 'LOCATION)
		       (append (list "At" (ask (ask self 'LOCATION) 'NAME))
			       text))))
     named-part)))

;;--------------------
;; mobile-thing
;;
;; A mobile thing is a thing that has a LOCATION that can change.

(define (create-mobile-thing name location) 
  ; symbol, location -> mobile-thing
  (create-instance mobile-thing name location))

(define (mobile-thing self name location)
  (let ((thing-part (thing self name location)))
    (make-handler
     'mobile-thing
     (make-methods
      'LOCATION  (lambda () location) ; This shadows message to thing-part!
      'CHANGE-LOCATION
      (lambda (new-location)
	(ask location 'DEL-THING self)
	(ask new-location 'ADD-THING self)
	(set! location new-location))
      'ENTER-ROOM    (lambda () #t)
      'LEAVE-ROOM    (lambda () #t)
      'CREATION-SITE (lambda () (ask thing-part 'location)))
     thing-part)))

;;--------------------
;; place
;;
;; A place is a container (so things may be in the place).
;;
;; A place has EXITS, which are passages from one place
;; to another.  One can retrieve all of the exits of a 
;; place, or an exit in a given direction from place. 

(define (create-place name)     ; symbol -> place
  (create-instance place name))

(define (place self name)
  (let ((named-part (named-object self name))
	(container-part (container self))
	(exits '()))
    (make-handler
     'place
     (make-methods
      'EXITS (lambda () exits)
      'EXIT-TOWARDS
      (lambda (direction)
	(find-exit-in-direction exits direction))
      'ADD-EXIT
      (lambda (exit)
	(let ((direction (ask exit 'DIRECTION)))
	  (if (ask self 'EXIT-TOWARDS direction)
	      (error (list name "already has exit" direction))
	      (set! exits (cons exit exits)))
	  'DONE)))
     container-part named-part)))

;;------------------------------------------------------------
;; exit
;;
;; An exit leads FROM one place TO another in some DIRECTION.

(define (create-exit from direction to)
  ; place, symbol, place -> exit
  (create-instance exit from direction to))

(define (exit self from direction to)
  (let ((named-object-part (named-object self direction)))
    (make-handler
     'exit
     (make-methods
      'INSTALL
      (lambda ()
	(ask named-object-part 'INSTALL)
	(if (not (null? (ask self 'FROM)))
	    (ask (ask self 'FROM) 'ADD-EXIT self)))
      'FROM         (lambda () from)
      'TO           (lambda () to)
      'DIRECTION    (lambda () direction)
      'USE
      (lambda (whom)
	(ask whom 'LEAVE-ROOM)
	(ask screen 'TELL-ROOM (ask whom 'location)
	     (list (ask whom 'NAME)
		   "moves from" 
		   (ask (ask whom 'LOCATION) 'NAME)
		   "to"
		   (ask to 'NAME)))
	(ask whom 'CHANGE-LOCATION to)
	(ask whom 'ENTER-ROOM)))
     named-object-part)))

(define (find-exit-in-direction exits dir)
  ; Given a list of exits, find one in the desired direction.
  (cond ((null? exits) #f)
        ((eq? dir (ask (car exits) 'DIRECTION))
         (car exits))
        (else (find-exit-in-direction (cdr exits) dir))))

(define (random-exit place)
  (pick-random (ask place 'EXITS)))

;;--------------------
;; person
;;
;; There are several kinds of person:  
;;   There are autonomous persons, including vampires, and there
;;   is the avatar of the user.  The foundation is here.
;;
;; A person can move around (is a mobile-thing),
;; and can hold things (is a container). A person responds to
;; a plethora of messages, including 'SAY to say something.
;;

(define (create-person name birthplace) ; symbol, place -> person
  (create-instance person name birthplace))

(define (person self name birthplace)
  (let ((mobile-thing-part (mobile-thing self name birthplace))
        (container-part    (container self))
        (health            3)
        (strength          1))
    (make-handler
     'person
     (make-methods
      'STRENGTH (lambda () strength)
      'HEALTH (lambda () health)
      'SAY
      (lambda (list-of-stuff)
	(ask screen 'TELL-ROOM (ask self 'location)
	     (append (list "At" (ask (ask self 'LOCATION) 'NAME)
			   (ask self 'NAME) "says --")
		     list-of-stuff))
	'SAID-AND-HEARD)
      'HAVE-FIT
      (lambda ()
	(ask self 'SAY '("Yaaaah! I am upset!"))
	'I-feel-better-now)
        
      'PEOPLE-AROUND        ; other people in room...
      (lambda ()
	(delq self (find-all (ask self 'LOCATION) 'PERSON)))
        
      'STUFF-AROUND         ; stuff (non people) in room...
      (lambda ()
	(let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
	       (stuff (filter (lambda (x) (not (ask x 'IS-A 'PERSON))) in-room)))
	  stuff))
        
      'PEEK-AROUND          ; other people's stuff...
      (lambda ()
	(let ((people (ask self 'PEOPLE-AROUND)))
	  (fold-right append '() (map (lambda (p) (ask p 'THINGS)) people))))
     
      'TAKE
      (lambda (thing)
	(cond ((ask self 'HAVE-THING? thing)  ; already have it
	       (ask self 'SAY (list "I am already carrying"
				    (ask thing 'NAME)))
	       #f)
	      ((or (ask thing 'IS-A 'PERSON)
		   (not (ask thing 'IS-A 'MOBILE-THING)))
	       (ask self 'SAY (list "I try but cannot take"
				    (ask thing 'NAME)))
	       #F)
	      (else
	       (let ((owner (ask thing 'LOCATION)))
		 (ask self 'SAY (list "I take" (ask thing 'NAME) 
				      "from" (ask owner 'NAME)))
		 (if (ask owner 'IS-A 'PERSON)
		     (ask owner 'LOSE thing self)
		     (ask thing 'CHANGE-LOCATION self))
		 thing))))
        
      'LOSE
      (lambda (thing lose-to)
	(ask self 'SAY (list "I lose" (ask thing 'NAME)))
	(ask self 'HAVE-FIT)
	(ask thing 'CHANGE-LOCATION lose-to))
        
      'DROP
      (lambda (thing)
	(ask self 'SAY (list "I drop" (ask thing 'NAME)
			     "at" (ask (ask self 'LOCATION) 'NAME)))
	(ask thing 'CHANGE-LOCATION (ask self 'LOCATION)))
      
      'GO-EXIT
      (lambda (exit)
	(ask exit 'USE self))
        
      'GO
      (lambda (direction) ; symbol -> boolean
	(let ((exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)))
	  (if (and exit (ask exit 'IS-A 'EXIT))
	      (ask self 'GO-EXIT exit)
	      (begin (ask screen 'TELL-ROOM (ask self 'LOCATION)
			  (list "No exit in" direction "direction"))
		     #F))))
      'SUFFER
      (lambda (hits perp)
	(ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
	(set! health (- health hits))
	(if (<= health 0) (ask self 'DIE perp))
	health)
        
      'DIE          ; depends on global variable "death-exit"
      (lambda (perp)
	(for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
		  (ask self 'THINGS))
	(ask screen 'TELL-WORLD
	     '("An earth-shattering, soul-piercing scream is heard..."))
	(ask self 'DESTROY))
      
      'ENTER-ROOM
      (lambda ()
	(let ((others (ask self 'PEOPLE-AROUND)))
	  (if (not (null? others))
	      (ask self 'SAY (cons "Hi" (names-of others)))))
	#T))
     mobile-thing-part container-part)))

;;--------------------
;; autonomous-person
;;
;; activity determines maximum movement
;; miserly determines chance of picking stuff up

(define (create-autonomous-person name birthplace activity miserly)
  (create-instance autonomous-person name birthplace activity miserly))

(define (autonomous-person self name birthplace activity miserly)
  (let ((person-part (person self name birthplace)))
    (make-handler
     'autonomous-person
     (make-methods
      'INSTALL
      (lambda ()
	(ask person-part 'INSTALL)
	(ask clock 'ADD-CALLBACK
	     (create-clock-callback 'move-and-take-stuff self 
				    'MOVE-AND-TAKE-STUFF)))
      'MOVE-AND-TAKE-STUFF
      (lambda ()
	;; first move
	(let loop ((moves (random-number activity)))
	  (if (= moves 0)
	      'done-moving
	      (begin
		(ask self 'MOVE-SOMEWHERE)
		(loop (- moves 1)))))
	;; then take stuff
	(if (= (random miserly) 0)
	    (ask self 'TAKE-SOMETHING))
	'done-for-this-tick)
      'DIE
      (lambda (perp)
	(ask clock 'REMOVE-CALLBACK self 'move-and-take-stuff)
	(ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
	(ask person-part 'DIE perp))
      'MOVE-SOMEWHERE
      (lambda ()
	(let ((exit (random-exit (ask self 'LOCATION))))
	  (if (not (null? exit)) (ask self 'GO-EXIT exit))))
      'TAKE-SOMETHING
      (lambda ()
	(let* ((stuff-in-room (ask self 'STUFF-AROUND))
	       (other-peoples-stuff (ask self 'PEEK-AROUND))
	       (pick-from (append stuff-in-room other-peoples-stuff)))
	  (if (not (null? pick-from))
	      (ask self 'TAKE (pick-random pick-from))
	      #F))))
     person-part)))

;;
;; hall-monitor
;;
(define (create-hall-monitor name birthplace speed irritability)
  (create-instance hall-monitor name birthplace speed irritability))

(define (hall-monitor self name birthplace speed irritability)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'hall-monitor
     (make-methods
      'INSTALL
      (lambda ()
	(ask auto-part 'INSTALL)
	(ask clock 'ADD-CALLBACK
	     (create-clock-callback 'irritate-students self
				    'IRRITATE-STUDENTS)))
      'IRRITATE-STUDENTS
      (lambda ()
	(if (= (random irritability) 0)
	    (let ((people (ask self 'PEOPLE-AROUND)))
	      (if people
		  (begin
		    (ask self 'SAY '("What are you doing still up?"
				     "Everyone back to their rooms!"))
		    (for-each (lambda (person)
				(ask person 'EMIT 
				     (list (ask person 'NAME) "goes home to"
					   (ask (ask person 'CREATION-SITE) 'NAME)))
				(ask person 'CHANGE-LOCATION
				     (ask person 'CREATION-SITE)))
			      people)
		    'grumped)
		  (ask self 'SAY '("Grrr... When I catch those students..."))))
	    (if (ask self 'PEOPLE-AROUND)
		(ask self 'SAY '("I'll let you off this once...")))))
      'DIE
      (lambda (perp)
	(ask clock 'REMOVE-CALLBACK self 'irritate-students)
	(ask auto-part 'DIE perp)))
     auto-part)))

;;
;; troll
;;
(define (create-troll name birthplace speed hunger)
  (create-instance troll name birthplace speed hunger))

(define (troll self name birthplace speed hunger)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'troll
     (make-methods
      'INSTALL
      (lambda ()
	(ask auto-part 'INSTALL)
	(ask clock 'ADD-CALLBACK
	     (create-clock-callback 'eat-people self
				    'EAT-PEOPLE)))
      'EAT-PEOPLE
      (lambda ()
	(if (= (random hunger) 0)
	    (let ((people (ask self 'PEOPLE-AROUND)))
	      (if people
		  (let ((victim (pick-random people)))
		    (ask self 'EMIT
			 (list (ask self 'NAME) "takes a bite out of"
			       (ask victim 'NAME)))
		    (ask victim 'SUFFER (random-number 3) self)
		    'tasty)
		  (ask self 'EMIT
		       (list (ask self 'NAME) "'s belly rumbles"))))
	    'not-hungry-now))
      'DIE
      (lambda (perp)
	(ask clock 'REMOVE-CALLBACK self 'eat-people)
	(ask auto-part 'DIE perp)))
     auto-part)))

;;
;; spell
;;
(define (create-spell name location incant action)
  (create-instance spell name location incant action))

(define (spell self name location incant action)
  (let ((mobile-part (mobile-thing self name location)))
    (make-handler
     'spell
     (make-methods
      'INCANT
      (lambda () incant)
      'ACTION
      (lambda () action)
      'USE
      (lambda (caster target)
	(action caster target)))
     mobile-part)))

(define (clone-spell spell newloc)
  (create-spell (ask spell 'NAME)
		newloc
		(ask spell 'INCANT)
		(ask spell 'ACTION)))

;;--------------------
;; avatar
;;
;; The avatar of the user is also a person.

(define (create-avatar name birthplace)
  ; symbol, place -> avatar
  (create-instance avatar name birthplace))

(define (avatar self name birthplace)
  (let ((person-part (person self name birthplace)))
    (make-handler
     'avatar
     (make-methods
      'LOOK-AROUND          ; report on world around you
      (lambda ()
	(let* ((place (ask self 'LOCATION))
	       (exits (ask place 'EXITS))
	       (other-people (ask self 'PEOPLE-AROUND))
	       (my-stuff (ask self 'THINGS))
	       (stuff (ask self 'STUFF-AROUND)))
	  (ask screen 'TELL-WORLD (list "You are in" (ask place 'NAME)))
	  (ask screen 'TELL-WORLD
	       (if (null? my-stuff)
		   '("You are not holding anything.")
		   (append '("You are holding:") (names-of my-stuff))))
	  (ask screen 'TELL-WORLD
	       (if (null? stuff)
		   '("There is no stuff in the room.")
		   (append '("You see stuff in the room:") (names-of stuff))))
	  (ask screen 'TELL-WORLD
	       (if (null? other-people)
		   '("There are no other people around you.")
		   (append '("You see other people:") (names-of other-people))))
	  (ask screen 'TELL-WORLD
	       (if (not (null? exits))
		   (append '("The exits are in directions:") (names-of exits))
		   ;; heaven is only place with no exits
		   '("There are no exits... you are dead and gone to heaven!")))
	  'OK))

      'GO
      (lambda (direction)  ; Shadows person's GO
	(let ((success? (ask person-part 'GO direction)))
	  (if success? (ask clock 'TICK))
	  success?))
      
      'DIE
      (lambda (perp)
	(ask self 'SAY (list "I am slain!"))
	(ask person-part 'DIE perp)))
     
     person-part)))
