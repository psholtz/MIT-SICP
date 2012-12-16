;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  set up world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; MIT 6.001                                  Spring, 2007
;;; PROJECT 4

;;;========================================================================
;;; You can extend this part of the file to extend your world.
;;;========================================================================


;;; load up needed files

(load "objsys.scm")
(load "objtype.scm")

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

;; a special place

(define heaven '())

(define (create-world)
  ; Create some places
  (let ((10-250 (create-place '10-250))
        (lobby-10 (create-place 'lobby-10))
        (grendels-den (create-place 'grendels-den))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (eecs-hq (create-place 'eecs-hq))
        (eecs-ug-office (create-place 'eecs-ug-office))
        (edgerton-hall (create-place 'edgerton-hall))
        (34-301 (create-place '34-301))
        (32-123 (create-place '32-123))
        (stata-center (create-place 'stata-center))
        (student-street (create-place 'student-street))
        (6001-lab (create-place '6001-lab))
        (building-13 (create-place 'building-13))
        (great-court (create-place 'great-court))
        (student-center (create-place 'student-center))
        (bexley (create-place 'bexley))
        (baker (create-place 'baker))
        (next-house (create-place 'next-house))
        (legal-seafood (create-place 'legal-seafood))
        (graduation-stage (create-place 'graduation-stage)))
    
    ;; set up heaven
    (set! heaven (create-place 'heaven))
    
    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways grendels-den 'up 'down 32-123)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways baker 'west 'east next-house)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down 34-301)
    (can-go-both-ways 34-301 'up 'down eecs-hq)
    (can-go-both-ways 34-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'down 'up 32-123)
    (can-go-both-ways eecs-hq 'west 'east eecs-ug-office)
    (can-go-both-ways edgerton-hall 'north 'south legal-seafood)
    (can-go-both-ways eecs-hq 'up 'down 6001-lab)
    (can-go-both-ways legal-seafood 'east 'west great-court)
    (can-go-both-ways great-court 'up 'down graduation-stage)
    
    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-mobile-thing 'tons-of-code baker)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)
 ;   (create-mobile-thing 'diploma graduation-stage)
    
    (list 10-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall 34-301 6001-lab
          building-13 great-court stata-center student-street
          student-center bexley baker next-house legal-seafood
          graduation-stage)))

; all hacks exist in the bowels-of-stata.  When placing a hack
; in the outside world, the original hack from the bowels-of stata
; is cloned (using clone-hack; see objtypes.scm).
; There are no entrances, exits, or people in the bowels, preventing
;  the hacks there from being stolen.

(define (instantiate-hacks)
  (let ((bowels (create-place 'bowels-of-stata)))
    (let ((sp1
           (create-hack
            'BOIL
            bowels
            "hmm, I think there's something on your nose"
            (lambda (hacker target)
              (ask target 'EMIT
                   (list (ask target 'NAME) "grows boils on their nose")))))
          (sp2
           (create-hack
            'SLUG-HACK
            bowels
            "want to join the Sydney Linux Users Group?"
            (lambda (hacker target)
              (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
              (create-mobile-thing 'SLUG (ask target 'LOCATION))))))
      bowels)))

(define (populate-hacks rooms)
  (for-each (lambda (room)
              (cond ((= (random 3) 0)
                     (clone-hack (pick-random (ask bowels-of-stata 'THINGS)) room)
                     (ask screen 'TELL-WORLD (list "hack installed at" (ask room 'NAME))))))
            rooms))

(define (populate-players rooms)
  (let* ((students (map (lambda (name)
			  (create-autonomous-person name
						    (pick-random rooms)
						    (random-number 5)
						    (random-number 5)))
			'(ben-bitdiddle alyssa-hacker
			  course-6-frosh lambda-man)))
         ;uncomment after writing professors
;	 (profs (map (lambda (name)
;		       (create-professor name
;			  	     (pick-random rooms)
;                                    3
;                                   3))
;		     '(rob-miller eric-grimson)))
         ;uncomment after writing president
;         (president (map (lambda (name)
;                           (create-president name
;                                             (pick-random rooms)
;                                             (random-number 3)
;                                             (random-number 3)))
;                         '(susan-hockfield)))                                  
	 (house-masters (map (lambda (name)
                               (create-house-master name
                                                    (pick-random rooms)
                                                    (random-number 3)
                                                    (random-number 3)))
                             '(dr-evil mr-bigglesworth)))
	 (trolls (map (lambda (name)
			(create-troll name
				      (pick-random rooms)
				      (random-number 3)
				      (random-number 3)))
		      '(grendel registrar))))

    (append students
;	    profs        ;uncomment after writing professor
;           president    ;uncomment after writing president
	    house-masters trolls)))

(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define bowels-of-stata 'will-be-set-by-setup)

(define (setup name)
  (ask our-clock 'RESET)
  (ask our-clock 'ADD-CALLBACK
       (create-clock-callback 'TICK-PRINTER our-clock 'PRINT-TICK))
  (let ((rooms (create-world)))
    (set! bowels-of-stata (instantiate-hacks))

    (populate-hacks rooms)

    (populate-players rooms)

    (set! me (create-avatar name (pick-random rooms)))
    (ask screen 'SET-ME me)
    (set! all-rooms rooms)
    'ready))

;; Some useful example expressions...

; (setup 'ben-bitdiddle)
; (run-clock 5)
; (ask screen 'DEITY-MODE #f)
; (ask screen 'DEITY-MODE #t)
; (ask me 'LOOK-AROUND)
; (ask me 'TAKE (thing-named 'ENGINEERING-BOOK))
; (ask me 'GO 'up)
; (ask me 'GO 'down)
; (ask me 'GO 'north)

