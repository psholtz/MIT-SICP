;;; SETUP.SCM
;;;
;;; MIT 6.001                                    Spring, 2005
;;; PROJECT 4

;;;========================================================================
;;; You can extend this file to extend your world.
;;;========================================================================

;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

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
        (stata-center (create-place 'stata-center))
        (6001-lab (create-place '6001-lab))
        (building-13 (create-place 'building-13))
        (great-court (create-place 'great-court))
        (student-center (create-place 'student-center))
        (bexley (create-place 'bexley))
        (baker (create-place 'baker))
        (legal-seafood (create-place 'legal-seafood))
        (graduation-stage (create-place 'graduation-stage)))
    
    ; Connect up places
    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways grendels-den 'up 'down lobby-10)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east student-center)
    (can-go-both-ways student-center 'south 'north bexley)
    (can-go-both-ways bexley 'west 'east baker)
    (can-go-both-ways lobby-10 'north 'south building-13)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways building-13 'north 'south edgerton-hall)
    (can-go-both-ways edgerton-hall 'up 'down 34-301)
    (can-go-both-ways 34-301 'up 'down eecs-hq)
    (can-go-both-ways 34-301 'east 'west stata-center)
    (can-go-both-ways stata-center 'north 'south stata-center)
    (can-go-both-ways stata-center 'up 'down stata-center)
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
    (create-mobile-thing 'problem-set 10-250)
    (create-mobile-thing 'recitation-problem 10-250)
    (create-mobile-thing 'sicp stata-center)
    (create-mobile-thing 'engineering-book barker-library)
    (create-mobile-thing 'diploma graduation-stage)
    
    (list 10-250 lobby-10 grendels-den barker-library lobby-7
          eecs-hq eecs-ug-office edgerton-hall 34-301 6001-lab
          building-13 great-court stata-center
          student-center bexley baker legal-seafood
          graduation-stage)))

; all spells exist in the chamber-of-stata.  When placing a spell
; in the outside world, the original spell from the chamber-of stata
; is cloned (using clone-spell; see objtypes.scm).
; There are no entrances, exits, or people in the chamber, preventing
;  the spells there from being stolen.
(define (instantiate-spells)
  (let ((chamber (create-place 'chamber-of-stata)))
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (lambda (caster target)
       (ask target 'EMIT
	    (list (ask target 'NAME) "grows boils on their nose"))))
    (create-spell
     'slug-spell
     chamber
     "dagnabbit ekaterin"
     (lambda (caster target)
       (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
       (create-mobile-thing 'slug (ask target 'LOCATION))))
    chamber))

(define (populate-spells rooms)
  (for-each (lambda (room)
	      (clone-spell (pick-random (ask chamber-of-stata 'THINGS)) room))
	    rooms))

(define (populate-players rooms)
  (let* ((students (map (lambda (name)
			  (create-autonomous-person name
						    (pick-random rooms)
						    (random-number 3)
						    (random-number 3)))
			'(ben-bitdiddle alyssa-hacker
			  course-6-frosh lambda-man)))
;uncomment after writing professors
;	 (profs (map (lambda (name)
;		       (create-wit-professor name
;					     (pick-random rooms)
;					     (random-number 3)
;					     (random-number 3)))
;		     '(susan-hockfield eric-grimson)))
	 (monitors (map (lambda (name)
			  (create-hall-monitor name
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
;	    profs        ;uncomment after writing wit-professor
	    monitors trolls)))

(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define chamber-of-stata 'will-be-set-by-setup)

(define (setup name)
  (ask clock 'RESET)
  (ask clock 'ADD-CALLBACK
       (create-clock-callback 'tick-printer clock 'PRINT-TICK))
  (let ((rooms (create-world)))
    (set! chamber-of-stata (instantiate-spells))

    (populate-spells rooms)

    (populate-players rooms)

    ;uncomment after writing chosen one
;    (create-chosen-one 'hairy-cdr (pick-random rooms)
;		       (random-number 3) (random-number 3))
    
    (set! me (create-avatar name (pick-random rooms)))
    (ask screen 'SET-ME me)
    (set! all-rooms rooms)
    'ready))

;; Some useful example expressions...

; (setup 'ben-bitdiddle)
; (run-clock 5)
; (ask screen 'DEITY-MODE #f)
; (ask screen 'DEITY-MODE #t)
; (ask me 'look-around)
; (ask me 'take (thing-named 'engineering-book))
; (ask me 'go 'up)
; (ask me 'go 'down)
; (ask me 'go 'north)
;
; (show me)
; (show screen)
; (show clock)
; (pp me)

