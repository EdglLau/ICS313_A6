;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Samuel Kim, Edgar Lau, Cyrus Wu         Date:04/4/14
;;; Course: ICS313        Assignment: 6   
;;; File: assignment6.lisp

; This parameter shows the description to each place.
(defparameter *nodes* '((home (you are currently at home.
                            Default text.))
                        (town (you are in a small, quiet town.
                            there are some nice villagers greeting you nicely and there are some thiefs stealing their possesion.))
                        (graveyard (you are in the graveyard next to town. 
                            Default text))
                        (catacombs (you are in the catacombs underneath the town.
                            Default text.))
                        (forest (you are in a dreary forest outside of town. 
                            Default text))
                        (cave (you are in a dark cave. 
                            Default text))
                        (lake (you have come upon a lake. 
                            there is a boat.))
                        (island (you are on an island.
                            Default text))
                        (desert (you are in a desert. 
                            Default text))
                        (oasis (you are in an oasis.
                            Default text))))

; This function describes the location.
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

; This parameter contains the paths that players can take to move between places.
(defparameter *edges* '((home (town outside door))
                        (town (home home door)
                              (graveyard east path)
                              (forest south path))
                        (graveyard 
                              (town west path)
                       	      (catacombs down hole))
                        (catacombs 
                              (graveyard outside hole))
                        (forest 
                              (town north path)
                              (cave underground path)
                              (lake east path)
                              (desert south path)) 
                        (area6 ())
                        (area7 ())
                        (area8 ())
                        (area9 ())
                        (area10 ())))

; This function describes a specific direction of a path from one location.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; This function describes a locations paths going into and out of it.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; This parameter defines the object to this game.
(defparameter *objects* '(sword chain frog triforce-piece1 triforce-piece2 triforce-piece3 triforce-piece4 triforce))

; This parameter gives where the objects are in the game.
(defparameter *object-locations* '((sword home)
                                   ;(defaultitem1 area3)
                                   ;(defaultitem2 area3)
                                   ;(defaultitem3 area4)
                                   ))

; This function lists all the object to a specific location.
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

; This function describes the objects in a specific location.
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj here.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; This parameter gives the starting location for this game.
(defparameter *location* 'home)

; This function allows users to see where they are in this wizard's world.
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

; This function allow users to go to different locations.
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

; This function allow users to pickup something in a location.
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

; This function shows all of the objects that you picked up.
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

; This function shows if you have a specific object in your inventory.                          
(defun have (object)
    (member object (cdr (inventory))))
    
; This function prints all available commands in the Wizard's World.                                   
(defun help ()
  `(you can use one of the following commands- ,@*allowed-commands*))

; Alias to help, just lets user call help with "h" instead                  
(defun h ()
  (help))

; Alias to help, just lets user call help with "?" instead                  
(defun ? ()
  (help))


; This is a function from Dr. Reed's samples
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (caar lst) obj)
          t
          (our-member obj (cdr lst)))))


; This is a function from Dr. Reed's samples
; modified for objects
(defun our-member-obj (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          t
          (our-member-obj obj (cdr lst)))))


;;;=========================R-E-P-L=======================;;;
;  Wizard's World part 2

; This function allows the user to play the game. If user types quit, the game is over.
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

; This function forces users to put parentheses around their commands and
; quotes in front of any function commands.
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; This parameter gives the allowed commands for this game so no other commands will 
; be allowed for this game.
(defparameter *allowed-commands* '(look walk pickup inventory help h ?))

; This function evaluates the commands that is okay or not for the user to use.
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

; This function tweaks the text to this game.
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; This function prints the game in it's correct characters instead of just capitals.
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
;;;==========================Macros===============================;;;           

; Macro for new objects, need to push to *objects* and                      
; to the *object-location*                                                  
(defmacro new-object (object location)
  `(progn
     (cond
       ((and 
          (not (our-member-obj ',object *objects*))
          (our-member ',location *nodes*))
        (pushnew ',object *objects*)
        (pushnew '(,object ,location) *object-locations*))
       (t "Object already exists"))))

; test: add new object pillow to living-room                                
(new-object broom kitchen)

; Main macro for the new locations in game. We need to push a               
; new node with name and description.                                       
(defmacro new-location (object &body body)
  `(cond
   ((not (our-member ',object *nodes*))
    (pushnew '(,object (,@body)) *nodes*))
   (t "Location already exists")))

; Adding a new location in to test                                     
;(new-location bedroom You are now in the bedroom. Every wizard needs his sleep.)
(new-location outside You have jumped out the window and died. The end.)

; New implementation of new-path macro
; We only need the following args:
; origin, destination, direction, path
; Optional arg: direction-back
(defmacro new-path (origin destination direction path &optional (direction-back "unable"))
  `(cond
    ((or
      ; case 1:origin doesn't exist                                             
      (not (our-member ',origin *nodes*))
      ; case 2:destination doesn't exist                                        
      (not (our-member ',destination *nodes*)))
     ()"Missing location, cannot create path.")
     ; case 3:okay to implement                                                 
    (t(progn
        (if (equal ',direction-back "unable")
            nil
            ;now we need to test to see if the destination is already there       
          (cond
           ((our-member ',destination *edges*)
            (pushnew '(,origin ,direction-back ,path)
                 (cdr (assoc ',destination *edges*)) :test 'equal))
           (t (pushnew '(,destination
                          (,origin ,direction-back ,path)) *edges* :test 'equal))))

          ; add new location/direction/path to origin's list of edges           
        (pushnew '(,destination ,direction ,path)
                 (cdr (assoc ',origin *edges*)) :test 'equal)))))

; Add paths to the new location
;(new-path home bedroom east door west)
(new-path attic outside outside window "unable")

; Macro to run different commands inside the game-repl
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;the parameters give the combined object after the collected pieces are combined into one object.
(defparameter *half-triforce* nil)
(defparameter *three-quarters-triforce* nil)
(defparameter *triforce* nil)
(defparameter *horcrux-sword* nil)
(defparameter *He-Man-Sword* nil)
(defparameter *lightsaber* nil)

;The combine-half is the first macro action for combining the first half of the triforce, using triforce-piece1 and triforce-piece2
(game-action combine-half triforce-piece1 triforce-piece2 Blood-Island
  (if(not(have 'triforce-piece1))
      '(you do not have both the triforce-piece1 and triforce-piece2)
    (if (and(have 'triforce-piece2)(not *half-triforce*))
    (progn 
      (setf *half-triforce* 't)
      (new-object half-triforce Blood-Island)
      (setf *objects*
            (remove 'triforce-piece1 *objects*))
      (setf *objects*
            (remove 'triforce-piece2 *objects*))
      (pickup 'half-triforce)
      '(the half-triforce has been made.))
    '(you do not have the triforce-piece2.))))

;The combine-three-quarters is the second macro action for combining the first three pieces of the treasurekey, 
; using half-triforce and triforce-piece3
(game-action combine-three-quarters half-triforce triforce-piece3 Skeleton-Island
  (if(not(have 'half-triforce))
      '(you do not have both the half-triforce and triforce-piece3)
    (if (and(have 'triforce-piece3)(not *three-quarters-triforce*))
    (progn 
      (setf *three-quarters-triforce* 't)
      (new-object three-quarters-triforce Skeleton-Island)
      (setf *objects*
            (remove 'half-triforce *objects*))
      (setf *objects*
            (remove 'triforce-piece3 *objects*))
      (pickup 'three-quarters-triforce)
      '(the three-quarters-triforce has been made.))
    '(you do not have the triforce-piece3.))))

;The combine is the third macro action for combining the first three pieces of the treasurekey, 
; using three-quarters-triforce and triforce-piece4
(game-action combine three-quarters-triforce triforce-piece4 Mysterious-Island
  (if(not(have 'three-quarters-triforce))
      '(you do not have both the three-quarters-triforce and triforce-piece4)
    (if (and(have 'triforce-piece4)(not *triforce*))
    (progn 
      (setf *triforce* 't)
      (new-object triforce Mysterious-Island)
      (setf *objects*
            (remove 'three-quarters-triforce *objects*))
      (setf *objects*
            (remove 'triforce-piece4 *objects*))
      (pickup 'triforce)
      '(the triforce has been made.))
    '(you do not have the triforce-piece4.))))
   
;power-to-save macro combines the sword and the cross into one object, the horcrux-sword.
(game-action power-to-save sword cross Rum-Island
  (if(not(have 'sword))
      '(you do not have both the sword and the cross)
    (if (and(have 'cross)(not *horcrux-sword*))
    (progn 
      (setf *horcrux-sword* 't)
      (new-object horcrux-sword Rum-Island)
      (setf *objects*
            (remove 'sword *objects*))
      (setf *objects*
            (remove 'cross *objects*))
      (pickup 'horcrux-sword)
      '(the horcrux-sword has been made. you can now defeat Bloody Mary.))
    '(you do not have the cross.))))

;i-have-the-power combines the horcrux sword with the grayskull to form the he-man-sword.
(game-action i-have-the-power horcrux-sword grayskull Blood-Island
  (if(not(have 'horcrux-sword))
      '(you do not have both the horcrux-sword and the grayskull)
    (if (and(have 'grayskull)(not *he-man-sword*))
    (progn 
      (setf *he-man-sword* 't)
      (new-object he-man-sword Blood-Island)
      (setf *objects*
            (remove 'horcrux-sword *objects*))
      (setf *objects*
            (remove 'grayskull *objects*))
      (pickup 'he-man-sword)
      '(the he-man-sword has been made. you can now defeat Skeletor.))
    '(you do not have the grayskull.))))

;jedi-power combines the he-man-sword with the skeletor-sword to make the lightsaber
(game-action jedi-power he-man-sword skeletor-sword Skeleton-Island
  (if(not(have 'he-man-sword))
      '(you do not have both the he-man-sword and the skeletor-sword)
    (if (and(have 'skeletor-sword)(not *lightsaber*))
    (progn 
      (setf *lightsaber* 't)
      (new-object lightsaber Skeleton-Island)
      (setf *objects*
            (remove 'he-man-sword *objects*))
      (setf *objects*
            (remove 'skeletor-sword *objects*))
      (pickup 'lightsaber)
      '(the lightsaber has been made. you can now defeat One-Eyed Willy.))
    '(you do not have the skeletor-sword.))))

;fight-Morgan uses the game-action macro to determine the outcome of the game, whether you can move on or not.
(game-action fight-Morgan sword Captain-Morgan Rum-Island
             (cond ((have 'sword) (new-object cross Rum-Island)
                                  (new-object triforce-piece1 Rum-Island)
                                  '(You killed Captain Morgan and find the first piece of the key
                                  guarded by Captain Morgan. You continue 
                                  forward in the search of the keys to unlock the treasure!))
                  (t 
                   (setf *location* 'pirate-lair)
                   (setf *objects* '(sword))
                   ;(setf 'body nil)
                   (setf *objects-locations* '((sword pirate-lair)))
                    '(You fought valiantly but without a sword you are no match for Captain
                        Morgan. You lose! Try again!))))

;fight-Mary uses the game-action macro to determine the outcome of the game, whether you can move on.
(game-action fight-Mary horcrux-sword Bloody-Mary Blood-Island
             (cond ((have 'horcrux-sword) (new-object grayskull Blood-Island)
                                  (new-object triforce-piece2 Blood-Island)
                                  '(You killed Bloody-Mary and find the second piece of the key
                                  guarded by Bloody-Mary. You continue 
                                  forward in the search of the keys to unlock the treasure!))
                  (t 
                   (setf *location* 'pirate-lair)
                   (setf *objects* '(sword))
                   ;(setf *objects-locations 'body nil)
                   (setf *objects-locations* '((sword pirate-lair)))
                    '(You fought valiantly but without a horcrux-sword you are no match for Bloody-Mary. You lose! Try again!))))

;fight-Skeletor uses the game-action macro to determine the outcome of the game, whether you can move on or not.
(game-action fight-Skeletor he-man-sword Skeletor Skeleton-Island
             (cond ((have 'he-man-sword) (new-object skeletor-sword Skeleton-Island)
                                  (new-object triforce-piece3 Skeleton-Island)
                                  '(You killed Skeletor and find the third piece of the key
                                  guarded by Skeletor. You continue 
                                  forward in the search of the keys to unlock the treasure!))
                  (t 
                   (setf *location* 'pirate-lair)
                   (setf *objects* '(sword))
                   (setf *objects-locations* '((sword pirate-lair)))
                    '(You fought valiantly but without the he-man-sword you are no match for Skeletor. You lose! Try again!))))

;fight-Willy uses the game action macro to determine the outcome of the game, whether you can move on or not.
(game-action fight-Willy lightsaber One-Eyed-Willy Mystererious-Island
             (cond ((have 'lightsaber) (new-object triforce-piece4 Mysterious-Island)
                                  '(You killed One-Eyed-Willy and find the fourth piece of the key
                                  guarded by One-Eyed-Willy. You continue 
                                  forward in the search of the keys to unlock the treasure!))
                  (t 
                   (setf *location* 'pirate-lair)
                   (setf *objects* '(sword))
                   (setf *objects-locations* '((sword pirate-lair)))
                    '(You fought valiantly but without the lightsaber you are no match for One-Eyed-Willy. You lose! Try again!))))

;unlock-treasuse uses the game action macro to determine the outcome of the game. If you have the treasurekey, you win.
;(game-action unlock-treasure treasurekey Treasure-Chest Hawaii
 ;            (cond ((have 'treasurekey) '(ARR!! You unlocked the Treasure-Chest. The plunder is
  ;                                              yours! You win!))
   ;               (t                               
    ;                '(Sorry, you haven't collected all the pieces of the treasure-key and combine it to make the treasure-key. 
     ;                 You should look around more to find them.))))
