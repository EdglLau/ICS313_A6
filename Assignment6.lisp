;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Samuel Kim, Edgar Lau, Cyrus Wu         Date:04/4/14
;;; Course: ICS313        Assignment: 6   
;;; File: assignment6.lisp

; This parameter shows the description to each place.
(defparameter *nodes* '((house (you are currently at your house.
                        	You hear commotion outside.))
                        (village (you are in a small town.
                            	There is panic among the villagers as monsters have appeared and people are missing!))
                        (forest-trail (you are in the forest trail.
                            	A worn sign says there is a village to the north, a cave to the south, and a mountain to the west.))
                        (Castle (you are in the Castle.
                        	The king has offered a reward to someone who can kill the monsters.))
                        (catacombs (you are in the catacombs beneath the cathedral.
                           	Skulls align the walls and there is a foul smell in the air.))
                        (dark-cave (you are in a dark cave.))
                        (labyrinth (You trip and fall down a hole!
                            	You are trapped in the labyrinth! It is hard to see.))
                        (cathedral (you are in a cathedral.
                           	People around you are praying for their safety. ))
                        (mountain (you are on a tall mountain.
                           	You hear noises coming from a cave.))
                        (dragons-den (You are in the dragons den.
                            	Bones align the floor.))
                        (labyrinth2 (You are in a dark corridor.))
                        (labyrinth3 (You are in a dark corridor.))
                        (labyrinth4 (You are in a dark corridor.
                            	You hear growls coming from someplace deeper in the maze.))
                        (hole (You have fell down a hole and died! Game Over.))
                        (minotaur-lair (A large minotaur roars and charges at you.))))

; This function describes the location.
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

; This parameter contains the paths that players can take to move between places.
(defparameter *edges* '((house 
				(village outside door))
                        (village 
                        	(Castle west path)
                                (house inside door)
                                (cathedral east path)
                                (forest-trail south path))
                        (castle 
                        	(village east path))
                        (cathedral 
                        	(village west path)
                                (catacombs down hole))
                        (forest-trail 
                        	(village north path)
                                (mountain west path)
                                (dark-cave south path))
                        (catacombs 
                        	(cathedral up hole))
                        (mountain 
                        	(forest-trail east path)
                                (dragons-den up cave))
                        (dragons-den 
                        	(mountain outside passage))
                        (dark-cave 
                        	(forest-trail north path)
                                (labyrinth south broken-door))
                        (labyrinth 
                        	(labyrinth2 east path)
                                (hole south path)
                                (hole west path))
                        (labyrinth2 
                        	(hole north path)
                                (hole east path)
                                (labyrinth3 south path))
                        (labyrinth3 
                        	(hole east path)
                                (hole south path)
                                (labyrinth4 west path))
                        (labyrinth4 
                        	(hole north path)
                                (hole south path)
                                (minotaur-lair south path))
                        (minotaur-lair 
                        	(forest-trail up secret-passage))))

; This function describes a specific direction of a path from one location.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; This function describes a locations paths going into and out of it.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; This parameter defines the object to this game.
(defparameter *objects* '(sword torch))

; This parameter gives where the objects are in the game.
(defparameter *object-locations* '((sword house)
                                   (torch dark-cave)
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
(defparameter *location* 'house)

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
(defparameter *triforce* nil)
(defparameter *holy-sword* nil)
(defparameter *enchanted-sword* nil)

;The combine-half is the first macro action for combining the first half of the triforce, using triforce-piece1 and triforce-piece2
(game-action combine-half triforce-piece1 triforce-piece2 minotaur-lair
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

;The combine-to-triforce is the second macro action for combining the first three pieces of the treasurekey, 
; using half-triforce and triforce-piece3 to make into the whole triforce.
(game-action combine-to-triforce half-triforce triforce-piece3 
  (if(not(have 'half-triforce))
      '(you do not have both the half-triforce and triforce-piece3)
    (if (and(have 'triforce-piece3)(not *triforce*))
    (progn 
      (setf *triforce* 't)
      (new-object triforce dragons-den)
      (setf *objects*
            (remove 'half-triforce *objects*))
      (setf *objects*
            (remove 'triforce-piece3 *objects*))
      (pickup 'triforce)
      '(the triforce has been made.))
    '(you do not have the triforce-piece3.))))

;holy-power macro combines the sword and the cross into one object, the holy-sword.
(game-action holy-power sword cross minotaur-lair
  (if(not(have 'sword))
      '(you do not have both the sword and the cross)
    (if (and(have 'cross)(not *holy-sword*))
    (progn 
      (setf *holy-sword* 't)
      (new-object holy-sword minotaur-lair)
      (setf *objects*
            (remove 'sword *objects*))
      (setf *objects*
            (remove 'cross *objects*))
      (pickup 'holy-sword)
      '(the holy-sword has been made. you may now defeat a necromancer.))
    '(you do not have the cross.))))

;enchantment-power combines the holy-sword with the spellbook to form the enchanted sword.

(game-action enchantment-power holy-sword spellbook catacombs
  (if(not(have 'holy-sword))
      '(you do not have both the holy-sword and the spellbook)
    (if (and(have 'grayskull)(not *enchanted-sword*))
    (progn 
      (setf *enchanted-sword* 't)
      (new-object enchanted-sword catacombs)
      (setf *objects*
            (remove 'holy-sword *objects*))
      (setf *objects*
            (remove 'spellbook *objects*))
      (pickup 'enchanted-sword)
      '(the enchanted-sword has been made. you may now defeat the dragon.))
    '(you do not have the spellbook.))))

;fight-Minotaur uses the game-action macro to determine the outcome of the game, whether you can move on or not.
(game-action fight-Minotaur sword Killgore minotaur-lair 
             (cond ((have 'sword) (new-object cross minotaur-lair)
                                  (new-object triforce-piece1 minotaur-lair)
                                  '(You killed Killgore and find the first piece of the key
                                  guarded by it. You continue forward in your quest to fight all enemies!))
                  (t 
                   (setf *location* 'house)
                   (setf *objects* '(sword))
                   ;(setf 'body nil)
                   (setf *objects-locations* '((sword house)))
                    '(You fought valiantly but without a sword you are no match for the ferocious Minotaur. You lose! Try again!))))

;fight-Necromancer uses the game-action macro to determine the outcome of the game, whether you can move on.
(game-action fight-Necromancer holy-sword Nekro catacombs
             (cond ((have 'holy-sword) (new-object spellbook catacombs)
                                  (new-object triforce-piece2 catacombs)
                                  '(You have killed Nekro and have found the second piece of the key. You continue onward in your quest to defeat the monsters terrorizing these lands!))
                  (t 
                   (setf *location* 'house)
                   (setf *objects* '(sword))
                   ;(setf 'body nil)
                   (setf *objects-locations* '((sword house)))
                    '(You fought valiantly but without a holy sword you are no match for the deadly Nekro. You lose! Try again!))))


;fight-Willy uses the game action macro to determine the outcome of the game, whether you can move on or not.
(game-action fight-Dragon enchanted-sword Blue-Eyes-White-Dragon dragons-den
             (cond ((have 'enchanted-sword) (new-object triforce-piece3 dragons-den)
                                  '(You have killed Blue Eyes White Dragon and have found the last piece of the key.
                                    The monsters terrorizing the kingdom have all been slain. Hero, go to the king to receive righteous glory!))
                  (t 
                   (setf *location* 'house)
                   (setf *objects* '(sword))
                   ;(setf 'body nil)
                   (setf *objects-locations* '((sword house)))
                    '(You fought valiantly but without a enchanted sword, you are no match for the vicious Blue Eyes White Dragon. You lose! Try again!))))

;unlock-treasuse uses the game action macro to determine the outcome of the game. If you have the treasurekey, you win.
;(game-action unlock-treasure Triforce Treasure-Chest Castle
 ;            (cond ((have 'Triforce) '(The king is pleased that you You unlocked the Treasure-Chest. The plunder is
  ;                                              yours! You win!))
   ;               (t                               
    ;                '(Sorry, you haven't collected all the pieces of the treasure-key and combine it to make the treasure-key. 
     ;                 You should look around more to find them.))))
