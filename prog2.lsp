;-------------------------------------------------
(load "statefunctions")

; global variables
(defvar *mis*)     ;number of missionaries on left side
(defvar *can*)     ;number of cannables on left side
(defvar *boat* 2)    ;number of people that are allowed on the boat





;------------------------------------------------
(defun main()
  (cond
      ;get command line arguments
    ((= (length *args*) 3)
      (setf *mis* (parse-integer (car *args*)) *can* (parse-integer (cadr *args))
      )
   
    	;call solving function
    	(m-c *mis* *can*)
    ) 

    (t
        ;print out useage message
        (format t "~%~%~%Missionaries and Cannibals Puzzle~%")
        (format t   "---------------------------------~%")
        (format t "Usage: (m-c m c)~%")
        (format t "       M - Ammount of Missionaries~%")
        (format t "       C - Ammount of Cannibals~%")
    )
  )
)

;------------------------------------------------

;Missionary and Cannables Problem solver
(defun m-c (ml cl)
   
  ;initilize global varables
  (setq *mis* ml) ;missionaries left
  (setq *can* cl) ;cannables left
  (setq mr '0) ; missionaries right
  (setq cr '0) ; cannables right
  (setq s "l") ; boat to left size
  (defparameter *t* (string ""))

  ;check to see if there are more c then m at start
   (cond( (< ml cl)
        (format t "There are to many Cannibals for the missionaries to save~%")
        (format t "Sadly they where all eaten~%")
        (return-from m-c "No solution!")
        )
    )
 
  ;create left bank node

  (format t "~A Missionaries and ~A Cannibals:~%~%" ml cl)
  (format t "left bank      right bank      canoe      last move~%")
  (format t "---------      ----------      -----      ---------~%")
  (format t " ~A M, ~A C      ~A M, ~A C        left       start state ~%"
   ml cl mr cr  )

 (cond
     ;boat move from left to right
     ((string= s "r") (setf *t* (string "left to right"))(setf p "right"))
     ;boat move from right to left 
     ((string= s "l") (setf *t* (string "right to left"))(setf p "left "))
  )
  (format t " ~A M, ~A C      ~A M, ~A C        ~A      move ~A M, ~A C ~A ~%"
   ml cl mr cr p (- *mis* ml) (- *can* cl)  *t* )

  ;suppress printing NIL unpon return to interperter
  (values)  
)

;------------------------------------------------
;start state (all missionaries and cannables on left side)
(defun start-state () '( *mis* *can* 0 0 L))


;-----------------------------------------------
;goal state ( all missionaires and cannables on right side)
(defun goal-state? (state) 
  ;set goal state ( Ml Cl MR CR R )
  (and
    (and
      (and (=(first state) '0) (=(second state) '0)) ; no persons on left
      (and (=(third state) *mis*) (=(fourth state) *can*)) ; all persons on right
    )
    (and (string=(fifth state) 'R)) ; check for boat on right side
  )
)


;------------------------------------------------
;check to make sure cannabilism does not occur
(defun cannabilism (state)
  "check to make sure cannabilism does not occur"
  (cond
    ((< (first state) (second state) nil); more canables then missionaries on left side
    ((< (third state) (fourth state) nil) ; more cannables then missionaries on right side
    (t state) ; return state if true
  )
)

;------------------------------------------------
(defun dfs (state)
  "(dfs m c s): depth first search for MCP"
  ;check to see if current state is the goal state
  ;move one missionarie and one cannable
  ;move two missionaires
  ;move one missionarie
  ;move two cannables
  ;move one canable  
  ;return failure
  (return-from dfs nil) ; return nil if not found
)
  
   

;-------------------------------------------------
;run Missionaries and canables uponloading file
(main)

;-------------------------------------------------
