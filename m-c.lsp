;Missionaries and cannables
;
;By Charles Bonn and Marcus Habberling
;CSC461
;
;This program solves the Missionary and Cannables
;problem using recurrsive lisp functions.

;-------------------------------------------------
(load "statefunctions")

; global variables
(defvar *mis*)     ;number of missionaries on left side
(defvar *can*)     ;number of cannables on left side
(defvar *boat* 2)    ;number of people that are allowed on the boat




;-----------------------------------------------
;Function: Main
;Arguments: none
;
;Functionality: grabs user given parameters and
;displays a help message
;------------------------------------------------
(defun main()
   "(main) The main function that runs the program grabs user variables
   and displays useage"
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
;function m-c
;
;parameters: m - number of missionaries
;            c - number of canables
;
;functionality: print output and call dfs search
;------------------------------------------------
(defun m-c (m c)
   "(m-c m c) function call for MC "
  ;initilize global varables
  (setf *mis* m) ; missionaries
  (setf *can* c) ; cannables
  (defparameter *t* (string ""))

  ;check to see if there are more c then m at start
   (cond( (< m c)
        (format t "There are to many Cannibals for the missionaries to save~%")
        (format t "Sadly they where all eaten~%")
        (return-from m-c "No solution!")
        )
    )
 
  ;create left bank node

  (format t "~%~%~A Missionaries and ~A Cannibals:~%~%" m c)
  (format t "left bank      right bank      canoe      last move~%")
  (format t "---------      ----------      -----      ---------~%")
  
  (setf state (reverse(dfs(start-state)))) ; get and reverse states
  ;no solutions
 
  (cond ((= (length state) 0) (return-from m-c "NO SOLUTIONS") ))
  ;previous values
  (setf mp 0)
  (setf cp 0)
  (loop for x in state
     do     
     (setf m (car x))
     (setf c (car(cdr x)))
     (setf s (car (cddr x))); (cddr x))
     (cond
        ;start states
       ((and(= m 0) (= c 0)) (setf *t* (string "Start state"))(setf p "left "))
       ;boat move from left to right
       ((string= s "R") (setf *t* (string "left to right"))(setf p "right"))
       ;boat move from right to left 
       ((string= s "L") (setf *t* (string "right to left"))(setf p "left "))
     ) 
     ; output format string
      (format t " ~A M, ~A C      ~A M, ~A C        ~A      move ~A M, ~A C ~A ~%"
             (- *mis* m) (- *can* c) m c p (abs  (- mp m) )
             (abs (- cp c)) *t*)
   

    (setf mp m)
    (setf cp c)
  )
  ;suppress printing NIL unpon return to interperter
  (values)  
)
;-------------------------------------------------
;run Missionaries and canables uponloading file
(main)

;-------------------------------------------------
