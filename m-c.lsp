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
  (setf *mis* m) ;missionaries left
  (setf *can* c) ;cannables left
  (setf s "l") ; boat to left size
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
  (format t " ~A M, ~A C      ~A M, ~A C        left       start state ~%"
   m c '0 '0  )

 (cond
     ;boat move from left to right
     ((string= s "r") (setf *t* (string "left to right"))(setf p "right"))
     ;boat move from right to left 
     ((string= s "l") (setf *t* (string "right to left"))(setf p "left "))
  )
  ;(format t " ~A M, ~A C      ~A M, ~A C        ~A      move ~A M, ~A C ~A ~%"
  (format t " ~A~%"
  (dfs(start-state)) *t* )
  ;suppress printing NIL unpon return to interperter
  (values)  
)

;-------------------------------------------------
;run Missionaries and canables uponloading file
(main)

;-------------------------------------------------
