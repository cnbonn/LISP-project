





;-------------------------------------------------
(load "statefunctions")

; global variables
(defvar *mis*)     ;number of missionaries on left side
(defvar *can*)     ;number of cannables on left side
(defvar *boat*)    ;side that the canoe is on





;------------------------------------------------
(defun main()
  (cond
      ;get command line arguments
    ((= (length *args*) 3)
      (setf *mis* (parse-integer (car *args*)) *can* (parse-integer (cadr *args))
      )
    )
    ;call solving function
    (m-c *mis* *can*)

    (t
        ;print out useage message
        (format t "~%Missionaries and Cannibals Puzzle~%")
        (format t   "---------------------------------~%")
        (format t "Usage: (m-c m c)~%")
        (format t "       M - Ammount of Missionaries~%")
        (format t "       C - Ammount of Cannibals~%")
    )
  )
)

;------------------------------------------------

;Missionary and Cannables Problem solver
(defun m-c (m c)
   
  ;initilize global varables
  (setq *mis* m)
  (setq *can* c)

  (format t "~A Missionaries and ~A Cannibals:~%~%" m c)
  (format t "left bank      right bank      canoe      last move~%")
  (format t "---------      ----------      -----      ---------~%")
  (format t "~A M, ~A C      ~A M, ~A C      ~A       ~%") 
)


;-------------------------------------------------
;run Missionaries and canables uponloading file
(main)

;-------------------------------------------------
