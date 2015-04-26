

; global variables
(defvar *mis*)     ;number of missionaries on left side
(defvar *can*)     ;number of cannables on left side
(defvar *boat* 2)    ;number of people that are allowed on the boat

(defun start-state ()
  (list (list 0 0 'L))
)

(defun lunch (state)
  (let ( (mL (- *mis* (mis state)))
         (cL (- *can* (can state)))
         (mR (mis state))
         (cR (can state))
        )
  
   (or (> cL mL) (> cR mR ) )
    
  )
)

;Check for valid state
(defun valid-state (state)
  (cond
    ((and (= (mis state) *mis*) (= (can state) *can*) (string= (side state) 'L)) nil)
    ;All people on right but boat on left: invalid
    ((and (= (mis state) 0) (= (can state) 0) (string= (side state) 'R)) nil)
    ;All people on left but boar on right: invalid 
    ((or (> (mis state) *mis*) (> (can state) *can*)) nil)
    ;Missionary or canibal totals greater than max: invalid
    ((or (< (mis state) 0) (< (can state) 0) ) nil);Missionaries or canibals less than 0: invalid
    ((= (mis state) *can*) t); Number of missionaries = to max of cannibals: valid
    ((= (mis state) *mis*) t); All missionaries on the right side: valid
    ( (and (>= (mis state) (can state))
           (>= (- *mis* (mis state)) (- *can* (can state)))
       ) t) ;There is more missionaries on both sides: valid
    (t nil);All other states: invalid
  )
)


(defun mis (state)
  (first state)
)
(defun can (state)
  (second state)
)
(defun side (state)
  (third state)
)

;See if a state is already in the state list
(defun used-state (state state-list)
  (cond
    ((NULL state-list) nil)
    ( (or (equal state (car state-list) ); If the first item in the list is equal return true 
          (used-state state (cdr state-list))
      )   ; Or see if its true for the rest of the list
    t)
  )
)

(defun good-state (state state-list)
  (and (valid-state state) (not (used-state state state-list)) (not (lunch state)) )
)

(defun win-state( state )
  (and (= *mis* (mis state)) (= *can* (can state)) (equal (side state) 'R))
)

(defun mv2c  (state)
  (cond
    ((string= (side state) 'L) (list (mis state) (+ (can state) 2) 'R))
    ((string= (side state) 'R) (list (mis state) (- (can state) 2) 'L))
  )
)

(defun mv2m (state)
  (cond
    ((string= (side state) 'L) (list (+ (mis state) 2) (can state) 'R))
    ((string= (side state) 'R) (list (- (mis state) 2) (can state) 'L))
  )
)

(defun mv1m1c (state)
  (cond
    ((string= (side state) 'L) (list (+ (mis state) 1) (+ (can state) 1) 'R))
    ((string= (side state) 'R) (list (- (mis state) 1) (- (can state) 1) 'L))
  )
)

(defun mv1c  (state)
  (cond
    ((string= (side state) 'L) (list (mis state) (+ (can state) 1) 'R))
    ((string= (side state) 'R) (list (mis state) (- (can state) 1) 'L))
  )
)

(defun mv1m (state)
  (cond
    ((equal (side state) 'L) (list (+ (mis state) 1) (can state) 'R))
    ((equal (side state) 'R) (list (- (mis state) 1) (can state) 'L))
  )
)
;Pushes the new state on to the list to pass recursively without affecting the list used on this level
(defun pass-list (new curr-list)
  (cons new curr-list)
)

(defun dfs (state-list)
  (let (  (rlist 0) )
    (cond
      ((not (good-state (car state-list) (cdr state-list))) nil)
      ((win-state (car state-list)) state-list)
      ((setf rlist (dfs (pass-list (mv2c   (car state-list)) state-list))) rlist)
      ((setf rlist (dfs (pass-list (mv2m   (car state-list)) state-list))) rlist)
      ((setf rlist (dfs (pass-list (mv1m1c (car state-list)) state-list))) rlist)
      ((setf rlist (dfs (pass-list (mv1m   (car state-list)) state-list))) rlist)
      ((setf rlist (dfs (pass-list (mv1c   (car state-list)) state-list))) rlist)
      
      (t nil)
    )
  )
)
