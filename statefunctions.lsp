

; global variables
(defvar *mis*)     ;number of missionaries on left side
(defvar *can*)     ;number of cannables on left side
(defvar *boat* 2)    ;number of people that are allowed on the boat

;Check for valid state
(defun valid-state (state)
  (cond
    ((and (= (mis state) *mis*) (= (can state) *can*) (equal (side state) 'L)) nil)
    ;All people on right but boat on left: invalid
    ((and (= (mis state) 0) (= (can state) 0) (equal (side state) 'R)) nil)
    ;All people on left but boar on right: invalid 
    ((or (> (mis state) *mis*) (> (can state) *can*)) nil)
    ;Missionary or canibal totals greater than max: invalid
    ((or (< (mis state) 0) (< (can state) 0)) nil);Missionaries or canibals less than 0: invalid
    ((= (mis state) *can*) t); Number of missionaries = to max of cannibals: valid
    ((= (mis state) 0) t); All missionaries on the right side: valid
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
    ( (or (equal (state) (car state-list) ); If the first item in the list is equal return true 
          (used-state state (cdr state-list))
      )   ; Or see if its true for the rest of the list
    t)
  )
)

(defun add-state (state state-list)
  (and (valid-state state) (not (used-state m c state state-list)))
)

(defun win-state( state )
  (and (= *mis* (mis state)) (= *can* (can state)) (equal (side state) 'R))
)

(defun mv2c  (state)
  (cond
    ((equal (side state) 'L) (list (mis state) (+ 2 (can state)) 'R))
    ((equal (side state) '2) (list (mis state) (- 2 (can state)) 'L))
  )
)

(defun mv2m (state)
  (cond
    ((equal (side state) 'L) (list (+ 2 (mis state)) (can state) 'R))
    ((equal (side state) '2) (list (- 2 (mis state)) (can state) 'L))
  )
)

(defun mv1m1c (state)
  (cond
    ((equal (side state) 'L) (list (+ 1 (mis state)) (+ 1 (can state)) 'R))
    ((equal (side state) '2) (list (- 1 (mis state)) (- 1 (can state)) 'L))
  )
)

(defun mv1c  (state)
  (cond
    ((equal (side state) 'L) (list (mis state) (+ 1 (can state)) 'R))
    ((equal (side state) '2) (list (mis state) (- 1 (can state)) 'L))
  )
)

(defun mv2m (state)
  (cond
    ((equal (side state) 'L) (list (+ 1 (mis state)) (can state) 'R))
    ((equal (side state) '2) (list (- 1 (mis state)) (can state) 'L))
  )
)
;Pushes the new state on to the list to pass recursively without affecting the list used on this level
(defun pass-list (new curr-list)
  (cons new curr-list)
)

(defun dfs (state-list)
  (let (  (rlist 0) (try 0 )(state (car state-list))  )
    (cond
      ((win-state state) state-list)
      ((and (set 'try (mv1m1c state)) (add-state (try)) (setf rlist (dfs (pass-list try state-list)))) rlist )
      ((and (set 'try (mv2m   state)) (add-state (try)) (setf rlist (dfs (pass-list try state-list)))) rlist )
      ((and (set 'try (mv1m   state)) (add-state (try)) (setf rlist (dfs (pass-list try state-list)))) rlist )
      ((and (set 'try (mv2c   state)) (add-state (try)) (setf rlist (dfs (pass-list try state-list)))) rlist )
      ((and (set 'try (mv1c   state)) (add-state (try)) (setf rlist (dfs (pass-list try state-list)))) rlist )
      (t nil)
    )
  )
)
