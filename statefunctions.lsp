

;Check for valid state
(defun valid-state (m c b max-m max-c)
  (cond
    ((and (= m max-m) (= c max-c) (equal b 'L)) nil);All people on right but boat on left: invalid
    ((and (= m 0) (= c 0) (equal b 'R)) nil);All people on left but boar on right: invalid 
    ((or (> m max-m) (> c max-c)) nil);Missionary or canibal totals greater than max: invalid
    ((or (< m 0) (< c 0)) nil);Missionaries or canibals less than 0: invalid
    ((= m max-c) t); Number of missionaries = to max of cannibals: valid
    ((= m 0) t); All missionaries on the right side: valid
    ( (and (>= m c) 
           (>= (- max-m m) (- max-c c)) 
           ) t) ;There is more missionaries on both sides: valid
    (t nil);All other states: invalid
  )
)

;See if a state is already in the state list
(defun used-state (m c b state-list)
  (cond
    ((NULL state-list) nil)
    ( (or (equal (list m c b) (car state-list) ); If the first item in the list is equal return true 
          (used-state m c b (cdr state-list))
      )   ; Or see if its true for the rest of the list
    t)
  )
)

(defun add-state? (m c b state-list max-m max-c)
  (and (valid-state m c b max-m max-c) (not (used-state m c b state-list)))
)

;Pushes the new state on to the list to pass recursively without affecting the list used on this level
(defun pass-list (m c b curr-list)
  (cons (list m c b) curr-list)
)
