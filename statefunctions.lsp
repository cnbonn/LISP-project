
;Check for valid state
(defun valid-state (m c b max-m max-c)
  (cond
    ((and (= m max-m) (= c max-c) (equal b "L")) nil)
    ((and (= m 0) (= c 0) (equal b "R")) nil)
    ((or (> m max-m) (> c max-c)) nil)
    ((= m max-c) t)
    ((= m 0) t)
    ((<= (abs (- m c)) (- max-m max-c)) t)
    (t nil)
  )
)
