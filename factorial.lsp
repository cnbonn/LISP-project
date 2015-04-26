#|
        ***** factorial.lsp *****

Three versions of a factorial function (recursion, DO, DOTIMES).

Author: John M. Weiss, Ph.D.
Posted Spring 2015 for CSC461 Programming Languages class.
|#

; Recursive factorial function.
; Set (TRACE fact-r) to watch recursion in action.
(defun fact-r (n)
    "(fact-r n): returns n!, using recursion"
    (cond
        ((minusp n) nil)                        ; negative factorials are undefined
        ((<= n 1) 1)                            ; 0! = 1! = 1
        (t (* n (fact-r (1- n))))               ; N! = N * (N - 1)!
    )
)

; Iterative factorial function (DO version).
(defun fact-do (n)
    "(fact-do n): returns n!, using a do loop"
   (do  ((i n (1- i)) (f 1 (* f i)))            ; var initializations and updates
        ((= i 0) f)                             ; termination condition
    )                                           ; empty loop body
)

; Iterative factorial function (DOTIMES version).
(defun fact-dotimes (n)
    "(fact-dotimes n): returns n!, using a dotimes loop"
    (let ((f 1))                                ; bind local vars
        (dotimes (i n f)                        ; loop n times
            (setf f (* f (1+ i)))               ; compute next factorial
        )
    )
)

