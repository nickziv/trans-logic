;;; Useful control structures, and supporting list-functions


(in-package :trans-logic.control)

(defmacro ignore-code (&body body)
  `(progn
     )
  )

(defun pushr1 (ls elem)
  "Pushes a single elem to the rightmost side of the list"
  (assert (listp ls))
  (append ls (list elem)))

(defun pushl1 (ls elem)
  "Pushes a single elem to the leftmost side of the list"
  (assert (listp ls))
  (append (list elem) ls))

(defun pushr (ls &rest elems)
  "Pushes a list of elems to the rightmost side of the list"
  (reduce #'pushr1 elems :initial-value ls))

(defmacro pushr! (ls &rest elems)
  "Same as pushr, but overwrites the list"
  `(setf ,ls (pushr ,ls ,@elems)))

(defun pushl (ls &rest elems)
  "Pushes a list of elems to the leftmost side of the list"
  (reduce #'pushl1 elems :initial-value ls))

(defun pushlr (ls &rest elems)
  (pushr (pushl ls elems) elems)
  )

(defun pushrl (ls &rest elems)
  "Same as above. But added for symmetry"
  (pushl (pushr ls elems) elems)
  )

(defmacro pushl! (ls &rest elems)
  "Same as pushl, but overwrites the list"
  `(setf ,ls (pushl ,ls ,@elems)))

(defun head-n (list n)
  "Get the first n elems of a list"
  (iter:iter
    (iter:for x from 1 to n)
    (iter:for y in list)
    (iter:collect y)))

(defun popl-n (ls n)
  "Pops the leftmost n elems of a list"
  (if (< n (length ls))
      (progn (iter:iter
               (iter:for x from 1 to n)
               (setq ls (cdr ls)))
             ls)
      nil))

(defmacro popl-n! (ls n)
  "Same as popl-n but overwrites the list"
  `(setf ,ls (popl-n ,ls ,n)))


(defun popr-n (ls n)
  "Pops the rightmost n elems of a list"
  (if (< n (length ls))
      (head-n ls (- (length ls) n))
      nil))

(defmacro popr-n! (ls n)
  "Same as popr-n but overwrites the list"
  `(setf ,ls (popr-n ,ls ,n)))

(defun popl (ls)
  "Pops the leftmost elem from the list"
  (popl-n ls 1))

(defmacro popl! (ls)
  "Same as popl but overwrite the list"
  `(setf ,ls (popl ,ls)))

(defun popr (ls)
  "Pops the rightmost elem from the list"
  (popr-n ls 1))

(defmacro popr! (ls)
  "Same as popr but overwrite the list"
  `(setf ,ls (popr ,ls)))


(defun poprl (ls)
  (popl (popr ls))
  )

(defun poplr (ls)
  (popr (popl ls))
  )

(defun create-list-bindings (names-arr list)
  (let ((ret '()))
    (dotimes (i (length names-arr))
      (pushr! ret (list (elem names-arr i) `(car (nthcdr (+ ,i) ,list))))
      )
    ret
    )
  )

(defun elem (arr i)
  (cond
    ((>= i (length arr))
     nil)
    ((and t)
     (elt arr i))
    )
  )

(defun create-arr-bindings (names-arr off arr)
  (let ((ret '()))
    (dotimes (i (length names-arr))
      (pushr! ret (list (elem names-arr i) `(elem ,arr (+ ,off ,i))))
      )
    ret
    )
  )

(defmacro do-cons (args &body body)
  (assert (= (length args) 2))
  (let ((e (car args))
        (l (cadr args))
        )
    `(do ((,e ,l (cdr ,e))) ((not ,e) ,e)
       ,@body
       )
    )
  )

(defmacro do-group (args &body body)
  "Just like dolist, except it allows arbitrary look-ahead"
  (assert (>= (length args) 2))
  (let* ((e (gensym))
         (es (coerce (popr args) 'vector))
         (g (car (last args)))
         (i (gensym)))
    (assert (symbolp e))
    `(progn
       (do-cons (,e ,g)
         (let (,@(create-list-bindings `,es `,e))
           ,@body
           )
         )
       )
    )
 )

(defmacro do-until (test &body body)
  `(do ()
       (,test
        nil)
     ,@body
     )
  )

(defmacro do-array (args &body body)
  "Just like do-group, except it works on arrays"
  (assert (>= (length args) 2))
  (let* ((off (gensym))
         (es (coerce (popr args) 'vector))
         (g (car (last args)))
         (i (gensym)))
    (assert (symbolp off))
    `(progn
       (dotimes (,off (length ,g))
         (let (,@(create-arr-bindings `,es `,off `,g))
           ,@body
           )
         )
       )
    )
  )
