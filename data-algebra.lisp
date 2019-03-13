;;;
;;; This is an implementation of the data algebra (described by Sheperd and
;;; Bloor), as a CL library. We have a first-class set-class. And set-elements,
;;; which can be: symbols, numbers, strings, and so forth. They cannot be other
;;; lisp constructs (like lists/collections/objects).
;;;
;;; We can convert lisp collections into sets, if we like, and back.
;;;
;;; We use this data-algebra as the basis for higher-level logic like
;;; transformational logic, which is an intriguing and esoteric logic formulated
;;; by Bollinger. Not yet sure how practically useful the algebra and the logic
;;; each are, so this code-base is a kind of experiment.
;;;


(in-package :trans-logic.data-algebra)


(defvar valid-operators '(assert))

;;; Commonly used sets.
;;; We can often special case the set-operators when using them on sets like
;;; universe and empty. It is impossible for us to create the universe set as
;;; anything other than an abstract set.
;;(defvar symbolic-sets '(universe empty continuum counting fractional orientational))
(defvar symbolic-sets '())


(defmacro with-sets (bindings &body body)
  ;;; TODO validate that all the forms used don't contain arbitrary lisp code
  `(let* ,bindings ,@body)
  )

(defmacro def-primitive (name args &body body)
  `(progn
     (push ',name valid-operators)
     (defmethod ,name ,args ,@body)
     )
  )

(defmacro def-operator (name args &body body)
  ;;; TODO validate that all the forms used don't contain arbitrary lisp code
  `(progn
     (push ',name valid-operators)
     (defmethod ,name ,args ,@body)
     )
  )

(defmacro parameterize-set (name args &body body)
  "Creates parameterized set. Not validated."
  `(progn
     (push ',name valid-operators)
     (defun ,name ,args ,@body)
     )
  )

(defmacro def-symbolic-set (name &body has-elem-body)
  "Defines a symbolic set, and an implementation for testing if an element is a
   member of that set. The implementation is arbitrary lisp code (i.e. not
   validated). Returns lisp-style booleans (t, nil)."
  `(progn
     (assert (not (member ',name symbolic-sets)))
     (push ',name symbolic-sets)
     (def-primitive has-elem? ((a (eql ',name)) elem)
       ,@has-elem-body
       )
     (export ',name :trans-logic.data-algebra)
     )
  )

;;; XXX 'notation' is a bad name. Too ambiguous to refer to macros only.
;;; Sometimes we want defuns. Maybe def-syntax?
;;; Currently defuns are used to do 2 things: (1) impl funcs, and (2)
;;; parameterized sets.
(defmacro def-notation (name args &body body)
  ;;; Wrapper around def-macro. Body not validated.
  `(progn
     (push ',name valid-operators)
     (defmacro ,name ,args ,@body)
     )
  )

(def-symbolic-set empty
  ;;; Empty sets contain nothing.
  nil
  )

(def-symbolic-set universal
  ;;; Universal set contains everything
  t
  )

(defclass generic-set ()
  (
   ;;; Can either be a literal set or a symbolic set (i.e. all prime digits of
   ;;; pi).
   (members :initarg :members :initform nil :accessor members)
   )
  )

(defun dedup-list (ls)
  ;;; Wrapper aroud remove-duplicates. Tired of typing the test KW.
  (remove-duplicates ls :test #'equal)
  )

(defun mk-set (&rest things)
  ;;; We cannot create empty sets.
  (assert (and things))
  (make-instance 'generic-set :members (dedup-list things))
  )

(defun mk-empty-set ()
  ;;; Used for testing, creates a generic-set with no elements.
  (make-instance 'generic-set)
  )

(def-notation @s (&rest things)
  (if things
      `(mk-set ,@things)
      `'empty
      )
  )

(defun list-to-set (things &rest init-sets)
  (if (not things)
      (return-from list-to-set 'empty)
      )
  (let ((type 'generic-set)
        )
    (cond
      ((and init-sets)
       (if (equal (length (dedup-list (mapcar #'type-of init-sets))) 1)
           (setf type (type-of (car init-sets)))
           )
       )
      )
    (make-instance type :members (dedup-list things))
    )
  )

(def-primitive empty-set? ((a generic-set))
  (equal (members a) nil)
  )

(def-primitive empty-set? ((a symbol))
  (equal a 'empty)
  )

(def-primitive universal-set? ((a symbol))
  (equal a 'universal)
  )


(def-primitive @set-or-bin ((a generic-set) (b generic-set))
  (list-to-set (dedup-list (append (members a) (members b))) a b)
  )

(def-primitive @set-or-bin ((a generic-set) (b symbol))
  (assert (member b symbolic-sets))
  (if (empty-set? b)
      a
      (if (universal-set? b)
          b
          )
      )
  )

(def-primitive @set-or-bin ((a symbol) (b generic-set))
  (@set-or-bin b a)
  )

(def-primitive @set-or ((a generic-set) b &rest sets)
  (let ((ret (@set-or-bin a b))
        )
    (do-group (s sets)
      (setf ret (@set-or-bin ret s))
      )
    ret
    )
  )

(def-operator @set-xor-bin ((a generic-set) (b generic-set))
  (with-sets ((common (@set-and a b))
              )
    (@set-or (@set-diff a common) (@set-diff b common))
    )
  )

(def-primitive @set-xor ((a generic-set) (b generic-set) &rest sets)
  (let ((ret (@set-xor-bin a b))
        )
    (do-group (s sets)
      (setf ret (@set-xor-bin ret s))
      )
    ret
    )
  )


(def-primitive has-elem? ((a generic-set) elem)
  (if (member elem (members a) :test #'equal?)
      t
      nil
      )
  )

(def-notation is-elem? (elem a)
  `(has-elem? ,a ,elem)
  )


;;; Note that we only allow `set` as the type of the left operand. It is easy to
;;; ask if a concrete set is part of an abstract set. I _think_ we can support
;;; subset operations on 2 abstract sets, if the truth tables are identical.
(def-primitive @set<= ((a generic-set) (b generic-set))
  (if (and (members a))
      (do-group (e (members a))
        (if (not (has-elem? b e))
            (return-from @set<= 'empty)
            )
        )
      )
  a
  )

(def-primitive @set< ((a generic-set) (b generic-set))
  (if (and (not (empty-set? (@set<= a b)))
           (empty-set? (@set= a b)))
      a
      'empty
      )
  )


;;; XXX This is wrong. Should return 'a', if true, but will return 'b'...
(def-operator @set>= ((a generic-set) (b generic-set))
  (if (not (empty-set? (@set<= b a)))
      a
      'empty
      )
  )

(def-operator @set> ((a generic-set) (b generic-set))
  (if (not (empty-set? (@set< b a)))
      a
      'empty
      )
  )

(defmethod @set-and-bin ((a generic-set) (b generic-set))
  (list-to-set (remove-if #'(lambda (e) (not (has-elem? b e))) (members a)) a b)
  )

(defmethod @set-diff-bin ((a generic-set) (b generic-set))
  (list-to-set (remove-if #'(lambda (e) (has-elem? b e)) (members a)) a b)
  )

(def-operator @set-and ((a generic-set) (b generic-set) &rest sets)
  (let ((ret (@set-and-bin a b))
        )
    (do-group (s sets)
      (setf ret (@set-and-bin ret s))
      )
    ret
    )
  )

(def-operator @set-diff ((a generic-set) (b generic-set) &rest sets)
  (let ((ret (@set-diff-bin a b))
        )
    (do-group (s sets)
      (setf ret (@set-diff-bin ret s))
      )
    ret
    )
  )

(def-operator @size ((s generic-set))
  (length (members s))
  )

(def-operator @powerset ((a generic-set))
  (assert nil)
  )

(def-operator @cartesian-product ((a generic-set) (b generic-set))
  "This returns a relation, where the elements in each set are paired up in
   couplets."
  )

;;; XXX does this even make sense on a plain set???? Maybe assert-nil?
(def-operator @transpose ((a generic-set))
  (assert nil)
  )

;;;
;;; A pure couplet is actually a special set. It conforms to the mathematical
;;; definition of the couplet as shown in the book, but is significantly fatter
;;; memory wise, because it also inherits the members of a set, and redundantly
;;; stores the yin and yang in that set.
;;;
(defclass pure-couplet (generic-set)
  (
   (yin :initarg :yin :initform nil :accessor yin)
   (yang :initarg :yang :initform nil :accessor yang)
   )
  )


(defun mk-pure-couplet (&key yin yang)
  (make-instance 'pure-couplet
                 :yin yin :yang yang
                 :members
                 (list (mk-set yin)
                       (mk-set yin yang)))
  )

;;;
;;; A more efficient, but less pure implemenetation of a couplet, which carries
;;; no extra baggage. This is the default couplet we use throughout the code.
;;; The price we pay for this, is that we can't use normal set operations on a
;;; couplet -- though mixing couplets with sets using unions or differences
;;; seems to carry no practical value.
;;;
(defclass couplet ()
  (
   (yin :initarg :yin :initform nil :accessor yin)
   (yang :initarg :yang :initform nil :accessor yang)
   )
  )

(defun mk-couplet (&key yin yang)
  (make-instance 'couplet :yin yin :yang yang)
  )

(defmacro @c (yin yang)
  "Shorthand for creating literal couplets"
  `(mk-couplet :yin ,yin :yang ,yang)
  )

(def-primitive equal? ((a number) (b number))
  (equal a b)
  )


;;; Empty sets represent false-hood, and non-empty sets represent truth.
;;; OK, so what if we try (equal? 'empty 'empty)
;;; They are equal, and we return 'empty. Which can be interpreted as
;;; inequality. If given 2 empty sets we should return a universal set.
(def-primitive equal? ((a generic-set) (b generic-set))
  (if (not (equal? (@size a) (@size b)))
      nil
      (progn
        (let ((ls-a (members a))
              (ls-b (members b))
              (same t)
              )
          (do-group (e ls-a)
            (if (not (member e ls-b :test #'equal?))
                (progn
                  (setf same nil)
                  (return-from equal? same)
                  )
                )
            )
          (return-from equal? same)
          )
        )
      )
  )

(def-primitive equal? ((a symbol) (b symbol))
  (equal a b)
  )

;;; Set equality is like primitive equality, except that instead of return the
;;; lisp boolean symbols we return a set. Emptiness implies falsehood, while
;;; anything else impies truth. If sets A and B are equal, we return A, unless A
;;; is also empty. In which case we return the univeral set. In principle,
;;; generic-sets are empty only during testing (we don't let the user create
;;; empty generic-sets, only symbolic sets).
(def-primitive @set= ((a generic-set) (b generic-set))
  (if (equal? a b)
      (if (empty-set? a)
          'universal
          a
          )
      'empty
      )
  )

(def-primitive @set= ((a symbol) (b symbol))
  (if (equal? a b)
      (if (empty-set? a)
          'universal
          a
          )
      )
  )

(def-primitive equal? ((c1 couplet) (c2 couplet))
  (and (equal? (yin c1) (yin c2))
       (equal? (yang c1) (yang c2))
       )
  )

(def-primitive @transpose ((a couplet))
  (mk-couplet :yin (yang a) :yang (yin a))
  )

(def-primitive @compose-bin ((a couplet) (b couplet))
  (if (not (equal (yang a) (yin b)))
      (return-from @compose-bin 'empty)
      )
  (mk-couplet :yin (yin a) :yang (yang b))
  )

(def-primitive @compose ((a couplet) (b couplet) &rest couplets)
  (let ((ret (@compose-bin a b))
        )
    (do-group (c couplets)
      (setf ret (@compose-bin ret c))
      (if (equal ret 'empty)
          (return-from @compose 'empty)
          )
      )
    ret
    )
  )

(defclass relation (generic-set)
  ;;; A set of couplets
  (
   )
  )

(defun mk-relation (&key elems)
  (let ((ret (make-instance 'relation :members elems))
        )
    ret
    )
  )

(def-notation @r (&rest elems)
  `(mk-relation :elems (list ,@elems))
  )

(def-primitive yin ((r relation))
  (list-to-set (mapcar #'yin (members r)))
  )

(def-primitive yang ((r relation))
  (list-to-set (mapcar #'yang (members r)))
  )

(def-primitive @transpose ((r relation))
  (list-to-set (mapcar #'@transpose (members r)) r)
  )

(def-primitive @compose-bin ((a relation) (b relation))
  (let ((compositions '())
        )
    (do-group (c1 (members a))
      (do-group (c2 (members b))
        (push (@compose c1 c2) compositions)
        )
      )
    (mk-relation :elems (dedup-list (remove 'empty compositions)))
    )
  )

(def-primitive @compose ((a relation) (b relation) &rest relations)
  (let ((ret (@compose-bin a b))
        )
    (do-group (r relations)
      (setf ret (@compose-bin ret r))
      (if (equal ret 'empty)
          (return-from @compose 'empty)
          )
      )
    ret
    )
  )

(defmethod @enumerate ((a relation))
  ;;; This only works on relations that contain couplets of the form:
  ;;;  (@c XYZ nil)
  ;;; Not sure if this is the right behaviour.
  ;;;
  ;;; This is equivalent to a composition between two relations A and B,
  ;;; where A contains our yin-only couplets, and B contains yang-only couplets,
  ;;; which were derived from the truncated set of positive integers.
  ;;;
  ;;; Or perhaps a (@transpose (@project (@transpose rel) $set-of-trunc-pos-ints))
  ;;; Not sure why projection happens only on yin and not yang. Arbitrary choice.
  (let ((ret '())
        (i 0)
        )
    (do-group (c (members a))
      (push (@compose c (@c nil i)) ret)
      (incf i)
      )
    )
  )


(defun @inc-set (size)
  "Creates an increment set of size `size`"
  (let ((ret (mk-relation))
        )
    (dotimes (i size)
      (push (@c i (+ i 1)) (members ret))
      )
    ret
    )
  )

(defun @dec-set (size)
  "Generates a decrement set of size `size`"
  (let ((ret (mk-relation))
        )
    (dotimes (i size)
      (push (@c i (- i 1)) (members ret))
      )
    ret
    )
  )

(defmacro def-relation (name)
  (let ((full-name (string-to-symbol
                    (str-cat "relation-"
                             (symbol-to-string name))))
        )
    `(defclass ,full-name (relation)
       (
        )
       )
    )
  )

(defmacro number? (n)
  `(numberp ,n)
  )

(defmacro symbol? (n)
  `(or (stringp ,n) (symbolp ,n))
  )

(defmacro generic-set? (n)
  `(typep ,n 'generic-set)
  )

(defmacro relation? (n)
  `(typep ,n 'relation)
  )

(defmacro couplet? (n)
  `(typep ,n 'couplet)
  )

(defmacro clan? (n)
  `(typep ,n 'clan)
  )

(defmacro def-symb-unary-op (name case-sensitive &body body)
  `(progn
     (defmethod ,name ((s string))
       ,@body
       )
     (defmethod ,name ((s symbol))
       (let ((ret (,name (symbol-to-string s)))
             )
         (if (stringp ret)
             (string-to-symbol ret ,case-sensitive)
             ret
             )
         )
       )
     )
  )

(defmacro def-symb-binary-op (name &body body)
  `(progn
     (defmethod ,name ((s1 string) (s2 string))
       ,@body
       )
     (defmethod ,name (s1 s2)
       (assert (and (symbol? s1) (symbol? s2)))
       (let ((ret (,name (if (symbolp s1) (symbol-to-string s1) s1)
                         (if (symbolp s2) (symbol-to-string s2) s2)
                         )
                  )
             )
         (if (symbolp s1)
             (string-to-symbol ret)
             ret
             )
         )
       )
     )
  )

(def-symb-binary-op symb<
  (if (string< s1 s2) t)
  )

(def-symb-binary-op symb>
  (if (string> s1 s2) t)
  )

(def-symb-binary-op symb=
  (string= s1 s2)
  )

(def-primitive equal? ((a string) b)
  (assert (symbol? b))
  (symb= a b)
  )

(def-primitive equal? ((a symbol) b)
  (assert (symbol? b))
  (symb= a b)
  )

(def-symb-binary-op symb-match
  (str-matches s2 s1)
  )

(def-symb-binary-op symb-cat
  (str-cat s1 s2)
  )

(def-symb-unary-op symb-reverse nil
  (reverse s)
  )

(def-symb-unary-op symb-length t
  (length s)
  )

(def-symb-unary-op symb-upcase t
  (string-upcase s)
  )

(def-symb-unary-op symb-downcase t
  (string-downcase s)
  )


(defmethod subset-if ((a generic-set) func)
  (let ((ret (make-instance (type-of a)))
        )
    (do-group (elem (members a))
      (if (funcall func elem)
          (push elem (members ret))
          )
      )
    ret
    )
  )

(defmacro @subset-if (set &body code)
  ;;; TODO validate the forms in `code`
  `(subset-if ,set
              #'(lambda (x)
                  ,@code
                  )
              )
  )

(defclass relation-array (relation)
  ;;; A relation that represents an array.
  ;;; elem^pos
  (
   )
  )


(def-operator @insert ((a relation-array) (c couplet))
  ;;; Couplet contains position. We insert elem at position and down-shit all
  ;;; other elems.
  ;;; TODO figure out if doing @compose below on an empty inc-set produces an
  ;;; empty set, which in turn makes all of this a no-op.
  ;;; XXX Note that we are not checking the types here...
  (with-sets ((upshift (@subset-if a (>= (yang x) (yang c))))
              (inc-set (@inc-set (@size upshift)))
              (new (@compose upshift inc-set))
              )
    (@set-or (@set-diff a upshift) (@r c) new)
    )
  )

(def-operator @remove ((a relation-array) pos)
  (assert (pos >= 0))
  (with-sets ((c (@c pos pos))
              (downshift (@subset-if a (> (yang x) pos)))
              (dec-set (@dec-set (@size downshift)))
              (new (@compose downshift dec-set))
              )
    (@set-or (@set-diff a (@compose a (@r c)) downshift) new)
    )
  )

(def-operator @elem ((a relation-array) pos)
  (assert (pos >= 0))
  (with-sets ((c (@c pos pos))
              )
    (@compose a c)
    )
  )

;;; Adds new couplet, removes couplet with the same yang.
(def-operator @store ((a relation-array) (c couplet))
  (@set-or (@set-diff a (@compose a (@r (@c (yang c) (yang c))))) (@r c))
  )

;;; Deletes elem at pos, does not downshift
(def-operator @delete ((a relation-array) pos)
  (@set-diff a (@compose a (@r (@c pos pos))))
  )

(def-primitive @diag ((a generic-set))
  (mk-relation :elems (mapcar #'(lambda (e) (@c e e)) (members a)))
  )

(def-operator @yin-project ((a relation) (b generic-set))
  ;;; Create relation C, which is like relation A, but the yin values are the
  ;;; atoms in B. In other words we filter out all couplets that don't have yins
  ;;; present in b.
  (@compose (@diag b) a)
  )

(def-operator @yang-project ((a relation) (b generic-set))
  ;;; Create relation C, which is like relation A, but the yang values are the
  ;;; atoms in B.
  (@compose a (@diag b))
  )

;;; XXX I dont think these are needed...
(defmethod @yin-project ((a relation) (b relation))
  ;;; Create relation C, which is like relation A, but the yin values are the
  ;;; couplets in B.
  )

(defmethod @yang-project ((a relation) (b relation))
  ;;; Create relation C, which is like relation A, but the yang values are the
  ;;; couplets in B.
  )

(def-primitive yin-functional? ((r relation))
  (let* ((yang (yang r))
         )
    (if (equal (@size yang) (@size r))
        t
        nil
        )
    )
  )

(def-primitive yang-functional? ((r relation))
  (yin-functional? (@transpose r))
  )

(defclass clan (generic-set)
  (
   )
  )

(defun mk-clan (&key elems)
  (let ((ret (make-instance 'clan :members elems))
        )
    ret
    )
  )

(def-notation @clan (&rest elems)
  `(mk-clan :elems (list ,@elems))
  )

(def-primitive yin ((c clan))
  (apply #'@set-or (mapcar #'yin (members c)))
  )

(def-primitive yang ((c clan))
  (apply #'@set-or (mapcar #'yang (members c)))
  )

(def-primitive @transpose ((c clan))
  (list-to-set (mapcar #'@transpose (members c)) c)
  )

(def-primitive @compose-bin ((c1 clan) (c2 clan))
  (let ((compositions '())
        )
    (do-group (r1 (members a))
      (do-group (r2 (members b))
        (push (@compose r1 r2) compositions)
        )
      )
    (list-to-set compositions c1 c2)
    )
  )

(def-primitive @compose ((a clan) (b clan) &rest clans)
  (let ((ret (@compose-bin a b))
        )
    (do-group (c clans)
      (setf ret (@compose-bin ret c))
      (if (equal ret 'empty)
          (return-from @compose 'empty)
          )
      )
    ret
    )
  )


(def-primitive yin-functional? ((c clan))
  (do-group (r (members c))
    (if (not (yin-functional? r))
        (return-from yin-functional? nil)
        )
    )
  t
  )

(def-primitive yang-functional? ((c clan))
  (do-group (r (members c))
    (if (not (yang-functional? r))
        (return-from yang-functional? nil)
        )
    )
  t
  )

(def-primitive yang-regular? ((c clan))
  (do-group (r (members c))
    (if (not (yang-functional? r))
        (return-from yang-regular? nil)
        )
    (if (not (equal (yang r) (yang c)))
        (return-from yang-regular? nil)
        )
    )
  t
  )

(def-primitive yin-regular? ((c clan))
  (do-group (r (members c))
    (if (not (yin-functional? r))
        (return-from yin-regular? nil)
        )
    (if (not (equal (yin r) (yin c)))
        (return-from yin-regular? nil)
        )
    )
  t
  )

(def-primitive @cross-set-or ((c1 clan) (c2 clan))
  (let ((ret '())
        )
    (do-group (r1 (members c1))
      (do-group (r2 (members c2))
        (push ret (@set-or r1 r2))
        )
      )
    (dedup-list ret)
    )
  )

(def-primitive @cross-set-and ((c1 clan) (c2 clan))
  (let ((ret '())
        )
    (do-group (r1 (members c1))
      (do-group (r2 (members c2))
        (push ret (@set-and r1 r2))
        )
      )
    (dedup-list ret)
    )
  )

(def-primitive @superstriction ((r1 relation) (r2 relation))
  (if (@set<= r2 r1)
      r1
      'empty
      )
  )

(def-primitive @substriction ((r1 relation) (r2 relation))
  (if (@set<= r1 r2)
      r1
      'empty
      )
  )

;;; While the book distinguishes between the *striction and cross-*striction
;;; operators, I prefer the ambiguity of overloading the method. We also define
;;; the cross- variants since users would probably grep for those first.
(def-primitive @superstriction ((c1 clan) (c2 clan))
  (let ((ret '())
        )
    (do-group (r1 (members c1))
      (do-group (r2 (members c2))
        (push ret (@superstriction r1 r2))
        )
      )
    (list-to-set ret c1 c2)
    )
  )

(def-primitive @substriction ((c1 clan) (c2 clan))
  (let ((ret '())
        )
    (do-group (r1 (members c1))
      (do-group (r2 (members c2))
        (push ret (@substriction r1 r2))
        )
      )
    (list-to-set ret c1 c2)
    )
  )

(def-operator @cross-superstriction ((c1 clan) (c2 clan))
  (@superstriction c1 c2)
  )

(def-operator @cross-substriction ((c1 clan) (c2 clan))
  (@substriction c1 c2)
  )

(defmethod @minimal-key ((c clan))
  )

(defmethod @valid-keys ((c clan))
  ;;; The set of valid keys is at least as large as the clan. Not sure that this
  ;;; is useful.
  (assert nil)
  )

(defmethod @natural-join ((c1 clan) (c2 clan))
  )


(defmethod @inner-join ((c1 clan) (c2 clan))
  )

(defmethod @outer-join ((c1 clan) (c2 clan))
  )

;;; A horde is a set of clans. I guess it is analogous to multiple tables in a
;;; DB? As I understand it, it is trivial to map the clan-methods to hordes.
(defclass horde (generic-set)
  (
   )
  )

(defun everything< (a b)
  ;;; Compares everything.
  (cond
    ((and (couplet? a) (generic-set? b))
     t
     )
    ((and (couplet? a) (relation? b))
     t
     )
    ((and (generic-set? a) (relation? b))
     t
     )
    ((and (couplet? a) (clan? b))
     t
     )
    ((and (generic-set? a) (clan? b))
     t
     )
    ((and (relation? a) (clan? b))
     t
     )
    ((and (number? a) (symbol? b))
     t
     )
    ((and (number? a) (number? b))
     (< a b)
     )
    ((and (symbol? a) (symbol? b))
     (symb< a b)
     )
    ((and (couplet? a) (couplet? b))
     (or (everything< (yin a) (yin b))
         (everything< (yang a) (yang b)))
     )
    ((and (clan? a) (clan? b))
     (< (@size a) (@size b))
     )
    ((and (relation? a) (relation? b))
     (< (@size a) (@size b))
     )
    ((and (generic-set? a) (generic-set? b))
     (< (@size a) (@size b))
     )
    )
  )

(defmethod sorted-members ((s generic-set))
  ;;; TODO
  (members s)
  )

(defmethod print-object ((obj generic-set) stream)
  (princ (cons '@s (sorted-members obj)) stream)
  )

(defmethod print-object ((obj couplet) stream)
  (princ (list '@c (yin obj) (yang obj)) stream)
  )

(defmethod print-object ((obj relation) stream)
  (princ (cons '@r (sorted-members obj)) stream)
  )

(defmethod print-object ((obj clan) stream)
  (princ (cons '@clan (sorted-members obj)) stream)
  )
