;;;
;;; String handling functions.
;;;

(ql:quickload "cl-ppcre")
(in-package :trans-logic.string)

(defun string-to-symbol (s &optional case-sensitive)
  (if (and s)
      (intern (if case-sensitive s (string-upcase s)))
      nil)
  )

(defun integer-to-string (int)
  (write-to-string int)
  )

(defun symbol-to-string (s &optional upcase)
  (if (and upcase)
      (symbol-name s)
      (string-downcase (symbol-name s))
      )
  )

(defun string-to-keyword (s)
  (if (and s)
      (intern (string-upcase s) "KEYWORD")
      nil))

(defun symbol-to-keyword (s)
  (string-to-keyword (symbol-to-string s)))

(defun keyword-to-string (k &optional upcase)
  (symbol-to-string k  upcase)
  )

(defun str-cat-2 (s1 s2)
  "Concatenates 2 strings"
  (concatenate 'string s1 s2))

(defun str-cat (&rest strings)
  "Concatenates 2 or more strings"
  (reduce #'str-cat-2 strings))

(defun str-cat-ls (strings)
  "Concatenates list of strings"
  (let ((total-len 0)
        (ret-str nil)
        )
    (do-group (s strings)
      (incf total-len (length s))
      )
    (setf ret-str (make-array total-len :element-type 'character :fill-pointer 0))
    (do-group (s strings)
      (do-array (c s)
        (vector-push c ret-str)
        )
      )
    ret-str
    )
  )

(defun str-last-char (s)
  "Gets the last character of a string"
  (char s (- (length s) 1)))

(defun is-str-suffix-one (suf str)
  "Checks if the strings has the given suffix"
  (if (> (length suf) (length str))
      nil
      (let* ((off (- (length str) (length suf)))
             (end (subseq str off (length str))))
        (string= suf end)
        )))

(defun is-str-prefix-one (pre str)
  "Checks if the strings has the given prefix"
  (if (> (length pre) (length str))
      nil
      (let* ((off (length pre))
             (end (subseq str 0 off)))
        (string= pre end)
        )))

(defun fn-or (&rest args)
  "Apparently `or` is a macro, not a function, so we can't just pass it as such"
  (let ((cur args))
    (tagbody again
       (cond
         ((not cur)
          (return-from fn-or nil))
         ((not (car cur))
          (setf cur (cdr cur))
          (go again))
         ((and t)
          (return-from fn-or t))
         )
       )
    )
  )

(defmacro def-are-strs-*ix (ls-name str-name)
  `(defun ,ls-name (*s str)
     "Applies is-str-suffix-str over a list, returns true if any match"
     (apply #'fn-or (map 'list #'(lambda (s) (,str-name s str)) *s))
     ))

(def-are-strs-*ix are-strs-suffix is-str-suffix-one)
(def-are-strs-*ix are-strs-prefix is-str-prefix-one)

(defmacro def-is-str-*ix (gen-name ls-name str-name)
  `(defun ,gen-name (*/s str)
     (if (listp */s)
         (,ls-name */s str)
         (,str-name */s str)
         )
     ))

(def-is-str-*ix is-str-suffix are-strs-suffix is-str-suffix-one)
(def-is-str-*ix is-str-prefix are-strs-prefix is-str-prefix-one)


(defun str-has (regex str)
  (cl-ppcre:all-matches regex str))

(defun str-split (regex str &key unlistify)
  "Splits the string at each place that the given pcre regex is matched"
  (let ((res (cl-ppcre:split regex str))
        )
    (if (and unlistify (= 1 (length res)))
        (setf res str)
        )
    res
    )
  )

(defun str-matches (regex str &key ignore-case)
  "returns nil if there is not match in str, t otherwise"
  (if (not ignore-case)
      (if (cl-ppcre:all-matches regex str) t nil)
      (if (cl-ppcre:all-matches regex
                                (string-downcase str))
          t
          nil)
      )
  )

(defun str-replace (regex targ str)
  (cl-ppcre:regex-replace-all regex str targ))
