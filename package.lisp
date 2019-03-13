(in-package :cl-user)

(defpackage :trans-logic.control
  (:use :cl)
  (:export
   #:ignore-code
   #:pushr
   #:pushl
   #:pushr!
   #:pushl!
   #:pushlr
   #:pushrl
   #:head-n
   #:popl-n
   #:popl-n!
   #:popr-n
   #:popr-n!
   #:popl
   #:popl!
   #:popr
   #:popr!
   #:poprl
   #:poplr
   #:elem
   #:do-cons
   #:do-group
   #:do-until
   #:do-array
   )
  )

(defpackage :trans-logic.string
  (:use :cl)
  (:import-from :trans-logic.control
                #:do-group #:do-array)
  (:export
   #:string-to-symbol
   #:integer-to-string
   #:symbol-to-string
   #:string-to-keyword
   #:symbol-to-keyword
   #:keyword-to-string
   #:str-cat-ls
   #:str-cat
   #:are-strs-suffix
   #:are-strs-prefix
   #:is-str-suffix
   #:is-str-prefix
   #:str-has
   #:str-split
   #:str-matches
   #:str-replace
   )
  )

(defpackage :trans-logic.data-algebra
  (:use :cl)
  (:import-from :trans-logic.control
                #:do-group #:ignore-code)
  (:import-from :trans-logic.string
                #:str-cat
                #:symbol-to-string
                #:string-to-symbol
                #:str-matches
                )
  (:export
   #:valid-operators
   #:symbolic-sets
   #:def-primitive
   #:def-operator
   #:parameterize-set
   #:def-symbolic-set
   #:mk-empty-set
   #:@s
   #:@c
   #:@r
   #:@clan
   #:empty-set?
   ;;#:empty
   ;;#:universal
   #:@set-or
   #:@set-and
   #:@set-xor
   #:@set-diff
   #:@diag
   #:has-elem?
   #:is-elem?
   #:@set<=
   #:@set<
   #:@set>=
   #:@set>
   #:@size
   #:@powerset
   #:@cartesian-product
   #:@transpose
   #:@compose
   #:equal?
   #:@set=
   #:yin
   #:yang
   #:@inc-set
   #:@dec-set
   #:with-sets
   #:number?
   #:symbol?
   #:symb<
   #:symb>
   #:symb=
   #:symb-match
   #:symb-cat
   #:symb-reverse
   #:symb-length
   #:symb-upcase
   #:symb-downcase
   #:yin-project
   #:yang-project
   #:yin-functional?
   #:yang-functional?
   #:yin-regular?
   #:yang-regular?
   #:@cross-set-or
   #:@cross-set-and?
   #:@superstriction
   #:@substriction
   #:@cross-superstriction
   #:@cross-substriction
   #:@minimal-key
   #:@natural-join
   #:@inner-join
   #:@outer-join
   )
  )

