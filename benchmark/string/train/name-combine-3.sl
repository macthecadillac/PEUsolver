(set-logic SLIA)
(synth-fun f ((_arg_0 String) (_arg_1 String)) String
    ((Start String (ntString))
     (ntString String (_arg_0 _arg_1 " " "."
(str.++ ntString ntString)
(str.replace ntString ntString ntString)
(str.at ntString ntInt)
(int.to.str ntInt)
(str.ite ntBool ntString ntString)
(str.substr ntString ntInt ntInt)
))
      (ntInt Int (0 1 2
(+ ntInt ntInt)
(- ntInt ntInt)
(str.len ntString)
(str.to.int ntString)
(str.indexof ntString ntString ntInt)
))
(ntBool Bool (true false
(= ntInt ntInt)
(str.prefixof ntString ntString)
(str.suffixof ntString ntString)
(str.contains ntString ntString)
))
))
(constraint (= (f "Launa" "Withers") "L. Withers"))
(constraint (= (f "Lakenya" "Edison") "L. Edison"))
(constraint (= (f "Brendan" "Hage") "B. Hage"))
(constraint (= (f "Bradford" "Lango") "B. Lango"))
(constraint (= (f "Rudolf" "Akiyama") "R. Akiyama"))
(constraint (= (f "Lara" "Constable") "L. Constable"))

(check-synth)
(define-fun f_1 ((_arg_0 String) (_arg_1 String)) String (str.++ (str.++ (str.++ (str.at _arg_0 0) ".") " ") _arg_1))
