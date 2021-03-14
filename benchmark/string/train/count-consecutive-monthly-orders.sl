; https=//exceljet.net/formula/count-consecutive-monthly-orders
(set-logic SLIA)
(synth-fun f ((_arg_0 String)) Int 
 ( (Start Int (ntInt)) 
 (ntString String (
	_arg_0
	"" " " "0"
	(str.++ ntString ntString) 
	(str.replace ntString ntString ntString) 
	(str.at ntString ntInt)
	(int.to.str ntInt)
	(str.ite ntBool ntString ntString)
	(str.substr ntString ntInt ntInt)
)) 
 (ntInt Int (
	
	1 0 -1 1
	(+ ntInt ntInt)
	(- ntInt ntInt)
	(str.len ntString)
	(str.to.int ntString)
	(int.ite ntBool ntInt ntInt)
	(str.indexof ntString ntString ntInt)
)) 
 (ntBool Bool (
	
	true false
	(= ntInt ntInt)
	(str.prefixof ntString ntString)
	(str.suffixof ntString ntString)
	(str.contains ntString ntString)
)) ))
(constraint (= (f "7 0 0 5 4 4") 3))
(constraint (= (f "0 0 2 3 3 0") 3))
(constraint (= (f "5 6 4 6 0 7") 4))
(constraint (= (f "0 4 5 0 0 2") 2))
(constraint (= (f "3 0 3 0 1 2") 2))
(constraint (= (f "5 3 2 5 6 1") 6))
(check-synth)
(define-fun f_1 ((_arg_0 String)) Int (int.ite (str.contains _arg_0 "0") (int.ite (str.suffixof "0" _arg_0) (+ (+ 1 1) 1) (int.ite (str.prefixof "0" _arg_0) (+ 1 1) (int.ite (= (+ (str.indexof _arg_0 "0" 1) -1) 1) (int.ite (str.contains _arg_0 (int.to.str 1)) (+ 1 1) (+ (+ 1 1) 1)) (+ (+ (+ 1 1) 1) 1)))) (str.indexof _arg_0 (str.at _arg_0 0) 1)))
