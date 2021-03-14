; https=//exceljet.net/formula/count-line-breaks-in-cell
(set-logic SLIA)
(synth-fun f ((_arg_0 String)) Int 
 ( (Start Int (ntInt)) 
 (ntString String (
	_arg_0
	"" " " "/n"
	(str.++ ntString ntString) 
	(str.replace ntString ntString ntString) 
	(str.at ntString ntInt)
	(int.to.str ntInt)
	(str.ite ntBool ntString ntString)
	(str.substr ntString ntInt ntInt)
)) 
 (ntInt Int (
	
	1 0 -1 0 1
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
(constraint (= (f "one") 1))
(constraint (= (f "one/ntwo") 2))
(constraint (= (f "one/ntwo/nthree") 3))
(check-synth)
(define-fun f_1 ((_arg_0 String)) Int (str.len (str.++ (int.to.str (str.indexof _arg_0 "/n" 1)) (int.to.str (str.len _arg_0)))))
