; https=//exceljet.net/formula/count-specific-words-in-a-cell
(set-logic SLIA)
(synth-fun f ((_arg_0 String) (_arg_1 String)) Int 
 ( (Start Int (ntInt)) 
 (ntString String (
	_arg_0 _arg_1
	"" " "
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
(constraint (= (f "The fox jumped over the fox" "fox") 2))
(constraint (= (f "The fox jumped over the fox" "ox") 2))
(constraint (= (f "The fox jumped over the fox" "Fox") 0))
(check-synth)
(define-fun f_1 ((_arg_0 String) (_arg_1 String)) Int (+ (int.ite (str.suffixof _arg_1 _arg_0) 1 -1) 1))
