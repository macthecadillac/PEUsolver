; https=//stackoverflow.com/questions/2171308/how-to-make-a-sub-string-selection-and-concatenation-in-excel
(set-logic SLIA)
(synth-fun f ((_arg_0 String)) String 
 ( (Start String (ntString)) 
 (ntString String (
	_arg_0
	"" " "
	(str.++ ntString ntString) 
	(str.replace ntString ntString ntString) 
	(str.at ntString ntInt)
	(int.to.str ntInt)
	(str.ite ntBool ntString ntString)
	(str.substr ntString ntInt ntInt)
)) 
 (ntInt Int (
	
	1 0 -1
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
(constraint (= (f "John Doe") "J Doe"))
(constraint (= (f "Mayur Naik") "M Naik"))
(constraint (= (f "Nimit Singh") "N Singh"))
(check-synth)
(define-fun f_1 ((_arg_0 String)) String (str.replace _arg_0 (str.substr _arg_0 1 (str.indexof _arg_0 " " 1)) " "))
