(set-logic SLIA)
(synth-fun f ((_arg_0 String) (_arg_1 String)) String
    ((Start String (ntString))
     (ntString String (_arg_0 _arg_1 " " "," "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "New York" 
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
(constraint (= (f "University of Pennsylvania" "Phialdelphia, PA, USA")
"Phialdelphia, PA, USA"))

(constraint (= (f "UCLA" "Los Angeles, CA")
"Los Angeles, CA, USA"))

(constraint (= (f "Cornell University" "Ithaca, New York, USA")
"Ithaca, NY, USA"))

(constraint (= (f "Penn" "Philadelphia, PA, USA")
"Philadelphia, PA, USA"))

(constraint (= (f "University of Maryland College Park" "College Park, MD")
"College Park, MD, USA"))

(constraint (= (f "University of Michigan" "Ann Arbor, MI, USA")
"Ann Arbor, MI, USA"))

(constraint (= (f "UC Berkeley" "Berkeley, CA")
"Berkeley, CA, USA"))

(constraint (= (f "MIT" "Cambridge, MA")
"Cambridge, MA, USA"))

(constraint (= (f "Rice University" "Houston, TX")
"Houston, TX, USA"))

(constraint (= (f "Yale University" "New Haven, CT, USA")
"New Haven, CT, USA"))

(constraint (= (f "Columbia University" "New York, NY, USA")
"New York, NY, USA"))

(constraint (= (f "NYU" "New York, New York, USA")
"New York, NY, USA"))

(constraint (= (f "UC Berkeley" "Berkeley, CA")
"Berkeley, CA, USA"))

(constraint (= (f "UIUC" "Urbana, IL")
"Urbana, IL, USA"))

(constraint (= (f "Temple University" "Philadelphia, PA")
"Philadelphia, PA, USA"))

(constraint (= (f "Harvard University" "Cambridge, MA, USA")
"Cambridge, MA, USA"))

(constraint (= (f "University of Connecticut" "Storrs, CT, USA")
"Storrs, CT, USA"))

(constraint (= (f "Drexel University" "Philadelphia, PA")
"Philadelphia, PA, USA"))

(constraint (= (f "New Haven University" "New Haven, CT, USA")
"New Haven, CT, USA"))

(constraint (= (f "University of California, Santa Barbara" "Santa Barbara, CA, USA")
"Santa Barbara, CA, USA"))


(check-synth)
