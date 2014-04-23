; docformat = 'rst'
;+
; This function returns the ASPRS class string from an integer
; 
; :Hidden:
; 
; :Params:
;   val : in, required, type=integer
;     Integer value of the class. Usually a value between 0 and 15
;
; :Categories:
;   GENERAL
;   
;-
Function ClassificationID, val

case val of

0:  classID = "Created"
1:  classID = "Unclassified"
2:  classID = "Ground"
3:  classID = "Low vegetation"
4:  classID = "Medium vegetation"
5:  classID = "High vegetation"
6:  classID = "Building"
7:  classID = "Low point (noise)"
8:  classID = "Model key-point (mass point)"
9:  classID = "Water"
10: classID = "Reserved for ASPRS definition"
11: classID = "Reserved for ASPRS definition"
12: classID = "Overlap points"
else: classID = "Reserved for ASPRS definition"

endcase

return, classID

End

