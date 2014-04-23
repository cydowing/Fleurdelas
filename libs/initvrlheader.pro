Pro InitVRLHeader, vrlStruct

compile_opt idl2, logical_predicate

    ; Define and read public header structure

vrlStruct = {$
    reserved                     : 0US, $                        
    userID                       : bytarr(16),  $                
    recordID                     : 0US,  $                       
    recordLengthAfterHearder     : 0US, $                        
    Description                  : bytarr(32)  $
}

End            