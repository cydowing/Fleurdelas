Pro InitEVRLHeader, evrlStruct

compile_opt idl2, logical_predicate

    ; Define and read public header structure

evrlStruct = {$
    reserved                     : 0US, $                        
    userID                       : bytarr(16),  $                
    recordID                     : 0US,  $                       
    recordLengthAfterHearder     : 0ULL, $                        
    Description                  : bytarr(32)  $
    }

End            