Function vlrToStringVlr, vlr

stringVLRStr = {$
    reserved                     : 0US, $                        
    userID                       : "",  $                
    recordID                     : 0US,  $                       
    recordLengthAfterHearder     : 0ULL, $                        
    Description                  : ""  $
    }

stringVLR = replicate(stringVLRStr, n_elements(vlr))

stringVLR.reserved = vlr.reserved
stringVLR.userID = strcompress(string(vlr.userID),/remove_all)
stringVLR.recordID = vlr.recordID
stringVLR.recordLengthAfterHearder = vlr.recordLengthAfterHearder
stringVLR.Description = strcompress(string(vlr.Description),/remove_all)

return, stringVLR

End
