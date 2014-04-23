Function lasPointByteStrip, data

  ; Just to test the bits shift procedure to retreive the information
  strip = bytarr(8)

    strip[0] = data.nReturn and '00000111'bb               ; myReturnNumber
    strip[1] = ishft(data.nReturn, -3) and '111'bb         ; myNumberOfReturns
    strip[2] = ishft(data.nReturn, -3) and '1'bb           ; myScanDirection
    strip[3] = ishft(data.nReturn, -1) and '1'bb           ; myEdge
    
    strip[4] = data.class and '00011111'bb                 ; myClassification 
    strip[5] = ishft(data.class, -5) and '1'bb             ; mySynthetic
    strip[6] = ishft(data.class, -1) and '1'bb             ; myKeyPoint 
    strip[7] = ishft(data.class, -1) and '1'bb             ; myWithheld   

    
    return, strip

End