Pro pointclass__define

  void = {pointclass, $
    x    : 0.D ,$
    y    : 0.D ,$
    z    : 0.D $
  }
  
End


Function pointclass::init, cox, coy, coz

  Compile_opt idl2
  ;resolve_routine, 'vectorclass'
  
  
  case n_params() of
    0 : begin
          self.x = 0.D
          self.y = 0.D
          self.z = 0.D
        end
    1 : begin
          if n_elements(cox) eq 3 then begin
            self.x = double(cox[0])
            self.y = double(cox[1])
            self.z = double(cox[2])
          endif else print, 'Pointclass - Wrong number of elements for initialization...'
        end
    3 : begin
          if n_elements(cox) ne 0 then self.x = double(cox) else self.x = 0.
          if n_elements(coy) ne 0 then self.y = double(coy) else self.y = 0.
          if n_elements(coz) ne 0 then self.z = double(coz) else self.z = 0.
        end
    else : print, 'PointClass - Wrong number of elements for initialization...'
  
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro pointclass::cleanup

  Compile_opt idl2
  
    
End


Function pointclass::x

  return, self.x
  
End



Function pointclass::y

  return, self.y
  
End


Function pointclass::z

  return, self.z
  
End


Function pointclass::xyz

  return, [self.x, self.y, self.z]
  
End


Function pointclass::setX, value

  self.x = value
  return, 'done'
  
End



Function pointclass::setY, value

  self.y = value
  return, 'done'
  
End


Function pointclass::setZ, value

  self.z = value
  return, 'done'
  
End


Function pointclass::addVector, vector

  self.x += vector.x()
  self.y += vector.y()
  self.z += vector.z()
  return, [self.x, self.y, self.z]

End


Function pointclass::subVector, vector

  self.x -= vector.x()
  self.y -= vector.y()
  self.z -= vector.z()
  return, [self.x, self.y, self.z]
  
End


Function pointclass::sqrtDistance, point

  return, ( (self.x - point.x())^2 + (self.y - point.y())^2 + (self.z - point.z())^2)
  
End


Function pointclass::distance, point

  return, sqrt( self.sqrtDistance(point))
  
End


Function pointclass::makeVector, point2

  return, vectorclass( point2.x()-self.x, point2.y()-self.y, point2.z()-self.z )
  
End


Function pointclass::duplicateToPointArrayClass, nDim

  if n_elements(nDim) eq 0 then begin
    print, 'A dimension value is needed...'
    return, 0
  endif else begin
    return, pointarrayclass( replicate(self.x(),nDim),replicate(self.y(),nDim), replicate(self.z(),nDim) )
  endelse
  
End
