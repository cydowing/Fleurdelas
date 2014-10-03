;+
; To be revisited for a better handle or initialisation
; possibility to initialize directly with a vector2Darrayclass argument
;-
Function vector2Darrayclass::init, cox, coy

  Compile_opt idl2

 self.pt = ptr_new( [[cox],[coy]] )
 self.column = (size(cox,/dimensions))[0]
 self.row = 2

  ; Initializing the object
  return, 1
  
End


Pro vector2Darrayclass::cleanup

  Compile_opt idl2
  
End


Function vector2Darrayclass::x, value

  return, (*self.pt)[*,0]
  
End



Function vector2Darrayclass::y

  return, (*self.pt)[*,1]
  
End


Function vector2Darrayclass::z

  return, (*self.pt)[*,2]
  
End


Function vector2Darrayclass::xyz

  return, (*self.pt)
  
End


Function vector2Darrayclass::coordinateValueByIndex, i, j

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif
  
  if j ge self.row then begin
    print, 'Row value outside of data range...'
    return, 0
  endif
  
  return, (*self.pt)[i,j]
  
End


Function vector2Darrayclass::setX, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,0] = value
    return, 'done'
  endelse
  
End



Function vector2Darrayclass::setY, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,1] = value
    return, 'done'
  endelse
  
End


Function vector2Darrayclass::setZ, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,2] = value
    return, 'done'
  endelse
  
End


Function vector2Darrayclass::extractPoint, pointId

  if pointId ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    return, pointclass($
                      self.coordinateValueByIndex(pointId,0) ,$
                      self.coordinateValueByIndex(pointId,1) ,$
                      self.coordinateValueByIndex(pointId,2)  $
                      )
  endelse
  
End


Function vector2Darrayclass::translate, vector2

  (*self.pt)[*,0] += vector2.x()
  (*self.pt)[*,1] += vector2.y()
  (*self.pt)[*,2] += vector2.z()
  return, 1

End


Function vector2Darrayclass::scaleUp, vector2

  (*self.pt)[*,0] *= vector2.x()
  (*self.pt)[*,1] *= vector2.y()
  (*self.pt)[*,2] *= vector2.z()
  return, 1


End


Function vector2Darrayclass::scaleDown, vector2

  temp = 1. / vector2.xyz()
  (*self.pt)[*,0] *= temp[0]
  (*self.pt)[*,1] *= temp[1]
  (*self.pt)[*,2] *= temp[2]
  return, 1
  
  
End


Function vector2Darrayclass::opposite

  return, -(*self.pt)

End


Function vector2Darrayclass::length

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2 )
  
end



Function vector2Darrayclass::horizLength

  return, sqrt( ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 )
  
end


Function vector2Darrayclass::squareLength

  return, ((*self.pt)[*,0])^2 + ((*self.pt)[*,1])^2 + ((*self.pt)[*,2])^2
 
end


Pro vector2Darrayclass::normalizeLength

  inv = 1. / self.length()
  (*self.pt) *= inv
  
End


Function vector2Darrayclass::getNormLength

  inv = 1. / self.length()
  return, ( (*self.pt) * inv )
  
End


Function vector2Darrayclass::dot, vector2

  dot = ((*self.pt)[*,0] * vector2.x()) + ((*self.pt)[*,1] * vector2.y()) + ((*self.pt)[*,2] * vector2.z())
  return, dot
  
End


Function vector2Darrayclass::getRadAngle, vector2

  return, acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vector2Darrayclass::getDegAngle, vector2

  return, 180./!PI * acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vector2Darrayclass::det, vector2

  detX = ((*self.pt)[*,1] * vector2.z()) - ((*self.pt)[*,2] * vector2.y())
  detY = ((*self.pt)[*,2] * vector2.x()) - ((*self.pt)[*,0] * vector2.z())
  detZ = ((*self.pt)[*,0] * vector2.y()) - ((*self.pt)[*,1] * vector2.x())
;  print, detX
;  print, detY
;  print, detZ
  return, vector2Darrayclass(detX, detY, detZ)

End


Function vector2Darrayclass::paralVolume, vector2

  tempvec = vector2Darrayclass(self.det(vector2))
  return, tempvec.length()

End


;+
; a function that creates a local coordinate system
; based on the vector hold in self.
; 
; IDL> print, vec6.localCoordinateSystem()
; <ObjHeapVar6(VECTORCLASS)><ObjHeapVar7(VECTORCLASS)>
; IDL> t = vec6.localCoordinateSystem()
; IDL> print, t[0].xyz()
; 0.0000000     0.062378287     -0.99805260
;-
Function vector2Darrayclass::localCoordinateSystem

  tempv2 = fltarr(self.column,3)

  id = where( (*self.pt)[*,0] gt (*self.pt)[*,1], count, complement = compId )
  
  if count gt 0 then begin
    invLen = 1. / sqrt( ((*self.pt)[id,0])^2 + ((*self.pt)[id,2])^2 )
    tempv2[id,0] = -(*self.pt)[id,2] * invLen
    tempv2[id,1] = 0.0
    tempv2[id,2] = (*self.pt)[id,0] * invLen
  endif
  if n_elements(compId) gt 0 then begin
    invLen = 1. / sqrt( ((*self.pt)[compId,0])^2 + ((*self.pt)[compId,2])^2 )
    tempv2[compId,0] = 0.0
    tempv2[compId,1] = -(*self.pt)[compId,2] * invLen
    tempv2[compId,2] = (*self.pt)[compId,0] * invLen
  endif
  print, tempv2[*,0]
  print, tempv2[*,1]
  print, tempv2[*,2]
  print, 'v2'
  v2 = vector2Darrayclass(tempv2[*,0], tempv2[*,1], tempv2[*,2])
  
  ;v3 = fltarr(self.column)
  print, 'v3'
  v3 = vector2Darrayclass(self.det(v2))

  ;with this line the 3 vectors coordinates are return
  ;return, [ [self.xyz()],[v2.xyz()],[v3.xyz()] ] 
  
  ; with this line the actual vector objects are return - most likely more efficient
  return, [ v2, v3 ]
  
End



Pro vector2Darrayclass__define

  void = {vector2Darrayclass, $
    pt       : ptr_new() ,$  ; pointer to the points array store as fltarr(n,3)
    column   : 0         ,$  ; number of columns in the array
    row      : 0          $  ; number of rows in the array
  }
  
End