


Function pointarrayclass::init, cox, coy, coz

  Compile_opt idl2
  
  case n_params() of
    0 : begin
          
        end
    1 : begin
            if (size(cox,/dimensions))[0] ne 3 and (size(cox,/dimensions))[1] ne 3 then begin
              print, 'Wrong size of input data...'
              print, 'Make sure that one of the dimension is 3...'
              return, 0
            endif else begin
              if (size(cox,/dimensions))[0] gt (size(cox,/dimensions))[1] then begin
                self.pt = ptr_new(cox)
                self.column = (size(cox,/dimensions))[0]
                if n_elements(size(cox,/dimensions)) gt 1 then self.row = (size(cox,/dimensions))[1]
              endif else self.pt = ptr_new(transpose(cox))
            endelse
        end
    3 : begin
          if (size(cox,/dimensions))[0] ne (size(coy,/dimensions))[0] or $
             (size(cox,/dimensions))[0] ne (size(coz,/dimensions))[0] then begin
             print, 'Wrong size of input data...'
             return, 0
          endif else begin
             ; It is the user duty to provid columns major array coordinates
             self.pt = ptr_new( [[cox],[coy],[coz]] )
             self.column = (size(cox,/dimensions))[0]
             self.row = 3
          endelse
        end
    else : print, ' Pointarrayclass - Wrong number of elements for initialization...'
  
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro pointarrayclass::cleanup

  Compile_opt idl2
  
    
End


Function pointarrayclass::x, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,0]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,0]
    
  endelse
  
End



Function pointarrayclass::y, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,1]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,1]
    
  endelse
  
End


Function pointarrayclass::z, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)[*,2]
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,2]
    
  endelse
  
End


Function pointarrayclass::xyz, id

  if n_elements(id) eq 0 then begin
  
    return, (*self.pt)
  
  endif else begin
    
    if id ge self.getDim() then print, 'Provided dimension too big...'
    
    return, (*self.pt)[id,*]
    
  endelse
  
End


Function pointarrayclass::coordinateValueByIndex, i, j
  
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


Function pointarrayclass::setX, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,0] = value
    return, 'done'
  endelse
  
End



Function pointarrayclass::setY, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,1] = value
    return, 'done'
  endelse
  
End


Function pointarrayclass::setZ, i, value

  if i ge self.column then begin
    print, 'Column value outside of data range...'
    return, 0
  endif else begin
    (*self.pt)[i,2] = value
    return, 'done'
  endelse
  
End


Function pointarrayclass::extractPoint, pointId

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


Function pointarrayclass::addPointToArray, pt2


End


Function pointarrayclass::removePointToArray, pointId


End


Function pointarrayclass::addVector, vector

  (*self.pt)[*,0] += vector.x()
  (*self.pt)[*,1] += vector.y()
  (*self.pt)[*,2] += vector.z()
  return, 1

End


Function pointarrayclass::subVector, vector

  (*self.pt)[*,0] -= vector.x()
  (*self.pt)[*,1] -= vector.y()
  (*self.pt)[*,2] -= vector.z()
  return, 1
  
End


Function pointarrayclass::sqrtDistance, point

  return, ( ((*self.pt)[*,0] - point.x())^2 + ((*self.pt)[*,1] - point.y())^2 + ((*self.pt)[*,2] - point.z())^2 )
  
End


Function pointarrayclass::distance, point

  return, sqrt( self.sqrtDistance(point))
  
End


;+
; This function should be called makeVectorArrayFromSinglePoint to make more sense
;-
Function pointarrayclass::makeVectorArray, point2

  if strlowcase(obj_class(point2)) eq 'pointarrayclass' then begin
    dum = self.makeVectorArrayFromPointArray(point2)
    return, dum
  endif else begin
    return, vectorarrayclass($
                            [replicate(point2.x(), self.column)-(*self.pt)[*,0]], $
                            [replicate(point2.y(), self.column)-(*self.pt)[*,1]], $
                            [replicate(point2.z(), self.column)-(*self.pt)[*,2]]  $
                            )
  endelse
  
End


Function pointarrayclass::makeVectorArrayFromPointArray, point2

  if strlowcase(obj_class(point2)) eq 'pointclass' then begin
    dum = self.makeVectorArray(point2)
    return, dum
  endif else begin
    return, vectorarrayclass($
      point2.x()-(*self.pt)[*,0], $
      point2.y()-(*self.pt)[*,1], $
      point2.z()-(*self.pt)[*,2]  $
      )
;    return, vectorarrayclass($
;      point2.x()-(*self.pt)[0,*], $
;      point2.y()-(*self.pt)[1,*], $
;      point2.z()-(*self.pt)[2,*]  $
;      )
    endelse
    
End


; With this function create a stack of vectorArray where each vectorArray holds the vectors from
;the points array to one point of point2 pointarray
Function pointarrayclass::makeVectorArrayStackFromPointArray, point2

  dim = point2.getDim()
  oArr = objarr(dim)
  
  for i = 0, dim-1 do begin
    
    ; Extracing the boundary point
    tempPoint = pointclass(point2.xyz(i))
    ; Duplicating the pointclass to an pointArrayClass
    ptArr = tempPoint.duplicateToPointArrayClass(self.getDim())
    ; Reseting the z coordinate for 'flat' vector
    dum = ptArr.transformTo2D()
    ; Creating the vectorArrayClass
    temp = self.makeVectorArrayFromPointArray(ptArr)
    oArr[i] = temp
    
  endfor

  return, oArr

End



Function pointarrayclass::getDim

  return, n_elements((*self.pt)[*,0])
  
End


Function pointarrayclass::transformTo2D

  (*self.pt)[*,2] = (*self.pt)[*,2] * 0.D
  return, 1
  
End


Pro pointarrayclass__define

  void = {pointarrayclass, $
    pt       : ptr_new() ,$  ; pointer to the points array store as fltarr(n,3)
    column   : 0         ,$  ; number of columns in the array
    row      : 0          $  ; number of rows in the array
  }
  
End

