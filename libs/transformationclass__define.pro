Pro transformationclass__define

  void = {transformationclass, $
    matrix    : dblarr(4,4) ,$
    invmatrix : dblarr(4,4)  $
  }
  
End


Function transformationclass::init, matrix, invmatrix

  Compile_opt idl2
  if n_elements(a) ne 0 then print, a
  case n_params() of
    0 : begin
          self.matrix = identity(4)
          self.invmatrix = identity(4)
        end
    1: begin
        if n_elements(matrix) ne 0 then begin
           ; checking matrix size and making sure it is a sqare matrix
           dim = size(matrix, /dimensions)
           if dim[0] ne dim[1] or dim[0] ne 4 then begin
            print, 'A 4x4 matrix is required...'
            return, 0
           endif else begin
            self.matrix = matrix
            self.invmatrix = invert(matrix)
           endelse
        endif 
      end
      2: begin
          if n_elements(matrix) ne 0 and n_elements(invmatrix) ne 0 then begin
            ; checking matrix size and making sure it is a sqare matrix
            dim = size(matrix, /dimensions)
            if dim[0] ne dim[1] or dim[0] ne 4 then begin
              print, 'A 4x4 matrix is required...'
              return, 0
            endif else begin
              self.matrix = matrix
            endelse
            invdim = size(invmatrix, /dimensions)
            if invdim[0] ne invdim[1] or invdim[0] ne 4 then begin
              print, 'A 4x4 matrix is required...'
              return, 0
            endif else begin
              self.invmatrix = invmatrix
            endelse
            
          endif
         end
    else: print, 'Hummm... something went wrong...'
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro transformationclass::cleanup

  Compile_opt idl2
  
;  ptr_free, $
;    self.world
    
End


Function transformationclass::getMatrix

  return, self.matrix
  
End


Function transformationclass::getInvMatrix

  return, self.invmatrix
  
End


Function transformationclass::invertTransformation

  return, transformationclass(self.invmatrix, self.matrix)
  
End


;+
; Not sure this is 100% correct, need more investigations on the subject
; It just consider that the substration of the two transformation matrix
; should gives a null matrix ?!
;-
Function transformationclass::transformEquality, transform

  if strlowcase(obj_class(transform)) ne 'transformationclass' then begin
    print, 'Please provide a transformation class object (transformationclass)...'
    return, 0
  endif else begin
    if total( self.matrix - transform.getmatrix() ) eq 0 then return, 1 else return, 0
  endelse
    
End


;+
; Not sure this is 100% correct, need more investigations on the subject
; It just consider that the substration of the two transformation matrix
; should gives a null matrix ?!
;-
Function transformationclass::isIdentity

  if total( self.matrix - identity(4) ) eq 0 then return, 1 else return, 0
  
End


Function transformationclass::translate, vector2

  if strlowcase(obj_class(vector2)) ne 'vectorclass' then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif else begin
    matrix = [$
      [ 1, 0, 0, vector2.x() ],$
      [ 0, 1, 0, vector2.y() ],$
      [ 0, 0, 1, vector2.z() ],$
      [ 0, 0, 0, 1 ] $
      ]
    invmatrix = [$
      [ 1, 0, 0, -vector2.x() ],$
      [ 0, 1, 0, -vector2.y() ],$
      [ 0, 0, 1, -vector2.z() ],$
      [ 0, 0, 0, 1 ] $
      ]
    return, transformationclass(matrix, invmatrix)
  endelse
  
End


Function transformationclass::scale, sx, sy, sz

  if n_params() ne 3 then begin
    print, 'Please provide one scale value for each axis...'
    return, 0
  endif else begin
    matrix = [$
      [ sx, 0, 0, 0 ],$
      [ 0, sy, 0, 0 ],$
      [ 0, 0, sz, 0 ],$
      [ 0, 0, 0, 1 ] $
      ]
    invmatrix = [$
      [ 1./sx, 0, 0, 0 ],$
      [ 0, 1./sy, 0, 0 ],$
      [ 0, 0, 1./sz, 0 ],$
      [ 0, 0, 0, 1 ] $
      ]
    return, transformationclass(matrix, invmatrix)
  endelse
  
End


;+
; Unifinished method as I'm not sure what the exact implementation is
; from book physically based rendering, p.80
;-
Function transformationclass::hasScale

  la2 = ( self.matrix * (vectorclass(1,0,0)).squareLength() )
  print, la2
  lb2 = ( self.matrix * (vectorclass(0,1,0)).squareLength() )
  print, lb2
  lc2 = ( self.matrix * (vectorclass(0,0,1)).squareLength() )
  print, lc2
  
  return, 1
  
End


Function transformationclass::rotateX, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    matrix = [$
      [ 1, 0, 0, 0 ],$
      [ 0, tcos, -tsin, 0 ],$
      [ 0, tsin, tcos,  0 ],$
      [ 0, 0, 0, 1 ] $
      ]
    return, transformationclass(matrix, transpose(matrix))
  endelse
End


Function transformationclass::rotateY, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    matrix = [$
      [ tcos, 0, tsin, 0 ],$
      [ 0, 1, 0, 0 ],$
      [ -tsin, 0, tcos, 0 ],$
      [ 0, 0, 0, 1 ] $
      ]
    return, transformationclass(matrix, transpose(matrix))
   endelse
  
End

Function transformationclass::rotateZ, angle

  if n_params() ne 1 then begin
    print, 'Please provide one rotation angle...'
    return, 0
  endif else begin
    tcos = cos(angle)
    tsin = sin(angle)
    matrix = [$
      [ tcos, -tsin, 0, 0 ],$
      [ tsin, tcos, 0,  0 ],$
      [ 0, 0, 1, 0 ],$
      [ 0, 0, 0, 1 ] $
      ]
    return, transformationclass(matrix, transpose(matrix))
   endelse
   
End


Function transformationclass::rotateAnyAxis, angle, vecAxis

  if n_elements(angle) eq 0 then begin
    print, 'Please provide an angle value...'
    return, 0
  endif
  if strlowcase(obj_class(vectorAxis)) ne 'vectorclass' then begin 
    print, 'Please provide one rotation angle...'
    return, 0
  endif
  
  vecAxis.normalizeLength
  s = sin(angle)
  c = cos(angle)
  
  mat = fltarr(4,4)
  mat[0,0] = vecAxis.x() * vecAxis.x() + ( 1. - vecAxis.x() * vecAxis.x() ) * c
  mat[1,0] = vecAxis.x() * vecAxis.y() * ( 1. - c ) - vecAxis.z() * s
  mat[2,0] = vecAxis.x() * vecAxis.z() * ( 1. - c ) + vecAxis.y() * s
  mat[3,0] = 0
  
  mat[0,1] = vecAxis.x() * vecAxis.y() * ( 1. - c ) + vecAxis.z() * s
  mat[1,1] = vecAxis.y() * vecAxis.y() + ( 1. - vecAxis.y() * vecAxis.y() ) * c
  mat[2,1] = vecAxis.y() * vecAxis.z() * ( 1. - c ) - vecAxis.x() * s
  mat[3,1] = 0
  
  mat[0,2] = vecAxis.x() * vecAxis.z() * ( 1. - c ) - vecAxis.y() * s
  mat[1,2] = vecAxis.y() * vecAxis.z() * ( 1. - c ) + vecAxis.x() * s
  mat[2,2] = vecAxis.z() * vecAxis.z() + ( 1. - vecAxis.z() * vecAxis.z() ) * c
  mat[3,2] = 0
  
  mat[0,3] = 0
  mat[1,3] = 0
  mat[2,3] = 0
  mat[3,3] = 1
  
  return, transformationclass(mat, transpose(mat))
End


Function transformationclass::lookAt, camPos, camAim, upVec
  
  if n_params() ne 3 then begin
    print, 'Please provide three values...'
    return, 0
  endif
  if strlowcase(obj_class(camPos)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif
  if strlowcase(obj_class(camAim)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif
  if strlowcase(obj_class(upVec)) ne 'vectorclass' then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif
  
  mat = fltarr(4,4)
  
  ; Initializing the camera position
  mat[3,0] = camPos.x()
  mat[3,1] = camPos.y()
  mat[3,2] = camPos.z()
  mat[3,3] = 1
  
  dir = (camPos.makeVector(camAim))
  dir = (camPos.makeVector(camAim))
  dir.normalizeLength
  upVec.normalizeLength
  ; print, upVec, dir
  left = vectorclass(upVec.det(dir))
  left.normalizeLength
  newUp = vectorclass(dir.det(left))
  newUp.normalizeLength
  
  mat[0,0] = left.x()
  mat[0,1] = left.y()
  mat[0,2] = left.z()
  mat[0,3] = 0.
  
  mat[1,0] = newUp.x()
  mat[1,1] = newUp.y()
  mat[1,2] = newUp.z()
  mat[1,3] = 0.
  
  mat[2,0] = dir.x()
  mat[2,1] = dir.y()
  mat[2,2] = dir.z()
  mat[2,3] = 0.
  
  print,'Matrice debbug...'
  print, invert(mat)
  print, 'Matrice inverse...'
  print, mat
  
  return, transformationclass(invert(mat), mat)
  

End


;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationclass::transfPoint, point, weight

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    if n_elements(weight) ne 0 then p = transpose([point.xyz(),weight]) $
          else p = transpose([point.xyz(),1.])
    return, p
  endelse

End


;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationclass::transfVector, vec, weight

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec)) ne 'vectorclass' then begin
    print, 'Please provide a vector class object (vectorclass)...'
    return, 0
  endif else begin
    if n_elements(weight) ne 0 then v = transpose([vec.xyz(),weight]) $
          else v = transpose([vec.xyz(),1.])
    return, v
  endelse
  
End


;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationclass::transfPointArray, point, weight

;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass' then begin
    print, 'Please provide a point array class object (pointarrayclass)...'
    return, 0
  endif else begin
    temp = size(point.xyz(),/dimensions)
    p = fltarr(temp[0],temp[1]+1)
    if n_elements(weight) ne 0 then begin
      p[0,0] = point.xyz()
      p[0,temp[1]] = replicate(weight,temp[0])
    endif else begin
      p[0,0] = point.xyz()
      p[0,temp[1]] = replicate(1.,temp[0])
    endelse
    return, p
  endelse

End


;+
; This function transform a 3 coordinates point into a 4 coordinates point
; by adding an homogeneous weight (1 by default)
;-
Function transformationclass::transfVectorArray, vec, weight

;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
  if strlowcase(obj_class(vec)) ne 'vectorarrayclass' then begin
      print, 'Please provide a vector array class object (vectorarrayclass)...'
      return, 0
    endif else begin
      temp = size(vec.xyz(),/dimensions)
      p = fltarr(temp[0],temp[1]+1)
      if n_elements(weight) ne 0 then begin
        p[0,0] = vec.xyz()
        p[0,temp[1]] = replicate(weight,temp[0])
      endif else begin
        p[0,0] = vec.xyz()
        p[0,temp[1]] = replicate(1.,temp[0])
      endelse
      return, p
    endelse
  
End


Function transformationclass::pointTransform, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    p = self.transfPoint(point)
    x = p[0]
    y = p[1]
    z = p[2]
    w = p[3]
    if w ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, pointclass(newx, newy, newz) else return, pointclass( ([newx, newy, newz]*invw) )
  endelse
  
End


Function transformationclass::pointTransformWithPtr, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointclass' then begin
    print, 'Please provide a point class object (pointclass)...'
    return, 0
  endif else begin
    p = self.transfPoint(point)
    x = p[0]
    y = p[1]
    z = p[2]
    w = p[3]
    if w ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, ptr_new(pointclass(newx, newy, newz)) else return, ptr_new(pointclass( ([newx, newy, newz]*invw) ))
  endelse
  
End


;-
Function transformationclass::pointArrayTransform, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass' then begin
    print, 'Please provide a point array class object (pointarrayclass)...'
    return, 0
  endif else begin
    p = self.transfPointArray(point)
    x = p[*,0]
    y = p[*,1]
    z = p[*,2]
    w = p[*,3]
    if w[0] ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, pointarrayclass(newx, newy, newz) else return, pointarrayclass( ([newx, newy, newz]*invw) )
  endelse
  
End


Function transformationclass::pointArrayTransformWithPtr, point

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(point)) ne 'pointarrayclass' then begin
    print, 'Please provide a point array class object (pointarrayclass)...'
    return, 0
  endif else begin
    p = self.transfPointArray(point)
    x = p[*,0]
    y = p[*,1]
    z = p[*,2]
    w = p[*,3]
    if w[0] ne 1 then invw = 1. / w
    newx = self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z + self.matrix[3,0] * w
    newy = self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z + self.matrix[3,1] * w
    newz = self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z + self.matrix[3,2] * w
    neww = self.matrix[0,3] * x + self.matrix[1,3] * y + self.matrix[2,3] * z + self.matrix[3,3] * w
    if w eq 1 then return, ptr_new(pointarrayclass(newx, newy, newz)) else return, ptr_new(pointarrayclass(([newx, newy, newz]*invw)))
  endelse
  
End
;-

;############################
; ORIGINAL METHOD
;############################
;Function transformationclass::vectorTransform, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorclass' then begin
;    print, 'Please provide a vector class object (vectorclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, vectorclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      )
;  endelse
;  
;End
;
;
;Function transformationclass::vectorTransformWithPtr, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, ptr_new( vectorclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      ) )
;  endelse
;  
;End
;#################################
; End of original methods
;#################################

;-
; Modified version of the original methods
; It should handle the vectorclass and vectorarrayclass seamlessly
; to be test...
;-
Function transformationclass::vectorTransform, vec2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec2)) eq 'vectorclass' or strlowcase(obj_class(vec2)) eq 'vectorarrayclass' then begin
    x = vec2.x()
    y = vec2.y()
    z = vec2.z()
    if strlowcase(obj_class(vec2)) eq 'vectorclass' then begin
      return, vectorclass( $
                          self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
                          self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
                          self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
                          )
    endif else begin
      return, vectorarrayclass( $
                          self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
                          self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
                          self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
        )
    endelse
  endif else begin
    print, 'Please provide a vector class or vector array object (vectorclass or vertocarrayclass)...'
    return, 0
  endelse

End


Function transformationclass::vectorTransformWithPtr, vec2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(vec2)) ne 'vectorclass' or strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
    print, 'Please provide a vector class or vector array object (vectorclass or vertocarrayclass)...'
    return, 0
  endif else begin
    x = vec2.x()
    y = vec2.y()
    z = vec2.z()
    if strlowcase(obj_class(vec2)) eq 'vectorclass' then begin
      return, ptr_new( vectorclass( $
        self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
        self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
        self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
        ) )
    endif else begin
      return, ptr_new( vectorarrayclass( $
        self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
        self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
        self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
        ) )
    endelse
  endelse
    
End
;############################################
; End of modified methods
;############################################


;-
;Function transformationclass::vectorArrayTransform, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector array class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, vectorarrayclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      )
;  endelse
;  
;End
;
;
;Function transformationclass::vectorArrayTransformWithPtr, vec2
;
;  if n_params() ne 1 then begin
;    print, 'Please provide a value...'
;    return, 0
;  endif
;  if strlowcase(obj_class(vec2)) ne 'vectorarrayclass' then begin
;    print, 'Please provide a vector array class object (vectorarrayclass)...'
;    return, 0
;  endif else begin
;    x = vec2.x()
;    y = vec2.y()
;    z = vec2.z()
;    return, ptr_new( vectorarrayclass( $
;      self.matrix[0,0] * x + self.matrix[1,0] * y + self.matrix[2,0] * z, $
;      self.matrix[0,1] * x + self.matrix[1,1] * y + self.matrix[2,1] * z, $
;      self.matrix[0,2] * x + self.matrix[1,2] * y + self.matrix[2,2] * z  $
;      ) )
;  endelse
;  
;End
;-


;-
; This function should handle normalclass
;-
Function transformationclass::normalTransform, normVec

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(normVec)) ne 'normalclass' then begin
    print, 'Please provide a normal class object (normalclass)...'
    return, 0
  endif else begin
  x = normVec.x()
  y = normVec.y()
  z = normVec.z()
  return, normalclass( $
    self.invMatrix[0,0] * x + self.invMatrix[0,1] * y + self.invMatrix[0,2] * z, $
    self.invMatrix[1,0] * x + self.invMatrix[1,1] * y + self.invMatrix[1,2] * z, $
    self.invMatrix[2,0] * x + self.invMatrix[2,1] * y + self.invMatrix[2,2] * z  $
    )
  endelse
  
End


Function transformationclass::rayTransform, ray2

  if n_params() ne 1 then begin
    print, 'Please provide one value...'
    return, 0
  endif
  if strlowcase(obj_class(ray2)) ne 'rayclass' then begin
    print, 'Please provide a ray class object (rayclass)...'
    return, 0
  endif else begin
    retOrigin = self.pointTransform(ray2.getOrigin())
    retDirection = self.vectorTransform(ray2.getDirection())
    return, rayclass(retOrigin, retDirection, ray2.getMint(), ray2.getMaxt())
  endelse
  
End


Function transformationclass::diffRayTransform, ray2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(ray2)) ne 'rayclass' then begin
    print, 'Please provide a ray class object (rayclass)...'
    return, 0
  endif else begin
    retOrigin = self.pointTransform(ray.getOrigin())
    retDirection = self.vectorTransform(ray.getDirection())
    return, rayclass(retOrigin, retDirection)
  endelse
  
End


Function transformationclass::transformBBox, bBox

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(bBox)) ne 'bboxclass' then begin
    print, 'Please provide a bounding box class object (bboxclass)...'
    return, 0
  endif else begin
    bbox_coor = bBox.getBox()
    dum0 = self.pointTransform(bbox_coor[0])
    dum1 = self.pointTransform(bbox_coor[1])
    return, bboxclass(dum0, dum1)
  endelse
  
End


Function transformationclass::multiplyTransform, transf2

  if n_params() ne 1 then begin
    print, 'Please provide a value...'
    return, 0
  endif
  if strlowcase(obj_class(transf2)) ne 'transformationclass' then begin
    print, 'Please provide a transformation class object (transformationclass)...'
    return, 0
  endif else begin
    m1 = self.getMatrix() ## transf2.getMatrix()
    m2 = transf2.getInvMatrix() ## self.getInvMatrix()
    return, transformationclass(m1, m2)
  endelse

End


Function transformationclass::swapsHandeness

  return, determ( (self.getMatrix())[0:2,0:2] ) lt 0.0
  
End


Function transformationclass::orthographic, zNer, zFar

  tempTrsfrm = transformationclass()
  tempScale = vectorclass( [1., 1., 1.] / (zFar-Fnear) )
  tempTrans = vectorclass( 0., 0., -zNear )
  return, transformationclass( (tempTrsfrm.scale(tempScale)).getMatrix() * (tempTrsfrm.translate(tempTrans)).getMatrix() )
  
End


Function transformationclass::perspective, fov, n, f

  tpersp = [$
          [1.0, 0.0, 0.0, 0.0],$
          [0.0, 1.0, 0.0, 0.0],$
          [0.0, 0.0, f/(f-n),-f/(f-n)],$
          [0.0, 0.0, 1.0, 0.0] $
          ]
   invTanAng = 1.0 / tan( (!dtor * fov) / 2.0)
   persp = transformationclass(tpersp)
   return, persp.scale(invTanAng, invTanAng, 1.0)

End

 

    