Pro vectorclass__define

  void = {vectorclass, $
    vx    : 0.D ,$
    vy    : 0.D ,$
    vz    : 0.D ,$
    inherits IDL_Object $
  }
  
End


;+
; To be revisited for a better handle or initialisation
; possibility to initialize directly with a vectorclass argument
;-
Function vectorclass::init, cox, coy, coz

  Compile_opt idl2

  case n_params() of
    1 : begin
          if n_elements(cox) eq 3 then begin
            self.vx = double(cox[0])
            self.vy = double(cox[1])
            self.vz = double(cox[2])
          endif else print, 'Vectorclass - Wrong number of elements for initialization...'
        end
    3 : begin
          if n_elements(cox) ne 0 then self.vx = double(cox) else self.vx = 0.
          if n_elements(coy) ne 0 then self.vy = double(coy) else self.vy = 0.
          if n_elements(coz) ne 0 then self.vz = double(coz) else self.vz = 0.
        end
  else : begin
          self.vx = 0.
          self.vy = 0.
          self.vz = 0.
        end
  
  endcase
  
  ; Initializing the object
  return, 1
  
End


Pro vectorclass::cleanup

  Compile_opt idl2
  
End


Function vectorclass::x

  return, self.vx
  
End



Function vectorclass::y

  return, self.vy
  
End


Function vectorclass::z

  return, self.vz
  
End


Function vectorclass::xyz

  return, [self.vx, self.vy, self.vz]
  
End


Pro vectorclass::setX, value

  self.vx = value
  ;return, 'done'
  
End



Pro vectorclass::setY, value

  self.vy = value
  ;return, 'done'
  
End


Pro vectorclass::setZ, value

  self.vz = value
  ;return, 'done'
  
End


Function vectorclass::translate, vector2

  self.vx += vector2.x()
  self.vy += vector2.y()
  self.vz += vector2.z()
  return, [self.vx, self.vy, self.vz]

End


;+
; This is an overload of the + function
; Can remplace the translate method
;-
Function vectorclass::_overloadPlus, Left, Right

  if strlowcase(obj_class(Left)) ne strlowcase(obj_class(Right)) then begin
    print, 'You try to add to differents objects types...'
    return, 0
  endif else begin
    return, vectorclass(self.xyz() + Right.xyz())
  endelse
  
End



Function vectorclass::scaleUp, vector2

  self.vx *= vector2.x()
  self.vy *= vector2.y()
  self.vz *= vector2.z()
  return, [self.vx, self.vy, self.vz]


End

;+
; This is an overload of the * function
; Can remplace the ScaleUp method
;-
Function vectorclass::_overloadAsterisk, Left, Right

  if strlowcase(obj_class(Left)) ne strlowcase(obj_class(Right)) then begin
    print, 'You try to add to differents objects types...'
    return, 0
  endif else begin
    return, vectorclass(self.xyz() * Right.xyz())
  endelse
  
End



Function vectorclass::scaleDown, vector2

  temp = 1. / vector2.xyz()
  self.vx *= temp[0]
  self.vy *= temp[1]
  self.vz *= temp[2]
  return, [self.vx, self.vy, self.vz]
  
  
End

;+
; This is an overload of the / function
; Can remplace the Scale Down method
;-
Function vectorclass::_overloadSlash, Left, Right

  if strlowcase(obj_class(Left)) ne strlowcase(obj_class(Right)) then begin
    print, 'You try to add to differents objects types...'
    return, 0
  endif else begin
    temp = 1. / Right.xyz()
    return, vectorclass(self.xyz() * temp)
  endelse
  
End


Function vectorclass::opposite

  return, [-self.vx, -self.vy, -self.vz]

End


Function vectorclass::length

  return, sqrt( (self.vx)^2 + (self.vy)^2 + (self.vz)^2 )
  
end


Function vectorclass::horizlength

  return, sqrt( (self.vx)^2 + (self.vy)^2 )
  
end


Function vectorclass::squareLength

  return, (self.vx)^2 + (self.vy)^2 + (self.vz)^2
  
end


Pro vectorclass::normalizeLength

  inv = 1. / self.length()
  self.vx *= inv
  self.vy *= inv
  self.vz *= inv
  
End


Function vectorclass::getNormLength

  inv = 1. / self.length()
  return, ( [self.vx, self.vy, self.vz] * inv )
  
End


Function vectorclass::dot, vector2

  dot = (self.vx * vector2.x()) + (self.vy * vector2.y()) + (self.vz * vector2.z())
  return, dot
  
End


Function vectorclass::getRadAngle, vector2

  return, acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vectorclass::getDegAngle, vector2

  return, 180./!PI * acos( self.dot(vector2) / ( self.length() * vector2.length() ) )
  
End


Function vectorclass::det, vector2

  detX = (self.vy * vector2.z()) - (self.vz * vector2.y())
  detY = (self.vz * vector2.x()) - (self.vx * vector2.z())
  detZ = (self.vx * vector2.y()) - (self.vy * vector2.x())
  return, [detX, detY, detZ]

End


Function vectorclass::paralVolume, vector2

  tempvec = vectorclass(self.det(vector2))
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
Function vectorclass::localCoordinateSystem

  if self.vx gt self.vy then begin
    invLen = 1. / sqrt( (self.vx)^2 + (self.vz)^2 )
    v2 = vectorclass(-self.vz * invLen, 0., self.vx * invLen)
  endif else begin
    invLen = 1. / sqrt( (self.vy)^2 + (self.vz)^2 )
    v2 = vectorclass(0., self.vz * invLen, -self.vy * invLen) 
  endelse
  
  v3 = vectorclass(self.det(v2))

  ;with this line the 3 vectors coordinates are return
  ;return, [ [self.xyz()],[v2.xyz()],[v3.xyz()] ] 
  
  ; with this line the actual vector objects are return - most likely more efficient
  return, [ v2, v3 ]
  
End


Function vectorclass::duplicateToVectorArrayClass, nDim

  if n_elements(nDim) eq 0 then begin
    print, 'A dimension value is needed...'
    return, 0
  endif else begin
    return, vectorarrayclass( replicate(self.x(),nDim),replicate(self.y(),nDim), replicate(self.z(),nDim) )
    endelse

End
