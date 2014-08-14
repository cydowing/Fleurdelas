; docformat = 'rst'
;+
; The purpose of this object is to read, manipulate at the point level LAS file.
; The object is able to handle any LAS 1.0, 1.1, 1.2, 1.3 with point format 0, 1, 2, 3, 4, 5.
; It also has the ability to write LAS file.
;       
; :Author:
;   Antoine Cottin
;       
; :Fields:
;   lasFilePath : Path to the LAS file.
;   surveyDay=survey day
;       
; :Uses:
;   lasObj=Obj_New("carbslib")
;
; :Examples:
;
;   Init the object
;     
;     lasObj = obj_new('carbslib')
;     
;   Load a LAS file into the Object:
;   
;     lasObj->loadData, day="206", flightline=5, /QUIET
;     or
;     lasObj->loadDataWithPath, "/path/to/the/las/file", /QUIET
;     
;   Getting the bounding box (coverage) of the loaded LAS file:
;   
;     boxEdge = lasObj->getHeaderProperty(/BOUNDINGBOX)
;     
;   Extract data from a geographical area (box):
;   
;      selectData = lasObj->getData(BOUNDINGBOX=geoBox)
;
;
; :History:
;
;    Create by Antoine Cottin, July 2011.
;    
;    September 2012
;     -Support of LAS 1.3 and greater
;     -Add getter and setter for Vlr and EVlr
;     
;    January 2012
;     -Implementation of carbslib::writeLAS
;      Still in beta      
;     
;    September 2013
;     -Add comments and header comments
;     
;    14 September 2013
;     -Change carbslib::getData(pointNumber=index) : 
;      using the index array as one block instead of loop on the block
;      and trying to locate the exact record.
;    
;    January 2014:
;     -carbslib::tileData :
;      Binary file structure modify, two new fields to the header
;      two new methods have been added, carbslib::readTile() and carbslib::writeTile() which
;      superseed the old method 
;     -carbslib::dump :
;      Implementation of an ASCII dump method
;     -carbslib::restoreData :
;      Restore the original data from the load method (not sure this is really useful)
;     -carbslib:radiusSelection :
;      selection of points within a circle
;     -carbslib::polygonSelection :
;      Selection of points inside a polygon
;     -carbslib::extractTrees :
;      A working prototype method that extracts trees directly from the point cloud.
;      It uses a recursive approach
;       
;    25 February 2014
;     -Improvment of version handling :
;      There was an issue in the header handling through the version in the number of point by returns    
;     -carbslib::writeLAS :
;      using the index array as one block instead of loop on the block
;      and trying to locate the exact record.
;     -Removing all data members and associate methods referencing to the old writeLAS method
;     -Integration of carbslib::readVLR
;     -Integration of carbslib::readLAS
;     -Integration of carbslib::addGeoKey - WIP
;     -Improvement of carbslib::cleanup
;        
;-
Pro carbslib__define

; Definition of the data hold by the object
void = {carbslib, $
  lasFilePath       : '',$                    ; String representing the path to the LAS file
  surveyDay         : '',$                    ; String holding the Julian Survey day - only with ARSF-NERC dataset
  tempDirPath       : '',$                    ; String holding the temporary directory path to store temp file(s) if required
  rootPath          : '',$                    ; String of the local path where the project is located - for relative path generation
  lasHeader         : ptr_new(),$             ; Pointer to the header of the LAS file
  lasDataStr        : ptr_new(),$             ; Pointer to the Point data structure
  lasDataStrSz      : 0B,$                    ; Size in bytes of the point data structure
  getDataIndex      : ptr_new(),$             ; Pointer to the index of selected data from the last getData call
  lasData           : ptr_new(),$             ; Pointer to the point data from the last getData call
  lasDataIndBackup  : ptr_new(),$             ; Pointer to an index of the original data set to retrieve original data set
  lasDataExtent     : dblarr(4),$             ; Pointer to a four element double array that contains the geographical extend of the selected data - [xMax, xMin, yMax, yMin]
  lasNTiles         : 0UL,$                   ; Number of tiles generated
  lasDataTile       : ptr_new(),$             ; Pointer to a structure that contains the tile index and the points index
  recTileNumb       : 0L,$                    ; Tile number for recurrent call of extractTrees
  lasTileExtent     : dblarr(4),$             ; Pointer to a four element double array that contains the geographical extend of the Tiles - [xMax, xMin, yMax, yMin]
  lasTileFileName   : '',$                    ; String of the Tile Index File.
  lasWaveDsptr      : ptr_new(),$             ; Pointer to the waveform packet descriptor containing :  bits per sample, wf conversion type, # of samples, temporal spacing, digitizer gain, digitizer offset
  lasWaveDsptrHdr   : ptr_new(),$             ; Pointer to the waveform packet descriptor header
  lasWaveEvlrHeader : ptr_new(),$             ; Pointer to the waveform packet header
  lasWave           : ptr_new(),$             ; Pointer to the waveform data from the last getData call
  vlrFileID         : ptr_new(),$             ; Pointer to an string array that the path to the temporary files that hold the files name 
  vlrByteSize       : ptr_new(),$             ; Pointer to an Unsigned Long array that holds the byte size of each key
  vlrId             : ptr_new(),$             ; Pointer to a byte array that holds a byte flag to describ the geokey
  vlrArr            : ptr_new(),$             ; Pointer to a pointer array that contains the key in reading order header/key
  selectArray       : ptr_new(),$             ; Binary Array that acts like a selected flag
  recursiveN        : 0ULL,$                  ; Counter flag for the tree extraction method recursion
  out               : obj_new(),$             ; Object holding the console output object
  xTile             : 300.0 ,$                ; x(meter/feets)/easting(meter/feets)/longitude(degree) size of the tile
  yTile             : 300.0  $                ; y(meter/feets)/northing(meter/feets)/latitude(degree) size of the tile
  }

  
End


;+
; Loads a LAS file using a fully qualified path. Use this method for general use of the lib.
;
; :Categories:
;   GENERAL
;   
; :Returns:
;   Nothing
;   
; :Uses:
;   Obj->loadData, sting_of_fully_qualified_path
;   
; :Examples:
;     setup the object
;       lasObj = obj_new('carbslib')
;     load a LAS file
;       lasObj->loadDataWithPath, '/path/to/the/file.LAS'
;
; :Keywords:
;   inputFile : in, required, type=string
;     fully qualified path name of a las file
;   _ref_extra : in, optional, type=string
;     flag to setup the log output. It can be set as
;       -verbose : output all the message to the current console
;       -file : output all message to a log file
;       -quiet : turn off log information
;
; :Author:
;   Antoine Cottin
;-
Pro carbslib::loadDataWithPath, inputFile=inputFile, _ref_extra=logMode

  ; Initializing console printing
  self.out = obj_new('consoleOutput', _extra = logMode)


  ; Setting up a temp directory to hold temporary informations
  ; cd into the project directory
  carbslibPath = FILE_DIRNAME(ROUTINE_FILEPATH())
  CD, carbslibPath
  CD, '../'
  spawn, 'pwd', rootPth
  self.rootPath = rootPth
  ; Create a temp file
  tempDirPath = './temp/'
  self.tempDirPath = tempDirPath
  command = 'mkdir '+ tempDirPath
  spawn, command
  
  dum = self.readLAS(inputFile, header, dataStr)
  

  self.lasFilePath = inputFile
  self.lasHeader = ptr_new(header)
  self.lasDataStr = ptr_new(dataStr)

  if header.nRecords ne 0 then begin
    
    dum = self.readVLR(inputFile, header, vlrFileArr, vlrByteSizeArr, vlrId, vlrArr, self.out)
    self.vlrFileID = ptr_new(vlrFileArr)
    self.vlrByteSize = ptr_new(vlrByteSizeArr)
    self.vlrId = ptr_new(vlrId)
    self.vlrArr = ptr_new(vlrArr)
  
  endif
  
  self.lasDataExtent = (self.getHeaderProperty(/BOUNDINGBOX))[0:3]
  self.selectArray = ptr_new(bytarr(self.getHeaderProperty(/numberOfPoints)))
  self.lasDataIndBackup = ptr_new(indgen(self.getHeaderProperty(/numberOfPoints), /UL64))
   
  if n_elements(tileSize) ne 0 then begin
    self.xTile = tileSize[0]
    self.yTile = tileSize[1]
  endif else begin
    self.Xtile = 300.
    self.Ytile = 300.
  endelse
  
  if n_elements(tileData) ne 0 then begin
    exist = self.lookingForFile(self.lasFilePath, '.tid', tileFileName)
    if exist eq 1 then begin
      self.lasTileFileName = tileFileName 
      dum = self.readTile()
    endif else begin
      ; Load data into memory
      dum = self.getData(/ALL)
      dumt = self.tileData(self.xTile, self.yTile, self.lasDataExtent)
    endelse
  endif
   
   
  ; TODO: strip the file name and fine the survey day or ask for one
  ;self.surveyDay = surveyDay   
   
  case header.pointFormat of
    0:self.lasDataStrSz = 20
    1:self.lasDataStrSz = 28
    2:self.lasDataStrSz = 26
    3:self.lasDataStrSz = 34
    4:self.lasDataStrSz = 57
    5:self.lasDataStrSz = 63
  endcase

End


;+
; Restore the original loaded data for the general index
;
; :Category:
; 	LAS
;
; :Return:
; 	1
;
;	:Uses:
;		dum = lasobj.restore()
;
;	:Example:
;		dum = lasobj.restore()
;
; :History:
; 	Development history
;
; :Author: antoine
;-
Function carbslib::restoreData

  dum = self.getData(pointNumber = (*self.lasDataIndBackup))
  return, 1
  
End



;+
; This function looks for a file based on the file name and a provided extension.
; It will returns 1 if the file exits and 0 if not.
;
; :Category:
; 	GENERAL
;
; :Return:
;   Binary value, 1 the new file with that extention exist, 0 that new file doesn't exist
;
;	:Uses:
;   dum = Obj.lookingForFile(filebasename, ext)
;
;	:Example:
;		exist = self.lookingForFile(inFile, '.tid', tileFileName)
;
; :Params:
;    basename : in, string, type = string
;     Represents the original path file name
;    ext : in, required, type = string
;     file extention '.xxx' to add at the end of the name
;    tileFileName: in, required, type=string
;     Represents the new file name
;
; :History:
;   February 2014
;     - add
;
; :Author: antoine
;-
Function carbslib::lookingForFile, basename, ext, tileFileName

  if strlowcase(!version.OS_NAME) eq "linux" or strlowcase(!version.OS_NAME) eq "mac os x" then spath='/' else spath='\'
  sep=strcompress(strmid(spath, 0, 1,/reverse_offset))
  path = file_dirname(basename)
  file = file_basename(basename)
  tileFileName = strcompress( path + spath + strmid(file, 0, strpos(file, '.', /reverse_search )) + ext)
  
  exist = file_test(tileFileName)
  
  return, exist

End



Function carbslib::readTile, outFile=outFile

  ; start time
  T = SYSTIME(1)
  
  ; If a file name is provided that look for it
  if n_elements(outFile) ne 0 then begin
    tileFileName = outFile
    exist = file_test(tileFileName)
  endif else begin
    inFile = self.lasFilePath
    exist = self.lookingForFile(inFile, '.tid', tileFileName)
  endelse
  
  if exist eq 1 then begin
  
    ; Read the file
    
    ; Tiling the data & writing the data into a file
    openr, rlun, tileFileName, /get_lun
    nRecords = 0UL
    readu, rlun, nRecords
    ;print, 'nRecords:', nRecords
    box = dblarr(4)
    readu, rlun, box
    
    ; Defining the Tile index structure
    baseTileStructure = {tileNumber:0UL, index:ptr_new(), off:ptr_new()}
    tileStructure = replicate(baseTileStructure, nRecords)
    
    nTile = 0UL
    for d=0,nRecords-1,1 do begin
    
      readu, rlun, nTile
      ;print, 'Tile index:', nTile
      tileStructure[d].tileNumber = nTile
      nElements = 0UL
      readu, rlun, nElements
      ;print, 'nElements:', nElements
      dumArray = ulonarr(nElements)
      readu, rlun, dumArray
      tileStructure[d].index = ptr_new(dumArray)
      dumArray = bytarr(nElements)
      readu, rlun, dumArray
      tileStructure[d].off = ptr_new(dumArray)
      
    endfor
    
    free_lun, rlun, /FORCE
    
    ; Putting the tile structure in the core data
    self.lasNTiles = ulong(nRecords)
    self.lasDataTile = ptr_new(tileStructure)
    self.lasTileExtent = box
    
    self.out->print,1, strcompress('Reading tile file done...')
    self.out->print,1, strcompress('Time :'+string(SYSTIME(1) - T) +' Seconds')
    
  endif else begin
    
    self.out->print, 2, strcompress('No file found, creating one...')
    self.tileData, self.xTile, self.yTile, self.lasDataExtent
    
   Return, 0
    
  endelse
  
  Return, 1
  
End




;+
; This method write a tile file
;
; :Category:
; 	LAS
;
; :Return:
; 	Writes a binary file on the disk
;
;	:Uses:
;		The call method
;
;	:Example:
;	  To write the file at the default location
;		 dum = lasobj.writeTile()
;		To write the file at a specific location
;		 dum - lasobj.writeTile(outFile = '/path/to/the/file')
;
;
; :Keywords:
;    outFile = int, optional, type=string
;       path file name
;
; :History:
; 	Development history
;
; :Author:
;   Antoine Cottin
;-
Function carbslib::writeTile, outFile=outFile

  T = SYSTIME(1)
  
  ; If a file name is provided that look for it
  if n_elements(outFile) ne 0 then begin
    tileFileName = outFile
    exist = file_test(tileFileName)
  endif else begin
    inFile = self.lasFilePath
    exist = self.lookingForFile(inFile, '.tid', tileFileName)
  endelse
  
  
  self.out->print,1, 'Writing Tile Data File...'
  self.out->print,2, 'Be aware that any previous tile file will be overwrite...'
  ;exist = self.lookingForFile(self.lasFilePath, '.tid', tileFileName)
  
  tileStructure = self.getTileIndex()
  
  ; Defining the Tile index structure
  ;baseTileStructure = {tileNumber:0, index:ptr_new(), off:ptr_new()}
  ;baseTileStructure = {tileNumber:0, index:ptr_new(), histogram:lonarr(10)}
  ;tileStructure = replicate(baseTileStructure, nX * nY)
  
  ; Tiling the data & writing the data into a file
  openw, wlun, tileFileName, /get_lun
  ; Writing the number of records - initialize
  writeu, wlun, ulong(self.lasNTiles) ;0UL
  ; Writting the geographical extent of the tiling
  writeu, wlun, double(self.lasTileExtent)
  
  for i = 0, self.lasNTiles-1, 1 do begin
    
;    writeu, wlun, ulong(tileStructure[i].tileNumber)
;    writeu, wlun, ulong(n_elements((*tileStructure[i].index)))
;    writeu, wlun, ulong( (*(tileStructure.[i].index)) )
;    writeu, wlun, byte( ((*(tileStructure[i].off))) )

    writeu, wlun, ulong( (tileStructure.tileNumber)[i] )
    writeu, wlun, ulong(n_elements( (*(tileStructure.index)[i]) ))
    writeu, wlun, ulong( (*(tileStructure.index)[i]) )
    writeu, wlun, byte( (*(tileStructure.off)[i]) )

  endfor

  free_lun, wlun, /FORCE
  
  self.out->print,1, strcompress('Writing the tiling data done...')
  self.out->print,1, strcompress('Time :'+string(SYSTIME(1) - T) +' Seconds')
  
  Return, 1
  
End




;+
; Tiles the LAS file.
; The procedure will first search for an existing Tile InDex file (.TID).
; If the file is found it is read and assign to the object.
; If the file is not found, it will create it and assign it to the object.
; For now the tile size (100 m x 100 m is hard coded but is meant to be change in the future.
; To get the tile index, use the method carbslib::getTileIndex().
;
; :Categories:
;   GENERAL
;   
; :Returns:
;   Nothing
;
; :Uses:
;   Obj->tileData
;
; :Examples:
;   For Example::
;
;     setup the object
;       lasObj = obj_new('carbslib')
;
;     load the 5th flightline of survey from December 5th 2003
;       lasObj->tileData
;       
;  :History:
;   February 2014
;     - add an index field in the tile structure
;     - add bounding box option
;
;-
;+
; :Description:
;    Describe the procedure.
;
; :Category:
; 	What is the general purpose of this method
;
; :Return:
; 	If any, what is the output of this method
;
;	:Uses:
;		The call method
;
;	:Example:
;		A quick example on how to use this method
;
; :Params:
;    xTile
;    yTile
;    box
;
;
;
; :History:
; 	Development history
;
; :Author: antoine
;-
Pro carbslib::tileData, xTile, yTile, box, outFile = outFile

; start time
T = SYSTIME(1)

; If a file name is provided that look for it
if n_elements(outFile) ne 0 then begin
  tileFileName = outFile
  exist = file_test(tileFileName)
endif else begin
  inFile = self.lasFilePath
  exist = self.lookingForFile(inFile, '.tid', tileFileName)
endelse



if exist eq 1 then begin
  
  ; Read the file
  dum = self.readTile()
  
;+
;  ; Tiling the data & writing the data into a file
;  openr, rlun, tileFileName, /get_lun
;  nRecords = 0UL
;  readu, rlun, nRecords
;  ;print, 'nRecords:', nRecords
;  box = dblarr(4)
;  readu, rlun, box
;  
;  ; Defining the Tile index structure
;  baseTileStructure = {tileNumber:0UL, index:ptr_new(), off:ptr_new()}
;  tileStructure = replicate(baseTileStructure, nRecords)
;  
;  nTile = 0UL
;  for d=0,nRecords-1,1 do begin
;    
;    readu, rlun, nTile
;    ;print, 'Tile index:', nTile
;    tileStructure[d].tileNumber = nTile
;    nElements = 0UL
;    readu, rlun, nElements
;    ;print, 'nElements:', nElements
;    dumArray = ulonarr(nElements)
;    readu, rlun, dumArray
;    tileStructure[d].index = ptr_new(dumArray)
;    dumArray = bytarr(nElements)
;    readu, rlun, dumArray
;    tileStructure[d].off = ptr_new(dumArray)
;    
;  endfor
;  
;  free_lun, rlun
;  
;  ; Putting the tile structure in the core data
;  self.lasNTiles = ulong(nRecords)
;  self.lasDataTile = ptr_new(tileStructure)
;  self.lasTileExtent = box
;-

  self.out->print,1, strcompress('Reading tile file done...')
  self.out->print,1, strcompress('Time :'+string(SYSTIME(1) - T) +' Seconds')

endif else begin
  
;  dum = self.writeTileData()
;+
  ; Create the file
  ; If no tile size is provided then use the one store into the object
  if n_elements(xTile) eq 0 then tileSizeX = self.xTile else tileSizeX = xTile & self.xTile = xTile
  if n_elements(yTile) eq 0 then tileSizeY = self.yTile else tileSizeY = YTile & self.yTile = yTile

  self.out->print,1, strcompress('Starting Tile the data...')
  
  if ptr_valid(self.lasData) then begin
  
    ; Getting the bounding box of the LAS file - added the box as parameter to cut the data
    if n_elements(box) eq 0 then box = self->getHeaderProperty(/boundingBox)
    
    deltaX = ( box[0] - box[1] )
    deltaY = ( box[2] - box[3] )
    
    nX = ceil( deltaX / tileSizeX )
    nY = ceil( deltaY / tileSizeY )
    
    ; if the data cover is smaller than the tile size, set it up to 1
    if nX eq 0 then nX = 1
    if nY eq 0 then nY = 1
    
    roundUpX = deltaX / nX
    roundUpY = deltaY / nY
    
    newXBox = ( ( indgen(nX+1) * roundUpX ) ) + box[1]
    newYBox = ( ( indgen(nY+1) * roundUpY ) ) + box[3]
    
    ; Defining the Tile index structure
    baseTileStructure = {tileNumber:0, index:ptr_new(), off:ptr_new()}
    ;baseTileStructure = {tileNumber:0, index:ptr_new(), histogram:lonarr(10)}
    tileStructure = replicate(baseTileStructure, nX * nY)
    
  ;  ; Tiling the data & writing the data into a file
  ;  openw, wlun, tileFileName, /get_lun
  ;  ; Writing the number of records - initialize
  ;  writeu, wlun, 0UL
  ;  ; Writting the geographical extent of the tiling
  ;  writeu, wlun, double(box)
    
    loop = 0L
    tileWithData = 0UL
    for i=0,nY+1-2,1 do begin
      for j=0,nX+1-2,1 do begin
      
      print, [newXBox[j+1], newXBox[j], newYBox[i+1], newYBox[i]]
      dumData = self->getData(boundingBox = [newXBox[j+1], newXBox[j], newYBox[i+1], newYBox[i]])
      selectedDataIndex = self->getSelectedDataIndex()
      tileStructure[loop].tileNumber = loop
      tileStructure[loop].index = ptr_new(selectedDataIndex) 
      tileStructure[loop].off = ptr_new(bytarr(n_elements(selectedDataIndex)))
       
  ;    if ptr_valid(tileStructure[loop].index) then begin
  ;    writeu, wlun, ulong(tileStructure[loop].tileNumber)
  ;    writeu, wlun, ulong(n_elements(selectedDataIndex))
  ;    writeu, wlun, ulong(selectedDataIndex)
  ;    writeu, wlun, bytarr(n_elements(selectedDataIndex))
  ;    tileWithData += 1UL
  ;    
  ;    endif
          
      loop = loop + 1
      
      endfor
    endfor
    
  ;  ; Writing the tile index file
  ;  point_lun, wlun, 0
  ;  writeu, wlun, ulong(tileWithData)
  ;  free_lun, wlun
    
    ; Putting the tile structure in the core data
    self.lasNTiles = ulong(nX * nY)
    self.lasDataTile = ptr_new(tileStructure)
    self.lasTileFileName = tileFileName
  
    
    self.out->print,1, strcompress('Tiling done...')
    self.out->print,1, strcompress('Time :'+string(SYSTIME(1) - T) +' Seconds')
    
    dum = self.writeTileData()
    
    endif else begin
    
    self.out->print,2, "No data load !"
    
  endelse

  
endelse

End



;+
; Display information on object's methods.
;
; :Categories:
;   GENERAL
;   
; :Returns:
;   Display procedures, functions and instance data of the object. 
;
; :Uses:
;   Obj->help
;
; :Examples:
;   For Example::
;
;     setup the object
;       lasObj = obj_new('carbslib')
;
;     Call for help on the object
;       lasObj->help
;
;-
Pro carbslib::help

  help, self, /objects
  return

End



;+
; Cleanup the object. This method is call automatically using the obj_destroy method.
;
; :Categories:
;   GENERAL
;   
; :Returns:
;   Nothing
;
; :Uses:
;   obj_destroy, Obj
;
; :Examples:
;     setup the object
;       lasObj = obj_new('carbslib')
;
;     Destroying the object
;       obj_destroy, lasObj
;       
;-
Pro carbslib::cleanup

  Compile_opt idl2
   
  ; Removing the temporary files
  self.out->print,1 , 'Destroying carbslib object...'
  self.out->print,1 , 'Removing temporary files...'
  CD, '~/IDLWorkspace83/Saints/'
  ; Removing a temp file
  command = 'rm -r '+ self.tempDirPath
  spawn, command
  
  
  ; Freeing all data member pointers
  self.out->print,1 , 'Cleaning memory...'
  ptr_free, $
  self.lasHeader,$
  self.lasDataStr,$
  self.getDataIndex,$
  self.lasData,$  
  self.lasDataIndBackup,$
  self.lasDataTile,$
  self.lasWaveDsptrHdr,$
  self.lasWaveEvlrHeader,$
  self.lasWave,$
  self.vlrFileID,$
  self.vlrByteSize,$
  self.vlrId,$
  self.vlrArr,$
  self.selectArray
  
  ; Destroying the consoleOutput object
  self.out->print,1 , 'Destroying remaining objects...'
  self.out->print,1 , 'Bye :)'
  obj_destroy, self.out
  
End



;+
; :Hidden:
;
;-
Pro carbslib::testArg, boundingBox=b, pointNumber=v, all=all
 
if arg_present(boundingBox) then print, "boundingBox argument present"
if n_elements(b) ne 0 then print, b 
if n_elements(v) ne 0 then print, v 
if keyword_set(all) then print, "Keyword /all sets"

;Pro xyz, a, b, test = t, value = v
;
;if n_elements(t) ne 0 then print, t
;if n_elements(v) ne 0 then print, v
;
;if arg_present(a) then print, "Arg a present"
;if arg_present(b) then print, "Arg b present"
;
;
;End

End



;+
; This function build the folder path for a specific type of data specify as argument.
;
; :Categories:
;   ARSF-NERC
;
; :Uses:
;   Obj->projectPathBuilder(_ref_extra=pathType,day=surveyDay)
;
; :Examples:
;   For examples::
;
;       To create the path and list files of all the LAS 1.3 files that have been collected on day 208
;
;         Result=Obj->projectPathBuilder(/LIDAR_LAS13,DAY='208')
;
;
; :Keywords:
;   day : in, required, type=string 
;
; 
;   _ref_extra : in, required, type=string
;     /DEM_BNG, /DEM_WGS84, /LIDAR_ASCII, /LIDAR_LAS10, /LIDAR_LAS13, /NAVIGATION, /CAMERA_LOG, /CAMERA_PHOTO, /CAMERA_THUMB
;
;
; :Returns:
;
;       Return an strarr[fileNamePath,fileNameSearch] where:
;
;         fileNamePath: is the absolute path of the selected data type
;         
;         fileNameSearch: is the root name of the selected data type
;
;-
Function carbslib::projectPathBuilder,_ref_extra=pathType,day=surveyDay

  Compile_opt idl2
  
   
  ; setting the root name of the file directory
  if strlowcase(!version.os) eq 'linux' then begin
    root='/mnt/urban-bess/Working_data'
    osPathSep=path_sep()
    
  endif else begin
    root='Z:\Working_data'
    osPathSep=path_sep()
    
  endelse
  
  ; TODO: root and the below variables should be store in a file for easy interpolarity embedded this information into the XML file
  project='RG12_10'
  project_year='2012'
  projectNameSep='-'
  processDate='201[2,3]????'
  
  
  if (n_elements(pathType) eq 0) then begin
    print, 'An argument is required...'
    print, 'Ending process...'
    ;return,0
  endif else begin
  
    case 1 of
    
      strlowcase(pathType) eq "dem_bng": begin
        searchPath=['lidar','dem']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]
        ;print,fileNamePath
        fileNameSearch='*ASTER-bng.dem'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "dem_wgs84": begin
        searchPath=['lidar','dem']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]
        ;print,fileNamePath
        fileNameSearch='*ASTER-wgs84_latlong.dem'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "lidar_ascii": begin
        searchPath=['lidar','flightlines','ascii']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]+osPathSep+searchPath[2]
        ;print,fileNamePath
        fileNameSearch='*.txt'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "lidar_las10": begin
        searchPath=['lidar','flightlines','las1.0']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]+osPathSep+searchPath[2]
        ;print,fileNamePath
        fileNameSearch='*.LAS'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "lidar_las13": begin
        searchPath=['lidar','flightlines','las1.3']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]+osPathSep+searchPath[2]
        ;print,fileNamePath
        fileNameSearch='*.LAS'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "navigation": begin
        searchPath=['lidar','navigation']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]
        ;print,fileNamePath
        fileNameSearch='*.sol'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "sup": begin
        searchPath=['lidar','navigation']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]
        ;print,fileNamePath
        fileNameSearch='*.sup'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "camera_log": begin
        searchPath=['camera','eventfile']
        fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
          projectNameSep+searchPath[0]+osPathSep+searchPath[1]
        ;print,fileNamePath
        fileNameSearch='*.csv'
        result=[fileNamePath,fileNameSearch]
      end
      
      strlowcase(pathType) eq "camera_photo":begin
      searchPath=['camera','photographs']
      fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
        projectNameSep+searchPath[0]+osPathSep+searchPath[1]
      ;print,fileNamePath
      fileNameSearch='*.tif'
      result=[fileNamePath,fileNameSearch]
    end
    
    strlowcase(pathType) eq "camera_thumb":begin
    searchPath=['camera','thumbnails']
    fileNamePath=root+osPathSep+project+projectNameSep+surveyDay+$
      projectNameSep+searchPath[0]+osPathSep+searchPath[1]
    ;print,fileNamePath
    fileNameSearch='*.jpg'
    result=[fileNamePath,fileNameSearch]
  end
  
  ;hyperspectral - TBD
  
  ELSE: begin
    print,'Nothing to return...'
    result=['TBD','TBD']
  end
  
endcase


endelse

return, result


End


;+
; This procedure is obsolete
;
; :Returns:
;   Nothing
;
; :Hidden:
;-
Pro carbslib::loadWave

; Check if the waveforms are inside the LAS file if not find the wave file and open it
 

; Make sure that all packet size are the same size using stdev
; Make sure that all the waveform are consecutive
; point lun to first record and readu :: ((*(self.lasData)).offsetWaveData)

openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

; Checking if all the packet have the same size
; A consecutive order equal to the point one is assumed if all the offsetWaveData fields are the same
;checkSize = (((*(self.lasData)).wpacketsize)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).wpacketsize)[*(self.getDataIndex)], -1))
;if total(checkSize) eq 0.0 then packetSize = ((*(self.lasData)).wpacketsize)[0]
;checkStart = (((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)], -1))
;if total(checkStart) eq 0.0 then waveStart = ((*(self.lasData)).offsetWaveData)[0]

packetSize = ((*(self.lasData)).wpacketsize)[0]
waveStart = ((*(self.lasData)).offsetWaveData)[0]

tempWaveStr = {waveStruc,wave:bytarr(256)}
tempWave = replicate(tempWaveStr,n_elements(*(self.getDataIndex)))
point_lun, inputLun, ((*(self.lasHeader)).startWaveform + waveStart + (long((*(self.getDataIndex))[0]) * long(packetSize)))
readu, inputLun, tempWave



free_lun, inputLun

self.lasWave = ptr_new(tempWave)


End



;+
; Init the carbslib Object.
;
; :Categories:
;   GENERAL
;   
; :Returns:
;   an object
;
; :Uses:
;   Obj = obj_new('carbslib')
;
; :Examples:
;   For Example::
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;-
Function carbslib::init

  Compile_opt idl2
  
;  self.out = obj_new('consoleOutput')
  ; Initializing the object
  return, 1

End



;+
; This function selects specific field(s) from the LAS point structure.
; It is call internally by the GET methods that retreive points.
; 
; THIS FUNCTION IS TEMPORARY DISABLE AS IT PRODUCES ERRORS.
; 
; :Categories:
;   GENERAL
;   
; :Returns:
;   An array of structure compose of the selected field(s).
;
; :Uses:
;   Result=Obj->lasPointFieldSelector(tempData, _ref_extra=ex, out)
;
; :Examples:
;   For Example::
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;       
;     Load a LAS file:
;       lasObj->loadDataWithPath, '/path/to/the/file.LAS'
;       
;     Get all the data and return only the coordinate fields (X, Y, Z):
;       Data = lasObj->getData(/all, ['easting', 'northing', 'elevation'])
;     
; :Params:
;   tempData : in, required, type=array of structure
;     This is an array of structure that represent sub-set of point(s) return by a GET method.
;   out : in, required, type=obj
;     The console output object required to print out the log message.
; 
; :Keywords:
;   _ref_extra : in, optional, type=strarr
;     A string array that describ the field(s) that need to be return. 
;     If not string array is present, then all the fields are return.
;   
;-
Function carbslib::lasPointFieldSelector, tempData, _ref_extra=ex, out

;if n_elements(ex) eq 0 then keyList = strlowcase(tag_names(tempData)) else keyList = strlowcase(ex)
;tagList = strlowcase(tag_names(tempData))
;nkeyList = n_elements(keyList)
;
;;/allField
;
;flag = 0
;
;for o=0,nKeyList-1,1 do begin
;
;  validCheck = where(keyList[o] eq tagList, validCount)
;  
;    if validCount ne 0 then begin
;    
;      if flag eq 0 then begin
;        
;        data = create_struct(tagList[validCheck],tempData.(validCheck))
;        flag = 1
;        
;      endif else begin
;      
;        data = create_struct(data, tagList[validCheck],tempData.(validCheck))
;        
;      endelse
;      
;    endif else begin
;    
;      self.out->print,2, strcompress("Keyword " + string(keyList[o]) + " not found into point structure tags name...")
;      self.out->print,2, strcompress("Ignoring " + string(keyList[o]) + " keyword...")
;      ;return, 0
;            
;    endelse
;    
;endfor

data = tempData

out->print,1, "Return Point Structure description:"
out->printArray,1, tag_names(data)
return, data

        
End



;+
; This function returns the number of flightlines of a specific survey day
; 
; :Categories:
;   ARSF-NERC, GET
;
; :Returns:
;   An integer equals to the number of flightlines.
;
; :Uses:
;   Result=Obj->getNumberOfFlightline(day=surveyDay)
;
; :Examples:
;   For Example::
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;       
;     Get the number of flightline(s) for the flight on December 5th 2003: 
;       Result=Obj->getNumberOfFlightline(day=338)
;
; :Keywords:
;   day : in, required, type=string
;     Julian day of the survey
;
;-
Function carbslib::getNumberOfFlightline, day=surveyDay

  Compile_opt idl2
  
  tmp=self->projectpathbuilder(/lidar_las13,day=surveyDay)
  inputFile=file_search(tmp[0],tmp[1])
  nLines = n_elements(inputFile)
  inputFile = 0
  return, nLines
  
End



;+
; This function returns the name of the current LAS file loaded in the object.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A `string` of a fully qualify file path.
;
; :Uses:
;   Result=Obj->getLoadFileName()
;
; :Examples:
;   For Example::
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003:
;       lasObj->loadData, day = '338', flightline = 5
;       
;     Get the path and name of the loaded file:
;       Result = lasObj->getLoadFileName()
;
;-
Function carbslib::getLoadFileName

  return, self.lasFilePath

End



;+
; This function returns the number of flightlines of a specific survey day
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   An integer equals to the number of flightlines.
;
; :Uses:
;   Result=Obj->getNumberOfFlightline(day=surveyDay)
;
; :Examples:
;   
;     Define the bounding coordinate for the selection
;     geoBox = [488401.968647,487901.968647,235421.265389,234921.265386]
;     
;     Initialize the object
;     lasObj = obj_new('carbslib')
;     
;     Load the 5th flightline from December 5th 2003 data 
;     lasObj->loadData, day='338', flightline=5, /QUIET
;     
;     Select points that lies inside the bounding box.
;     Result = lasObj->getData(boundingBox=geoBox)
;
; :Keywords:
;   boundingBox : in, optional, type=dblarr(6)
;     Geographical limit to filter the data. It can be Easting and/or Northing and/or Elevation values.
;     The array need to be of one of the following format:
;       [xMax, xMin, yMax, yMin, zMax, zMin] or,
;       [zMax, zMin] or,
;       [zMax] or,
;       [zMin]
;   pointNumber : in, optional, type=long
;     PointNumber can be either one point index (long) and range of continuous points [pointMinIndex, pointMaxIndex],
;     or a collection of discrete points lonarr(n).
;   max : in, optional, type=boolean
;     Set the boundingBox value as the maximum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   min :
;     Set the boundingBox value as the minimum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   all : in, optional, type=boolean
;     If present, will return all the points of the LAS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-  
Function carbslib::getData, boundingBox=b, max=max, min=min, pointNumber=v, all=all, _ref_extra=ex

; start time
T = SYSTIME(1)

   openr, getDataLun, self.lasFilePath, /get_lun, /swap_if_big_endian
    
    if n_elements(b) ne 0 then begin
    
    self.out->print,1, "Reading the data into memory..."
    
    ; Retriving the data packet
    ;openr, getDataLun, self.lasFilePath, /get_lun, /swap_if_big_endian
    tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
    point_lun, getDataLun, (*(self.lasHeader)).dataOffset
    readu, getDataLun, tempDataStr
    ;close, getDataLun
    tempData = fltarr((*(self.lasHeader)).nPoints,3)
    tempData[*,0] = (tempDataStr.east * (*(self.lasHeader)).xscale) + (*(self.lasHeader)).xoffset
    tempData[*,1] = (tempDataStr.north * (*(self.lasHeader)).yscale) + (*(self.lasHeader)).yoffset
    tempData[*,2] = (tempDataStr.elev * (*(self.lasHeader)).zscale) + (*(self.lasHeader)).zoffset
    
       ; checking the that boundingBox is correct and determine which bounding box type is it: geographic or elevation
       if (n_elements(b) eq 4) or (n_elements(b) eq 2) or (n_elements(b) eq 1) then begin
    
        case n_elements(b) of
          4:begin
          
            self.out->print,1,"Filtering data by coordinates..."           
            
            ; setting object data extent
            self.lasDataExtent = b
            
            index = where((tempData[*,0] le b[0]) and (tempData[*,0] ge b[1]) and $
                          (tempData[*,1] le b[2]) and (tempData[*,1] ge b[3]), indexCount)
            
            if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
            
            if (size(data))[2] ne 8 then $
              self.out->print,2, "Nothing return !" else $
              self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
    
              self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')    
            
            end
          
          2:begin
          
            self.out->print,1, "Filtering data by heights..."
            
            index = where((tempData[*,2] le b[0]) and (tempData[*,2] ge b[1]), indexCount)
            
            if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
            
            if (size(data))[2] ne 8 then $
              self.out->print,2, "Nothing return !" else $
              self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
              
            self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds') 
          
          end
          
          1:Begin
          
            if keyword_set(max) then begin
            
              index = where((tempData[*,2] le b[0]), indexCount)
            
              if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
              
              if (size(data))[2] ne 8 then $
              self.out->print,2, "Nothing return !" else $
              self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
      
              self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
              
            endif else begin
            
            if keyword_set(min) then begin
            
              index = where((tempData[*,2] ge b[0]), indexCount)
            
              if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
              
              if (size(data))[2] ne 8 then $
              self.out->print,2, "Nothing return !" else $
              self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
      
              self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
              
              endif
              
             endelse
          
          end
    
        endcase 
        
        endif else begin
          free_lun, getDataLun, /FORCE
          return, 0
        endelse
        
        tempData = 0
        
    endif;

    ; pointNumber parameter set -> returning one point selected by it number
    if n_elements(v) eq 1 then begin

    tempData = assoc(getDataLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset , /packed)
    data = self->lasPointFieldSelector((tempData[v]), _ref_extra=ex, self.out)
    index = v
    
    ;self->loadWave
    
    self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
    
    endif
    
    ; pointNumber parameter set to interval [lowerLimit,upperLimit] -> returning the points within the interval
    if n_elements(v) eq 2 then begin
    
    numbElements = v[1] - v[0]
      
      index = lindgen(numbElements) + v[0]
      
      tempdum = self->getData(/all)
      dum = tempdum[index]
      ;print, dum
      data = self->lasPointFieldSelector(dum, _ref_extra=ex, self.out)
      
      self.out->print,1, strcompress("Number of point record(s) returned: "+string(numbElements))
      self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
      
    endif
    
    ; pointNumber parameter set to an index array [index_array] -> returning the points corresponding to the index
    if n_elements(v) gt 2 then begin
    
    numbElements = n_elements(v)

      ; Much more efficient way to retreive points via INDEX array
      ;A = ASSOC(getDataLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset) 
      tempdum = self->getData(/all)
      dum = tempdum[v]
      ;print, dum
      
      ;print, 'n elements: ', n_elements(dum)
      data = self->lasPointFieldSelector(dum, _ref_extra=ex, self.out)
      
      index = v
      
      self.out->print,1, strcompress("Number of point record(s) returned: "+string(numbElements))
      self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
      
    endif
      
        
  
    ; keyword /all set -> returning all the points of the LAS file
    if keyword_set(all) then begin
  
      self.out->print,1,"Formating data..."

      ; Retriving the data packet
      ;openr, getDataLun, self.lasFilePath, /get_lun, /swap_if_big_endian
      tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
      point_lun, getDataLun, (*(self.lasHeader)).dataOffset
      readu, getDataLun, tempDataStr
    
      ;data = self->lasPointFieldSelector(tempDataStr, _ref_extra=ex, self.out)
      data=tempDataStr
      
      index = lindgen((*(self.lasHeader)).nPoints)

      if (size(data))[2] ne 8 then $
              self.out->print,2,"Nothing return !" else $
              self.out->print,1,strcompress("Number of point record(s) returned: " + string((*(self.lasHeader)).nPoints))

      self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')  

    endif
    

; Updating the selection index table and the data structure array into object
self.lasData = ptr_new(data)
;print,index
self.getDataIndex = ptr_new(index)

free_lun, getDataLun, EXIT_STATUS=exVal, /FORCE
return, data


End




;+
; This function returns the number of flightlines of a specific survey day
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   An integer equals to the number of flightlines.
;
; :Uses:
;   Result=Obj->getNumberOfFlightline(day=surveyDay)
;
; :Examples:
;
;     Define the bounding coordinate for the selection
;     geoBox = [488401.968647,487901.968647,235421.265389,234921.265386]
;
;     Initialize the object
;     lasObj = obj_new('carbslib')
;
;     Load the 5th flightline from December 5th 2003 data
;     lasObj->loadData, day='338', flightline=5, /QUIET
;
;     Select points that lies inside the bounding box.
;     Result = lasObj->getData(boundingBox=geoBox)
;
; :Keywords:
;   boundingBox : in, optional, type=dblarr(6)
;     Geographical limit to filter the data. It can be Easting and/or Northing and/or Elevation values.
;     The array need to be of one of the following format:
;       [xMax, xMin, yMax, yMin, zMax, zMin] or,
;       [zMax, zMin] or,
;       [zMax] or,
;       [zMin]
;   pointNumber : in, optional, type=long
;     PointNumber can be either one point index (long) and range of continuous points [pointMinIndex, pointMaxIndex],
;     or a collection of discrete points lonarr(n).
;   max : in, optional, type=boolean
;     Set the boundingBox value as the maximum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   min :
;     Set the boundingBox value as the minimum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   all : in, optional, type=boolean
;     If present, will return all the points of the LAS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-
Function carbslib::getDataFromSelectedData, boundingBox=b, max=max, min=min, pointNumber=v, all=all, _ref_extra=ex

  ; start time
  T = SYSTIME(1)
  
  openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  if n_elements(b) ne 0 then begin
  
    self.out->print,1, "Reading the data into memory..."
    
    ; Retriving the data packet
    ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
    tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
    point_lun, inputLun, (*(self.lasHeader)).dataOffset
    readu, inputLun, tempDataStr
    ;close, inputLun
    tempData = fltarr((*(self.lasHeader)).nPoints,3)
    tempData[*,0] = (tempDataStr.east * (*(self.lasHeader)).xscale) + (*(self.lasHeader)).xoffset
    tempData[*,1] = (tempDataStr.north * (*(self.lasHeader)).yscale) + (*(self.lasHeader)).yoffset
    tempData[*,2] = (tempDataStr.elev * (*(self.lasHeader)).zscale) + (*(self.lasHeader)).zoffset
    
    ; checking the that boundingBox is correct and determine which bounding box type is it: geographic or elevation
    if (n_elements(b) eq 4) or (n_elements(b) eq 2) or (n_elements(b) eq 1) then begin
    
      case n_elements(b) of
        4:begin
        
        self.out->print,1,"Filtering data by coordinates..."
        
        ; setting object data extent
        self.lasDataExtent = b
        
        index = where((tempData[*,0] le b[0]) and (tempData[*,0] ge b[1]) and $
          (tempData[*,1] le b[2]) and (tempData[*,1] ge b[3]), indexCount)
          
        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
        
        if (size(data))[2] ne 8 then $
          self.out->print,2, "Nothing return !" else $
          self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
          
        self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
        
      end
      
      2:begin
      
      self.out->print,1, "Filtering data by heights..."
      
      index = where((tempData[*,2] le b[0]) and (tempData[*,2] ge b[1]), indexCount)
      
      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
      
      if (size(data))[2] ne 8 then $
        self.out->print,2, "Nothing return !" else $
        self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
        
      self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
      
    end
    
    1:Begin
    
    if keyword_set(max) then begin
    
      ;print, "Not scale and translate value:", ((b[0] - (*(self.lasHeader)).zoffset) / (*(self.lasHeader)).zscale)
      
      index = where((tempData[*,2] le b[0]), indexCount)
      
      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
      
      if (size(data))[2] ne 8 then $
        self.out->print,2, "Nothing return !" else $
        self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
        
      self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
      
    endif else begin
    
      if keyword_set(min) then begin
      
        index = where((tempData[*,2] ge b[0]), indexCount)
        
        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
        
        if (size(data))[2] ne 8 then $
          self.out->print,2, "Nothing return !" else $
          self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
          
        self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
        
      endif
      
    endelse
    
  end
  
endcase

endif else return, 0

tempData = 0

endif;

; pointNumber parameter set -> returning one point selected by it number
if n_elements(v) eq 1 then begin

  tempData = assoc(inputLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset , /packed)
  data = self->lasPointFieldSelector((tempData[v]), _ref_extra=ex, self.out)
  index = v

  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif

; pointNumber parameter set to interval [lowerLimit,upperLimit] -> returning the points within the interval
if n_elements(v) eq 2 then begin

  numbElements = v[1] - v[0]
  
  index = lindgen(numbElements) + v[0]
  
  tempdum = self->getData(/all)
  dum = tempdum[index]
  ;print, dum
  data = self->lasPointFieldSelector(dum, _ref_extra=ex, self.out)
  
  self.out->print,1, strcompress("Number of point record(s) returned: "+string(numbElements))
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif

; pointNumber parameter set to an index array [index_array] -> returning the points corresponding to the index
if n_elements(v) gt 2 then begin

  numbElements = n_elements(v)
  
  ; Much more efficient way to retreive points via INDEX array
  ;A = ASSOC(inputLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset)
  tempdum = self->getData(/all)
  dum = tempdum[v]
  ;print, dum
  
  ;print, 'n elements: ', n_elements(dum)
  data = self->lasPointFieldSelector(dum, _ref_extra=ex, self.out)
  
  index = v
  
  self.out->print,1, strcompress("Number of point record(s) returned: "+string(numbElements))
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif



; keyword /all set -> returning all the points of the LAS file
if keyword_set(all) then begin

  self.out->print,1,"Formating data..."
  
  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
  point_lun, inputLun, (*(self.lasHeader)).dataOffset
  readu, inputLun, tempDataStr
  
  ;data = self->lasPointFieldSelector(tempDataStr, _ref_extra=ex, self.out)
  data=tempDataStr
  
  index = lindgen((*(self.lasHeader)).nPoints)
  
  if (size(data))[2] ne 8 then $
    self.out->print,2,"Nothing return !" else $
    self.out->print,1,strcompress("Number of point record(s) returned: " + string((*(self.lasHeader)).nPoints))
    
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif


; Updating the selection index table and the data structure array into object
self.lasData = ptr_new(data)
;print,index
self.getDataIndex = ptr_new(index)

; Returning requested data
free_lun, inputLun
return, data


End



;+
; This function returns the start and end time of a flightline. Time usually express in GPS time.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   a `dblarr[2]` containing the [startTime, endTime].
;
; :Uses:
;   Result=Obj->getDataTime()
;
; :Examples:
;   For Example::
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;       lasObj->loadData, inputFile = /Path/to/las/file
;       
;     Get the start & end time of the flightline:
;       Result = lasObj->getDataTime()
;
;-
Function carbslib::getDataTime

    minTime=min(((*self.lasData).time), max=maxTime)
    return, [minTime, maxTime]
    
End



;+
; This function returns a point selection base on class index from the points from a LAS file.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getDataFromClass(CLASS=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to Ground:
;       Result = lasObj->getDataFromClass(CLASS=2)
;       
; :Keywords:
;   class : in, required, type=integer
;     Class index as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;     
;-
Function carbslib::getDataFromClass, class=cl, outputId=outputId

; start time
T = SYSTIME(1)

openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

self.out->print,1, "Reading the data into memory..."

; Retriving the data packet
;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
point_lun, inputLun, (*(self.lasHeader)).dataOffset
readu, inputLun, tempDataStr

self.out->print,1,"Filtering data by classification..."

stringClass = ClassificationID(cl)

self.out->print,1,"Filtering " + strcompress( stringClass ) + "..."

cache = 16b + 8b + 4b + 2b + 1b
myClassification = tempDataStr.class
result = cache and myClassification

index = where( result eq cl, indexCount )
  
if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0

if (size(data))[2] ne 8 then $
  self.out->print,2, "Nothing return !" else $
  self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
  
self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')

; Returning requested data
free_lun, inputLun
if keyword_set(outputid) then  begin
  self.out->print,1, "The returned records are point's index..."
  return, index
endif else begin
  self.out->print,1, "The returned records are point's data..."
  return, data
endelse


    
End



;+
; This function returns a point selection base on class index from the points loaded in memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getSelectedDataByClass(CLASS=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to Ground:
;       Result = lasObj->getSelectedDataByClass(CLASS=2)
;       
; :Keywords:
;   class : in, required, type=integer
;     Class index as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;     
;-
Function carbslib::getSelectedDataByClass, class=cl, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data from memory..."
  
  if size((*(self.lasData)),/dimensions) ne 0 then begin
  
  tempDataStr = (*(self.lasData))
  
  self.out->print,1,"Filtering data by classification..."
  
  stringClass = ClassificationID(cl)
  
  self.out->print,1,"Filtering " + strcompress( stringClass ) + "..."
  
  ; Extracting the following bits
  ; '00011111'
  cache = 16b + 8b + 4b + 2b + 1b
  myClassification = tempDataStr.class
  result = cache and myClassification
  
  index = where( result eq cl, indexCount , /NULL)
  
  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
  
  if (size(data))[2] ne 8 then $
    self.out->print,2, "Nothing return !" else $
    self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
    
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
  ; Returning requested data
  ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
  ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
  ; require and an update of the store data would happen forbidden another call of this function on the same data set 
  ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).

  if keyword_set(outputid) then  begin
    self.out->print,1, "The returned records are point's index..."
    return, index
  endif else begin
    self.out->print,1, "The returned records are point's data..."
    return, data
  endelse
  
  endif else return, 0
  
  
  
End



;+
; This function returns a point selection base on the RETURN NUMBER from the points loaded in memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getSelectedDataByReturnNumber(RETURN_NUMBER=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to second return:
;       Result = lasObj->getSelectedDataByReturnNumber(RETURN_NUMBER=2)
;
; :Keywords:
;   return_number : in, required, type=integer
;     return number as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function carbslib::getSelectedDataByReturnNumber, return_number=rtnb, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data from memory..."
  
  if size((*(self.lasData)),/dimensions) ne 0 then begin
  
    tempDataStr = (*(self.lasData))
    
    self.out->print,1,"Filtering data by return number..."
    
    self.out->print,1,"Filtering number " + strcompress( rtnb ) + "..."
    
    ; Extracting the following bits
    ; '00000111'
    cache = 4b + 2b + 1b
    tempReturnNumber = tempDataStr.nReturn
    result = cache and tempReturnNumber
    
    index = where( result eq rtnb, indexCount )
    
    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
    
    if (size(data))[2] ne 8 then $
      self.out->print,2, "Nothing return !" else $
      self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
      
    self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
    
    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).
    
    if keyword_set(outputid) then  begin
      self.out->print,1, "The returned records are point's index..."
      return, index
    endif else begin
      self.out->print,1, "The returned records are point's data..."
      return, data
    endelse
    
  endif else return, 0
  
  
  
End



;+
; This function returns a point selection base on the RETURN NUMBER from the points in a LAS file.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getDataFromReturnNumber(RETURN_NUMBER=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to second return:
;       Result = lasObj->getDataFromReturnNumber(RETURN_NUMBER=2)
;
; :Keywords:
;   return_number : in, required, type=integer
;     return number as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function carbslib::getDataFromReturnNumber, return_number=rtnb, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data into memory..."
  
  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
  point_lun, inputLun, (*(self.lasHeader)).dataOffset
  readu, inputLun, tempDataStr
  
  self.out->print,1,"Filtering data by return number..."
    
  self.out->print,1,"Filtering number " + strcompress( rtnb ) + "..."
  
  ; Extracting bits '00000111'
  cache = 4b + 2b + 1b
  tempReturnNumber = tempDataStr.nReturn
  result = cache and tempReturnNumber
    
  index = where( result eq rtnb, indexCount )
  
  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
  
  if (size(data))[2] ne 8 then $
    self.out->print,2, "Nothing return !" else $
    self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
    
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
  ; Returning requested data
  free_lun, inputLun
  if keyword_set(outputid) then  begin
    self.out->print,1, "The returned records are point's index..."
    return, index
  endif else begin
    self.out->print,1, "The returned records are point's data..."
    return, data
  endelse
  
  
  
End



;+
; This function returns a point selection base on the RETURN NUMBER from the points loaded in memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getSelectedDataByNumberOfReturns(RETURN_NUMBER=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to second return:
;       Result = lasObj->getSelectedDataByNumberOfReturns(RETURN_NUMBER=2)
;
; :Keywords:
;   return_number : in, required, type=integer
;     return number as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function carbslib::getSelectedDataByNumberOfReturns, return_number=rtnb, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data from memory..."
  
  if size((*(self.lasData)),/dimensions) ne 0 then begin
  
    tempDataStr = (*(self.lasData))
    
    self.out->print,1,"Filtering data by return number..."
    
    self.out->print,1,"Filtering number " + strcompress( rtnb ) + "..."
    
    ; Extracting the following bits
    ; '00000111'
    cache = 8b + 16b + 32b
    tempNumberOfReturns = tempDataStr.nReturn
    result = cache and tempNumberOfReturns
    
    index = where( result eq rtnb, indexCount )
    
    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
    
    if (size(data))[2] ne 8 then $
      self.out->print,2, "Nothing return !" else $
      self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
      
    self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
    
    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).
    
    if keyword_set(outputid) then  begin
      self.out->print,1, "The returned records are point's index..."
      return, index
    endif else begin
      self.out->print,1, "The returned records are point's data..."
      return, data
    endelse
    
  endif else return, 0
  
  
  
End




Function carbslib::removeOffPoints, tileID, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data from memory..."
  
  if size((*(self.lasData)),/dimensions) ne 0 or ptr_valid(self.lasDataTile) eq 1 then begin
  
    tempDataStr = (*(self.lasData))
    
    self.out->print,1,"Filtering data by OFF tile index..."
    
    index = where( (*(*self.lasDataTile)[tileID].off) eq 1, indexCount, COMPLEMENT = comp, /NULL )
    
    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(comp), _ref_extra=ex, self.out) else data = 0
    
    if (size(data))[2] ne 8 then $
      self.out->print,2, "Nothing return !" else $
      self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
      
    self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
    
    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).
    
    if keyword_set(outputid) then  begin
      self.out->print,1, "The returned records are point's index..."
      return, index ; comp
    endif else begin
      self.out->print,1, "The returned records are point's data..."
      return, data
    endelse
    
  endif else return, 0
  
  
  
End



;+
; This function returns a point selection base on the RETURN NUMBER from the points in a LAS file.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getDataFromNumberOfReturns(RETURN_NUMBER=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('carbslib')
;       lasObj->loadData, inputFile = /Path/to/las/file
;
;     Get all the points corresponding to second return:
;       Result = lasObj->getDataFromNumberOfReturns(RETURN_NUMBER=2)
;
; :Keywords:
;   return_number : in, required, type=integer
;     return number as define by ASPRS standard. More information on the classification at
;     `the ASPRS website <http://www.asprs.org/a/society/committees/lidar/LAS_1-4_R6.pdf>`.
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function carbslib::getDataFromNumberOfReturns, return_number=rtnb, outputId=outputId

  ; start time
  T = SYSTIME(1)
  
  openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  
  self.out->print,1, "Reading the data into memory..."
  
  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = replicate(*(self).lasDataStr, (*(self.lasHeader)).nPoints)
  point_lun, inputLun, (*(self.lasHeader)).dataOffset
  readu, inputLun, tempDataStr
  
  self.out->print,1,"Filtering data by return number..."
  
  self.out->print,1,"Filtering number " + strcompress( rtnb ) + "..."
  
  ; Extracting bits '00000111'
  cache = 8b + 16b + 32b
  tempNumberOfReturns = tempDataStr.nReturn
  result = cache and tempNumberOfReturns
  
  index = where( result eq rtnb, indexCount )
  
  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.out) else data = 0
  
  if (size(data))[2] ne 8 then $
    self.out->print,2, "Nothing return !" else $
    self.out->print,1, strcompress("Number of point record(s) returned: "+string(indexCount))
    
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
  ; Returning requested data
  free_lun, inputLun
  if keyword_set(outputid) then  begin
    self.out->print,1, "The returned records are point's index..."
    return, index
  endif else begin
    self.out->print,1, "The returned records are point's data..."
    return, data
  endelse
  
  
  
End




;+
; This function returns the waveform(s) associated to the loaded points set.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   Return the wave package as a `bytarr(n,m)` where
;   n is the number of samples per waveform and m is the number of waveform(s) requested.
;
; :Uses:
;   Result=Obj->getWave()
;
; :Examples:
;
;     Setup the object:
;     
;       lasObj = obj_new('carbslib')
;
;     Load the 5th flightline of survey from December 5th 2003:
;       
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get the 100th point of the wile:
;     
;       Result = lasObj->getData(pointNumber=100)
;       
;     Get the associate waveform:
;     
;       Wave = lasObj->getWave(/loadedPoints)
;
; :Keywords:
;   all : in, optional, type=boolean
;     If set, returns all the waveforms of the file as an `intarr(n,m)` where
;     n is the number of samples per waveform and m is the number of waveform(s) returned.
;   loadedPoints : in, optional, type=boolean
;     If set, returns the waveforms associate to the points loaded in memory during the last call of carbslib::getData() method.
;     It will return a `intarr(n,m)` where n is the number of samples per waveform and m is the number of waveform(s) returned.
;   electric: in, optional, type=boolean
;     If set, the return waveform are converted to voltage value as recorded by the system. 
;     It will return a `dblarr(n,m)` where n is the number of samples per waveform and m is the number of waveform(s) returned.
;
;-
Function carbslib::getWave, all=all, loadedPoints=loadedPoints, electric=electric

;TODO: Check if the waveforms are inside the LAS file if not find the wave file and open it
if (*(self.lasHeader)).versionMinor le 2 then begin

  self.out->print,2,"The LAS file version does not contain any waveforms..."
  self.out->print,2,"Nothing return !"
  
  tempWave = {wave:0b}
  
endif else begin

  ; Checking where the waveforms are; in file or in another file
  check = globalEncodingReader(*(self.lasHeader))
  
  if check[1] eq 1 then openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian else begin
  
  ; strip down the name add the extention .wdp and open the file
  
  endelse  

  
  if keyword_set(all) then begin
  
  ; Declare the ERROR LABEL
    ON_IOERROR, BAD
  
    tempWaveStr = {waveStruc,wave:bytarr((*(self.lasWaveDsptr)).numberOfSamples)}
    point_lun, inputLun, ((*(self.lasHeader)).startWaveform )
    initevrlheader, evrlStruct
    readu, inputLun, evrlStruct
    print, "evrlStruct", evrlStruct
    self.lasWaveEvlrHeader = ptr_new(evrlStruct)
    
    nRecordsEvrl = long(evrlStruct.recordLengthAfterHearder / ((*(self.lasWaveDsptr)).numberOfSamples))
    nRecordsHeader = long((*(self.lasHeader)).nPoints)
    deltaRecords = abs(long(nRecordsEvrl) - long(nRecordsHeader))
    
    if nRecordsEvrl ne nRecordsHeader then begin
   
      self.out->print,2,"Difference between the number of laser soundings and the number of waveforms..."
      self.out->print,2, strcompress(string(deltaRecords) +" record(s) share the same waveforms")
      
    endif
    
    if (evrlStruct.recordLengthAfterHearder mod ((*(self.lasWaveDsptr)).numberOfSamples)) ne 0 then $
        self.out->print,2,"Number of bytes not consistent with the number of records..."
    
    
    tempWave = replicate(tempWaveStr,evrlStruct.recordLengthAfterHearder/((*(self.lasWaveDsptr)).numberOfSamples))
    readu, inputLun, tempWave
    
    GOTO, DONE
  
  endif
  
  if keyword_set(loadedPoints) then begin
  print, 'loadedPoint case statement'
  case 1 of
  
    (n_elements(*(self.getDataIndex)) eq 1):begin
  
  ;TODO: manage if the loadedPoints referes to /all -> need to check if all the waveforms are presents !!!
  ;offsetWaveData:0ULL, :
  
    ; Declare the ERROR LABEL
    ON_IOERROR, BAD

    if ((*(self.lasData)).wPacketSize) gt 0 then begin
      
      tempWaveStr = {waveStruc,wave:bytarr((*(self.lasData)).wPacketSize)}
      tempWave = replicate(tempWaveStr,1)
      evrlSize = 60 ;bytes 
      
      ;point_lun, inputLun, ((*(self.lasHeader)).startWaveform + evrlSize + (*(self.lasData)).offsetWaveData) ;
      point_lun, inputLun, ((*(self.lasHeader)).startWaveform + (*(self.lasData)).offsetWaveData) ; This approach seems to work better and avoid EOF for the last record
      readu, inputLun, tempWave
      
      GOTO, DONE
      
    endif else begin
    
      tempWave = {wave:0B}
      
    endelse
    
    end
    
    else: print, 'Please provide only one point...'
    
    endcase
    
  endif
  
  BAD: PRINT, !ERR_STRING
 
  ; Close and free the input/output unit.
  DONE: free_lun, inputLun
  free_lun, inputLun
  self.lasWave = ptr_new(tempWave)

endelse

if keyword_set(electric) then begin

  tempWave = (tempWave.wave + ((*(self.lasWaveDsptr)).digitizerOffset)) * ((*(self.lasWaveDsptr)).digitizerGain)
  return, tempWave
  
endif else begin

  return, tempWave.wave * 1

endelse

End



;+
; This function returns the Tile InDex (.tid) file content.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   Return an array of structure holding the tile information.
;
; :Uses:
;   Get all the tile(s)
;   Result=Obj->getTileIndex()
;   
;   Get a specific tile
;   Result=Obj->getTileIndex(index)
;   
; :Keywords:
;   index : in, optional, type=integer(LONG, LONG64)
;     Tile's index number.
;
;
;-
Function carbslib::getTileIndex, index

  if n_elements(index) ne 0 then begin
    
    if index ge self.lasNTiles then begin
      self.out->print, 2, "Tile number out of range..."
      return, 0
    endif else begin
      return, (*self.lasDataTile)[i]
    endelse
    
  endif else return, (*(self.lasDataTile))
  
End



;+
; This function returns the index of the last loaded points into memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   Return an `lonarr(n)` where n is the number of points.
;
; :Uses:
;   Result=Obj->getSelectedDataIndex()
;
;-
Function carbslib::getSelectedDataIndex

  if ptr_valid(self.getDataIndex) then return, (*(self.getDataIndex)) else print, 'Nothing to return, please update the selection first...'

End



;+
; This function gets specific field of the LAS header.
;
; :Categories:
;   LAS, GET
;
; :Uses:
;   Result=Obj->getHeaderProperty(/KEYWORDS)
;
; :Keywords:
;   header : in, optional, type=boolean
;     If set, will return the complet header structure.
;     The result will be a structure of header type
;   signature : in, optional, type=boolean
;     If set, will return the FILE SIGNATURE field.
;     The result will be a `bytarr(4)`.
;   versionmajor : in, optional, type=boolean
;     If set, will return the VERSION MAJOR field.
;     The result will be a `0B`.
;   versionminor : in, optional, type=boolean
;     If set, will return the VERSION MINOR field.
;     The result will be a `0B`.
;   systemid : in, optional, type=boolean
;     If set, will return the VERSION SYSTEM IDENTIFIER field.
;     The result will be a `bytarr(32)`.
;   softwareid : in, optional, type=boolean
;     If set, will return the GENERATING SOFTWARE field.
;     The result will be a `bytarr(32)`.
;   surveyday : in, optional, type=boolean
;     If set, will return the FILE CREATION DAY OF YEAR field.
;     The result will be a `0US`.
;   surveyyear : in, optional, type=boolean
;     If set, will return the FILE CREATION YEAR field.
;     The result will be a `0US`.
;   sizeofheader : in, optional, type=boolean
;     If set, will return the HEADER SIZE field.
;     The result will be a `0US`.
;   dataoffset : in, optional, type=boolean
;     If set, will return the OFFSET TO POINT DATA field.
;     The result will be a `0UL`.
;   numberofvlr : in, optional, type=boolean
;     If set, will return the NUMBER OF VARIABLE LENGTH RECORDS field.
;     The result will be a `0UL`.
;   pointformat : in, optional, type=boolean
;     If set, will return the POINT DATA FORMAT ID field.
;     The result will be a `0B`.
;   pointlength : in, optional, type=boolean
;     If set, will return the POINT DATA RECORD LENGTH field.
;     The result will be a `0US`.
;   numberofpoints : in, optional, type=boolean
;     If set, will return the NUMBER OF POINT RECORDS field.
;     The result will be a `0UL`.
;   numberofreturns : in, optional, type=boolean
;     If set, will return the NUMBER OF POINTS BY RETURN field.
;     The result will be a `0B`.
;   xscale : in, optional, type=boolean
;     If set, will return the X SCALE FACTOR field.
;     The result will be a `0D`.
;   yscale : in, optional, type=boolean
;     If set, will return the Y SCALE FACTOR field.
;     The result will be a `0D`.
;   zscale : in, optional, type=boolean
;     If set, will return the Z SCALE FACTOR field.
;     The result will be a `0D`.
;   xoffset : in, optional, type=boolean
;     If set, will return the X OFFSET field.
;     The result will be a `0D`.
;   yoffset : in, optional, type=boolean
;     If set, will return the Y OFFSET field.
;     The result will be a `0D`.
;   zoffset : in, optional, type=boolean
;     If set, will return the Z OFFSET field.
;     The result will be a `0D`.
;   xmax : in, optional, type=boolean
;     If set, will return the MAX X field.
;     The result will be a `0D`.
;   xmin : in, optional, type=boolean
;     If set, will return the MIN X field.
;     The result will be a `0D`.
;   ymax : in, optional, type=boolean
;     If set, will return the MAX Y field.
;     The result will be a `0D`.
;   ymin : in, optional, type=boolean
;     If set, will return the MIN Y field.
;     The result will be a `0D`.
;   zmax : in, optional, type=boolean
;     If set, will return the MAX Z field.
;     The result will be a `0D`.
;   zmin : in, optional, type=boolean
;     If set, will return the MIN Z field.
;     The result will be a `0D`.
;   boundingbox : in, optional, type=boolean
;     If set, will return the bounding box of the LAS file.
;     The result will be a `dblarr(4)` of [XMAX,XMIN,YMAX,YMIN,ZMAX,ZMIN].
;-
Function carbslib::getHeaderProperty,$
  header=header,$
  signature=signature,$
  versionMajor=versionMajor,$
  versionMinor=versionMinor,$
  systemID=systemId,$
  softwareID=softwareID,$
  surveyDay=surveyDay,$
  surveyYear=surveyYear,$
  sizeOfHeader=headerSize,$
  dataOffset=dataOffset,$
  numberOfVLR=numberOfVLR,$
  pointFormat=pointFormat,$
  pointLength=pointLength,$
  numberOfPoints=numberOfPoints,$
  numberOfReturns=numberOfReturns,$
  xScale=xScale,$
  yScale=yScale,$
  zScale=zScale,$
  xOffset=xOffset,$
  yOffset=yOffset,$
  zOffset=zOffset,$
  xMax=xMax,$
  xMin=xMin,$
  yMax=yMax,$
  yMin=yMin,$
  zMax=zMax,$
  zMin=zMin,$
  boundingBox=boundingBox       

if keyword_set(header) then return, (*(self.lasHeader))
if keyword_set(signature) then return, (*(self.lasHeader)).signature
if keyword_set(versionMajor) then return, (*(self.lasHeader)).versionMajor
if keyword_set(versionMinor) then return, (*(self.lasHeader)).versionMinor
if keyword_set(systemID) then return, (*(self.lasHeader)).systemID
if keyword_set(softwareID) then return, (*(self.lasHeader)).softwareID
if keyword_set(surveyDay) then return, (*(self.lasHeader)).day
if keyword_set(surveyYear) then return, (*(self.lasHeader)).year
if keyword_set(sizeOfHeader) then return, (*(self.lasHeader)).headerSize
if keyword_set(dataOffset) then return, (*(self.lasHeader)).dataOffset
if keyword_set(numberOfVLR) then return, (*(self.lasHeader)).nRecords
if keyword_set(pointFormat) then return, (*(self.lasHeader)).pointFormat
if keyword_set(pointLength) then return, (*(self.lasHeader)).pointLength
if keyword_set(numberOfPoints) then return, (*(self.lasHeader)).nPoints
if keyword_set(numberOfReturns) then return, (*(self.lasHeader)).nReturns
if keyword_set(xScale) then return, (*(self.lasHeader)).xScale
if keyword_set(yScale) then return, (*(self.lasHeader)).yScale
if keyword_set(zScale) then return, (*(self.lasHeader)).zScale
if keyword_set(xOffset) then return, (*(self.lasHeader)).xOffset
if keyword_set(yOffset) then return, (*(self.lasHeader)).yOffset
if keyword_set(zOffset) then return, (*(self.lasHeader)).zOffset
if keyword_set(xMax) then return, (*(self.lasHeader)).xMax
if keyword_set(xMin) then return, (*(self.lasHeader)).xMin
if keyword_set(yMax) then return, (*(self.lasHeader)).yMax
if keyword_set(yMin) then return, (*(self.lasHeader)).yMin
if keyword_set(zMax) then return, (*(self.lasHeader)).zMax
if keyword_set(zMin) then return, (*(self.lasHeader)).zMin
if keyword_set(boundingBox) then return, [(*(self.lasHeader)).xMax,(*(self.lasHeader)).xMin,(*(self.lasHeader)).yMax,(*(self.lasHeader)).yMin,(*(self.lasHeader)).zMax,(*(self.lasHeader)).zMin]

End



Function carbslib::getVlr,$
  waveDescriptorHeader=waveDescriptorHeader,$
  waveDescriptorArray=waveDescriptorArray,$
  geoTiff=geoTiff,$
  vlrHeaderOfGeoKey=vlrHeaderOfGeoKey,$
  vlrHeaderArray=vlrHeaderArray,$
  vlrRecords=vlrRecords,$             ; Header of the VLR records
  ;vlrKeyEntry=vlrKeyEntry,$
  vlrGeneric=vlrGeneric,$
  vlrWholeGeneric=vlrWholeGeneric,$
  vlrGeoKeyHeader=vlrGeoKeyHeader,$
  vlrGeoKeyArray=vlrGeoKeyArray,$
  vlrHeader_Array=vlrHeader_Array,$
  vlrRecord_Array=vlrRecord_Array,$
  vlrFileID = vlrFileID,$
  vlrByteSize = vlrByteSize,$
  vlrId = vlrId 
  
if keyword_set(WaveDescriptorHeader) then return, (*(self.lasWaveDsptrHdr))
if keyword_set(WaveDescriptorArray) then return, (*(self.lasWaveDsptr))
if keyword_set(geoTiff) then return, 0
if keyword_set(vlrRecords) then return, (*(self.lasVLRRecords))
;if keyword_set(vlrKeyEntry) then return, (*(self.lasVLRKeyEntry))
if keyword_set(vlrGeneric) then return, (*(self.lasVLRGeneric))
if keyword_set(vlrWholeGeneric) then return, (*(self.lasVLRWholeGeneric))
if keyword_set(vlrHeaderOfGeoKey) then return, (*(self.lasVlrGeoKeyHeader))
if keyword_set(vlrGeoKeyHeader) then return, (*(self.lasGeoKeyHeader))
if keyword_set(vlrGeoKeyArray) then return, (*(self.lasGeoKeyArray))
if keyword_set(vlrHeader_Array) then return, (*(self.vlrHeader_Array))
if keyword_set(vlrRecord_Array) then return, (*(self.vlrRecord_Array))
if keyword_set(vlrFileID) then return, (*(self.vlrFileID))
if keyword_set(vlrByteSize) then return, (*(self.vlrByteSize))
if keyword_set(vlrId) then return, (*(self.vlrId))
End



;+
; This function gets the EXTENDED VARIABLE LENGTH RECORDS header.
;
; :Categories:
;   LAS, GET
;
; :Uses:
;   Result=Obj->getEVLR(/KEYWORDS)
;
; :Keywords:
;   header : in, optional, type=boolean
;     If set, will return the header structure of the EXTENDED VARIABLE LENGTH RECORDS.
;     The result will be a structure::
;
;         evrlStruct = {$
;          reserved                     : 0US, $
;          userID                       : bytarr(16),  $
;          recordID                     : 0US,  $
;          recordLengthAfterHearder     : 0ULL, $
;          Description                  : bytarr(32)  $
;         }
;         
;-
Function carbslib::getEVlr,$
  header=header
  
if keyword_set(header) then return, (*(self.lasWaveEvlrHeader))  
  
End




;+
; This function return the size in bytes of the point data structure.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   Return an `0B` value.
;
; :Uses:
;   Result=Obj->getPointSize()
;
;-
Function carbslib::getPointSize
 
return, self.lasDataStrSz  
  
End



;+
; This function sets the header of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;   
; :Params:
;   temp : in, required, type=structure
;
; :Uses:
;   Result=Obj->setHeader(temp=tempvalue)
;
;-
Function carbslib::setHeader, temp
(*(self.lasHeader))=temp
End



;+
; This function sets the HEADER SIGNATURE field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`bytarr(4)`
;
; :Uses:
;   Result=Obj->setHeaderSignature(temp=tempvalue)
;
;-
Function carbslib::setHeaderSignature, temp
(*(self.lasHeader)).signature = temp
End



;+
; This function sets the VERSION MAJOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0B`
;
; :Uses:
;   Result=Obj->setHeaderVersionMajor(temp=tempvalue)
;
;-
Function carbslib::setHeaderVersionMajor, temp
(*(self.lasHeader)).versionMajor = temp
End



;+
; This function sets the VERSION MINOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0B`
;
; :Uses:
;   Result=Obj->setHeaderVersionMinor(temp=tempvalue)
;
;-
Function carbslib::setHeaderVersionMinor, temp
(*(self.lasHeader)).versionMinor = temp
End  



;+
; This function sets the SYSTEM IDENTIFIER field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`bytarr(32)`
;
; :Uses:
;   Result=Obj->setHeaderSystemID(temp=tempvalue)
;
;-
Function carbslib::setHeaderSystemID, temp

  dum = ((*(self.lasHeader)).systemID)
  dum[0] = temp
  ((*(self.lasHeader)).systemID) = dum

End



;+
; This function sets the GENERATING SOFTWARE field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`bytarr(32)`
;
; :Uses:
;   Result=Obj->setHeaderSoftwareID(temp=tempvalue)
;
;-
Function carbslib::setHeaderSoftwareID, temp
 
  dum = ((*(self.lasHeader)).softwareID)
  dum[0] = temp
  ((*(self.lasHeader)).softwareID) = dum

End



;+
; This function sets the FILE CREATION DAY OF YEAR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0US`
;
; :Uses:
;   Result=Obj->setHeaderDay(temp=tempvalue)
;
;-
Function carbslib::setHeaderDay, temp
(*(self.lasHeader)).day = temp
End



;+
; This function sets the FILE CREATION YEAR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0US`
;
; :Uses:
;   Result=Obj->setHeaderYear(temp=tempvalue)
;
;-
Function carbslib::setHeaderYear, temp
(*(self.lasHeader)).year = temp
End



;+
; This function sets the OFFSET TO POINT DATA field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0UL`
;
; :Uses:
;   Result=Obj->setHeaderDataOffset(temp=tempvalue)
;
;-
Function carbslib::setHeaderDataOffset, temp
(*(self.lasHeader)).dataOffset = temp
End



;+
; This function sets the NUMBER OF VARIABLE LENGTH RECORDS field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0UL`
;
; :Uses:
;   Result=Obj->setHeaderNRecords(temp=tempvalue)
;
;-
Function carbslib::setHeaderNRecords, temp
(*(self.lasHeader)).nRecords = temp
End



;+
; This function sets the POINT DATA FORMAT ID field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0B`
;
; :Uses:
;   Result=Obj->setHeaderPointFormat(temp=tempvalue)
;
;-
Function carbslib::setHeaderPointFormat, temp
(*(self.lasHeader)).pointFormat = temp
End



;+
; This function sets the POINT DATA RECORD LENGTH field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0US`
;
; :Uses:
;   Result=Obj->setHeaderPointLength(temp=tempvalue)
;
;-
Function carbslib::setHeaderPointLength, temp
(*(self.lasHeader)).pointLength = temp
End



;+
; This function sets the NUMBER OF POINTS RECORDS field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0UL`
;
; :Uses:
;   Result=Obj->setHeaderNPoints(temp=tempvalue)
;
;-
Function carbslib::setHeaderNPoints, temp
(*(self.lasHeader)).nPoints = temp
End



;+
; This function sets the NUMBER OF POINTS BY RETURN field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`ulonarr(7)`
;
; :Uses:
;   Result=Obj->setHeaderNReturns(temp=tempvalue)
;
;-
Function carbslib::setHeaderNReturns, temp
(*(self.lasHeader)).nReturns = temp
End



;+
; This function sets the X SCALE FACTOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderXScale(temp=tempvalue)
;
;-
Function carbslib::setHeaderXScale, temp
(*(self.lasHeader)).xScale = temp
End



;+
; This function sets the Y SCALE FACTOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderYScale(temp=tempvalue)
;
;-
Function carbslib::setHeaderYScale, temp
(*(self.lasHeader)).yScale = temp
End



;+
; This function sets the Z SCALE FACTOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderZScale(temp=tempvalue)
;
;-
Function carbslib::setHeaderZScale, temp
(*(self.lasHeader)).zScale = temp
End



;+
; This function sets the X OFFSET field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderXOffset(temp=tempvalue)
;
;-
Function carbslib::setHeaderXOffset, temp
(*(self.lasHeader)).xOffset = temp
End



;+
; This function sets the Y OFFSET field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderYOffset(temp=tempvalue)
;
;-
Function carbslib::setHeaderYOffset, temp
(*(self.lasHeader)).yOffset = temp
End



;+
; This function sets the Z OFFSET field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderZOffset(temp=tempvalue)
;
;-
Function carbslib::setHeaderZOffset, temp
(*(self.lasHeader)).zOffset = temp
End



;+
; This function sets the MAX X field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderXMax(temp=tempvalue)
;
;-
Function carbslib::setHeaderXMax, temp
(*(self.lasHeader)).xMax = temp
End



;+
; This function sets the MAX Y field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderYMax(temp=tempvalue)
;
;-
Function carbslib::setHeaderYMax, temp
(*(self.lasHeader)).yMax = temp
End



;+
; This function sets the MAX Z field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderZMax(temp=tempvalue)
;
;-
Function carbslib::setHeaderZMax, temp
(*(self.lasHeader)).zMax = temp
End



;+
; This function sets the MIN X field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderXMin(temp=tempvalue)
;
;-
Function carbslib::setHeaderXMin, temp
(*(self.lasHeader)).xMin = temp
End



;+
; This function sets the MIN Y field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderYMin(temp=tempvalue)
;
;-
Function carbslib::setHeaderYMin, temp
(*(self.lasHeader)).yMin = temp
End



;+
; This function sets the MIN Z field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0.0D`
;
; :Uses:
;   Result=Obj->setHeaderZMin(temp=tempvalue)
;
;-
Function carbslib::setHeaderZMin, temp
(*(self.lasHeader)).zMin = temp
End


;+
; This function sets the START OF WAVEFORM DATA PACKET RECORD field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=`0ULL`
;
; :Uses:
;   Result=Obj->setHeader(temp=tempvalue)
;
;-
Function carbslib::setHeaderStartWaveform, temp
(*(self.lasHeader)).startWaveform = temp
End



;+
; This function sets the WAVEFORM PACKET HEADER field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=structure
;     The array needs to be of the following format::
;     
;          vrlStruct = {$
;            reserved                     : 0US, $
;            userID                       : bytarr(16),  $
;            recordID                     : 0US,  $
;            recordLengthAfterHearder     : 0US, $
;            Description                  : bytarr(32)  $
;          }
;     
;
; :Uses:
;   Result=Obj->setVlrWaveDescriptorHeader(temp=tempvalue)
;
;-
Function carbslib::setVlrWaveDescriptorHeader, temp
;(*(self.lasHeader)).lasWaveDsptrHdr = temp
(*(self.lasWaveDsptrHdr)) = temp
End


;+
; This function sets the WAVEFORM PACKET DESCRIPTOR field of the LAS file. To make the changes permanent, the file need to be written.
;
; :Categories:
;   GENERAL, SET
;
; :Returns:
;   none
;
; :Params:
;   temp : in, required, type=structure
;     The array needs to be of the following format::
;
;       wfDescriptor = {$
;          bitsPerSample:0B,$
;          waveformCompressionType:0B,$
;          numberOfSamples:0UL,$
;          temporalSampleSpacing:0UL,$
;          digitizerGain:0.0D,$
;          digitizerOffset:0.0D $
;          }
;
;
; :Uses:
;   Result=Obj->setVlrWaveDescriptorArray(temp=tempvalue)
;
;-
Function carbslib::setVlrWaveDescriptorArray, temp
;(*(self.lasHeader)).lasWaveDsptr = temp
(*(self.lasWaveDsptr)) = temp
End






;+
; This function sets the Extended Variable Length Records.
;
; :Categories:
;   LAS, SET
;
; :Uses:
;   Result=Obj->setEVLr(temp)
;   
; :Params:
;   temp : in, required, type=structure
;     A binary structure of the EVLR.
;
;-
Function carbslib::setEVlr, temp
(*(self.lasWaveEvlrHeader)) = temp  
End



;+
; This Procedure write a LAS file. Still in beta.
;
; :Categories:
;   GENERAL, LAS
;
; :Returns:
;   1 if the file is correclty write.
;   0 if error.
;   
; :History:
;   Septembre 2012 - initial writing
;   February 2014 - Fully functional
;
;-
Pro carbslib::writeLAS, outFile

openw, 1, outFile

self.out->print, 1, "Writing the new file on the disk at " + strcompress(string(outFile),/remove_all)

; Defining System ID
sysID = bytarr(32)
sysID[0] = byte('carbslib by Carbomap Ltd')
dum = self.setHeaderSystemID(sysID)
; Defining Software ID
softID = Bytarr(32)
softID[0] = Byte('carbslib::writeLAS')
dum = self.setHeaderSoftwareID(softID)

lasHeader = self->getHeaderProperty(/header)

writeu, 1, lasHeader
self.out->print, 1, "Writing public header..."
point_lun, -1, posHeader
if posHeader eq lasHeader.headerSize then self.out->print, 1,  "Public header successfully written..."


; For each VLR, open get the byte size of the record
; Open the corresponding file
; Read the file
; write the raw content into the new LAS file
self.out->print, 1, "Writing Variable Length Records..."
vlrFileID = self->getvlr(/vlrFileID)
vlrByteSize = self->getvlr(/vlrByteSize)
rB = 0L
wB = 0L
posVlrArray = 0L
for x=0, self.getHeaderProperty(/numberOfVLR)-1 do begin
openr, 2, vlrFileID[x]
dum = bytarr(vlrByteSize[x])
readu, 2, dum
point_lun, -2, r
rB += r
close, 2
writeu,1,dum
point_lun, -1, w
if x eq 0 then begin
  wB += w-posHeader 
endif else begin
  wB += (w - wB)
endelse
endfor
if rB eq wB-posHeader then begin
byte1 = wB-posHeader
self.out->print, 1, "Variable Length Records successfully written..."
self.out->print, 1, strcompress(string(byte1),/remove_all) + " bytes have been written..."
endif else begin
  self.out->print, 2, "Something wrong with the Variable Length Records block..."
endelse


self.out->print, 1, "Checking file integrity..."
totalBytesWritten = posHeader + byte1 ;+ byte2 + byte3 + byte4 + byte5 + byte6
if lasHeader.dataOffset eq totalBytesWritten then self.out->print, 1, "File integrity pass..." else begin
  self.out->print, 2, "File integrity fail..."
  self.out->print, 2, strcompress(string(totalBytesWritten),/remove_all)+" bytes have been written so far, or the file header stipulates "+strcompress(string(lasHeader.dataOffset),/remove_all)+" bytes..."
  if lasHeader.dataOffset-totalBytesWritten eq 2 then self.out->print, 1, "The header is followed by 2 user-defined bytes..."
  self.out->print, 2, "Moving "+strcompress(string(lasHeader.dataOffset-totalBytesWritten),/remove_all)+" bytes ahead..."
  point_lun, -1, actualPos
  point_lun, 1, actualPos + (lasHeader.dataOffset-totalBytesWritten)
  self.out->print, 2, "Done... resuming writting process..."
  endelse

pointSize = self->getPointSize()
theoriticalDataBlockSize = pointSize * lasHeader.nPoints
;print, theoriticalDataBlockSize
point_lun, -1, posBeforeDataBlock

self.out->print, 1, "Writing points data records..."
data = self->getData(/all)
writeu, 1, data


point_lun, -1, posAfterDataBlock
byte7 = posAfterDataBlock-posBeforeDataBlock

self.out->print, 1, "Checking file integrity..."
fileIntegrity = theoriticalDataBlockSize - byte7
if fileIntegrity eq 0 then self.out->print, 1, "File integrity pass..." else begin
  self.out->print, 3, "File integrity fail..."
  endelse



; Checking if the file have waveform information
if ptr_valid(temp01) then begin

self.out->print, 1, "Position before waveform block: ", posAfterDataBlock
self.out->print, 1, "What the header helds", lasHeader.startWaveform

waveformBlock = self->getWave(/all)
waveformHeader = self->getEVlr(/header)

self.out->print, 1, "Size information of the Wave Packet: ", size(waveformBlock)

self.out->print, 1, "Writing waveforms header (EVLR header)..."
writeu, 1, waveformHeader
point_lun, -1, posWaveformHeader
self.out->print, 1, "Amount of bytes written for the waveform header: "+strcompress(string(posWaveformHeader-posAfterDataBlock),/remove_all)

self.out->print, 1, "Writing waveforms header (EVLR header)..."
writeu, 1, byte(waveformBlock)
point_lun, -1, posWaveformBlock
self.out->print, 1, "Amount of bytes written for the waveform block: "+strcompress(string(posWaveformBlock-posWaveformHeader),/remove_all)

endif

self.out->print, 1, "Finilizing the file..."
free_lun, 1
self.out->print, 1, "Done."

close,1

End




Function carbslib::getSelectedDataMaximumHeight, outputId=outputId

;print, (*(self.lasHeader)).zscale , (*(self.lasHeader)).zoffset
  maxH = max( ((*self.lasData).elev * 0.001) + 0., maxSub )
  self.out->print,1, "Maximum height is " + strcompress(string(maxH), /REMOVE_ALL) + " m..."
;  maxH = max( ( (*self.lasData).elev * (*(self.lasHeader)).zscale ) + (*(self.lasHeader)).zoffset, maxSub )

  if keyword_set(outputid) then  begin
    self.out->print,1, "The returned records are point's index..."
    return, maxSub[0]
  endif else begin
    self.out->print,1, "The returned is the elevation information..."
    return, maxH[0]
  endelse

End



Function carbslib::getSelectedDataRandom, seed, n, outputId=outputId

  dum = n_elements(self.getSelectedDataIndex())
  rNumb = randomu(seed, n)
  
  if keyword_set(outputid) then  begin
    self.out->print,1, "The returned records are point's index..."
    return, round(dum * rNumb)
  endif else begin
    self.out->print,1, "The returned is the elevation information..."
    return, self.getData(pointNumber=round(dum * rNumb))
  endelse

End




;+
; :Description:
;    Dump point data to CSV ASCII file.
;
; :Category:
; 	LAS
;
; :Return:
; 	If any, what is the output of this method
;
;	:Uses:
;		The call method
;
;	:Example:
;		A quick example on how to use this method
;
; :Params:
;    val: in, optional, type = any
;     A value to be associated with each data record.
;     If a single value is provided, than it will be duplicate for each data record
;
; :Keywords:
;    outputPath = in, optional, type = string
;     A string that represents the path of the output file
;
; :History:
; 	Development history
; 	 -January 2014
; 	   Creation
;
; :Author: antoine
;-
Function carbslib::dump, val, outputPath=outputPath

  x = ((*self.lasData).east * (*(self.lasHeader)).xscale) + (*(self.lasHeader)).xoffset
  y = ((*self.lasData).north * (*(self.lasHeader)).yscale) + (*(self.lasHeader)).yoffset
  z = ((*self.lasData).elev * 0.001) + 0.
  
  nX = n_elements(x)
  
  self.out->print,1, 'Printing out ' + strcompress(string(nX)) + ' point structure into dump file...'
  
  if keyword_set(outputPath) then openw, lun, outputPath, /APPEND, /GET_LUN else $
                                  openw, lun, './data/ascii_dump.csv', /APPEND, /GET_LUN
  
  nVal = n_elements(val)
  
  case nVal of
  
  0: WRITE_CSV, './data/dump.csv', x, y, z
  
  1: begin
    
      temp = transpose([$
          [x],$
          [y],$
          [z],$
          [replicate(val, nX)] $
          ])
      printf, lun, temp
     
     end
     
  nx: printf, lun, transpose([[x],[y],[z],[val]])   
  
  else: self.out->print,3,'Hummm something went wrong...'
    
  endcase
  
  free_lun, lun, /FORCE
  
  return, 1
  
End



Function carbslib::getScaledCoordinates

  return, [ $
          [ ((*self.lasData).east * (*(self.lasHeader)).xscale) + (*(self.lasHeader)).xoffset ] ,$
          [ ((*self.lasData).north * (*(self.lasHeader)).yscale) + (*(self.lasHeader)).yoffset ] ,$
          [ ((*self.lasData).elev * (*(self.lasHeader)).zscale) + (*(self.lasHeader)).zoffset ] $
          ]
          
End



Function carbslib::updateSelectArray, index

    (*self.selectArray)[index] += 1
    return, 1
    
End



Function carbslib::resetselectArray

    (*self.selectArray) *= 0
    return, 1
    
End



Function carbslib::radiusSelection, center, radius, indexPoints

  ; Restoring data into memory
  ;dum = self.restoreData()

  ; Loading data using the provided index data
  ; If not index provided, then load all the data
  if n_elements(indexPoints) eq 0 then begin

    loadData = self.getData(/all)
    ; Creating a Points Array Class using all points in memory
    points2DArr = pointarrayclass(self.getScaledCoordinates())
    dum = points2DArr.transformTo2D()

  endif else begin

    loadData = self.getData(pointNumber = indexPoints)
    ; Creating a Points Array Class using all points in memory
    points2DArr = pointarrayclass(self.getScaledCoordinates())
    ;dum = points2DArr.transformTo2D()

  endelse

  ; Checking the validity of the center points
  if n_elements(center) eq 0 then begin

    self.out->print, 2, 'No center points found...'
    return, 0

  endif else begin
    
    pointData = self.getData(pointNumber = center)
    point = pointclass(self.getScaledCoordinates())
    ;dum = point.setZ(0.D)
    
  endelse
  
  ; Checking the validity of the radius length
  if n_elements(radius) eq 0 or radius le 0. then begin

    self.out->print, 2, 'Problem with the radius definition...'
    return, 0

  endif

  ; Creating 2D vectors
  tempVecs = points2DArr.makeVectorArray(point)
  vecs = tempVecs.horizLength()
  ; Filtering vecs be length
  finalIndex = where(vecs le radius, /NULL)

  return, finalIndex


End


Function carbslib::polygonSelection, boundPoints, indexPoints

  ; Restoring data into memory
  ;dum = self.restoreData()
  
  ; Loading data using the provided index data
  ; If not index provided, then load all the data
  if n_elements(indexPoints) eq 0 then begin
    
    dum = self.getData(/all)
    
  endif else begin
    
    dum = self.getData(pointNumber = indexPoints)
    
  endelse

  ; Checking the validity of the bounding points
  if n_elements(boundPoints) eq 0 then begin
    
    self.out->print, 2, 'No bounding points found...'
    return, 0
    
  endif

  ; Number of boundary points
  nSeg = boundPoints.getDim()
  
  ; Number of points loaded in memory
  nPoints = n_elements(dum)
  
  ; Creating a Points Array Class using all points in memory
  points2DArr = pointarrayclass(self.getScaledCoordinates())
  dum = points2DArr.transformTo2D()
  
  oArr = points2DArr.makeVectorArrayStackFromPointArray(boundPoints) 
  ; Shit one row to the right
  shiftOArr = shift(oArr, 1)
  
  totalAngle = dblarr(nPoints)
  
  for i = 0, nSeg-1, 1 do begin
;  for i = nSeg-1, 0, 1 do begin
    
    tempDot = oArr[i].dot(shiftOArr[i])
    tempDet = oArr[i].det(shiftOArr[i])
    totalAngle += atan(tempDet.z(), tempDot)
    
  endfor
  
  finalIndex = where( abs(totalAngle) gt 0.001D, /NULL )  ;!PI
  
  return, finalIndex

End



;+
; :Description:
;    The purpose of this method is the read and extract the Variable Length Records from
;    the public header block of the LAS file.
;
; :Category:
;   LAS
;
; :Return:
;   Will return 4 arrays:
;    vlrFileID - A string array that the path to the temporary files that hold the files name
;    vlrByteSize - An Unsigned Long array that holds the byte size of each key
;    vlrId - A byte array that holds a byte flag to describ the geokey
;    vlrArr - A pointer array that contains the key in reading order header/key
;
; :Uses:
;   dum = readVRL(inputFile, header, vlrFileArr, vlrByteSizeArr, vlrId ,vlrArr)
;
; :Example:
;   This is not a public method
;
; :History:
;   Create by Antoine Cottin, January 2012.
;   February 2014 : remodeling of the whole procedure for better results
;
; :Author:
;   Antoine Cottin
;-
Function carbslib::readVLR, inputFile, header, vlrFileArr, vlrByteSizeArr, vlrId ,vlrArr, obj

  ; Creating a binary file that will hold the VLR Records
  ;CD, '~/IDLWorkspace83/Saints/'
  close, 2

  ; Creating a temp file that hold ALL the VLR records
  ; XXX: will need to be changed we integrated to carbslib using self.tempDirPath
  openw, 2, './temp/vlrRecords.bin'

  ; Closing all lun(s) to avoid any issue
  close, 1


  self.out->print,1, "Number of Variable Length Records: " + strcompress(string(fix(header.nRecords)))
  self.out->print,1, "Reading variable length records..."
  ; If number of VLR not null then read them one at a time and print them into console

  if header.nRecords ne 0 then begin

    InitVRLHeader, vrlStruct
    openr, 1, inputFile, /swap_if_big_endian
    point_lun, 1, header.headerSize

    ; String array containing the file name of the temp VLR files
    vlrFileArr = strarr(header.nRecords)
    ; Long array containing the byte size of each vlr block header adn key.
    vlrByteSizeArr = lonarr(header.nRecords)
    ; A byte array containing a descriptor flag for the type of key
    ; 1 = waveform descriptor | 2 = GeoKey | 3 = Generic
    vlrId = bytarr(header.nRecords)
    ; This is pointer array that will hold the VLR, header/key, in reading order
    vlrArr = ptrarr(header.nRecords * 2)

    for w=0,header.nRecords-1,1 do begin

      outputFile = strcompress('./temp/vlr_0' + string(w) + '.gkey',/REMOVE_ALL)
      vlrFileArr[w] = outputFile

      readu, 1, vrlStruct
      writeu, 2, vrlStruct

      ; Creating a temp file that hold the nth VLR record - one file per record
      openw, 3, outputFile
      writeu, 3, vrlStruct
      vlrArr[w] = ptr_new(vrlStruct)

      case 1 of

        (vrlStruct.recordID ge 100) and (vrlStruct.recordID lt 356): begin
          self.out->print,1, "Waveform packet descriptor found"

          wfDescriptor = {$
            bitsPerSample:0B,$
            waveformCompressionType:0B,$
            numberOfSamples:0UL,$
            temporalSampleSpacing:0UL,$
            digitizerGain:0.0D,$
            digitizerOffset:0.0D $
          }
          readu, 1, wfDescriptor

          waveDescriptor = wfDescriptor


          writeu, 2, waveDescriptor
          writeu, 3, waveDescriptor
          vlrId[w] = 1
          vlrArr[w+1] = ptr_new(waveDescriptor)


        end

        (vrlStruct.recordID eq 34735): begin
          self.out->print,1,'"GeoKeyDirectoryTag Record" found'

          vlrGeoKeyHeader = vrlStruct

          gkdTag = {$
            wKeyDirectoryVersion:0US,$
            wKeyRevision:0US,$
            wMinorRevision:0US,$
            wNumberOfKeys:0US$    ;TODO to update if we add fields in there
        }

        readu,1,gkdTag
        ;        print, "Number of geokey:",gkdTag.wNumberOfKeys
        geoKeyHeader = gkdTag

        sKeyEntry = {$
          wKeyID:0US,$
          wTIFFTagLocation:0US,$
          wCount:0US,$
          wValueOffset:0US$
      }

      tempKeyEntry = replicate(sKeyEntry, gkdTag.wNumberOfKeys)
      readu,1,tempKeyEntry

      geoKeyArray = tempKeyEntry

      writeu, 2, geoKeyHeader
      writeu, 2, geoKeyArray
      writeu, 3, geoKeyHeader
      writeu, 3, geoKeyArray
      vlrId[w] = 2
      tempStruc = {header:gkdTag, key:tempKeyEntry}
      vlrArr[w+1] = ptr_new(tempStruc)
      tempStruc = 0

    end

    else: begin

      generic = bytarr(vrlStruct.recordLengthAfterHearder)
      readu,1,generic


      writeu, 2, generic
      writeu, 3, generic
      vlrId[w] = 3
      vlrArr[w+1] = ptr_new(generic)

    end

  endcase

  point_lun, -3, endPos
  close, 3
  vlrByteSizeArr[w] = endPos


endfor

close,1
close, 2

endif else begin

  self.out->print,2, 'No Variable Length Records to read'
  vlrFileArr = 0
  vlrByteSizeArr = 0
  vlrId = 0
  vlrArr = 0

endelse

End



;+
;   The purpose of this method is the read the LAS file.
;   This method is automatically called when the carbslib::loadData() is invoque.
;
; :Category:
; 	LAS
;
; :Return:
;   The point data structure and the header structure
;
;	:Uses:
;   dum = readLAS(inputLun, minorVersion, majorVersion, header, dataStr)
;
; :Params:
;    inputLun: in, required, type=string
;     A string that represents the LAS file fully qualified path
;    minorVersion: in, required, type=byte
;     A byte flag representing the LAS minor version
;    majorVersion: in, required, type=byte
;     A byte flag representing the LAS major version
;    header: out, required, type=structure 
;     A structure holding the LAS header structure
;    dataStr: out, required, type=structure
;     A structure holding the LAS point structure
;
; :History:
;   Create by Antoine Cottin, Jully 2012.
;   February 2014 :
;     -Remodeling of the whole procedure for better results
;     -Integration to the carbslib object
;
; :Author:
;   Antoine Cottin
;   
; :Hidden:
;-
Function carbslib::getLASHeaderDataStr, inputLun, minorVersion, majorVersion, header, dataStr

  self.out->print,1,strcompress("LAS Version " + string(fix(majorVersion)) + "." + strcompress(string(fix(minorVersion)),/remove_all) + " detected.")
  self.out->print,1, "Initializing the header..."
  InitHeaderLAS, header, minorVersion
  self.out->print,1, "Reading header..."
  readu, inputLun, header
  
    self.Out->print,1,'=============== HEADER ==============='
  self.Out->print,1, Strcompress("System identifier: " + String(header.Systemid))
  self.Out->print,1, Strcompress("Generating software: " + String(header.Softwareid))
  self.Out->print,1, Strcompress("Day/Year of file creation: " + String(Fix(header.Day)) + "/" + Strcompress(String(Fix(header.Year)), /REMOVE_ALL) ) 
  self.Out->print,1, Strcompress("Header size: " + String(Fix(header.Headersize)))
  self.Out->print,1, Strcompress("Byte offset to data block: " + String(header.Dataoffset))
  self.Out->print,1, Strcompress("File contains " + String(header.Npoints) + " points.")
  self.Out->print,1, Strcompress("Point format: " + String(Fix(header.Pointformat)))
  self.Out->print,1, Strcompress("Point size: " + String(header.Pointlength) + " bytes.")
  self.Out->print,1, Strcompress("Number of Variable Length Records: " + String(Fix(header.Nrecords)))
  self.Out->print,1, Strcompress("Number of points per return: ")
  self.Out->printArray,1, Strcompress(String(header.Nreturns), /REMOVE_ALL)
  self.Out->print,1, Strcompress("Scale factor x y z: " + String(header.Xscale) + " " + String(header.Yscale) + " " + String(header.Zscale))
  self.Out->print,1, Strcompress("Offset factor: " + String(header.Xoffset) + " " + String(header.Yoffset) + " " + String(header.Zoffset))
  self.Out->print,1, Strcompress("Minimum x y z: " + String(header.Xmin) + " " + String(header.Ymin) + " " + String(header.Zmin))
  self.Out->print,1, Strcompress("Maximum x y z: " + String(header.Xmax) + " " + String(header.Ymax) + " " + String(header.Zmax))
  self.Out->print,1,'======================================'

  self.out->print,1, "Initializing the point structure..."
  InitDataLAS, dataStr,  pointFormat = header.pointFormat
  self.out->print,1, "Original point Structure description:"
  self.out->printArray,1, tag_names(dataStr)
  
  return, 1

End



;
;+
;   The purpose of this method is the read the LAS file.
;   This method is automatically called when the carbslib::loadData() is invoque.
;
; :Category:
; 	LAS
;
; :Return:
;   The point data structure and the header structure
;
;	:Uses:
;   dum = readLAS(inputFile, header, dataStr)
;
; :Params:
;    inputFile: in, required, type=string
;     A string that represents the LAS file fully qualified path
;    header: out, required, type=structure 
;     A structure holding the LAS header structure
;    dataStr: out, required, type=structure
;     A structure holding the LAS point structure
;
; :History:
;   Create by Antoine Cottin, Jully 2012.
;   February 2014 :
;     -Remodeling of the whole procedure for better results
;     -Integration to the carbslib object
;
; :Author:
;   Antoine Cottin
;
; :Hidden:
;-
Function carbslib::readLAS, inputFile, header, dataStr

  compile_opt idl2, logical_predicate

  CD, self.rootPath
  
  ; Testing the file name
  fileTest = file_info(inputFile, /NOEXPAND_PATH)
  if fileTest.exists eq 1 then begin
    self.out->print,1, "Valid path and file name."
  endif else begin
    self.out->print,3, "Problem with the file path and/or name."
    self.out->print,3, "Please check your input."
    self.out->print,3, "Program closing."
    return, 0
  endelse

  ; Open the file
  openr, inputLun, inputFile, /get_lun, /swap_if_big_endian

  ; Check if the file is a LAS file
  signature = bytarr(4)
  readu, inputLun, signature

  if string(signature) eq 'LASF' then begin

    self.out->print,1, 'LAS file detected..."
    self.out->print,1, "Looking for version number..."
    point_lun,inputLun, 24
    majorVersion = 1B
    minorVersion = 1B
    readu, inputLun, majorVersion
    readu, inputLun, minorVersion

    ; Closing and re-opening the file to reinitialize the pointer
    free_lun,inputLun
    openr, inputLun, inputFile, /get_lun, /swap_if_big_endian

    dum = self.getLASHeaderDataStr(inputLun, minorVersion, majorVersion, header, dataStr)

    free_lun, inputLun

  endif else begin
    self.out->print,3, "LAS file not recognize"
    self.out->print,3, "Closing the file and terminating..."
    free_lun, inputLun
    return, 0
  endelse

  close, inputLun

  return, 1

end


;+
; 
;-
;+
; :Description:
;    This Function aim to modify the GeoKey record of the VLR record
;    Still in beta - WIP
;
; :Category:
; 	What is the general purpose of this method
;
; :Return:
; 	If any, what is the output of this method
;
;	:Uses:
;		The call method
;
;	:Example:
;		A quick example on how to use this method
;
;
;
;
;
; :History:
; 	Development history
;
; :Author: antoine
;-

Function carbslib::addGeoKey


  out = obj_new('consoleOutput')
  ;a = obj_new('carbslib')
  ;a->load_data, "I:\RG12_10-206b-FW-lidar-20121217\fw_laser\las1.3\LDR-FW-RG12_10-2012-206b-03.LAS"
  vlrGeoKeyArray=a->getvlr(/vlrGeoKeyArray)

  for x=0,n_elements(vlrGeoKeyArray)-1,1 do begin

    case vlrGeoKeyArray(x).wKeyID of

      3076:begin
      print,"Finding key " + strcompress(getGeoTiffKeyName(vlrGeoKeyArray(x).wKeyID)) +"["+strcompress(string(vlrGeoKeyArray(x).wKeyID),/remove_all)+"]..."
      print,"Changing wValueOffset from"+strcompress(vlrGeoKeyArray(x).wValueOffset)+" to 9001..."
      ;TODO: Ask user to enter the new value here
      vlrGeoKeyArray(x).wValueOffset=9001US
      print,"Done!"
    end

    4096:begin
    print,"Finding key " + strcompress(getGeoTiffKeyName(vlrGeoKeyArray(x).wKeyID)) +"["+strcompress(string(vlrGeoKeyArray(x).wKeyID),/remove_all)+"]..."
    print,"Changing wValueOffset from"+strcompress(vlrGeoKeyArray(x).wValueOffset)+" to 5001..."
    ;TODO: Ask user to enter the new value here
    vlrGeoKeyArray(x).wValueOffset=5001US
    print,"Done!"
  end

  else:begin
  print,"Finding key " + strcompress(getGeoTiffKeyName(vlrGeoKeyArray(x).wKeyID)) +"["+strcompress(string(vlrGeoKeyArray(x).wKeyID),/remove_all)+"]..."
  print,"Nothing to do!"
end

endcase

endfor


print, "Adding new field(s) to the geokey vlrGeoKeyArray..."

vlrGeoKeyArray=[vlrGeoKeyArray,{wKeyID:3072US,wTIFFTagLocation:0US,wCount:1US,wValueOffset:27700US}]
nKey = 1

vlrHeaderOfGeoKey = a->getVlr(/vlrHeaderOfGeoKey)
vlrHeaderOfGeoKey.recordLengthAfterHearder = vlrHeaderOfGeoKey.recordLengthAfterHearder + (nKey * 8US)

print, "Commiting geoKeyArray changes to the file..."
dum = a->setVlrHeaderOfGeoKey(vlrGeoKeyArray)


vlrGeoKeyHeader = a->getVlr(/vlrGeoKeyHeader)
print,"Updating geoKeyHeader..."
print, "Adding " + strcompress(string(nKey),/remove_all) + " key(s) to the geoKey VLR..."
print, "Updating public header with these new information..."
vlrGeoKeyHeader.wNumberOfKeys=vlrGeoKeyHeader.wNumberOfKeys + nKey

print, "Commiting geoKeyHeader changes to the file..."
dum = a->setVlrGeoKeyHeader(vlrGeoKeyHeader)
print, "DUM:",dum

;print,vlrGeoKeyArrayGeoKeyHeader
; Update header if needed
; Each additional key adds 8 bytes
lasHeader = a->getHeaderProperty(/header)
print, "Updating number of Variable Length Records..."
dum = a->setHeaderNRecords(lasHeader.nRecords + 1UL)
print, "New value for number of Variable Length Records: " + strcompress(string(lasHeader.nRecords),/remove_all)
print, "Updating points block offset..."
dum = a->setHeaderDataOffset(lasHeader.dataOffset + (nKey * 8UL) + 10UL)
print, "Updating wavefrom block offset..."
if lasHeader.versionMinor ge 3 then dum = a->setHeaderStartWaveform(lasHeader.startWaveform + (nKey * 8ULL) +10UL)


End