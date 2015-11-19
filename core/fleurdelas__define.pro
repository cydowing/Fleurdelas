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
;   lasObj=Obj_New("fleurdelas")
;
; :Examples:
;
;   Init the object
;
;     lasObj = obj_new('fleurdelas')
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
;     -Implementation of fleurdelas::writeLAS
;      Still in beta
;
;    September 2013
;     -Add comments and header comments
;
;    14 September 2013
;     -Change fleurdelas::getData(pointNumber=index) :
;      using the index array as one block instead of loop on the block
;      and trying to locate the exact record.
;
;    January 2014:
;     -fleurdelas::tileData :
;      Binary file structure modify, two new fields to the header
;      two new methods have been added, fleurdelas::readTile() and fleurdelas::writeTile() which
;      superseed the old method
;     -fleurdelas::dump :
;      Implementation of an ASCII dump method
;     -fleurdelas::restoreData :
;      Restore the original data from the load method (not sure this is really useful)
;     -fleurdelas:selectionRadius :
;      selection of points within a circle
;     -fleurdelas::selectionPolygon :
;      Selection of points inside a polygon
;     -fleurdelas::extractTrees :
;      A working prototype method that extracts trees directly from the point cloud.
;      It uses a recursive approach
;
;    25 February 2014
;     -Improvment of version handling :
;      There was an issue in the header handling through the version in the number of point by returns
;     -fleurdelas::writeLAS :
;      using the index array as one block instead of loop on the block
;      and trying to locate the exact record.
;     -Removing all data members and associate methods referencing to the old writeLAS method
;     -Integration of fleurdelas::readVLR
;     -Integration of fleurdelas::readLAS
;     -Integration of fleurdelas::addGeoKey - WIP
;     -Improvement of fleurdelas::cleanup
;
;-



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
;   Obj->loadData, sting_of_fully_qualified_path [, /TILEDATA][, tileSize=tileSizeArray][ ,/[FILE|QUIET|LOG=/Path/To/Log]]
;
; :Examples:
;     setup the object
;       lasObj = obj_new('fleurdelas')
;     load a LAS file
;       lasObj->loadDataWithPath, '/path/to/the/file.LAS', /QUIET
;
; :Keywords:
;   inputFile : in, required, type=string
;     fully qualified path name of a las file
;   tileSize : in, optional, type=fltarr[2]
;     An array containing the tile size
;   tileData : in, optional, type=string
;     Flag to indicate that the data need to be tile
;   _ref_extra : in, optional, type=string
;     flag to setup the log output. It can be set as
;       -verbose : output all the message to the current console
;       -file : output all message to a log file
;       -quiet : turn off log information
;
; :Author:
;   Antoine Cottin
;-
Function fleurdelas::init, INPUTFILE=INPUTFILE, _EXTRA=LOGMODE

  ; Setting up Major and Minor version and associated release date of Fleurdelas
  self.fldVersionMajor = 0B
  self.fldVersionMinor = 80B
  self.fdlReleaseDate = 23112015
  
  ; Initializing console printing
  self.Out = Obj_new('consoleclass', _extra = logMode)

  ; Adding the possibility to initialize the object without an input file
  if keyword_set(INPUTFILE) then begin
    
    ; Setting up a temp directory to hold temporary informations
    ; cd into the project directory
    fleurdelasPath = File_dirname(Routine_filepath('fleurdelas__define', /either))
    Cd, fleurdelasPath
    Cd, '../'
    
    ; Adding some support for cross plateform compatibility
    if strlowcase(!version.os_family) eq 'unix' then begin
      Spawn, 'pwd', rootPth
      self.Rootpath = rootPth
      ; Create a temp file
      tempDirPath = './temp/'
      self.Tempdirpath = tempDirPath
      command = 'mkdir '+ tempDirPath
      Spawn, command
    endif else begin
      Spawn, 'cd', rootPth
      self.Rootpath = rootPth
      ; Create a temp file
      tempDirPath = rootPth + self.sysSep + 'temp'
      self.Tempdirpath = tempDirPath
      command = 'mkdir '+ tempDirPath
      Spawn, command
    endelse
    
    ;  CD, 'data/'
    dum = self.readLAS(INPUTFILE, header, dataStr)
    ; if dum eq 0 then it means that the file opening didn't worked...
    if dum eq 0 then return, 0
  
    self.Lasfilepath = INPUTFILE
    self.Lasheader = Ptr_new(header)
    self.Lasdatastr = Ptr_new(dataStr)
  
    if header.Nrecords ne 0 then begin
  
      dum = self.readVLR(INPUTFILE, header, vlrFileArr, vlrByteSizeArr, vlrId, vlrArr, self.Out)
      self.Vlrfileid = Ptr_new(vlrFileArr)
      self.Vlrbytesize = Ptr_new(vlrByteSizeArr)
      self.Vlrid = Ptr_new(vlrId)
      self.Vlrarr = Ptr_new(vlrArr)
  
    endif
  
    ; Set the geographical extent of the data member
    self.Lasdataextent = (self.getHeaderProperty(/BOUNDINGBOX))[0:3]
  ;  ; Set a byte array for selection purpose
  ;  self.Selectarray = Ptr_new(Bytarr(self.getHeaderProperty(/numberOfPoints)))
    ; Set a index array for the original index
    self.Lasdataindbackup = Ptr_new(Indgen(self.getHeaderProperty(/numberOfPoints), /UL64))
  
  ;  if keyword_set(TILESIZE) then begin
  ;    self.Xtile = tileSize[0]
  ;    self.Ytile = tileSize[1]
  ;  endif else begin
  ;    self.Xtile = 300.
  ;    self.Ytile = 300.
  ;  endelse
  ;
  ;  if keyword_set(TILEDATA) then begin
  ;    exist = self.lookingForFile(self.Lasfilepath, '.tid', tileFileName)
  ;    if exist eq 1 then begin
  ;      self.Lastilefilename = tileFileName
  ;      dum = self.readTile()
  ;    endif else begin
  ;      ; Load data into memory
  ;      dum = self.getData(/ALL)
  ;      dumt = self.tileData(customXTile = self.Xtile, customYTile = self.Ytile, box = self.Lasdataextent)
  ;    endelse
  ;  endif
  
  
    ; TODO: strip the file name and fine the survey day or ask for one
    ;self.surveyDay = surveyDay
  
    case header.Pointformat of
      0:self.Lasdatastrsz = 20
      1:self.Lasdatastrsz = 28
      2:self.Lasdatastrsz = 26
      3:self.Lasdatastrsz = 34
      4:self.Lasdatastrsz = 57
      5:self.Lasdatastrsz = 63
      6:self.Lasdatastrsz = 30
      7:self.Lasdatastrsz = 36
      8:self.Lasdatastrsz = 38
      9:self.Lasdatastrsz = 59
      10:self.Lasdatastrsz = 67
      
      
    endcase
    
    self.waveFileExt = 'wdp'
    
  Endif
  
  Return, 1

End



;+
; This function looks for a file based on the file name and a provided extension.
; It will returns 1 if the file exits and 0 if not.
;
; :Category:
;   GENERAL
;
; :Return:
;   Binary value, 1 the new file with that extention exist, 0 that new file doesn't exist
;
; :Uses:
;   dum = Obj.lookingForFile(filebasename, ext)
;
; :Example:
;   exist = self.lookingForFile(inFile, '.tid', tileFileName)
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
Function fleurdelas::lookingForFile, basename, ext, tileFileName

  if Strlowcase(!version.Os_name) eq "linux" or Strlowcase(!version.Os_name) eq "mac os x" then spath='/' else spath='\'
  sep=Strcompress(Strmid(spath, 0, 1,/reverse_offset))
  path = File_dirname(basename)
  file = File_basename(basename)
  tileFileName = Strcompress( path + spath + Strmid(file, 0, Strpos(file, '.', /reverse_search )) + ext)

  exist = File_test(tileFileName)

  Return, exist

End


;############################################################
;############################################################
;############################################################
;############################################################




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
;       lasObj = obj_new('fleurdelas')
;
;     Call for help on the object
;       lasObj->help
;
;-
Pro fleurdelas::Help

  Help, self, /objects
  Return

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
;       lasObj = obj_new('fleurdelas')
;
;     Destroying the object
;       obj_destroy, lasObj
;
;-
Pro fleurdelas::cleanup

  Compile_opt idl2, hidden
  
;PRINT, 'mthreadlock : ', self.mthreadlock
;  if self.Mthreadlock eq 0B then begin
    
  ; Removing the temporary files
  self.Out->print,1 , 'Destroying fleurdelas object...'
  self.Out->print,1 , 'Removing temporary files...'
  CD, self.rootPath
  ; Removing a temp file
  
  if strlowcase(!version.os_family) eq 'unix' then begin
    command = 'rm -r '+ self.tempDirPath
  spawn, command
  endif else begin
    command = 'del /F /Q '+ self.tempDirPath
  spawn, command
  endelse
  

  ; Removing a temp file
  if self.Mthreadlock eq 0 then begin
    command = 'rm -r '+ self.Tempdirpath
    Spawn, command
  endif else self.Out->print,1 , 'Cannot remove temporary file...'


  ; Freeing all data member pointers
  self.Out->print,1 , 'Cleaning memory...'
  Ptr_free, $
    self.lasHeader,$
    self.lasDataStr,$
    self.getDataIndex,$
    self.lasData,$
    self.lasDataIndBackup,$
;    self.lasDataTile,$
    self.lasWaveDsptrHdr,$
    self.lasWaveEvlrHeader,$
    self.lasWave,$
    self.vlrFileID,$
    self.vlrByteSize,$
    self.vlrId,$
    self.vlrArr
;    self.selectArray

  ; Destroying the consoleclass object
  self.Out->print,1 , 'Destroying remaining objects...'
  self.Out->print,1 , 'Bye :)'
  Obj_destroy, self.Out

;  endif
End




; Unload the data from object's data member
Function fleurdelas::cleanData

  self.Out->print,2 , 'Removing data from fleurdelas data member...'
  self.Lasdata = ptr_new(0)
  
  return, 1

End


;+
; Restore the original loaded data for the general index
;
; :Category:
;   LAS
;
; :Return:
;   1
;
; :Uses:
;   dum = lasobj.restore()
;
; :Example:
;   dum = lasobj.restore()
;
; :History:
;   Development history
;
; :Author: antoine
;-
Function fleurdelas::restoreData

  dum = self.getData(pointNumber = (*self.Lasdataindbackup))
  Return, 1

End




;+
; This procedure is obsolete
;
; :Returns:
;   Nothing
;
; :Hidden:
;-
Pro fleurdelas::loadWave

  ; Check if the waveforms are inside the LAS file if not find the wave file and open it


  ; Make sure that all packet size are the same size using stdev
  ; Make sure that all the waveform are consecutive
  ; point lun to first record and readu :: ((*(self.lasData)).offsetWaveData)

  Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian

  ; Checking if all the packet have the same size
  ; A consecutive order equal to the point one is assumed if all the offsetWaveData fields are the same
  ;checkSize = (((*(self.lasData)).wpacketsize)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).wpacketsize)[*(self.getDataIndex)], -1))
  ;if total(checkSize) eq 0.0 then packetSize = ((*(self.lasData)).wpacketsize)[0]
  ;checkStart = (((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)], -1))
  ;if total(checkStart) eq 0.0 then waveStart = ((*(self.lasData)).offsetWaveData)[0]

  packetSize = ((*(self.Lasdata)).Wpacketsize)[0]
  waveStart = ((*(self.Lasdata)).Offsetwavedata)[0]

  tempWaveStr = {waveStruc,wave:Bytarr(256)}
  tempWave = Replicate(tempWaveStr,N_elements(*(self.Getdataindex)))
  Point_lun, inputLun, ((*(self.Lasheader)).Startwaveform + waveStart + (Long((*(self.Getdataindex))[0]) * Long(packetSize)))
  Readu, inputLun, tempWave



  Free_lun, inputLun

  self.Laswave = Ptr_new(tempWave)


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
;       lasObj = obj_new('fleurdelas')
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
Function fleurdelas::lasPointFieldSelector, tempData, field = field

;  if n_elements(tempData) eq 0  then tempData = *(self.lasData)
;  
;  if keyword_set(field) eq 0 then keyList = strlowcase(tag_names(tempData)) else keyList = strlowcase(field)
;  tagList = strlowcase(tag_names(tempData))
;  nkeyList = n_elements(keyList)
;  
;  ;/allField
;  
;  flag = 0
;  
;  for o=0,nKeyList-1,1 do begin
;  
;    validCheck = where(keyList[o] eq tagList, validCount)
;  
;      if validCount ne 0 then begin
;  
;        if flag eq 0 then begin
;  
;          data = create_struct(tagList[validCheck],tempData.(validCheck))
;          flag = 1
;  
;        endif else begin
;  
;          data = create_struct(data, tagList[validCheck],tempData.(validCheck))
;  
;        endelse
;  
;      endif else begin
;  
;        self.out->print,2, strcompress("Keyword " + string(keyList[o]) + " not found into point structure tags name...")
;        self.out->print,2, strcompress("Ignoring " + string(keyList[o]) + " keyword...")
;        ;return, 0
;  
;      endelse
;  
;  endfor

  data = tempData

  self.out->print,1, "Return Point Structure description:"
  self.out->printArray,1, Tag_names(data)
  Return, data


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
;       lasObj = obj_new('fleurdelas')
;
;     Load the 5th flightline of survey from December 5th 2003:
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get the path and name of the loaded file:
;       Result = lasObj->getLoadFileName()
;
;-
Function fleurdelas::getLoadFileName

  Return, self.Lasfilepath

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
;     lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getData, BOUNDINGBOX = B, POINTNUMBER=V, ALL=ALL, $
                              XBOUND = XBOUND, YBOUND = YBOUND, ZBOUND = ZBOUND, $
                              MAX = MAX, MIN = MIN, $
                              NOUPDATE = NOUPDATE, OUTPUTID = OUTPUTID, $
                              _REF_EXTRA=EX
                              
  ; start time
  T = Systime(1)

;  Openr, getDataLun, self.Lasfilepath, /get_lun, /swap_if_big_endian
  
  if ptr_valid(self.lasdata) then begin
    
    tempData = self.getXYZ()
    tempDataStr = *(self.lasdata)
    
  endif else begin
    
    ; Retriving the data packet
    openr, getDataLun, self.lasFilePath, /get_lun, /swap_if_big_endian
    tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
    Point_lun, getDataLun, (*(self.Lasheader)).Dataoffset
    Readu, getDataLun, tempDataStr
    free_lun, getDataLun
      
  endelse

  if N_elements(b) ne 0 then begin

    self.Out->print,1, "Reading the data into memory..."
    
    tempData = Fltarr((*(self.Lasheader)).Npoints,3)
    tempData[*,0] = (tempDataStr.East * (*(self.Lasheader)).Xscale) + (*(self.Lasheader)).Xoffset
    tempData[*,1] = (tempDataStr.North * (*(self.Lasheader)).Yscale) + (*(self.Lasheader)).Yoffset
    tempData[*,2] = (tempDataStr.Elev * (*(self.Lasheader)).Zscale) + (*(self.Lasheader)).Zoffset
      
    ; checking the that boundingBox is correct and determine which bounding box type is it: geographic or elevation
    if (N_elements(b) eq 6) or (N_elements(b) eq 4) or (N_elements(b) eq 2) or (N_elements(b) eq 1) then begin

      case N_elements(b) of
        6:begin

          self.Out->print,1,"Filtering data by coordinates..."
  
          ; setting object data extent
          self.Lasdataextent = double(b[0:3])
  
          index = Where((tempData[*,0] le b[0]) and (tempData[*,0] ge b[1]) and $
                        (tempData[*,1] le b[2]) and (tempData[*,1] ge b[3]) and $
                        (tempData[*,2] le b[4]) and (tempData[*,2] ge b[5]), indexCount)
  
          if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0
  
          if (Size(data))[2] ne 8 then $
            self.Out->print,2, "Nothing return !" else $
            self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))
  
          self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

        end
      
        4:begin

        self.Out->print,1,"Filtering data by coordinates..."

        ; setting object data extent
        self.Lasdataextent = b

        index = Where((tempData[*,0] le b[0]) and (tempData[*,0] ge b[1]) and $
          (tempData[*,1] le b[2]) and (tempData[*,1] ge b[3]), indexCount)

        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

        if (Size(data))[2] ne 8 then $
          self.Out->print,2, "Nothing return !" else $
          self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

        self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

      end

      2:begin
      
      case 1 of
        ; If XBOUND is set => keeping all points between these two values
        Keyword_set(XBOUND): begin

          self.Out->print, 1,"Filtering data by X/Easting dimension..."
          index = Where((tempData[*,0] le B[1]) and (tempData[*,0] ge B[0]), $
            indexCount, /NULL)
          if index ne !NULL then data = tempDataStr[index] else data = !NULL

        end
        ; If YBOUND is set => keeping all points between these two values
        Keyword_set(YBOUND): begin

          self.Out->print, 1,"Filtering data by Y/Northing dimension..."
          index = Where((tempData[*,1] le B[1]) and (tempData[*,1] ge B[0]), $
            indexCount, /NULL)
          if index ne !NULL then data = tempDataStr[index] else data = !NULL

        end
        ; If ZBOUND is set => keeping all points between these two values
        Keyword_set(ZBOUND): begin

          self.Out->print, 1,"Filtering data by Z/Height dimension..."
          index = Where((tempData[*,2] le B[1]) and (tempData[*,2] ge B[0]), $
            indexCount, /NULL)
          if index ne !NULL then data = tempDataStr[index] else data = !NULL

        end
        ; If no associated keyword is set, then the altitude is considered and
        ; all the points lying within the two altitude are kept
        ELSE: begin

          self.Out->print, 1,"No Keyword set -> Filtering data by Z/Height dimension..."
          index = Where((tempData[*,2] le B[1]) and (tempData[*,2] ge B[0]), $
            indexCount, /NULL)
          if index ne !NULL then data = tempDataStr[index] else data = !NULL

        end

      endcase
      
;      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

      if (Size(data))[2] ne 8 then $
        self.Out->print,2, "Nothing return !" else $
        self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

      self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')


    end

    1:Begin

    if Keyword_set(max) then begin

      ;print, "Not scale and translate value:", ((b[0] - (*(self.lasHeader)).zoffset) / (*(self.lasHeader)).zscale)

      index = Where((tempData[*,2] le b[0]), indexCount)

      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

      if (Size(data))[2] ne 8 then $
        self.Out->print,2, "Nothing return !" else $
        self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

      self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    endif else begin

      if Keyword_set(min) then begin

        index = Where((tempData[*,2] ge b[0]), indexCount)

        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

        if (Size(data))[2] ne 8 then $
          self.Out->print,2, "Nothing return !" else $
          self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

        self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

      endif

    endelse

  end

endcase

endif else begin
  Free_lun, getDataLun, /FORCE
  Return, 0
endelse

tempData = 0

endif;

; pointNumber parameter set -> returning one point selected by it number
if N_elements(v) eq 1 then begin

  if ptr_valid(self.lasdata) eq 0 then begin
    
    Openr, getDataLun, self.Lasfilepath, /get_lun, /swap_if_big_endian
    tempData = Assoc(getDataLun, *(self).Lasdatastr, (*(self.Lasheader)).dataOffset , /packed)
    free_lun, getDataLun
    
  endif else tempData = (*(self.lasdata))[v]
  ;    print, 'data once read: ', (tempData[v]).time
  data = self->lasPointFieldSelector( tempData )

  index = v

  ;self->loadWave

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif

; pointNumber parameter set to interval [lowerLimit,upperLimit] -> returning the points within the interval
if N_elements(v) eq 2 then begin

  numbElements = v[1] - v[0]
  ;
  ;      tempData = replicate(*(self).lasDataStr, numbElements)
  ;      print, 'pointer position: ' ,( (*(self.lasHeader)).dataOffset + (long(self.lasDataStrSz) * long(v[0]) ) )
  ;      point_lun, getDataLun, ( (*(self.lasHeader)).dataOffset) + (long(self.lasDataStrSz) * long(v[0]))
  ;      readu, getDataLun, tempData
  ;      print, 'data once read: ', (tempData[0:10]).time
  ;      data = self->lasPointFieldSelector(tempData, _ref_extra=ex, self.out)
  ;      ;data = tempData
  ;      print, 'data after self->lasPointFieldSelector: ', (data[0:10]).time

  index = Lindgen(numbElements) + v[0]

  if ptr_valid(self.lasdata) eq 0 then tempData = self->getData(/all)
  dum = (*(self.lasdata))[v]
  ;print, dum
  data = self->lasPointFieldSelector(dum)

  self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(numbElements))
  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif

; pointNumber parameter set to an index array [index_array] -> returning the points corresponding to the index
if N_elements(v) gt 2 then begin

  numbElements = N_elements(v)

  ;###################################################################
  ; Have been commented for testing - To be remove
  ;     for n=0,numbElements-1,1 do begin
  ;    n = 0L
  ;    while n lt numbElements do begin
  ;
  ;      if n eq 0 then begin
  ;
  ;        tempDataStr = *(self).lasDataStr ;replicate(*(self).lasDataStr, numbElements)
  ;        point_lun, getDataLun, ( (*(self.lasHeader)).dataOffset + (long(v[n]) * long(self.lasDataStrSz)) )
  ;        readu, getDataLun, tempDataStr
  ;        dum = tempDataStr
  ;
  ;      endif else begin
  ;
  ;        point_lun, getDataLun, ( (*(self.lasHeader)).dataOffset + (long(v[n]) * long(self.lasDataStrSz)) )
  ;        readu, getDataLun, tempDataStr
  ;        dum = [dum,tempDataStr]
  ;
  ;      endelse
  ;    n+=1
  ;    endwhile
  ;;    endfor
  ;###################################################################


  ; Much more efficient way to retreive points via INDEX array
  ;A = ASSOC(getDataLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset)
  if ptr_valid(self.lasdata) eq 0 then tempData = self->getData(/all)
  dum = (*(self.lasdata))[v]
  ;print, dum

  ;print, 'n elements: ', n_elements(dum)
  data = self->lasPointFieldSelector(dum)
;  data = dum
  index = v

  self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(numbElements))
  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif



; keyword /all set -> returning all the points of the LAS file
if Keyword_set(all) then begin

  self.Out->print,1,"Formating data..."

  if not ptr_valid(self.lasData) then begin
  ; Retriving the data packet
  openr, getDataLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = Replicate(*(self.Lasdatastr), (*(self.Lasheader)).Npoints)
  Point_lun, getDataLun, (*self.Lasheader).Dataoffset
  Readu, getDataLun, tempDataStr
  free_lun, getDataLun
  data=tempDataStr
  endif else data = *(self.lasData)
  
  ;data = self->lasPointFieldSelector(tempDataStr)

  index = Lindgen((*(self.Lasheader)).Npoints)

  if (Size(data))[2] ne 8 then $
    self.Out->print,2,"Nothing return !" else $
    self.Out->print,1,Strcompress("Number of point record(s) returned: " + String((*(self.Lasheader)).Npoints))

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif


; Updating the selection index table and the data structure array into object
if not keyword_set(NOUPDATE) then begin
  self.Lasdata = Ptr_new(data)
  self.Getdataindex = Ptr_new(index)
endif else begin
  if keyword_set(OUTPUTID) then return, index else return, data
endelse
;; Updating internal selection flag
;; First clearing previous selection
;dum = self.resetselectArray()
;; Then update the matrix with selection
;dum = self.updateSelectArray(index)

; Returning requested data
;close, getDataLun, EXIT_STATUS=exVal, /FORCE

;Free_lun, getDataLun, EXIT_STATUS=exVal, /FORCE
Return, data


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
;     lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getDataFromSelectedData, BOUNDINGBOX = B, POINTNUMBER=V, ALL=ALL, $
                                              XBOUND = XBOUND, YBOUND = YBOUND, ZBOUND = ZBOUND, $
                                              MAX = MAX, MIN = MIN, $
                                              NOUPDATE = NOUPDATE, OUTPUTID = OUTPUTID, $
                                              _REF_EXTRA=EX

  ; start time
  T = Systime(1)

  Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian

  if N_elements(b) ne 0 then begin

    self.Out->print,1, "Reading the data into memory..."

;    ; Retriving the data packet
;    ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
;    tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
;    Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
;    Readu, inputLun, tempDataStr
;    ;close, inputLun
;    tempData = Fltarr((*(self.Lasheader)).Npoints,3)
;    tempData[*,0] = (tempDataStr.East * (*(self.Lasheader)).Xscale) + (*(self.Lasheader)).Xoffset
;    tempData[*,1] = (tempDataStr.North * (*(self.Lasheader)).Yscale) + (*(self.Lasheader)).Yoffset
;    tempData[*,2] = (tempDataStr.Elev * (*(self.Lasheader)).Zscale) + (*(self.Lasheader)).Zoffset
     
     tempDataStr = self.lasdata
     tempData = self.getxyz()

    ; checking the that boundingBox is correct and determine which bounding box type is it: geographic or elevation
    if (N_elements(b) eq 4) or (N_elements(b) eq 2) or (N_elements(b) eq 1) then begin

      case N_elements(b) of
        4:begin

        self.Out->print,1,"Filtering data by coordinates..."

        ; setting object data extent
        self.Lasdataextent = b

        index = Where((tempData[*,0] le b[0]) and (tempData[*,0] ge b[1]) and $
          (tempData[*,1] le b[2]) and (tempData[*,1] ge b[3]), indexCount)

        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

        if (Size(data))[2] ne 8 then $
          self.Out->print,2, "Nothing return !" else $
          self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

        self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

      end

      2:begin

      self.Out->print,1, "Filtering data by heights..."

      index = Where((tempData[*,2] le b[0]) and (tempData[*,2] ge b[1]), indexCount)

      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

      if (Size(data))[2] ne 8 then $
        self.Out->print,2, "Nothing return !" else $
        self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

      self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    end

    1:Begin

    if Keyword_set(max) then begin

      ;print, "Not scale and translate value:", ((b[0] - (*(self.lasHeader)).zoffset) / (*(self.lasHeader)).zscale)

      index = Where((tempData[*,2] le b[0]), indexCount)

      if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

      if (Size(data))[2] ne 8 then $
        self.Out->print,2, "Nothing return !" else $
        self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

      self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    endif else begin

      if Keyword_set(min) then begin

        index = Where((tempData[*,2] ge b[0]), indexCount)

        if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

        if (Size(data))[2] ne 8 then $
          self.Out->print,2, "Nothing return !" else $
          self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

        self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

      endif

    endelse

  end

endcase

endif else Return, 0

tempData = 0

endif;

; pointNumber parameter set -> returning one point selected by it number
if N_elements(v) eq 1 then begin

  tempData = Assoc(inputLun, *(self).Lasdatastr, (*(self.Lasheader)).dataOffset , /packed)
  ;    print, 'data once read: ', (tempData[v]).time
;  data = self->lasPointFieldSelector((tempData[v]), _ref_extra=ex, self.Out)
  data = self->lasPointFieldSelector(tempData[v])
  ;    print, 'data after lasPointFieldSelector: ', (data).time
  index = v

  ;self->loadWave

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif

; pointNumber parameter set to interval [lowerLimit,upperLimit] -> returning the points within the interval
if N_elements(v) eq 2 then begin

  numbElements = v[1] - v[0]
  ;
  ;      tempData = replicate(*(self).lasDataStr, numbElements)
  ;      print, 'pointer position: ' ,( (*(self.lasHeader)).dataOffset + (long(self.lasDataStrSz) * long(v[0]) ) )
  ;      point_lun, inputLun, ( (*(self.lasHeader)).dataOffset) + (long(self.lasDataStrSz) * long(v[0]))
  ;      readu, inputLun, tempData
  ;      print, 'data once read: ', (tempData[0:10]).time
  ;      data = self->lasPointFieldSelector(tempData, _ref_extra=ex, self.out)
  ;      ;data = tempData
  ;      print, 'data after self->lasPointFieldSelector: ', (data[0:10]).time

  index = Lindgen(numbElements) + v[0]

  dum = (*self.lasdata)[index]
  data = self->lasPointFieldSelector(dum)

  self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(numbElements))
  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif

; pointNumber parameter set to an index array [index_array] -> returning the points corresponding to the index
if N_elements(v) gt 2 then begin

  numbElements = N_elements(v)

  ;###################################################################
  ; Have been commented for testing - To be remove
  ;     for n=0,numbElements-1,1 do begin
  ;    n = 0L
  ;    while n lt numbElements do begin
  ;
  ;      if n eq 0 then begin
  ;
  ;        tempDataStr = *(self).lasDataStr ;replicate(*(self).lasDataStr, numbElements)
  ;        point_lun, inputLun, ( (*(self.lasHeader)).dataOffset + (long(v[n]) * long(self.lasDataStrSz)) )
  ;        readu, inputLun, tempDataStr
  ;        dum = tempDataStr
  ;
  ;      endif else begin
  ;
  ;        point_lun, inputLun, ( (*(self.lasHeader)).dataOffset + (long(v[n]) * long(self.lasDataStrSz)) )
  ;        readu, inputLun, tempDataStr
  ;        dum = [dum,tempDataStr]
  ;
  ;      endelse
  ;    n+=1
  ;    endwhile
  ;;    endfor
  ;###################################################################


  ; Much more efficient way to retreive points via INDEX array
  ;A = ASSOC(inputLun, *(self).lasDataStr, (*(self.lasHeader)).dataOffset)
  dum = (*(self.lasdata))[v]
  data = self->lasPointFieldSelector(dum)
  index = v

  self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(numbElements))
  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif



; keyword /all set -> returning all the points of the LAS file
if Keyword_set(all) then begin

  self.Out->print,1,"Formating data..."

  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
  Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
  Readu, inputLun, tempDataStr

  ;data = self->lasPointFieldSelector(tempDataStr, _ref_extra=ex, self.out)
  data=tempDataStr

  index = Lindgen((*(self.Lasheader)).Npoints)

  if (Size(data))[2] ne 8 then $
    self.Out->print,2,"Nothing return !" else $
    self.Out->print,1,Strcompress("Number of point record(s) returned: " + String((*(self.Lasheader)).Npoints))

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

endif

Free_lun, inputLun, /FORCE


; Updating the selection index table and the data structure array into object
if not keyword_set(NOUPDATE) then begin
  self.Lasdata = Ptr_new(data)
  self.Getdataindex = Ptr_new(index)
endif else begin
  if keyword_set(OUTPUTID) then Return, index else Return, data
endelse

Return, 1

End









;+
; This function returns the start and end time of a LAS file or the loaded data.
; Time usually express in GPS time.
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
;       lasObj = fleurdelas(inputfile='/path/to/file')
;
;     Get the start & end time of the flightline:
;       Result = lasObj->getDataTime()
;
;-
Function fleurdelas::getDataTime

  if ptr_valid(self.lasData) then begin
    
    self.Out->print,1, "Retreiving time from data member..."
    minTime=Min((*self.Lasdata).Time, max=maxTime)  
    
  endif else begin
    
    self.Out->print,2, "No data member..."
    self.Out->print,2, "Reading the data into memory..."
    Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian
    self.Out->print,1, "Retreiving time from data into memory..."
    tempData = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
    Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
    Readu, inputLun, tempData

    minTime=Min(tempData.Time, max=maxTime)

  endelse
  
  Return, [minTime, maxTime]
  
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
;       lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getDataFromClass, class=cl, outputId=outputId

  ; start time
  T = Systime(1)

  Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data into memory..."

  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
  Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
  Readu, inputLun, tempDataStr

  self.Out->print,1,"Filtering data by classification..."

  stringClass = ClassificationID(cl)

  self.Out->print,1,"Filtering " + Strcompress( stringClass ) + "..."

  cache = 16b + 8b + 4b + 2b + 1b
  myClassification = tempDataStr.Class
  result = cache and myClassification

  index = Where( result eq cl, indexCount )

  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

  if (Size(data))[2] ne 8 then $
    self.Out->print,2, "Nothing return !" else $
    self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

  ; Returning requested data
  Free_lun, inputLun
  if Keyword_set(outputid) then  begin
    self.Out->print,1, "The returned records are point's index..."
    Return, index
  endif else begin
    self.Out->print,1, "The returned records are point's data..."
    Return, data
  endelse



End



;+
; This function returns the number of point(s) hold in the data object.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A scalar value.
;
; :Uses:
;   Result=Obj->getSelectedDataNumberOfPoints()
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('fleurdelas')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to Ground:
;       Result = lasObj->getSelectedDataNumberOfPoints()
;
;-
Function fleurdelas::getSelectedDataNumberOfPoints

  Return, n_elements( *(self.lasData) )
  
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
;       lasObj = obj_new('fleurdelas')
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
;     A named variable that will hold the point's selection index
;   others : in, optional, type=boolean
;     A named variable that will hold the others points for the selection - complementary points
;   othersId : in, optional, type=boolean
;     A named variable that will hold the complementary point's selection index
;
; :History:
;   Antoine Cottin, February 2015: modifying the keywords in order to get everything into one call
;               which avoid the need to make multiple calls...
;-
Function fleurdelas::getSelectedDataByClass, $
                                CLASS = CL, $
                                OUTPUTID = OUTPUTID, $
                                SELECTINDEX = SELECTINDEX, $
                                OTHERS = OTHERS, $
                                POINTSCOMPSELECT = POINTSCOMPSELECT, $
                                INDEXOTHERS = INDEXOTHERS, $
                                INDEXCOMPSELECT = INDEXCOMPSELECT
                                
  ; start time
  T = Systime(1)

  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 then begin

    tempDataStr = (*(self.Lasdata))

    self.Out->print,1,"Filtering data by classification..."

    for d = 0, n_elements(cl)-1, 1 do begin
        
      stringClass = ClassificationID(cl[d])
  
      self.Out->print,1,"Filtering " + Strcompress( stringClass ) + "..."
  
  ;    ; Extracting the following bits
  ;    ; '00011111'
  ;    cache = 16b + 8b + 4b + 2b + 1b
  ;    myClassification = tempDataStr.Class
  ;    result = cache and myClassification
  
      result = tempDataStr.Class
      
      ind = Where( tempDataStr.Class eq cl[d], indexC, complement = compL, ncomplement = ncompL,  /NULL)
      
      if d eq 0 then begin
        index = ind
        indexCount = indexC
        comp = compL
        ncomp = ncompL
      endif else begin
        index = [index, ind]
        indexCount += indexC
        comp = [comp, compL]
        ncomp += ncompL
      endelse
      
    endfor

    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0
    
    if Size(data, /TYPE) ne 8 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).
    
    ; Returning complementary index
    if Keyword_set(INDEXOTHERS)then begin
      self.Out->print,1, "The returned records are point's index..."
      INDEXCOMPSELECT = comp
    endif
    
    if Keyword_set(OTHERS) then  begin
      if ncomp ne 0 then compData = self->lasPointFieldSelector(tempDataStr(comp)) else compData = 0
      self.Out->print,1, "The returned records are point's data..."
      POINTSCOMPSELECT = compData
    endif
    
    if Keyword_set(OUTPUTID) then  begin
      self.Out->print,1, "The returned records are point's data and the point index are stored in OUTPUTID variable..."
      SELECTINDEX = index
    endif
    
    Return, data
;    OUTPUTID = OUTPUTID, $
;      SELECTINDEX = SELECTINDEX, $
;      POINTSCOMPSELECT = POINTSCOMPSELECT, $
;      INDEXOTHERS = INDEXOTHERS, $
;      INDEXCOMPSELECT = INDEXCOMPSELECT


  endif else Return, 0



End



;+
; This function returns a point selection base on USER field from the points loaded in memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getSelectedDataByClass(VALUE=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('fleurdelas')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to Ground:
;       Result = lasObj->getSelectedDataByUser(VALUE=2)
;
; :Keywords:
;   value : in, required, type=integer
;     if value is a scalar then the keyword /MIN or /MAX is required.
;     If value is a 2 elements array, they will represent an interval
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function fleurdelas::getSelectedDataByUser, VALUE = VALUE, outputId=outputId, MIN = MIN, MAX = MAX

  ; start time
  T = Systime(1)

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 then begin

    self.Out->print,1,"Filtering data by classification..."
    
    if n_elements(VALUE) eq 1 then begin
      
      if keyword_set(MAX) then begin
        
         self.Out->print,1,"Filtering by User field " + Strcompress( VALUE ) + "..."
         index = Where( (*(self.Lasdata)).User le value, indexCount, /NULL)
         
      endif
      
      if Keyword_set(MIN) then begin

        self.Out->print,1,"Filtering by User field " + Strcompress( VALUE ) + "..."
        index = Where( (*(self.Lasdata)).User ge value, indexCount, /NULL)

      endif

    endif
    
    
    if N_elements(VALUE) eq 2 then begin

        self.Out->print,1,"Filtering by User values between " + Strcompress( VALUE[0] ) + " and " + Strcompress( VALUE[1] ) +  "..."
        index = Where( (*(self.Lasdata)).User ge value[0] and (*(self.Lasdata)).User le value[1], indexCount, /NULL)

    endif
    
;    ; This has been disable as produce an out of memory issue !? 
;    if indexCount ne !NULL then data = self->lasPointFieldSelector((*(self.Lasdata))[index]) else data = 0

    if (Size(data))[2] ne 8 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).

    if Keyword_set(outputid) then  begin
      self.Out->print,1, "The returned records are point's index..."
      Return, index
    endif else begin
      self.Out->print,1, "The returned records are point's data..."
      Return, data
    endelse

  endif else Return, 0



End




;+
; This function returns a point selection base on USER field from the points loaded in memory.
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   A lonarr(n) or a array of structure.
;
; :Uses:
;   Result=Obj->getSelectedDataByClass(VALUE=integer)
;
; :Examples:
;
;     Setup the object:
;       lasObj = obj_new('fleurdelas')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
;
;     Get all the points corresponding to Ground:
;       Result = lasObj->getSelectedDataByUser(VALUE=2)
;
; :Keywords:
;   value : in, required, type=integer
;     if value is a scalar then the keyword /MIN or /MAX is required.
;     If value is a 2 elements array, they will represent an interval
;   outputId : in, optional, type=boolean
;     If set, the return is a `lonarr(n)` of the index of the selected points.
;     If not set, the return is an `array of structure` of the selected points.
;
;-
Function fleurdelas::getDataByUser, VALUE = VALUE, outputId=outputId, MIN = MIN, MAX = MAX

  ; start time
  T = Systime(1)

  Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data into memory..."
  dum = self.getData(/ALL)

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 then begin

    self.Out->print,1,"Filtering data by classification..."

    if N_elements(VALUE) eq 1 then begin

      if Keyword_set(MAX) then begin

        self.Out->print,1,"Filtering by User <= " + Strcompress( VALUE ) + "..."
        index = Where( (*(self.Lasdata)).User le value, indexCount, /NULL)

      endif

      if Keyword_set(MIN) then begin

        self.Out->print,1,"Filtering by User >= " + Strcompress( VALUE ) + "..."
        index = Where( (*(self.Lasdata)).User ge value, indexCount, /NULL)

      endif

    endif


    if N_elements(VALUE) eq 2 then begin

      self.Out->print,1,"Filtering by User values between " + Strcompress( VALUE[0] ) + " and " + Strcompress( VALUE[1] ) +  "..."
      index = Where( (*(self.Lasdata)).User ge value[0] and (*(self.Lasdata)).User le value[1], indexCount, /NULL)

    endif

    ;    ; This has been disable as produce an out of memory issue !?
    ;    if indexCount ne !NULL then data = self->lasPointFieldSelector((*(self.Lasdata))[index]) else data = 0
    if indexCount ne !NULL then data = (*(self.Lasdata))[index] else data = 0
    
    ; Unload the data from object's data member
    dum = self.cleanData()
    
    if (Size(data))[2] ne 8 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).

    if Keyword_set(outputid) then  begin
      self.Out->print,1, "The returned records are point's index..."
      Return, index
    endif else begin
      self.Out->print,1, "The returned records are point's data..."
      Return, data
    endelse

  endif else Return, 0



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
;       lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getSelectedDataByReturnNumber, $
                      FIRST = FIRST, LAST = LAST, RETURN_NUMBER = RTNB, $
                      OUTPUTID=OUTPUTID, SELECTINDEX = SELECTINDEX, NUMBEROFPOINTSBYRETURN = NUMBEROFPOINTSBYRETURN
; to commit
; 
  ; start time
  T = Systime(1)

  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data from memory..."

  if (Size((*(self.Lasdata)),/n_element)) ne 0 then begin

    tempDataStr = (*(self.Lasdata))

    self.Out->print,1,"Filtering data by return number..."

;    self.Out->print,1,"Filtering number " + Strcompress( rtnb ) + "..."
;
;    ; Extracting the following bits
;    ; '00000111'
;    cache = 4b + 2b + 1b
;    tempReturnNumber = tempDataStr.Nreturn
;    result = cache and tempReturnNumber
;
;    index = Where( result eq rtnb, indexCount )

    ; Extracting bits '00001111'
    cache = bitMaskFromPoint((*(self.Lasheader)).(16), 'returnnumber')
    if (*(self.Lasheader)).(16) le 5 then tempReturnNumber = tempDataStr.Nreturn else tempReturnNumber = (tempDataStr.Nreturn)[0,*]
    
    
    if Keyword_set(FIRST) then begin
      self.Out->print,1,"Filtering the first return..."
      result = cache and tempReturnNumber
      index = Where( result eq 1, indexCount )
      
      result = 0
      tempReturnNumber = 0
      cache = 0
      
    endif
    
    if Keyword_set(LAST) then begin
      self.Out->print,1,"Filtering the last return..."
      ; Extracting the following bits
      result = cache and tempReturnNumber
      dcache = bitMaskFromPoint((*(self.Lasheader)).(16), 'numberofreturn')
      dresult = Ishft(dcache and tempReturnNumber, -4)
    
      index = Where( result eq dresult, indexCount )
    
      result = 0
      dresult = 0
      tempReturnNumber = 0
      cache = 0
      dcache = 0
      
    endif
    
    if Keyword_set(FIRST) eq 0 and Keyword_set(LAST) eq 0 then begin
      self.Out->print,1,"Filtering number " + Strcompress( rtnb ) + "..."
      result = cache and tempReturnNumber
      index = Where( result eq rtnb, indexCount )
      
;      result = 0
      
    endif
    
;    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0
;    if (Size(data))[2] ne 8 then $
    if indexCount eq 0 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).
    if keyword_set(numberOfPointsByReturn) then Return, indexCount
    
    if Keyword_set(outputid) then begin
      self.Out->print,1, "The returned records are point's index..."
      SELECTINDEX =  index
    endif
    
    Return, tempDataStr[index]

  endif else Return, 0



End



Function fleurdelas::getSelectedDataByIndex, INDEX = INDEX, XYZ = XYZ

  ; start time
  T = Systime(1)

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 then begin

;    tempDataStr = (*(self.Lasdata))

    self.Out->print,1,"Filtering data by index..."

    tempReturnNumber = (*(self.Lasdata))[INDEX]


    if Keyword_set(FIRST) then begin
      self.Out->print,1,"Filtering the first return..."
      result = cache and tempReturnNumber
      index = Where( result eq 1, indexCount )

      result = 0
      tempReturnNumber = 0
      cache = 0

    endif



    ;    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0
    ;    if (Size(data))[2] ne 8 then $
    if indexCount eq 0 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')


    if Keyword_set(outputid) then  begin
      self.Out->print,1, "The returned records are point's index..."
      Return, index
    endif else begin
      self.Out->print,1, "The returned records are point's data..."
      Return, data
    endelse

  endif else Return, 0



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
;       lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getDataFromReturnNumber, RETURN_NUMBER=RTNB, $
                                              FIRST = FIRST, LAST = LAST, $
                                              POINTSELECTION = POINTSELECTION, $
                                              NOUPDATE = NOUPDATE, OUTPUTID = OUTPUTID

  ; start time
  T = Systime(1)

  

  self.Out->print,1, "Reading the data into memory..."

  if keyword_set(POINTSELECTION) then begin
    tempDataStr = POINTSELECTION
  endif else begin
    if ptr_valid(self.lasdata) then begin
      tempDataStr = *(self.lasData)
    endif else begin
      Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian
      tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
      Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
      Readu, inputLun, tempDataStr
      Free_lun, inputLun
    endelse
  endelse
  
  self.Out->print,1,"Filtering data by return number..."


  ; Extracting bits '00001111'
  cache = bitMaskFromPoint((*(self.lasHeader)).(16), 'returnnumber')
  if (*(self.lasHeader)).(16) le 5 then tempReturnNumber = tempDataStr.Nreturn else tempReturnNumber = (tempDataStr.Nreturn)[0,*]
 

  if keyword_set(FIRST) then begin
    self.Out->print,1,"Filtering the first return..."
    result = cache and tempReturnNumber
    index = Where( result eq 1, indexCount )
  endif
  
  if Keyword_set(LAST) then begin
    self.Out->print,1,"Filtering the last return..."
    ; Extracting the following bits
    result = cache and tempReturnNumber
    dcache = bitMaskFromPoint((*(self.lasHeader)).(16), 'numberofreturn')
    dresult = ishft(dcache and tempReturnNumber, -4)

    index = Where( result eq dresult, indexCount )

  endif
  
  if Keyword_set(FIRST) eq 0 and Keyword_set(LAST) eq 0 then begin
    self.Out->print,1,"Filtering number " + Strcompress( rtnb ) + "..."
    index = Where( result eq rtnb, indexCount )
  endif


  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

  if (Size(data))[2] ne 8 then $
    self.Out->print,2, "Nothing return !" else $
    self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

  ; Returning requested data
;
;  if Keyword_set(outputid) then  begin
;    self.Out->print,1, "The returned records are point's index..."
;    Return, index
;  endif else begin
;    self.Out->print,1, "The returned records are point's data..."
;    Return, data
;  endelse


  ; Updating the selection index table and the data structure array into object
  if not keyword_set(NOUPDATE) then begin
    self.Lasdata = Ptr_new(data)
    self.Getdataindex = Ptr_new(index)
  endif else begin
    if keyword_set(OUTPUTID) then Return, index else Return, data
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
;       lasObj = obj_new('fleurdelas')
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
Function fleurdelas::getSelectedDataByNumberOfReturns, return_number=rtnb, outputId=outputId

  ; start time
  T = Systime(1)

  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 then begin

    tempDataStr = (*(self.Lasdata))

    self.Out->print,1,"Filtering data by return number..."

    self.Out->print,1,"Filtering number " + Strcompress( rtnb ) + "..."

    ; Extracting the following bits
    ; '00000111'
    cache = 8b + 16b + 32b
    tempNumberOfReturns = tempDataStr.Nreturn
    result = cache and tempNumberOfReturns

    index = Where( result eq rtnb, indexCount )

    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

    if (Size(data))[2] ne 8 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).

    if Keyword_set(outputid) then  begin
      self.Out->print,1, "The returned records are point's index..."
      Return, index
    endif else begin
      self.Out->print,1, "The returned records are point's data..."
      Return, data
    endelse

  endif else Return, 0



End




Function fleurdelas::removeOffPoints, tileID, outputId=outputId

  ; start time
  T = Systime(1)

  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data from memory..."

  if Size((*(self.Lasdata)),/dimensions) ne 0 or Ptr_valid(self.Lasdatatile) eq 1 then begin

    tempDataStr = (*(self.Lasdata))

    self.Out->print,1,"Filtering data by OFF tile index..."

    index = Where( (*(*self.Lasdatatile)[tileID].Off) eq 1, indexCount, COMPLEMENT = comp, /NULL )

    if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(comp), _ref_extra=ex, self.Out) else data = 0

    if (Size(data))[2] ne 8 then $
      self.Out->print,2, "Nothing return !" else $
      self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

    self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

    ; Returning requested data
    ; The keyword outputid have been setup to avoid the automatic update of the data that would occure in the following case:
    ; That this function returns an index array of the selected points. If so, a call of getData(pointNumber=indexArray) would be
    ; require and an update of the store data would happen forbidden another call of this function on the same data set
    ; (well without another call of getData(pointNumber=indexArray) to restore 'original' data).

    if Keyword_set(outputid) then  begin
      self.Out->print,1, "The returned records are point's index..."
      Return, index ; comp
    endif else begin
      self.Out->print,1, "The returned records are point's data..."
      Return, data
    endelse

  endif else Return, 0



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
;       lasObj = obj_new('fleurdelas')
;
;     Load the 5th flightline of survey from December 5th 2003
;       lasObj->loadData, day = '338', flightline = 5
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
Function fleurdelas::getDataFromNumberOfReturns, return_number=rtnb, outputId=outputId

  ; start time
  T = Systime(1)

  Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian

  self.Out->print,1, "Reading the data into memory..."

  ; Retriving the data packet
  ;openr, inputLun, self.lasFilePath, /get_lun, /swap_if_big_endian
  tempDataStr = Replicate(*(self).Lasdatastr, (*(self.Lasheader)).Npoints)
  Point_lun, inputLun, (*(self.Lasheader)).Dataoffset
  Readu, inputLun, tempDataStr

  self.Out->print,1,"Filtering data by return number..."

  self.Out->print,1,"Filtering number " + Strcompress( rtnb ) + "..."

  ; Extracting bits '00000111'
  cache = 8b + 16b + 32b
  tempNumberOfReturns = tempDataStr.Nreturn
  result = cache and tempNumberOfReturns

  index = Where( result eq rtnb, indexCount )

  if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index), _ref_extra=ex, self.Out) else data = 0

  if (Size(data))[2] ne 8 then $
    self.Out->print,2, "Nothing return !" else $
    self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

  self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

  ; Returning requested data
  Free_lun, inputLun
  if Keyword_set(outputid) then  begin
    self.Out->print,1, "The returned records are point's index..."
    Return, index
  endif else begin
    self.Out->print,1, "The returned records are point's data..."
    Return, data
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
;       lasObj = obj_new('fleurdelas')
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
;     If set, returns the waveforms associate to the points loaded in memory during the last call of fleurdelas::getData() method.
;     It will return a `intarr(n,m)` where n is the number of samples per waveform and m is the number of waveform(s) returned.
;   electric: in, optional, type=boolean
;     If set, the return waveform are converted to voltage value as recorded by the system.
;     It will return a `dblarr(n,m)` where n is the number of samples per waveform and m is the number of waveform(s) returned.
;
;-
Function fleurdelas::getWave, all=all, loadedPoints=loadedPoints, electric=electric

  ;TODO: Check if the waveforms are inside the LAS file if not find the wave file and open it
  if (*(self.Lasheader)).Versionminor le 2 then begin

    self.Out->print,2,"The LAS file version does not contain any waveforms..."
    self.Out->print,2,"Nothing return !"

    tempWave = {wave:0b}

  endif else begin

    ; Checking where the waveforms are; in file or in another file
    check = globalEncodingReader(*(self.Lasheader))

    if check[1] eq 1 then Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian else begin
      ; strip down the name add the extention .wdp and open the file
      waveName = addnameextention(self.Lasfilepath, ORIGINAL_EXTENTION = ORIGINAL_EXTENTION, NEW_EXTENTION = '.wdp')
      Openr, inputLun, waveName, /get_lun, /swap_if_big_endian
    endelse


    if Keyword_set(all) then begin

      ; Declare the ERROR LABEL
      ON_IOERROR, BAD

      tempWaveStr = {waveStruc,wave:Bytarr((*(self.Laswavedsptr)).Numberofsamples)}
      Point_lun, inputLun, ((*(self.Lasheader)).Startwaveform )
      initevrlheader, evrlStruct
      Readu, inputLun, evrlStruct
      Print, "evrlStruct", evrlStruct
      self.Laswaveevlrheader = Ptr_new(evrlStruct)

      nRecordsEvrl = Long(evrlStruct.Recordlengthafterhearder / ((*(self.Laswavedsptr)).Numberofsamples))
      nRecordsHeader = Long((*(self.Lasheader)).Npoints)
      deltaRecords = Abs(Long(nRecordsEvrl) - Long(nRecordsHeader))

      if nRecordsEvrl ne nRecordsHeader then begin

        self.Out->print,2,"Difference between the number of laser soundings and the number of waveforms..."
        self.Out->print,2, Strcompress(String(deltaRecords) +" record(s) share the same waveforms")

      endif

      if (evrlStruct.Recordlengthafterhearder mod ((*(self.Laswavedsptr)).Numberofsamples)) ne 0 then $
        self.Out->print,2,"Number of bytes not consistent with the number of records..."


      tempWave = Replicate(tempWaveStr,evrlStruct.Recordlengthafterhearder/((*(self.Laswavedsptr)).Numberofsamples))
      Readu, inputLun, tempWave

      GOTO, DONE

    endif

    if Keyword_set(loadedPoints) then begin
      Print, 'loadedPoint case statement'
      case 1 of

        (N_elements(*(self.Getdataindex)) eq 1):begin

        ;TODO: manage if the loadedPoints referes to /all -> need to check if all the waveforms are presents !!!
        ;offsetWaveData:0ULL, :

        ; Declare the ERROR LABEL
        ON_IOERROR, BAD

        if ((*(self.Lasdata)).Wpacketsize) gt 0 then begin

          tempWaveStr = {waveStruc,wave:Bytarr((*(self.Lasdata)).Wpacketsize)}
          tempWave = Replicate(tempWaveStr,1)
          evrlSize = 60 ;bytes

          ;point_lun, inputLun, ((*(self.lasHeader)).startWaveform + evrlSize + (*(self.lasData)).offsetWaveData) ;
          Point_lun, inputLun, ((*(self.Lasheader)).Startwaveform + (*(self.Lasdata)).Offsetwavedata) ; This approach seems to work better and avoid EOF for the last record
          Readu, inputLun, tempWave

          GOTO, DONE

        endif else begin

          tempWave = {wave:0B}

        endelse

      end

      else: Print, 'Please provide only one point...'

    endcase

  endif

  ;  if ptr_valid(self.lasData) then packetSize = ((*(self.lasData)).wpacketsize)[0]
  ;  if ptr_valid(self.lasData) then waveStart = ((*(self.lasData)).offsetWaveData)[0]
  ; print, ((*(self.lasWaveDsptr)).numberOfSamples)
  ; Checking if all the packet have the same size
  ; A consecutive order equal to the point one is assumed if all the offsetWaveData fields are the same
  ; checkSize = (((*(self.lasData)).wpacketsize)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).wpacketsize)[*(self.getDataIndex)], -1))
  ; if total(checkSize) eq 0.0 then packetSize = ((*(self.lasData)).wpacketsize)[0]
  ; checkStart = (((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)])-temporary(shift( ((*(self.lasData)).offsetWaveData)[*(self.getDataIndex)], -1))
  ; if total(checkStart) eq 0.0 then waveStart = ((*(self.lasData)).offsetWaveData)[0]
  ; Exception label. Print the error message.
  BAD: Print, !ERR_STRING

  ; Close and free the input/output unit.
  DONE: Free_lun, inputLun
  Free_lun, inputLun
  self.Laswave = Ptr_new(tempWave)

endelse

if Keyword_set(electric) then begin

  tempWave = (tempWave.Wave + ((*(self.Laswavedsptr)).Digitizeroffset)) * ((*(self.Laswavedsptr)).Digitizergain)
  Return, tempWave

endif else begin

  Return, tempWave.Wave * 1

endelse

End



;+
; :Description:
; This function will extract the maximum altitude from a selected points set.
;
; :Category:
;   GENERAL, ANALYSIS
;
; :Return:
;   either a point index if OUTPUTID is set to 1, or otherwise an elevation value.
;
; :Uses:
;   Result=Obj->getSelectedDataMaximumHeight()
;
; :Keywords:
;    outputId in, optional, type=boolean
;
; :History:
;   2013 : Original implementation
;
; :Author: antoine
;-
Function fleurdelas::getSelectedDataMaximumHeight, outputId=outputId

  ;print, (*(self.lasHeader)).zscale , (*(self.lasHeader)).zoffset
  maxH = Max( ((*self.Lasdata).Elev   * (*(self.Lasheader)).Zscale) + (*(self.Lasheader)).Zoffset, maxSub )
  self.Out->print,1, "Maximum height is " + Strcompress(String(maxH), /REMOVE_ALL) + " m..."
  ;  maxH = max( ( (*self.lasData).elev * (*(self.lasHeader)).zscale ) + (*(self.lasHeader)).zoffset, maxSub )

  if Keyword_set(outputid) then  begin
    self.Out->print,1, "The returned records are point's index..."
    Return, maxSub[0]
  endif else begin
    self.Out->print,1, "The returned is the elevation information..."
    Return, maxH[0]
  endelse

End



;+
; :Description:
; This function will extract the minimum altitude from a selected points set.
;
; :Category:
;   GENERAL, ANALYSIS
;
; :Return:
;   either a point index if OUTPUTID is set to 1, or otherwise an elevation value.
;
; :Uses:
;   Result=Obj->getSelectedDataMinimumHeight()
;
; :Keywords:
;    outputId in, optional, type=boolean
;
; :History:
;   2013 : Original implementation
;
; :Author: antoine
;-
Function fleurdelas::getSelectedDataMinimumHeight, outputId=outputId

  ;print, (*(self.lasHeader)).zscale , (*(self.lasHeader)).zoffset
  minH = Min( ((*self.Lasdata).Elev * (*(self.Lasheader)).Zscale) + (*(self.Lasheader)).Zoffset, minSub )
  self.Out->print,1, "minimum height is " + Strcompress(String(minH), /REMOVE_ALL) + " m..."
  ;  minH = min( ( (*self.lasData).elev * (*(self.lasHeader)).zscale ) + (*(self.lasHeader)).zoffset, minSub )

  if Keyword_set(outputid) then  begin
    self.Out->print,1, "The returned records are point's index..."
    Return, minSub[0]
  endif else begin
    self.Out->print,1, "The returned is the elevation information..."
    Return, minH[0]
  endelse

End



;+
; :Description:
;    This function will return an index of n randomly selected data points
;
; :Category:
;   GENERAL, ANALYSIS
;
; :Return:
;   eiter an index value or a point structure
;
; :Uses:
;   Result=Obj->getSelectedDataRandom(seed, n [,/OUTPUTID])
;
; :Params:
;    seed: in, required, type=integer
;     Integer value required to extablish the random distribution
;    n: in, required, type=integer
;     integer value that specifies the number of random value(s) to be return
;
; :Keywords:
;    outputId: in, optional, type=boolean
;     flag the type of returned value, can be an index number or a point array structure
;
; :History:
;   2013 : Original implementation
;
; :Author: antoine
;-
Function fleurdelas::getSelectedDataRandom, seed, n, outputId=outputId

  dum = N_elements(self.getSelectedDataIndex())
  rNumb = Randomu(seed, n)

  if Keyword_set(outputid) then  begin
    self.Out->print,1, "The returned records are point's index..."
    Return, Round(dum * rNumb)
  endif else begin
    self.Out->print,1, "The returned is the point's data structure..."
    Return, self.getData(pointNumber=Round(dum * rNumb))
  endelse

End



;+
; :Description:
;    This function will compute the anchor point from the point information.
;
; :Category:
;   LAS, WAVEFORM
;
; :Return:
;   a structure containing the x0,y0,z0 triplet coordinates
;
; :Uses:
;   Result = fleurdelas::getSelectedDataAnchorPoint()
;
; :History:
;   September 2014 - Original implementation
;
; :Author: antoine
;-
Function fleurdelas::getSelectedDataAnchorPoint

  ; wDescriptorIndex:0B, offsetWaveData:0ULL, wPacketSize:0UL, returnPointWaveLocation:0.0, X:0.0, Y:0.0, Z:0.0

  pointCoordinates = self.getXYZ()
  anchor = { anchor, $
    x0 : pointCoordinates[0] - ( (*self.Lasdata).Returnpointwavelocation * (*self.Lasdata).X ), $
    y0 : pointCoordinates[1] - ( (*self.Lasdata).Returnpointwavelocation * (*self.Lasdata).Y ), $
    z0 : pointCoordinates[2] - ( (*self.Lasdata).Returnpointwavelocation * (*self.Lasdata).Z )  $
  }

  Return, anchor

End



;+
; :Description:
;    This function will compute the pulse vector determined from the point and anchor coordinates.
;    The vector will have the direction of the emitting pulse
;
; :Category:
;   LAS, WAVEFORM
;
; :Return:
;   a vectorclass object
;
; :Uses:
;   Result = fleurdelas::getSelectedDataPulseVector()
;
; :History:
;   September 2014 - Original implementation
;
; :Author: antoine
;-
Function fleurdelas::getSelectedDataPulseVector

  anchor = self.getSelectedDataAnchorPoint()
  pointCoordinates = self.getXYZ()
  pulseVector = vectorclass(pointCoordinates[0] - anchor.X0, pointCoordinates[1] - anchor.Y0, pointCoordinates[2] - anchor.Z0 )

  Return, pulseVector.Normalizelength

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
Function fleurdelas::getSelectedDataByBoundingBox, $
                                BOUNDINGBOX = BBOX
                                

tempData = self.getXYZ()

self.Out->print,1,"Filtering data by coordinates..."

; setting object data extent
;self.Lasdataextent = BBOX

index = Where((tempData[*,0] le BBOX[0]) and (tempData[*,0] ge BBOX[1]) and $
              (tempData[*,1] le BBOX[2]) and (tempData[*,1] ge BBOX[3]), indexCount, /NULL)

;if indexCount ne 0 then data = self->lasPointFieldSelector(tempDataStr(index)) else data = 0

if (Size(data))[2] ne 8 then $
  self.Out->print,2, "Nothing return !" else $
  self.Out->print,1, Strcompress("Number of point record(s) returned: "+String(indexCount))

self.Out->print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

if indexCount ne !NULL then return, index else return, !NULL

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
Function fleurdelas::getSelectedDataIndex

  if Ptr_valid(self.Getdataindex) then Return, (*(self.Getdataindex)) else Print, 'Nothing to return, please update the selection first...'

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
Function fleurdelas::getHeaderProperty,$
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
  boundingBox=boundingBox,$
  plotBoundingBox=plotBoundingBox

  if Keyword_set(header) then Return, (*(self.Lasheader))
  if Keyword_set(signature) then Return, (*(self.Lasheader)).Signature
  if Keyword_set(versionMajor) then Return, (*(self.Lasheader)).Versionmajor
  if Keyword_set(versionMinor) then Return, (*(self.Lasheader)).Versionminor
  if Keyword_set(systemID) then Return, (*(self.Lasheader)).Systemid
  if Keyword_set(softwareID) then Return, (*(self.Lasheader)).Softwareid
  if Keyword_set(surveyDay) then Return, (*(self.Lasheader)).Day
  if Keyword_set(surveyYear) then Return, (*(self.Lasheader)).Year
  if Keyword_set(sizeOfHeader) then Return, (*(self.Lasheader)).Headersize
  if Keyword_set(dataOffset) then Return, (*(self.Lasheader)).Dataoffset
  if Keyword_set(numberOfVLR) then Return, (*(self.Lasheader)).Nrecords
  if Keyword_set(pointFormat) then Return, (*(self.Lasheader)).Pointformat
  if Keyword_set(pointLength) then Return, (*(self.Lasheader)).Pointlength
  if Keyword_set(numberOfPoints) then Return, (*(self.Lasheader)).Npoints
  if Keyword_set(numberOfReturns) then Return, (*(self.Lasheader)).Nreturns
  if Keyword_set(xScale) then Return, (*(self.Lasheader)).Xscale
  if Keyword_set(yScale) then Return, (*(self.Lasheader)).Yscale
  if Keyword_set(zScale) then Return, (*(self.Lasheader)).Zscale
  if Keyword_set(xOffset) then Return, (*(self.Lasheader)).Xoffset
  if Keyword_set(yOffset) then Return, (*(self.Lasheader)).Yoffset
  if Keyword_set(zOffset) then Return, (*(self.Lasheader)).Zoffset
  if Keyword_set(xMax) then Return, (*(self.Lasheader)).Xmax
  if Keyword_set(xMin) then Return, (*(self.Lasheader)).Xmin
  if Keyword_set(yMax) then Return, (*(self.Lasheader)).Ymax
  if Keyword_set(yMin) then Return, (*(self.Lasheader)).Ymin
  if Keyword_set(zMax) then Return, (*(self.Lasheader)).Zmax
  if Keyword_set(zMin) then Return, (*(self.Lasheader)).Zmin
  if Keyword_set(boundingBox) then Return, [(*(self.Lasheader)).Xmax,(*(self.Lasheader)).Xmin,(*(self.Lasheader)).Ymax,(*(self.Lasheader)).Ymin,(*(self.Lasheader)).Zmax,(*(self.Lasheader)).Zmin]
  if Keyword_set(plotBoundingBox) then Return, [(*(self.Lasheader)).Ymin, (*(self.Lasheader)).Xmin, (*(self.Lasheader)).Ymax,(*(self.Lasheader)).Xmax]

End




;+
; :Description:
;    Describe the procedure.
;
; :Category:
;   What is the general purpose of this method
;
; :Return:
;   If any, what is the output of this method
;
; :Uses:
;   The call method
;
; :Example:
;   A quick example on how to use this method
;
;
;
; :Keywords:
;    waveDescriptorHeader
;    waveDescriptorArray
;    geoTiff
;    vlrHeaderOfGeoKey
;    vlrHeaderArray
;    vlrRecords
;    vlrGeneric
;    vlrWholeGeneric
;    vlrGeoKeyHeader
;    vlrGeoKeyArray
;    vlrHeader_Array
;    vlrRecord_Array
;    vlrFileID
;    vlrByteSize
;    vlrId
;
; :History:
;   Development history
;
; :Author: antoine
;-
Function fleurdelas::getVlr,$
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

  if Keyword_set(WaveDescriptorHeader) then Return, (*(self.Laswavedsptrhdr))
  if Keyword_set(WaveDescriptorArray) then Return, (*(self.Laswavedsptr))
  if Keyword_set(geoTiff) then Return, 0
  if Keyword_set(vlrRecords) then Return, (*(self.Lasvlrrecords))
  ;if keyword_set(vlrKeyEntry) then return, (*(self.lasVLRKeyEntry))
  if Keyword_set(vlrGeneric) then Return, (*(self.Lasvlrgeneric))
  if Keyword_set(vlrWholeGeneric) then Return, (*(self.Lasvlrwholegeneric))
  if Keyword_set(vlrHeaderOfGeoKey) then Return, (*(self.Lasvlrgeokeyheader))
  if Keyword_set(vlrGeoKeyHeader) then Return, (*(self.Lasgeokeyheader))
  if Keyword_set(vlrGeoKeyArray) then Return, (*(self.Lasgeokeyarray))
  if Keyword_set(vlrHeader_Array) then Return, (*(self.Vlrheader_array))
  if Keyword_set(vlrRecord_Array) then Return, (*(self.Vlrrecord_array))
  if Keyword_set(vlrFileID) then begin
    if self.Vlrfileid eq !NULL then Return, (self.Vlrfileid) else Return, (*(self.Vlrfileid))
  endif
  if Keyword_set(vlrByteSize) then Return, (*(self.Vlrbytesize))
  if Keyword_set(vlrId) then Return, (*(self.Vlrid))
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
Function fleurdelas::getEVlr,$
  header=header

  if Keyword_set(header) then Return, (*(self.Laswaveevlrheader))

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
Function fleurdelas::getPointSize

  Return, self.Lasdatastrsz

End



;+
; :Description:
;    This function returns the selected data coordinates with the scale and offset factor applied,
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   Result = fltarr(n,3)
;    n: the number of point(s) selected by the previous fleurdelas::getData() call
;
; :Uses:
;   Result = fleurdelas::getXYZ()
;
; :Params:
;    index: in, optional, type=int
;     a index scalar or array
;
; :History:
;   JANUARY 2014: initial implementation
;
; :Author: antoine
;-
Function fleurdelas::getXYZ, index, RETURNASPOINTARRAYOBJ = RETURNASPOINTARRAYOBJ

  if ptr_valid(self.xyz) then begin
    
    if N_elements(index) eq 0 then begin
      temp =  *(self.xyz)
    endif else begin
      temp =  (*(self.xyz))[index,*]
    endelse
    
  endif else begin
   
    if N_elements(index) eq 0 then begin
      temp =  [ $
        [ ((*self.Lasdata).East * ((*(self.Lasheader)).Xscale)) + ((*(self.Lasheader)).Xoffset) ] ,$
        [ ((*self.Lasdata).North * ((*(self.Lasheader)).Yscale)) + ((*(self.Lasheader)).Yoffset) ] ,$
        [ ((*self.Lasdata).Elev * ((*(self.Lasheader)).Zscale)) + ((*(self.Lasheader)).Zoffset) ] $
        ]
    endif else begin
      temp =  [ $
        [ (((*self.Lasdata).East)[index] * ((*(self.Lasheader)).Xscale)) + ((*(self.Lasheader)).Xoffset) ] ,$
        [ (((*self.Lasdata).North)[index] * ((*(self.Lasheader)).Yscale)) + ((*(self.Lasheader)).Yoffset) ] ,$
        [ (((*self.Lasdata).Elev)[index] * ((*(self.Lasheader)).Zscale)) + ((*(self.Lasheader)).Zoffset) ] $
        ]
    endelse
    
    self.xyz = ptr_new(temp)
    
  endelse
  
  if Keyword_set(RETURNASPOINTARRAYOBJ) then return, pointarrayclass_sazerac(temp) else return, temp

End



;+
; :Description:
;    This function computes the pulse density as well as the NPS.
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   A structure of the form {area: sceneArea, pointDensity: pDensity, NPS: nomPointSpacing}
;
; :Uses:
;   Result = fleurdelas::getPointDensity()
;
; :Example:
;   A quick example on how to use this method
;
; :Params:
;    index
;
; :Keywords:
;    sceneArea
;    class
;
; :History:
;   September 2014 : initial draft implementation
;
; :Author: sam
;-
Function fleurdelas::getPointDensity, index, sceneArea=sceneArea, class=classvalue


  ; TODO:
  ;
  ; seperate the points by flight line (point ID)
  ; filter for the first return
  ; filter for 90% of the swath-width / scan angle
  ;
  ; grid the data with either 1m x 1m (ensuring that there is at least one point in every square for at least 90% of the squares)
  ; filter by class if this is what you want to do
  ;
  ; find the point density, and NPS, for each square
  ;
  ; then average these for the full dataset to find the point density and NPS for the whole dataset
  ;
  ; then if you area using a geobox average these for just the area in question


  if N_elements(class) ne 0 then index = self.getDataFromClass(class=classvalue)

  nPoints = N_elements(index)

  if N_elements(sceneArea) ne 0 then begin

    pDensity = nPoints / sceneArea

    nomPointSpacing = 1. / Sqrt(pDensity)

  endif else begin

    sceneArea = ( Max(((*self.Lasdata).East)[index]) - Min(((*self.Lasdata).East)[index]) ) * ( Max(((*self.Lasdata).North)[index]) - Min(((*self.Lasdata).North)[index]) )

    pDensity = nPoints / sceneArea

    nomPointSpacing = 1. / Sqrt(pDensity)

  endelse

  densityMetrics = {area: sceneArea, pointDensity: pDensity, NPS: nomPointSpacing}

  Return, densityMetrics

end



Function fleurdelas::printHeader, header, minorVersion

  self.Out->print,1,'======================== HEADER ========================='
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
  if minorVersion lt 4 then self.Out->printArray,1, Strcompress(String(header.Nreturns), /REMOVE_ALL) else self.Out->printArray,1, Strcompress(String(header.Npointsbyreturn), /REMOVE_ALL)
  self.Out->print,1, Strcompress("Scale factor x y z: " + String(header.Xscale) + " " + String(header.Yscale) + " " + String(header.Zscale))
  self.Out->print,1, Strcompress("Offset factor x y z: " + String(header.Xoffset) + " " + String(header.Yoffset) + " " + String(header.Zoffset))
  self.Out->print,1, Strcompress("Minimum x y z: " + String(header.Xmin) + " " + String(header.Ymin) + " " + String(header.Zmin))
  self.Out->print,1, Strcompress("Maximum x y z: " + String(header.Xmax) + " " + String(header.Ymax) + " " + String(header.Zmax))

  ; OLD FORMATING - Kepth for legacy
  ;  self.Out->print,1, Strcompress("X scale factor: " + String(header.Xscale))
  ;  self.Out->print,1, Strcompress("Y scale factor: " + String(header.Yscale))
  ;  self.Out->print,1, Strcompress("Z scale factor: " + String(header.Zscale))
  ;  self.Out->print,1, Strcompress("X offset factor: " + String(header.Xoffset))
  ;  self.Out->print,1, Strcompress("Y offset factor: " + String(header.Yoffset))
  ;  self.Out->print,1, Strcompress("Z offset factor: " + String(header.Zoffset))
  ;  self.Out->print,1, Strcompress("X Minimum: " + String(header.Xmin))
  ;  self.Out->print,1, Strcompress("X Maximum: " + String(header.Xmax))
  ;  self.Out->print,1, Strcompress("Y Minimum: " + String(header.Ymin))
  ;  self.Out->print,1, Strcompress("Y Maximum: " + String(header.Ymax))
  ;  self.Out->print,1, Strcompress("Z Minimum: " + String(header.Zmin))
  ;  self.Out->print,1, Strcompress("Z Maximum: " + String(header.Zmax))

  self.Out->printsep

return, 1

End



Function fleurdelas::printVLR, vlr

End


Function fleurdelas::printEVLR, EVlr

End



Function fleurdelas::printExtraBytes, data_type, extra_bytes_structure
  
  self.Out->print,1, "data type: " + Strcompress(String(Fix(data_type)),/remove_all) + " " + self.extraBytesDataName(data_type) +', name "' + Strcompress(String(extra_bytes_structure.Name),/remove_all) + '", description: "' + $
    Strcompress(String(extra_bytes_structure.Description)) + '", min: ' + Strcompress(String((extra_bytes_structure.Min)[0]),/remove_all) + ", max: " + Strcompress(String((extra_bytes_structure.Max)[0]),/remove_all) + $
    ", scale: " + Strcompress(String((extra_bytes_structure.Scale)[0]),/remove_all) + ", offset: " + Strcompress(String((extra_bytes_structure.Offset)[0]),/remove_all) ; (not set)

End



Function fleurdelas::view, $
                     DUMP = DUMP    ; To be implemented

  pts = self.getXYZ()
  xyz = Transpose( [ [pts[*,0]], [pts[*,1]], [pts[*,2]] ] )
  self.Out->print, 1, "Getting points cloud's intensity values..."
  dum = [Transpose(self.viewGetColorValues(/INTENSITY)),Transpose(self.viewGetColorValues(/INTENSITY)),Transpose(self.viewGetColorValues(/INTENSITY))]
  pInt = Ptr_new(dum)
  self.Out->print, 1, "Getting points cloud's classification values..."
  dum = [Transpose(self.viewGetColorValues(/CLASSIFICATION)),Transpose(self.viewGetColorValues(/CLASSIFICATION)),Transpose(self.viewGetColorValues(/CLASSIFICATION))]
  pClass = Ptr_new(dum)
  self.Out->print, 1, "Getting points cloud's elevation values..."
  dum = [Transpose(self.viewGetColorValues(/ELEVATION)),Transpose(self.viewGetColorValues(/ELEVATION)),Transpose(self.viewGetColorValues(/ELEVATION))]
  pElev = Ptr_new(dum)
  self.Out->print, 1, "Getting points cloud's RGB values..."
  pRGB = Ptr_new(Transpose(self.viewGetColorValues(/RGB)))
;  rgb = Transpose([[pts.R],[pts.G],[pts.B]])/256U
  obj = Idlgrpolygon(xyz, Style=0, Vert_Colors=*pRGB)
;  XObjView, o, TITLE='Fleurdelas Simple Viewer"
  xobjview_refresh, obj, pElev, pInt, pClass, pRGB
  

End



;+
; :Description:
;    This function will returns the RGB value hold into the data points
;    If no RGB values exist, then it will return the intensity
;
; :Category:
;   LAS, GENERAL, PLOT
;
; :Return:
;   Result = bytarr(n) or Result = bytarr(n,3)
;
; :Uses:
;   Result = fleurdelas::viewGetColorValues()
;
; :Keywords:
;    index: in, optional, type=int
;     scalar or array that represents the index
;
; :History:
;   AUGUST 2014
;
; :Author: antoine
;-
Function fleurdelas::viewGetColorValues, $
  INDEX = INDEX, $
  CLASSIFICATION = CLASSIFICATION, $
  ELEVATION = ELEVATION, $
  INTENSITY = INTENSITY, $
  RGB = RGB, $
  ANGLE = ANGLE, $
  FLIGHTLINE = FLIGHTLINE, $
  USER = USER

  ;elev    : 0L,  $     ; Z data
  ;inten   : 0US, $     ; Intensity
  ;nReturn : 0B,  $     ; Return number, number of returns, scan direction, edge
  ;class   : 0B,  $     ; Classification
  ;angle   : 0B,  $     ; Scan angle
  ;user    : 0B,  $     ; User data
  ;source  : 0US  $     ; Point source ID

  if Keyword_set(CLASSIFICATION) then begin

    if Keyword_set(INDEX) then Return, Bytscl(200*((*self.Lasdata).Class)[index]) else Return, Bytscl(200*(*self.Lasdata).Class)

  endif

  if Keyword_set(ELEVATION) then begin

    if Keyword_set(INDEX) then Return, Bytscl(((*self.Lasdata).Elev)[index]) else Return, Bytscl((*self.Lasdata).Elev)

  endif


  if Keyword_set(INTENSITY) then begin

    if Keyword_set(INDEX) then Return, (((*self.Lasdata).Inten)[index]) else Return, ((*self.Lasdata).Inten)

  endif


  if Keyword_set(RGB) then begin

    if (*(self.Lasheader)).(16) eq 2B or $
      (*(self.Lasheader)).(16) eq 3B or $
      (*(self.Lasheader)).(16) eq 5B or $
      (*(self.Lasheader)).(16) eq 9B    $
      then begin

      if N_elements(INDEX) eq 0 then begin
        Return, Bytscl([ $
          [ (*self.Lasdata).R ] ,$
          [ (*self.Lasdata).G ] ,$
          [ (*self.Lasdata).B ] $
          ])
      endif else begin
        Return, Bytscl([ $
          [ ((*self.Lasdata).R)[index] ] ,$
          [ ((*self.Lasdata).G)[index] ] ,$
          [ ((*self.Lasdata).B)[index] ] $
          ])
      endelse

    endif else begin
      self.Out->print, 2, "Point format doesn't support RGB value, will return intensity instead..."
      Return, self.viewGetColorValues(INDEX = INDEX, /INTENSITY)
    endelse

  endif


  if Keyword_set(ANGLE) then begin

    if Keyword_set(INDEX) then Return, Bytscl(((*self.Lasdata).Angle)[index]) else Return, Bytscl((*self.Lasdata).Angle)

  endif


  if Keyword_set(FLIGHTLINE) then begin

    if Keyword_set(INDEX) then Return, Bytscl(((*self.Lasdata).Source)[index]) else Return, Bytscl((*self.Lasdata).Source)

  endif

  if Keyword_set(USER) then begin

    if Keyword_set(INDEX) then Return, Bytscl(((*self.Lasdata).User)[index]) else Return, Bytscl((*self.Lasdata).User)

  endif

End



;+
; This function returns a waveform given a byte offset.
; This function is obsolete. please use fleurdelas::getWave().
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   Return an `bytarr(n)` where n (usually 256 samples) is the number of samples per waveform.
;
; :Uses:
;   Result=Obj->waveFromPacketOffset(waveOffset=offsetinbytes)
;
; :Keywords:
;   waveOffset : in, required, type=byte
;     The value in byte of the offset to the waveform. The offset is from the start of the waveforms packet.
;
;-
Function fleurdelas::waveFromPacketOffset, waveOffset = waveOffset

  Print, 'waveform OFFSET: ', waveOffset
  ;TODO: Check if the waveforms are inside the LAS file if not find the wave file and open it
  if (*(self.Lasheader)).Versionminor le 2 then begin

    self.Out->print,2,"The LAS file version does not contain any waveforms..."
    self.Out->print,2,"Nothing return !"

    tempWave = {wave:0b}

  endif else begin

    ; Checking where the waveforms are; in file or in another file
    check = globalEncodingReader(*(self.Lasheader))

    if check[1] eq 1 then Openr, inputLun, self.Lasfilepath, /get_lun, /swap_if_big_endian ;else begin

    ; strip down the name add the extention .wdp and open the file

  endelse

  ON_IOERROR, BAD

  if ((*(self.Lasdata)).Wpacketsize) gt 0 then begin

    tempWaveStr = {waveStruc,wave:Bytarr((*(self.Lasdata)).Wpacketsize)}
    tempWave = Replicate(tempWaveStr,1)

    Point_lun, inputLun, ((*(self.Lasheader)).Startwaveform) + waveOffset ; This approach seems to work better and avoid EOF for the last record
    Readu, inputLun, tempWave

    GOTO, DONE

  endif else begin

    tempWave = {wave:0B}

  endelse

  BAD: Print, !ERR_STRING

  ; Close and free the input/output unit.
  DONE: Free_lun, inputLun

  Return, tempWave


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
Function fleurdelas::setHeader, temp
  ptr_free, self.lasheader
  (*(self.Lasheader))=temp
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
Function fleurdelas::setHeaderSignature, temp
  (*(self.Lasheader)).Signature = temp
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
Function fleurdelas::setHeaderVersionMajor, temp
  (*(self.Lasheader)).Versionmajor = temp
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
Function fleurdelas::setHeaderVersionMinor, temp
  (*(self.Lasheader)).Versionminor = temp
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
Function fleurdelas::setHeaderSystemID, temp

  dum = ((*(self.Lasheader)).Systemid)
  dum[0] = temp
  ((*(self.Lasheader)).Systemid) = dum

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
Function fleurdelas::setHeaderSoftwareID, temp

  dum = ((*(self.Lasheader)).Softwareid)
  dum[0] = temp
  ((*(self.Lasheader)).Softwareid) = dum

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
Function fleurdelas::setHeaderDay, temp
  (*(self.Lasheader)).Day = temp
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
Function fleurdelas::setHeaderYear, temp
  (*(self.Lasheader)).Year = temp
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
Function fleurdelas::setHeaderDataOffset, temp
  (*(self.Lasheader)).Dataoffset = temp
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
Function fleurdelas::setHeaderNRecords, temp
  (*(self.Lasheader)).Nrecords = temp
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
Function fleurdelas::setHeaderPointFormat, temp
  (*(self.Lasheader)).Pointformat = temp
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
Function fleurdelas::setHeaderPointLength, temp
  (*(self.Lasheader)).Pointlength = temp
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
Function fleurdelas::setHeaderNPoints, temp

  ((*(self.Lasheader)).nPoints) = temp
  if ((*(self.Lasheader)).versionMinor) eq 4 then ((*(self.Lasheader)).XXXNPOINTS) = temp
  return, 1
  
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
Function fleurdelas::setHeaderNReturns, temp
  if ((*(self.Lasheader)).versionMinor) eq 4 then begin
    (*(self.Lasheader)).NPOINTSBYRETURN = temp
    (*(self.Lasheader)).XXXNreturns = fix(temp[0:4], TYPE=13)
  endif else begin
    (*(self.Lasheader)).Nreturns = temp
  endelse
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
Function fleurdelas::setHeaderXScale, temp
  (*(self.Lasheader)).Xscale = temp
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
Function fleurdelas::setHeaderYScale, temp
  (*(self.Lasheader)).Yscale = temp
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
Function fleurdelas::setHeaderZScale, temp
  (*(self.Lasheader)).Zscale = temp
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
Function fleurdelas::setHeaderXOffset, temp
  (*(self.Lasheader)).Xoffset = temp
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
Function fleurdelas::setHeaderYOffset, temp
  (*(self.Lasheader)).Yoffset = temp
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
Function fleurdelas::setHeaderZOffset, temp
  (*(self.Lasheader)).Zoffset = temp
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
Function fleurdelas::setHeaderXMax, temp
  print, 'old value: ' , (*(self.Lasheader)).Xmax
  (*(self.Lasheader)).Xmax = temp
  print, 'new value: ', (*(self.Lasheader)).Xmax
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
Function fleurdelas::setHeaderYMax, temp
  (*(self.Lasheader)).Ymax = temp
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
Function fleurdelas::setHeaderZMax, temp
  (*(self.Lasheader)).Zmax = temp
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
Function fleurdelas::setHeaderXMin, temp
  (*(self.Lasheader)).Xmin = temp
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
Function fleurdelas::setHeaderYMin, temp
  (*(self.Lasheader)).Ymin = temp
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
Function fleurdelas::setHeaderZMin, temp
  (*(self.Lasheader)).Zmin = temp
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
Function fleurdelas::setHeaderStartWaveform, temp
  (*(self.Lasheader)).Startwaveform = temp
End



;+
; CHANNEL needs to be a string array in this format ['R','G','b'] or any combinaison
;-
Function fleurdelas::bumpRGB, VALUE = VALUE, OPERATOR = OPERATOR, CHANNEL = CHANNEL

if not keyword_set(CHANNEL) then begin
  self.Out->print, 3, "Keyword CHANNEL not set..."
  self.Out->print, 3, "Cannot determine which channel needs to be treated...
  self.Out->print, 2, "Use CHANNEL = ['r','b'] to work on the Red and Blue channels..."
endif

 
 nChannel = n_elements(CHANNEL)
 
 if keyword_set(VALUE) and keyword_set(OPERATOR) then begin
  
   for i = 0, nChannel-1 do begin
    case 1 of
      strlowcase(CHANNEL[i]) eq 'r': begin
        case 1 of
          OPERATOR eq '+': (*(self.Lasdata)).r += value
          OPERATOR eq '-': (*(self.Lasdata)).r -= value
          OPERATOR eq '*': (*(self.Lasdata)).r *= value
          OPERATOR eq '/': (*(self.Lasdata)).r /= value
          else:
        endcase
      end
      
      strlowcase(CHANNEL[i]) eq 'g': begin
        case 1 of
          OPERATOR eq '+': (*(self.Lasdata)).g += value
          OPERATOR eq '-': (*(self.Lasdata)).g -= value
          OPERATOR eq '*': (*(self.Lasdata)).g *= value
          OPERATOR eq '/': (*(self.Lasdata)).g /= value
          else:
        endcase
      end
      
      strlowcase(CHANNEL[i]) eq 'b': begin
        case 1 of
          OPERATOR eq '+': (*(self.Lasdata)).b += value
          OPERATOR eq '-': (*(self.Lasdata)).b -= value
          OPERATOR eq '*': (*(self.Lasdata)).b *= value
          OPERATOR eq '/': (*(self.Lasdata)).b /= value
          else:
        endcase
      end
      
      else:
      
    endcase  
   endfor
   
 endif
 
 for i = 0, nChannel-1 do begin
   case 1 of
    
     strlowcase(CHANNEL[i]) eq 'r': (*(self.Lasdata)).r = hist_equal((*(self.Lasdata)).r)
     strlowcase(CHANNEL[i]) eq 'g': (*(self.Lasdata)).g = hist_equal((*(self.Lasdata)).g)
     strlowcase(CHANNEL[i]) eq 'b': (*(self.Lasdata)).b = hist_equal((*(self.Lasdata)).b)
     else: begin
       (*(self.Lasdata)).r = hist_equal((*(self.Lasdata)).r)
       (*(self.Lasdata)).g = hist_equal((*(self.Lasdata)).g)
       (*(self.Lasdata)).b = hist_equal((*(self.Lasdata)).b)
     end

   endcase
 endfor

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
Function fleurdelas::setVlrWaveDescriptorHeader, temp
  ;(*(self.lasHeader)).lasWaveDsptrHdr = temp
  (*(self.Laswavedsptrhdr)) = temp
End



;+
; :Description:
;    Change field value of single point.
;
; :Category:
;   LAS
;
; :Return:
;   none
;
; :Uses:
;   The call method
;
; :Example:
;   A quick example on how to use this method
;
;
;
; :Keywords:
;    id : in, required, type = 0UL or an array of ULong
;    east : in, optional, type = 0L
;     the new easting value(s)
;    north : in, optional, type = 0L
;     the new northing value(s)
;    elev : in, optional, type = 0L
;     the new elevation value(s)
;    intensity : in, optional, type = 0US/0B
;     the new intensity value(s)
;    class : in, optional, type = 0B
;     the new class value (0-12 accepted)
;    angle : in, optional, type = 0B
;     the new scan angle value(s)
;    user : in, optional, type = 0B
;     the new user value(s)
;    rgb :  : in, optional, type= bytarr(n,3)
;     where n represents the number of points to modify. The array is in column for speed performance
;    selected : in, optional, type = bool
;     if present, the id is related to the selected data already loaded in memory
;
; :History:
;   Development history
;    APRIL 2014 - Creation
;
; :Author: antoine
;-
Function fleurdelas::setDataPoint, $
                      id = id, $
                      east = east, north = north, elev = elev, $
                      intensity = intensity, $
                      class = class, $
                      angle = angle, $
                      user = user, $
                      rgb = rgb, $
                      selected = selected
                      

  if not Keyword_set(selected) then dum = self.getData(/ALL)
;  if n_elements(selected) eq 0 then dum = self.getData(/ALL)
  
  if N_elements(east) gt 0 then begin
    if n_elements(selected) eq 0 then id = indgen(n_elements(east), /UL64)
    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(east) then begin
      ; scaling and offseting back the provided east values
      newEast = (east - (*(self.Lasheader)).Xoffset) / (*(self.Lasheader)).Xscale
      ((*self.Lasdata)[id]).East = Long(newEast)
    endif else begin
      self.Out->print, 3, "The number of east values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(north) gt 0 then begin
    if n_elements(selected) eq 0 then id = indgen(n_elements(north), /UL64)
    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(north) then begin
      
      ; scaling and offseting back the provided north values
      newNorth = (north - (*(self.Lasheader)).Yoffset) / (*(self.Lasheader)).Yscale
      ((*self.Lasdata)[id]).North = Long(newNorth)
    endif else begin
      self.Out->print, 3, "The number of northing values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(elev) gt 0 then begin
    idGeneral = indgen(n_elements(elev), /UL64)
    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(elev) then begin
      
      ; scaling and offseting back the provided elev values
      newelev = long( (elev - (*(self.Lasheader)).Zoffset) / (*(self.Lasheader)).Zscale )

      if size(newElev, /N_DIMENSIONS) gt 1 then newelev = transpose(newelev)
      if size(id, /N_DIMENSIONS) gt 1 then id = transpose(id)
      
      ;(*self.Lasdata)[id].Elev = long(newElev)
      
      ; Brute force as doesn't work the other way :(
      nn = 0UL
      nnn = N_elements(elev)
      lnewElev = long(newElev)
      while nn lt nnn do begin
        (*self.lasdata)[nn].elev = newElev[nn]
        nn++
      endwhile

       print, min(newElev), max(newElev)
       print, min((*self.Lasdata).Elev), max((*self.Lasdata).Elev)

    endif else begin
      self.Out->print, 3, "The number of elevation values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(intensity) gt 0 then begin

    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(intensity) then begin
      ((*self.Lasdata)[id]).Intensity = intensity
    endif else begin
      self.Out->print, 3, "The number of intensity values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(class) gt 0 then begin

    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(class) then begin
      ((*self.Lasdata)[id]).Class = Byte(class)
    endif else begin
      self.Out->print, 3, "The number of class values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(angle) gt 0 then begin

    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(angle) then begin
      ((*self.Lasdata)[id]).Angle = Byte(angle)
    endif else begin
      self.Out->print, 3, "The number of angle values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  if N_elements(rgb) gt 0 then begin

    ; Making sure that the number of index value matches the number of values passed for change
    if N_elements(id) eq N_elements(rgb) and ( *self.Lasheader.Pointformat ge 2 and  *self.Lasheader.Pointformat le 3) then begin
      ((*self.Lasdata)[id]).R = rgb[*,0]
      ((*self.Lasdata)[id]).G = rgb[*,1]
      ((*self.Lasdata)[id]).B = rgb[*,2]
    endif else begin
      if ( *self.Lasheader.Pointformat ge 2 and  *self.Lasheader.Pointformat le 3) then self.Out->print, 3, "The point format doesn't accept RGB values..."
      if N_elements(id) eq N_elements(rgb) then self.Out->print, 3, "The number of rgb values doesn't match the number of id values..."
      Return, 0
    endelse

  endif

  Return, 1

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
Function fleurdelas::setVlrWaveDescriptorArray, temp
  ;(*(self.lasHeader)).lasWaveDsptr = temp
  (*(self.Laswavedsptr)) = temp
  
  Return, 1
  
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
Function fleurdelas::setEVlr, newEvlr
  
  ptr_free, self.laswaveevlrheader
  self.Laswaveevlrheader = ptr_new(newEvlr)
  
  Return, 1
  
End



Function fleurdelas::setVlr, newVlr

ptr_free, self.vlrArr
self.vlrArr = ptr_new(newVlr)

Return, 1

End


Function fleurdelas::setData, newData

ptr_free, self.lasData
self.lasData = ptr_new(newData)

Return, 1

End






;+
; :Description:
;   This Procedure write a LAS file. Still in beta.
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   1 if the file is correclty write.
;   0 if error.
;
; :Uses:
;   Dum = fleurdelas::writeLAS()
;
; :Keywords:
;    id: in, optional, type=int
;     a scalar or array that represent the points index
;    output: in, required, type=string
;     fully qualified path of the new las file
;    selected: in, optional, type=boolean
;     a flag to write the data selected in memory
;
; :History:
;   Septembre 2012 - initial writing
;   February 2014 - Fully functional
;
; :Author: antoine
;-
Function fleurdelas::writeLAS, ID = ID, OUTPUT = OUTPUT, SELECTED = SELECTED, $
                               NEWHEADER = NEWHEADER, NEWVLR = NEWVLR, NEWDATA = NEWDATA, NEWEVLR = NEWEVLR, NEWWAVE = NEWWAVE


  ; In the case of a new file generation from Pulsewaves, we need to update some info before starting writting them...
  ; Updating header
  if keyword_set(NEWHEADER) then self.lasHeader = ptr_new(NEWHEADER)
  if keyword_set(NEWVLR) then self.vlrArr = ptr_new(NEWVLR)
  if keyword_set(NEWDATA) then self.lasData = ptr_new(NEWDATA)
  ; if keyword_set(NEWEVLR) then self.
  
  
  Openw, lasLun, OUTPUT, /get_lun

  self.Out->print, 1, "Writing the new file on the disk at " + Strcompress(String(OUTPUT),/remove_all)

  ; Defining System ID
  sysID = Bytarr(32)
  sysID[0] = Byte('fleurdelas by Carbomap Ltd')
  dum = self.setHeaderSystemID(sysID)
  ; Defining Software ID
  softID = Bytarr(32)
  softIDString = 'fleurdelas::writeLAS (' + strcompress(string(self.fdlReleaseDate), /REMOVE_ALL) +')'
  softID[0] = Byte(softIDString)
  dum = self.setHeaderSoftwareID(softID)

  ; Updating header with id information
  if keyword_set(ID) then begin
    
    ; updating the number of points
    numbOfPoints = self.getSelectedDataNumberOfPoints()
    dum = self.setHeaderNPoints(numbOfPoints)
    
    
    ; Number of points
    dum = self.setHeaderNPoints(n_elements(id))
    
    ; Creation day and year should be updated
    CALDAT, JULDAY(), Month , Day , Year , Hour , Minute , Second
    julianDay = julday(Month , Day , Year) - julday(1,1,year)
    dum = self.setHeaderDay(fix(julianDay, type = 12))
    dum = self.setHeaderYear(fix(year, type = 12))
    
    
    ; Updating the number of points per returns
    if ((*(self.Lasheader)).versionMinor) eq 4 then begin
      returnNumberArray = ulon64arr(15)
      for rn = 1, 15 do begin
        dumPoints = self.getSelectedDataByReturnNumber(return_number=rn, /NUMBEROFPOINTSBYRETURN)
        returnNumberArray[rn-1] = dumPoints
        dum = self.setHeaderNReturns(returnNumberArray)
      endfor
    endif else begin
      returnNumberArray = Ulonarr(5)
      for rn = 1, 5 do begin
        dumPoints = self.getSelectedDataByReturnNumber(return_number=rn, /NUMBEROFPOINTSBYRETURN)
        returnNumberArray[rn-1] = dumPoints
        dum = self.setHeaderNReturns(returnNumberArray)
      endfor
    endelse
    
    ; Setting bounding box of the point cloud
    ; Getting points coordinates
    coor = self.getXYZ(id)
    ; Min, Max of easting
    maxX = max(coor[*,0], min=minX)
    dum = self.setHeaderXMax(maxX)
    dum = self.setHeaderXMin(minX)
    ; Min, Max of northing    
    maxY = max(coor[*,1], min=minY)
    dum = self.setHeaderYMax(maxY)
    dum = self.setHeaderYMin(minY)    
    ; Min, Max of elev
    maxZ = max(coor[*,2], min=minZ)
    dum = self.setHeaderZMax(maxZ)
    dum = self.setHeaderZMin(minZ)

  endif
  
  if keyword_set(SELECTED) then begin
    
    ; updating the number of points
    numbOfPoints = self.getSelectedDataNumberOfPoints()
    dum = self.setHeaderNPoints(numbOfPoints)


    ; Creation day and year should be updated

    ; Updating the number of points per returns
    if ((*(self.Lasheader)).Versionminor) eq 4 then begin
      returnNumberArray = Ulon64arr(15)
      for rn = 1, 15 do begin
        dumPoints = self.getSelectedDataByReturnNumber(return_number=rn, /NUMBEROFPOINTSBYRETURN)
        returnNumberArray[rn-1] = dumPoints
        dum = self.setHeaderNReturns(returnNumberArray)
      endfor
    endif else begin
      returnNumberArray = Ulonarr(5)
      for rn = 1, 5 do begin
        dumPoints = self.getSelectedDataByReturnNumber(return_number=rn, /NUMBEROFPOINTSBYRETURN)
        returnNumberArray[rn-1] = dumPoints
        dum = self.setHeaderNReturns(returnNumberArray)
      endfor
    endelse
    
    ; Setting bounding box of the point cloud
    ; Getting points coordinates
    coor = self.getXYZ()
    ; Min, Max of easting
    maxX = max(coor[*,0], min=minX)
    dum = self.setHeaderXMax(maxX)
    dum = self.setHeaderXMin(minX)
    ; Min, Max of northing    
    maxY = max(coor[*,1], min=minY)
    dum = self.setHeaderYMax(maxY)
    dum = self.setHeaderYMin(minY)    
    ; Min, Max of elev
    maxZ = max(coor[*,2], min=minZ)
    dum = self.setHeaderZMax(maxZ)
    dum = self.setHeaderZMin(minZ)
    
  endif
  
  
  lasHeader = self->getHeaderProperty(/header)

  Writeu, lasLun, lasHeader
  self.Out->print, 1, "Writing public header..."
  Point_lun, -lasLun, posHeader
  if posHeader eq lasHeader.Headersize then self.Out->print, 1,  "Public header successfully written..."


  ; For each VLR, open get the byte size of the record
  ; Open the corresponding file
  ; Read the file
  ; write the raw content into the new LAS file
  self.Out->print, 1, "Writing Variable Length Records..."
  vlrFileID = self->getvlr(/vlrFileID)
  
  if vlrFileID ne !NULL then begin
  
    vlrByteSize = self->getvlr(/vlrByteSize)
    rB = 0L
    wB = 0L
    posVlrArray = 0L
    for x=0, self.getHeaderProperty(/numberOfVLR)-1 do begin
      Openr, rLun, vlrFileID[x], /get_lun
      dum = Bytarr(vlrByteSize[x])
      Readu, rLun, dum
      Point_lun, -rLun, r
      rB += r
      Close, rLun, /FORCE
      Writeu, lasLun,dum
      Point_lun, -lasLun, w
      if x eq 0 then begin
        wB += w-posHeader
      endif else begin
        wB += (w - wB)
      endelse
    endfor
    if rB eq wB-posHeader then begin
      byte1 = wB-posHeader
      self.Out->print, 1, "Variable Length Records successfully written..."
      self.Out->print, 1, Strcompress(String(byte1),/remove_all) + " bytes have been written..."
    endif else begin
      self.Out->print, 2, "Something wrong with the Variable Length Records block..."
    endelse
  
  endif else begin
    
    ; No VLR write - length set to 0
    byte1 = 0
    
  endelse


  self.Out->print, 1, "Checking file integrity..."
  totalBytesWritten = posHeader + byte1 ;+ byte2 + byte3 + byte4 + byte5 + byte6
  if lasHeader.Dataoffset eq totalBytesWritten then self.Out->print, 1, "File integrity pass..." else begin
    self.Out->print, 2, "File integrity fail..."
    self.Out->print, 2, Strcompress(String(totalBytesWritten),/remove_all)+" bytes have been written so far, or the file header stipulates "+Strcompress(String(lasHeader.Dataoffset),/remove_all)+" bytes..."
    if lasHeader.Dataoffset-totalBytesWritten eq 2 then self.Out->print, 1, "The header is followed by 2 user-defined bytes..."
    self.Out->print, 2, "Moving "+Strcompress(String(lasHeader.Dataoffset-totalBytesWritten),/remove_all)+" bytes ahead..."
    Point_lun, -lasLun, actualPos
    Point_lun, lasLun, actualPos + (lasHeader.Dataoffset-totalBytesWritten)
    self.Out->print, 2, "Done... resuming writting process..."
  endelse

  pointSize = self->getPointSize()
  theoriticalDataBlockSize = pointSize * lasHeader.Npoints
  ;print, theoriticalDataBlockSize
  Point_lun, -lasLun, posBeforeDataBlock

  self.Out->print, 1, "Writing points data records..."

  if keyword_set(id) then begin
    self.Out->print, 1, "Using index for the data records..."
    data = self->getData(pointNumber = id)
  endif else begin
    if keyword_set(selected) then begin
      self.Out->print, 1, "Writing loaded data records..."
      data = *(self.lasData)
    endif else begin
      self.Out->print, 1, "Writing all data records..."
      data = self->getData(/all)
    endelse
  endelse

  Writeu, lasLun, data


  Point_lun, -lasLun, posAfterDataBlock
  byte7 = posAfterDataBlock-posBeforeDataBlock

  self.Out->print, 1, "Checking file integrity..."
  fileIntegrity = theoriticalDataBlockSize - byte7
  if fileIntegrity eq 0 then self.Out->print, 1, "File integrity pass..." else begin
    self.Out->print, 3, "File integrity fail..."
  endelse



  ; Checking if the file have waveform information
  if Ptr_valid(temp01) then begin

    self.Out->print, 1, "Position before waveform block: ", posAfterDataBlock
    self.Out->print, 1, "What the header helds", lasHeader.Startwaveform

    waveformBlock = self->getWave(/all)
    waveformHeader = self->getEVlr(/header)

    self.Out->print, 1, "Size information of the Wave Packet: ", Size(waveformBlock)

    self.Out->print, 1, "Writing waveforms header (EVLR header)..."
    Writeu, lasLun, waveformHeader
    Point_lun, -lasLun, posWaveformHeader
    self.Out->print, 1, "Amount of bytes written for the waveform header: "+Strcompress(String(posWaveformHeader-posAfterDataBlock),/remove_all)

    self.Out->print, 1, "Writing waveforms header (EVLR header)..."
    Writeu, lasLun, Byte(waveformBlock)
    Point_lun, -lasLun, posWaveformBlock
    self.Out->print, 1, "Amount of bytes written for the waveform block: "+Strcompress(String(posWaveformBlock-posWaveformHeader),/remove_all)

  endif

  self.Out->print, 1, "Finilizing the file..."
  Free_lun, lasLun
  self.Out->print, 1, "Done."

  Close,lasLun
  
  return, 1

End












;+
; :Description:
;    This function will dump the selected data into an ASCII CSV file.
;    Additional columns can be added to the coordinates.
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   none
;
; :Uses:
;   Result = fleurdelas::dump(val, outputPath=outputPath)
;
; :Params:
;    val: in, optional, type=int,float,double
;     an additional value to add to the coordinate triplet. It can be one single value for all
;     the coordinates or a array of value, one per coordinates
;
; :Keywords:
;    outputPath: in, optional, type=string
;     a fully qualify file path for new CSV file. If no path is provided then the data will be written
;     in the data folder of the project under fleurdelasDUMP.csv
;
; :History:
;   JANUARY 2014: initial implementation
;   SEPTEMBER 2014: 
;    refining the implementation using self.getXYZ() and cleaning some lines so
;
; :Author: antoine
;-
Function fleurdelas::dump, value = val, selection = selection, outputPath = outputPath, field = field

  if n_elements(selection) ne 0 then dum = self.getXYZ(selection) else dum = self.getXYZ() 
  if n_elements(selection) ne 0 then nX = (size(selection,/DIMENSIONS))[0] else nX = (size(dum,/DIMENSIONS))[0]
  
  if keyword_set(field) then dum = self.lasPointFieldSelector(*(self.lasData), field=field)
  os = os_define()
  if keyword_set(outputPath) then outFile = outputPath else outFile = os.home + os.sep + 'fleurdelasDUMP.csv'
  

  self.Out->print,1, 'Printing out ' + Strcompress(String(nX), /REMOVE_ALL) + ' point structure into ' + outFile

  if n_elements(val) ne 0 then nVal = N_elements(val[*,0]) else nVal = 0

  case nVal of

    0: if size(dum, /TYPE) eq 8 then WRITE_CSV, outFile, dum else WRITE_CSV, outFile, transpose(dum)

    1: begin
        Openw, lun, outFile, /APPEND, /GET_LUN
        temp = Transpose([$
          [dum],$
          [Replicate(val, nX)] $
          ])
        Printf, lun, temp
        Free_lun, lun, /FORCE
       end

    nx: begin
          Openw, lun, outFile, /APPEND, /GET_LUN
          Printf, lun, Transpose([[dum],[val]])
          Free_lun, lun, /FORCE
        end
        
    else: self.Out->print,3,'Hummm something went wrong...'

  endcase

  

  Return, 1

End







;+
; :Description:
;    This function updates the selected index array.
;    This function was develop for the fleurdelas::extractTrees() recursive function
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   none
;
; :Uses:
;   Dum = fleurdelas::updateSelectArray(index)
;
; :Params:
;    index: in, required, type=int
;     a scalar or array of int that represent an index number
;
; :History:
;   JANUARY 2014: initial implementation
;
; :Author: antoine
;-
Function fleurdelas::updateSelectArray, index

  (*self.Selectarray)[index] += 1
  Return, 1

End



;+
; :Description:
;    This function reset the selected index array.
;    This function was develop for the fleurdelas::extractTrees() recursive function
;
; :Category:
;   LAS, GENERAL
;
; :Return:
;   none
;
; :Uses:
;   Dum = fleurdelas::resetselectArray()
;
; :History:
;   JANUARY 2014: initial implementation
;
; :Author: antoine
;-
Function fleurdelas::resetselectArray

  (*self.Selectarray) *= 0
  Return, 1

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
Function fleurDeLas::readVLR, inputFile, header, vlrFileArr, vlrByteSizeArr, vlrId ,vlrArr, obj

  ; Creating a binary file that will hold the VLR Records
  

  ; Creating a temp file that hold ALL the VLR records
  ; XXX: will need to be changed we integrated to fleurDeLas using self.tempDirPath
  vlrFilePath = self.tempDirPath + self.sysSep + 'vlrRecords.bin'
  openw, wLun, vlrFilePath, /GET_LUN



  self.out->print,1, "Number of Variable Length Records: " + strcompress(string(fix(header.nRecords)))
  self.out->print,1, "Reading variable length records..."
  ; If number of VLR not null then read them one at a time and print them into console

  if header.nRecords ne 0 then begin

    InitVRLHeader, vrlStruct
    openr, rLun, inputFile, /swap_if_big_endian, /get_lun
    point_lun, rLun, header.headerSize

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

      outputFile = strcompress(self.tempDirPath + self.sysSep + 'vlr_0' + string(w) + '.gkey',/REMOVE_ALL)
      vlrFileArr[w] = outputFile

      readu, rLun, vrlStruct
      writeu, wLun, vrlStruct

      ; Creating a temp file that hold the nth VLR record - one file per record
      openw, wwLun, outputFile, /get_lun
      writeu, wwLun, vrlStruct
      vlrArr[w] = ptr_new(vrlStruct)

      case 1 of
        
        ; Extra-bytes record
        (vrlStruct.recordID eq 4): begin
          
          self.out->print,1, "Extra-bytes descriptor found"
          
          ; Determine the number of Extra-Bytes payload
          nEB = vrlStruct.RECORDLENGTHAFTERHEARDER / 192
          
          extra_bytes = ORDEREDHASH()
;          key = ["Amplitude","Reflectance","Deviation"]
          
          for ii = 0, nEB-1 do begin
            
            ; has read the VLR header, needs to read out the data_type to know the type
            ; getting the actual position in the file
            point_lun, -rLun, startPosEB
            ; moving to bytes ahead
            point_lun, rLun, startPosEB + 2
            ; reading the data_type byte
            data_type = 0UB
            options = 0UB
            readu, rLun, data_type, options
            ; creating the hash table and convert it a structure
            extra_bytes_structure = self.extraBytesMakeStructure(data_type, options)
            extra_bytes_plain = self.extraBytesMakeStructurePlainByte(data_type, options)
            ; moving back 4 bytes
            point_lun, rLun, startPosEB
            ; reading the extra-bytes structure
            readu, rLun, extra_bytes_structure
            
            ; repeating the read to get the plain structure for concatenation
            Point_lun, rLun, startPosEB
            ; reading the extra-bytes structure
            Readu, rLun, extra_bytes_plain

;            extra_bytes[extra_bytes_structure.name] = self.extraBytesDataType(data_type, options)
            extra_bytes[strcompress(string(extra_bytes_structure.name),/remove_all)] = self.extraBytesDataType(data_type, options)
            
            ; Printing Extra Bytes information
            dum = self.printExtrabytes(data_type, extra_bytes_structure)
            
            ; XXX: for now the Extra Bytes VLR are read but store as plain structure... might not be the best way to do that
            if ii eq 0 then concatEB = extra_bytes_plain else concatEB = [concatEB, extra_bytes_plain]

          endfor
          
          
          ; Convert hash to structure and create the new point data structure
          dum = self.extraBytesNewPointDataStructure(extra_bytes)
;          added_bytes = extra_bytes.ToStruct()
;          void = {newDataPoint, inherits *(self.lasDataStr), inherits added_bytes}
;          self.lasDataStr = ptr_new(void)
          
          writeu, wLun, concatEB
          writeu, wwLun, concatEB
          vlrId[w] = 1
          vlrArr[w+1] = ptr_new(concatEB)
          
          ; Changing self.lasDataStr structure accordingly
          ; Changing self.lasDataStrSz accordingly
        
        end
        
        
        (vrlStruct.recordID eq 7): begin
          self.out->print,1, "Superseded descriptor found"
        
        
        End
        
        
        
        (vrlStruct.recordID ge 100) and (vrlStruct.recordID le 355): begin
          self.out->print,1, "Waveform packet descriptor found"

          wfDescriptor = {$
            bitsPerSample:0B,$
            waveformCompressionType:0B,$
            numberOfSamples:0UL,$
            temporalSampleSpacing:0UL,$
            digitizerGain:0.0D,$
            digitizerOffset:0.0D $
          }
          readu, rLun, wfDescriptor

          waveDescriptor = wfDescriptor


          writeu, wLun, waveDescriptor
          writeu, wwLun, waveDescriptor
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

        readu, rLun, gkdTag
        ;        print, "Number of geokey:",gkdTag.wNumberOfKeys
        geoKeyHeader = gkdTag

        sKeyEntry = {$
          wKeyID:0US,$
          wTIFFTagLocation:0US,$
          wCount:0US,$
          wValueOffset:0US$
      }

      tempKeyEntry = replicate(sKeyEntry, gkdTag.wNumberOfKeys)
      readu, rLun,tempKeyEntry

      geoKeyArray = tempKeyEntry

      writeu, wLun, geoKeyHeader
      writeu, wLun, geoKeyArray
      writeu, wwLun, geoKeyHeader
      writeu, wwLun, geoKeyArray
      vlrId[w] = 2
      tempStruc = {header:gkdTag, key:tempKeyEntry}
      vlrArr[w+1] = ptr_new(tempStruc)
      tempStruc = 0

    end

    else: begin

      if vrlStruct.recordLengthAfterHearder ne 0 then begin
        generic = bytarr(vrlStruct.recordLengthAfterHearder)
        readu, rLun, generic


        writeu, wLun, generic
        writeu, wwLun, generic
        vlrId[w] = 3
        vlrArr[w+1] = ptr_new(generic)
      endif


    end

  endcase

  point_lun, -wwLun, endPos
  free_lun, wwLun, /FORCE
  vlrByteSizeArr[w] = total(vlrByteSizeArr) - header.headerSize


endfor

free_lun, rLun, /FORCE
free_lun, wLun, /FORCE
free_lun, wwLun, /FORCE

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
;   This method is automatically called when the fleurdelas::loadData() is invoque.
;
; :Category:
;   LAS
;
; :Return:
;   The point data structure and the header structure
;
; :Uses:
;   dum = fleurdelas::getLASHeaderDataStr(inputLun, minorVersion, majorVersion, header, dataStr)
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
;     -Integration to the fleurdelas object
;
; :Author:
;   Antoine Cottin
;
; :Hidden:
;-
Function fleurdelas::getLASHeaderDataStr, inputLun, minorVersion, majorVersion, header, dataStr

  self.Out->print,1,Strcompress("LAS Version " + String(Fix(majorVersion)) + "." + Strcompress(String(Fix(minorVersion)),/remove_all) + " detected.")
  self.Out->print,1, "Initializing the header..."
  InitHeaderLAS, header, minorVersion
  self.Out->print,1, "Reading header..."
  point_lun, inputLun, 0
  Readu, inputLun, header

  dum = self.printHeader(header,minorVersion)
  
  self.Out->print,1, "Initializing the point structure..."
  InitDataLAS, dataStr,  pointFormat = header.Pointformat
  self.Out->print,1, "Original point Structure description:"
  self.Out->printArray,1, Tag_names(dataStr)

  Return, 1

End



;
;+
;   The purpose of this method is the read the LAS file.
;   This method is automatically called when the fleurdelas::loadData() is invoque.
;
; :Category:
;   LAS
;
; :Return:
;   The point data structure and the header structure
;
; :Uses:
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
;     -Integration to the fleurdelas object
;
; :Author:
;   Antoine Cottin
;
; :Hidden:
;-
Function fleurdelas::readLAS, inputFile, header, dataStr

  compile_opt idl2, logical_predicate, hidden

  ; Testing the file name
  fileTest = File_info(inputFile);, /NOEXPAND_PATH)
  if fileTest.Exists eq 1 then begin
    self.Out->print,1, "Valid path and file name."
  endif else begin
    self.Out->print,3, "Problem with the file path and/or name."
    self.Out->print,3, "Please check your input."
    self.Out->print,3, "Program closing."
    Return, 0
  endelse

  ; Open the file
  self.Out->print,1, "Reading file " + strcompress(inputFile)
  Openr, inputLun, inputFile, /get_lun, /swap_if_big_endian

  ; Check if the file is a LAS file
  signature = Bytarr(4)
  Readu, inputLun, signature

  if String(signature) eq 'LASF' then begin

    self.Out->print,1, 'LAS file detected..."
    self.Out->print,1, "Looking for version number..."
    Point_lun,inputLun, 24
    majorVersion = 1B
    minorVersion = 1B
    Readu, inputLun, majorVersion
    Readu, inputLun, minorVersion

    ; Closing and re-opening the file to reinitialize the pointer
;    Free_lun,inputLun
;    Openr, inputLun, inputFile, /get_lun, /swap_if_big_endian
;    point_lun, inputLun, 0
    
    dum = self.getLASHeaderDataStr(inputLun, minorVersion, majorVersion, header, dataStr)

    Free_lun, inputLun, /FORCE

  endif else begin
    self.Out->print,3, "LAS file not recognize"
    self.Out->print,3, "Closing the file and terminating..."
    Free_lun, inputLun, /FORCE
    Return, 0
  endelse

  Free_lun, inputLun, /FORCE

  Return, 1

end


;+
; WIP
;-
Function fleurdelas::addGeoKey

  ;  (vrlStruct.recordID eq 34735): begin
  ;    obj->print,1,'"GeoKeyDirectoryTag Record" found'
  ;
  ;    vlrGeoKeyHeader = vrlStruct
  ;
  ;    gkdTag = {$
  ;      wKeyDirectoryVersion:0US,$
  ;      wKeyRevision:0US,$
  ;      wMinorRevision:0US,$
  ;      wNumberOfKeys:0US$    ;TODO to update if we add fields in there
  ;  }
  ;
  ;  readu,1,gkdTag
  ;  ;        print, "Number of geokey:",gkdTag.wNumberOfKeys
  ;  geoKeyHeader = gkdTag
  ;
  ;  sKeyEntry = {$
  ;    wKeyID:0US,$
  ;    wTIFFTagLocation:0US,$
  ;    wCount:0US,$
  ;    wValueOffset:0US$
  ;}
  ;
  ;tempKeyEntry = replicate(sKeyEntry, gkdTag.wNumberOfKeys)
  ;readu,1,tempKeyEntry


  out = Obj_new('consoleclass')
  ;a = obj_new('fleurdelas')
  ;a->load_data, "I:\RG12_10-206b-FW-lidar-20121217\fw_laser\las1.3\LDR-FW-RG12_10-2012-206b-03.LAS"
  vlrGeoKeyArray=a->getvlr(/vlrGeoKeyArray)

  for x=0,N_elements(vlrGeoKeyArray)-1,1 do begin

    case vlrGeoKeyArray(x).Wkeyid of

      3076:begin
            Print,"Finding key " + Strcompress(geotiffGetKeyNameFromCode(vlrGeoKeyArray(x).Wkeyid)) + "[" +Strcompress(String(vlrGeoKeyArray(x).Wkeyid),/remove_all)+"]..."
            Print,"Changing wValueOffset from"+Strcompress(vlrGeoKeyArray(x).Wvalueoffset) + " to 9001..."
            ;TODO: Ask user to enter the new value here
            vlrGeoKeyArray(x).Wvalueoffset=9001US
            Print,"Done!"
           end

      4096:begin
            Print,"Finding key " + Strcompress(geotiffGetKeyNameFromCode(vlrGeoKeyArray(x).Wkeyid)) + "[" + Strcompress(String(vlrGeoKeyArray(x).Wkeyid),/remove_all)+"]..."
            Print,"Changing wValueOffset from"+Strcompress(vlrGeoKeyArray(x).Wvalueoffset) + " to 5001..."
            ;TODO: Ask user to enter the new value here
            vlrGeoKeyArray(x).Wvalueoffset=5001US
            Print,"Done!"
           end

      else:begin
            Print,"Finding key " + Strcompress(geotiffGetKeyNameFromCode(vlrGeoKeyArray(x).Wkeyid)) + "[" + Strcompress(String(vlrGeoKeyArray(x).Wkeyid),/remove_all)+"]..."
            Print,"Nothing to do!"
           end
  
    endcase
  
  endfor


Print, "Adding new field(s) to the geokey vlrGeoKeyArray..."

vlrGeoKeyArray=[vlrGeoKeyArray,{wKeyID:3072US,wTIFFTagLocation:0US,wCount:1US,wValueOffset:27700US}]
nKey = 1

vlrHeaderOfGeoKey = a->getVlr(/vlrHeaderOfGeoKey)
vlrHeaderOfGeoKey.Recordlengthafterhearder = vlrHeaderOfGeoKey.Recordlengthafterhearder + (nKey * 8US)

Print, "Commiting geoKeyArray changes to the file..."
dum = a->setVlrHeaderOfGeoKey(vlrGeoKeyArray)


vlrGeoKeyHeader = a->getVlr(/vlrGeoKeyHeader)
Print,"Updating geoKeyHeader..."
Print, "Adding " + Strcompress(String(nKey),/remove_all) + " key(s) to the geoKey VLR..."
Print, "Updating public header with these new information..."
vlrGeoKeyHeader.Wnumberofkeys=vlrGeoKeyHeader.Wnumberofkeys + nKey

Print, "Commiting geoKeyHeader changes to the file..."
dum = a->setVlrGeoKeyHeader(vlrGeoKeyHeader)
Print, "DUM:",dum

;print,vlrGeoKeyArrayGeoKeyHeader
; Update header if needed
; Each additional key adds 8 bytes
lasHeader = a->getHeaderProperty(/header)
Print, "Updating number of Variable Length Records..."
dum = a->setHeaderNRecords(lasHeader.Nrecords + 1UL)
Print, "New value for number of Variable Length Records: " + Strcompress(String(lasHeader.Nrecords),/remove_all)
Print, "Updating points block offset..."
dum = a->setHeaderDataOffset(lasHeader.Dataoffset + (nKey * 8UL) + 10UL)
Print, "Updating wavefrom block offset..."
if lasHeader.Versionminor ge 3 then dum = a->setHeaderStartWaveform(lasHeader.Startwaveform + (nKey * 8ULL) +10UL)


End
















Function fleurdelas::extraBytesMakeStructure, data_type, options

compile_opt idl2

keys = ['reserved','data_type','options','name','unused','no_data','min','max','scale','offset','description']
extra_bytes_hash = orderedhash()

extra_bytes_hash[keys[0]] = bytarr(2)
extra_bytes_hash[keys[1]] = 0UB
extra_bytes_hash[keys[2]] = 0UB
extra_bytes_hash[keys[3]] = bytarr(32)
extra_bytes_hash[keys[4]] = bytarr(4)
extra_bytes_hash[keys[5]] = self.extraBytesDataUpcasting(data_type)
extra_bytes_hash[keys[6]] = self.extraBytesDataUpcasting(data_type)
extra_bytes_hash[keys[7]] = self.extraBytesDataUpcasting(data_type)
extra_bytes_hash[keys[8]] = dblarr(3)
extra_bytes_hash[keys[9]] = dblarr(3)
extra_bytes_hash[keys[10]] = bytarr(32)

;; For now we just consider everything as 2 bytes array will look at uppercasting later
;extra_bytes_hash[keys[0]] = Bytarr(2)
;extra_bytes_hash[keys[1]] = 0UB
;extra_bytes_hash[keys[2]] = 0UB
;extra_bytes_hash[keys[3]] = Bytarr(32)
;extra_bytes_hash[keys[4]] = Bytarr(4)
;extra_bytes_hash[keys[5]] = bytarr(24)
;extra_bytes_hash[keys[6]] = bytarr(24)
;extra_bytes_hash[keys[7]] = bytarr(24)
;extra_bytes_hash[keys[8]] = Dblarr(3)
;extra_bytes_hash[keys[9]] = Dblarr(3)
;extra_bytes_hash[keys[10]] = Bytarr(32)

; Convert hash to structure and returning the structure
return, extra_bytes_hash.ToStruct()

end


; This is useful for structure concatenation
Function fleurdelas::extraBytesMakeStructurePlainByte, data_type, options

  compile_opt idl2

  keys = ['reserved','data_type','options','name','unused','no_data','min','max','scale','offset','description']
  extra_bytes_hash = Orderedhash()

  ; For now we just consider everything as 2 bytes array will look at uppercasting later
  extra_bytes_hash[keys[0]] = Bytarr(2)
  extra_bytes_hash[keys[1]] = 0UB
  extra_bytes_hash[keys[2]] = 0UB
  extra_bytes_hash[keys[3]] = Bytarr(32)
  extra_bytes_hash[keys[4]] = Bytarr(4)
  extra_bytes_hash[keys[5]] = bytarr(24)
  extra_bytes_hash[keys[6]] = bytarr(24)
  extra_bytes_hash[keys[7]] = bytarr(24)
  extra_bytes_hash[keys[8]] = Dblarr(3)
  extra_bytes_hash[keys[9]] = Dblarr(3)
  extra_bytes_hash[keys[10]] = Bytarr(32)

  ; Convert hash to structure and returning the structure
  Return, extra_bytes_hash.ToStruct()

end



Function fleurdelas::extraBytesDataType, data_type, options

compile_opt idl2

Case data_type of
0:
1:  data =  0UB         ; 1 byte
2:  data =  0B          ; 1 byte
3:  data =  0US         ; 2 bytes
4:  data =  0S          ; 2 bytes
5:  data =  0UL         ; 4 bytes
6:  data =  0L          ; 4 bytes
7:  data =  0ULL        ; 8 bytes
8:  data =  0LL         ; 8 bytes
9:  data =  0.0         ; 4 bytes
10: data =  0.0D        ; 8 bytes
11: data =  bytarr(2)   ; 2 bytes
12: data =  bytarr(2)
13: data =  intarr(2)
14: data =  uintarr(2)
15: data =  ulonarr(2)
16: data =  lonarr(2)
17: data =  ulon64arr(2)
18: data =  lon64arr(2)
19: data =  fltarr(2)
20: data =  dblarr(2)
21: data =  bytarr(3)    ; 3 bytes
22: data =  bytarr(3)
23: data =  intarr(3)
24: data =  uintarr(3)
25: data =  ulonarr(3)
26: data =  lonarr(3)
27: data =  ulon64arr(3)
28: data =  lon64arr(3)
29: data =  fltarr(3)
30: data =  dblarr(3)
ELSE:
Endcase

return, data


End




Function fleurdelas::extraBytesDataName, data_type

  compile_opt idl2

  Case data_type of
    0:  data = "(Undocumented extra bytes)"
    1:  data = "(uchar)"
    2:  data = "(char)"
    3:  data = "(ushort)"
    4:  data = "(short)"
    5:  data = "(ulong)"
    6:  data = "(long)"
    7:  data = "(unsigned long long)"
    8:  data = "(long long)"
    9:  data = "(float)"
    10: data = "(double)"   
    11:  data = "(uchar[2])"
    12:  data = "(char[2])"
    13:  data = "(ushort[2])"
    14:  data = "(short[2])"
    15:  data = "(ulong[2])"
    16:  data = "(long[2])"
    17:  data = "(unsigned long long[2])"
    18:  data = "(long long[2])"
    19:  data = "(float[2])"
    20:  data = "(double[2])"
    21:  data = "(uchar[3])"
    22:  data = "(char[3])"
    23:  data = "(ushort[3])"
    24:  data = "(short[3])"
    25:  data = "(ulong[3])"
    26:  data = "(long[3])"
    27:  data = "(unsigned long long[3])"
    28:  data = "(long long[3])"
    29:  data = "(float[3])"
    30: data = "(double[3])"
    ELSE:
  Endcase

  Return, data


End




Function fleurdelas::extraBytesDataUpcasting, data_type

  compile_opt idl2

  Case data_type of
    0:  data =  bytarr(24)
    1:  data =  ulon64arr(3)
    2:  data =  lon64arr(3)
    3:  data =  ulon64arr(3)
    4:  data =  lon64arr(3)
    5:  data =  ulon64arr(3)
    6:  data =  lon64arr(3)
    7:  data =  ulon64arr(3)
    8:  data =  lon64arr(3)
    9:  data =  dblarr(3)
    10: data =  dblarr(3)
    11:  data =  Ulon64arr(3)
    12:  data =  Lon64arr(3)
    13:  data =  Ulon64arr(3)
    14:  data =  Lon64arr(3)
    15:  data =  Ulon64arr(3)
    16:  data =  Lon64arr(3)
    17:  data =  Ulon64arr(3)
    18:  data =  Lon64arr(3)
    19:  data =  Dblarr(3)
    20: data =  Dblarr(3)
    21:  data =  Ulon64arr(3)
    22:  data =  Lon64arr(3)
    23:  data =  Ulon64arr(3)
    24:  data =  Lon64arr(3)
    25:  data =  Ulon64arr(3)
    26:  data =  Lon64arr(3)
    27:  data =  Ulon64arr(3)
    28:  data =  Lon64arr(3)
    29:  data =  Dblarr(3)
    30: data =  Dblarr(3)
    ELSE:
  Endcase

  Return, data

End




Function fleurdelas::extraBytesNewPointDataStructure, added_bytes

  basePoint = Orderedhash()

  for i=0, N_tags(*(self.Lasdatastr))-1 do begin

    basePoint[(Tag_names(*(self.Lasdatastr)))[i]] = (*(self.Lasdatastr)).(i)

  endfor

  newPoint = basePoint + added_bytes

  self.Lasdatastr = Ptr_new(newPoint.toStruct())
  
  return, 1

End




Pro fleurdelas__define

  COMPILE_OPT idl2, HIDDEN
  
  ; Definition of the data hold by the object
  void = {fleurdelas, $
    fldVersionMajor   : 0B,$                    ; Fleurdelas Major Version
    fldVersionMinor   : 0B,$                    ; Fleurdelas Minor Version
    fdlReleaseDate    : 0,$                     ; Fleurdelas Release date version
    lasFilePath       : '',$                    ; String representing the path to the LAS file
    surveyDay         : '',$                    ; String holding the Julian Survey day - only with ARSF-NERC dataset
    tempDirPath       : '',$                    ; String holdinf the temporary directory path to store temp file(s) if required
    rootPath          : '',$                    ; Path of the directory where the project is located
    waveFileExt       : '',$                    ; Extention of the external waveform file, WPD | INW
    sysSep            : '',$
    lasHeader         : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the header of the LAS file
    lasDataStr        : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the Point data structure
    lasDataStrSz      : 0B,$                    ; Size in bytes of the point data structure
    getDataIndex      : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the index of selected data from the last getData call
    lasData           : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the point data from the last getData call
    lasDataIndBackup  : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to an index of the original data set to retrieve original data set
    lasDataExtent     : Dblarr(4),$             ; Pointer to a four element double array that contains the geographical extend of the selected data - [xMax, xMin, yMax, yMin]
;    lasNTiles         : 0UL,$                   ; Number of tiles generated
;    lasDataTile       : Ptr_new(),$             ; Pointer to a structure that contains the tile index and the points index
;    recTileNumb       : 0L,$                    ; Tile number for recurrent call of extractTrees
;    recStartFlag      : 0B,$                    ; A flag to say it the very first loop of the recursive process
;    recData           : ptr_new(),$             ; Pointer to a data array that is required for the recursive process
;    recNData          : 0ULL,$                  ; Number of elements in the recData field
;    lasTileExtent     : Dblarr(4),$             ; Pointer to a four element double array that contains the geographical extend of the Tiles - [xMax, xMin, yMax, yMin]
;    lasTileFileName   : '',$                    ; String of the Tile Index File.
;    lasTileHisto      : Ptr_new(),$             ; Pointer to a ULON64ARR that holds the histogram distribution of points per tile
    lasWaveDsptr      : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the waveform packet descriptor containing :  bits per sample, wf conversion type, # of samples, temporal spacing, digitizer gain, digitizer offset
    lasWaveDsptrHdr   : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the waveform packet descriptor header
    lasWaveEvlrHeader : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the waveform packet header
    lasWave           : Ptr_new(/ALLOCATE_HEAP),$             ; Pointer to the waveform data from the last getData call
    vlrFileID         : Ptr_new(!NULL),$        ; Pointer to an string array that the path to the temporary files that hold the files name
    vlrByteSize       : Ptr_new(!NULL),$        ; Pointer to an Unsigned Long array that holds the byte size of each key
    vlrId             : Ptr_new(!NULL),$        ; Pointer to a byte array that holds a byte flag to describ the geokey
    vlrArr            : Ptr_new(!NULL),$        ; Pointer to a pointer array that contains the key in reading order header/key
    xyz               : Ptr_new(!NULL),$        ; Pointer that hold the XYZ coordinates from the points cloud
;    selectArray       : Ptr_new(),$             ; Binary Array that acts like a selected flag
;    recursiveN        : 0ULL,$                  ; Counter flag for the tree extraction method recursion
    out               : Obj_new(),$             ; Object holding the console output object
;    xTile             : 300.0 ,$                ; x(meter/feets)/easting(meter/feets)/longitude(degree) size of the tile
;    yTile             : 300.0 ,$                ; y(meter/feets)/northing(meter/feets)/latitude(degree) size of the tile
    mthreadlock       : 0B  $                   ; A bit that avoid the deletion of the temp dir while computing in multi-threading mode
;    Vis_Pix           : 0l, $                   ; Plot procedure - beta - Index of the invisible pixmap
;    F_Vis_Pix         : 0l  $                   ; Plot procedure - beta - Index of the invisible pixmap flightlines
  }


End
