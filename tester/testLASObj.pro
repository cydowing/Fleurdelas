;+
; :Description:
;    This is a simple pro file to demonstrate how to use the IDL laslib
;
; :Category:
; 	HELP
;
; :Return:
; 	None
;
;	:Uses:
;		.COMPILE testLASObj.pro
;		testLASObj
;
; :History:
; 	April 2014
; 	 - Creation
;
; :Author: antoine
;-
Pro testLASObj

; Initialization of the carbslib object 
lasobj = obj_new('carbslib')

; Loading the data from the MiltonKeynes.las file
; The input file can be a relative path to the IDL root path, or a fully qualified path
lasobj.loadDataWithPath, inputFile = './data/MiltonKeynes.las'

; Get all the points that have been loaded into memory - for further manipulations
; Dum is an array of structure (of LAS point)
dum = lasobj.getData(/ALL)

; Get the 250th data point
dum = lasobj.getData(pointNumber = 250)

; Get the associated waveform (if it exist)
duw = lasobj.getWave()

; Get a set of data points - 100th to 199th point
index = indgen(100)+100
dum = lasobj.getData(pointNumber = index)

; Also note that the point data can be filtered by height - check the laslib::getData comment for more info

; Get data that lie into the bounding box define by the 4 elements array
dum = lasobj.getData(boundingBox=[486492.218750D, 486419.937500D, 239796.812500D, 239399.734375D])

; Dump the load data points (the ones from the last laslib::getData() call) into an ascii file
dum = lasobj.dump()


End
