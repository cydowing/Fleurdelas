; docformat = 'rst'
;+
; This program initializes a structure to read each point data record
; from a .las lidar file.
;
; :Category:
; 	LAS
;
; :Return:
;   The program returns a single structure corresponding to
;   a single data record of a .las file.
;   
;   For more information on the .las lidar data format, see http://www.lasformat.org
;
;	:Uses:
;   InitDataLAS, pData, PointFormat=PointFormat
;
;       PointFormat: Specifies the requested format of the data record.
;
;	:Example:
;		A quick example on how to use this method
;
; :Params:
;    pData: out, required, type=structure
;     This represent the returned structure corresponding to the point format
;
; :Keywords:
;    pointFormat: in, required, type=byte
;     This byte describe the point format
;
; :History:
;   Written by David Streutker, March 2006.
;   Modify by Antoine Cottin, July 2012.
;     - Add support to LAS file point format 4 and 5
;
; :Author:
;   Antoine Cottin
;-
pro initdatalas, pData, pointFormat = pointFormat

compile_opt idl2, logical_predicate

    ; Define the data structure

pData = {format0,  $
    east    : 0L,  $     ; X data
    north   : 0L,  $     ; Y data
    elev    : 0L,  $     ; Z data
    inten   : 0US, $     ; Intensity
    nReturn : 0B,  $     ; Return number, number of returns, scan direction, edge
    class   : 0B,  $     ; Classification
    angle   : 0B,  $     ; Scan angle
    user    : 0B,  $     ; User data
    source  : 0US  $     ; Point source ID
}

    ; Modifying the point structure in function of the pointFormat

if pointFormat eq 1 then pData = {format1, inherits format0, time:0.0D}    ; GPS time field

if pointFormat eq 2 then pData = {format2, inherits format0, R:0US, G:0US, B:0US}    ; RGB channels values

if pointFormat eq 3 then pData = {format3, inherits format0, time:0.0D, R:0US, G:0US, B:0US}    ; GPS time field & RGB channels values

if pointFormat eq 4 then pData = {format4, inherits format0, time:0.0D, wDescriptorIndex:0B, offsetWaveData:0ULL, wPacketSize:0UL, returnPointWaveLocation:0.0, X:0.0, Y:0.0, Z:0.0 }

if pointFormat eq 5 then pData = {format5, inherits format3, wDescriptorIndex:0B, offsetWaveData:0ULL, wPacketSize:0UL, returnPointWaveLocation:0.0, X:0.0, Y:0.0, Z:0.0 }

end