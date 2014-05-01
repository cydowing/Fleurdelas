; docformat = 'IDL'
 
;+
; NAME:
;
;       InitHeaderLAS
;
; PURPOSE:
;
;       This program initializes a structure to read the header of a .las
;       lidar file.
;
;       For more information on the .las lidar data format, see http://www.lasformat.org
;
; AUTHOR:
;
;       Antoine Cottin
;       Carbomap Ltd
;       Email : a.cottin@carbomap.com
;       
;
; CALLING SEQUENCE:
;
;       InitHeaderLAS, header
;
; RETURN VALUE:
;
;       The program returns a structure corresponding to the header of a .las file.
;
; KNOWN ISSUES:
;
;       None.
;
; MODIFICATION HISTORY:
;
;       Written by David Streutker, March 2006.
;       Modify by Antoine Cottin, July 2012.
;         - Add support to LAS 1.3 and above
;       Modify by Antoine Cottin, September 2012.
;         - Correct header nReturns field
;
;###########################################################################
Pro initheaderlas, header, minorVersion

compile_opt idl2

    ; Define and read public header structure

header = {header0, $
    signature       : byte('LASF'), $               ; File signature
    fileSource      : 0US,  $                       ; File source ID
    reserved        : 0US,  $                       ; Reserved
    guid1           : 0UL, $                        ; Project ID - GUID data 1
    guid2           : 0US,  $                       ; Project ID - GUID data 2
    guid3           : 0US,  $                       ; Project ID - GUID data 3
    guid4           : bytarr(8), $                  ; Project ID - GUID data 4
    versionMajor    : 1B, $                         ; Version major
    versionMinor    : 1B, $                         ; Version minor
    systemID        : bytarr(32), $                 ; System identifier
    softwareID      : bytarr(32), $                 ; Generating software
    day             : 0US,    $                     ; File creation day of year
    year            : 0US,    $                     ; File creation year
    headerSize      : 0US,  $                       ; Header size
    dataOffset      : 0UL, $                        ; Offset to point data
    nRecords        : 0UL,   $                      ; Number of variable length records
    pointFormat     : 1B,    $                      ; Point data format ID
    pointLength     : 0US,   $                      ; Point data record length
    nPoints         : 0UL,   $                      ; Number of point records
    nReturns        : ulonarr(5), $                 ; Number of points by return -> LAS specs says 7 but seems not to be true
    xScale          : 0D, $                         ; X scale factor
    yScale          : 0D, $                         ; Y scale factor
    zScale          : 0D, $                         ; Z scale factor
    xOffset         : 0D, $                         ; X offset
    yOffset         : 0D, $                         ; Y offset
    zOffset         : 0D, $                         ; Z offset
    xMax            : 0D, $                         ; Max X
    xMin            : 0D, $                         ; Min X
    yMax            : 0D, $                         ; Max Y
    yMin            : 0D, $                         ; Min Y
    zMax            : 0D, $                         ; Max Z
    zMin            : 0D  $                         ; Min Z
}

if minorVersion eq 3 then header = {header1, inherits header0, startWaveform:0ULL}  ; Start of Waveform Data Packet Record
if minorVersion eq 4 then header = {header1, inherits header0, $
                                    startWaveform   : 0ULL, $                       ; Start of Waveform Data Packet Record
                                    firstEVLR       : 0ULL, $                       ; Start of first Extended Variable Length Record
                                    nEVLR           : 0UL,  $                       ; Number of Extended Variable Length Record
                                    nPoints         : 0ULL, $                       ; Number of Point Records
                                    nPointsByReturn : Ulon64arr(15) $              ; Number of Points by Return
                                    }
                                    
End
                                    
