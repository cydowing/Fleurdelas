Function headerToStringArray, header

fieldNames = [$
'File signature',$
'File source ID',$
'Global encoding',$
'GPS time type',$
'Is the waveform packet internal',$
'Is the waveform packet external',$
'Return numbers synthetically generated',$
'Project ID - GUID data 1',$
'Project ID - GUID data 2',$
'Project ID - GUID data 3',$
'Project ID - GUID data 4',$
'Version major',$
'Version minor',$
'System identifier',$
'Generating software',$
'File creation day of year',$
'File creation year',$
'Header size',$
'Offset to point data',$
'Number of variable length records',$
'Point data format ID',$
'Point data record length',$
'Number of point records',$
'Number of points for first return',$
'Number of points for second return',$
'Number of points for third return',$
'Number of points for fourth return',$
'Number of points for fith return',$
'X scale factor',$
'Y scale factor',$
'Z scale factor',$
'X offset',$
'Y offset',$
'Z offset',$
'Max X',$
'Min X',$
'Max Y',$
'Min Y',$
'Max Z',$
'Min Z',$
'Start of Waveform data packet'$
]


stringArray = strarr(2,n_elements(fieldNames))

stringArray[0,*] = fieldNames

stringArray[1,0] = strcompress(string(header.signature), /REMOVE_ALL)
stringArray[1,1] = strcompress(string(header.fileSource), /REMOVE_ALL)
stringArray[1,2] = strcompress(string(header.reserved), /REMOVE_ALL)
stringArray[1,3] = strcompress(string((globalEncodingReader(header))[0], format = '(B0)'), /REMOVE_ALL)
stringArray[1,4] = strcompress(string((globalEncodingReader(header))[1], format = '(B0)'), /REMOVE_ALL)
stringArray[1,5] = strcompress(string((globalEncodingReader(header))[2], format = '(B0)'), /REMOVE_ALL)
stringArray[1,6] = strcompress(string((globalEncodingReader(header))[3], format = '(B0)'), /REMOVE_ALL)
stringArray[1,7] = strcompress(string(header.guid1), /REMOVE_ALL)
stringArray[1,8] = strcompress(string(header.guid2), /REMOVE_ALL)
stringArray[1,9] = strcompress(string(header.guid3), /REMOVE_ALL)
stringArray[1,10] = strcompress(string(header.guid4), /REMOVE_ALL)
stringArray[1,11] = strcompress(string(header.versionMajor, format = '(B0)'), /REMOVE_ALL)
stringArray[1,12] = strcompress(string(header.versionMinor, format = '(B0)'), /REMOVE_ALL)
stringArray[1,13] = strcompress(string(header.systemID))
stringArray[1,14] = strcompress(string(header.softwareID))
stringArray[1,15] = strcompress(string(header.day), /REMOVE_ALL)
stringArray[1,16] = strcompress(string(header.year), /REMOVE_ALL)
stringArray[1,17] = strcompress(string(header.headerSize), /REMOVE_ALL)
stringArray[1,18] = strcompress(string(header.dataOffset), /REMOVE_ALL)
stringArray[1,19] = strcompress(string(header.nRecords), /REMOVE_ALL)
stringArray[1,20] = strcompress(string(header.pointFormat, format = '(B0)'), /REMOVE_ALL)
stringArray[1,21] = strcompress(string(header.pointLength), /REMOVE_ALL)
stringArray[1,22] = strcompress(string(header.nPoints), /REMOVE_ALL)
stringArray[1,23] = strcompress(string((header.nReturns)[0]), /REMOVE_ALL)
stringArray[1,24] = strcompress(string((header.nReturns)[1]), /REMOVE_ALL)
stringArray[1,25] = strcompress(string((header.nReturns)[2]), /REMOVE_ALL)
stringArray[1,26] = strcompress(string((header.nReturns)[3]), /REMOVE_ALL)
stringArray[1,27] = strcompress(string((header.nReturns)[4]), /REMOVE_ALL)
stringArray[1,28] = strcompress(string(header.xScale), /REMOVE_ALL)
stringArray[1,29] = strcompress(string(header.yScale), /REMOVE_ALL)
stringArray[1,30] = strcompress(string(header.zScale), /REMOVE_ALL)
stringArray[1,31] = strcompress(string(header.xOffset), /REMOVE_ALL)
stringArray[1,32] = strcompress(string(header.yOffset), /REMOVE_ALL)
stringArray[1,33] = strcompress(string(header.zOffset), /REMOVE_ALL)
stringArray[1,34] = strcompress(string(header.xMax), /REMOVE_ALL)
stringArray[1,35] = strcompress(string(header.xMin), /REMOVE_ALL)
stringArray[1,36] = strcompress(string(header.yMax), /REMOVE_ALL)
stringArray[1,37] = strcompress(string(header.yMin), /REMOVE_ALL)
stringArray[1,38] = strcompress(string(header.zMax), /REMOVE_ALL)
stringArray[1,39] = strcompress(string(header.zMin), /REMOVE_ALL)
if header.pointFormat ge 4 then stringArray[1,40] = strcompress(string(header.startWaveform), /REMOVE_ALL) else stringArray[1,40] = 'N/A'

return, stringArray

End