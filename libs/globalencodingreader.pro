Function globalEncodingReader, header

gpsTimeType = header.reserved and '00000001'bb
wdpIsInternal = ishft(header.reserved, -1) and '00000001'bb
wdpIsExternal = ishft(header.reserved, -1) and '00000001'bb
syntheticReturns = ishft(header.reserved, -1) and '00000001'bb

return, [gpsTimeType, wdpIsInternal, wdpIsExternal, syntheticReturns]

End 