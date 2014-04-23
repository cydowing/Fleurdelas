Function xyzConverter, point, header

x = (point.east * header.xScale) + header.xOffset
y = (point.north * header.yScale) + header.yOffset
z = (point.elev * header.zScale) + header.zOffset

return, [x,y,z]

End
