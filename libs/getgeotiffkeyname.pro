Function getGeoTiffKeyName, code

case code of
  1024 : value = "GTModelTypeGeoKey"
  1025 : value = "GTRasterTypeGeoKey"
  1026 : value = "GTCitationGeoKey"
  2048 : value = "GeographicTypeGeoKey"
  2049 : value = "GeogCitationGeoKey"
  2050 : value = "GeogGeodeticDatumGeoKey"
  2051 : value = "GeogPrimeMeridianGeoKey"
  2052 : value = "GeogLinearUnitsGeoKey"
  2053 : value = "GeogLinearUnitSizeGeoKey"
  2054 : value = "GeogAngularUnitsGeoKey"
  2055 : value = "GeogAngularUnitSizeGeoKey"
  2056 : value = "GeogEllipsoidGeoKey"
  2057 : value = "GeogSemiMajorAxisGeoKey"
  2058 : value = "GeogSemiMinorAxisGeoKey"
  2059 : value = "GeogInvFlatteningGeoKey"
  2060 : value = "GeogAzimuthUnitsGeoKey"
  2061 : value = "GeogPrimeMeridianLongGeoKey"
  3072 : value = "ProjectedCSTypeGeoKey"
  3073 : value = "PCSCitationGeoKey"
  3074 : value = "ProjectionGeoKey"
  3075 : value = "ProjCoordTransGeoKey"
  3076 : value = "ProjLinearUnitsGeoKey"
  3077 : value = "ProjLinearUnitSizeGeoKey"
  3078 : value = "ProjStdParallel1GeoKey or ProjStdParallelGeoKey"
  3079 : value = "ProjStdParallel2GeoKey"
  3080 : value = "ProjNatOriginLongGeoKey or ProjOriginLongGeoKey"
  3081 : value = "ProjNatOriginLatGeoKey or ProjOriginLatGeoKey"
  3082 : value = "ProjFalseEastingGeoKey"
  3083 : value = "ProjFalseNorthingGeoKey"
  3084 : value = "ProjFalseOriginLongGeoKey"
  3085 : value = "ProjFalseOriginLatGeoKey"
  3086 : value = "ProjFalseOriginEastingGeoKey"
  3087 : value = "ProjFalseOriginNorthingGeoKey"
  3088 : value = "ProjCenterLongGeoKey"
  3089 : value = "ProjCenterLatGeoKey"
  3090 : value = "ProjCenterEastingGeoKey"
  3091 : value = "ProjCenterNorthingGeoKey"
  3092 : value = "ProjScaleAtNatOriginGeoKey or ProjScaleAtOriginGeoKey"
  3093 : value = "ProjScaleAtCenterGeoKey"
  3094 : value = "ProjAzimuthAngleGeoKey"
  3095 : value = "ProjStraightVertPoleLongGeoKey"
  4096 : value = "VerticalCSTypeGeoKey"
  4097 : value = "VerticalCitationGeoKey"
  4098 : value = "VerticalDatumGeoKey"
  4099 : value = "VerticalUnitsGeoKey"
  else : value = "Not a recognize key value!!!"
endcase 

return, value

End


Function getKeyNameFromGeoTiffkey, key

  case 1  of
    strupcase(key) eq "GTMODELTYPEGEOKEY"                              : value = 1024
    strupcase(key) eq "GTRASTERTYPEGEOKEY"                             : value = 1025
    strupcase(key) eq "GTCITATIONGEOKEY"                               : value = 1026
    strupcase(key) eq "GEOGRAPHICTYPEGEOKEY"                           : value = 2048
    strupcase(key) eq "GEOGCITATIONGEOKEY"                             : value = 2049
    strupcase(key) eq "GEOGGEODETICDATUMGEOKEY"                        : value = 2050
    strupcase(key) eq "GEOGPRIMEMERIDIANGEOKEY"                        : value = 2051
    strupcase(key) eq "GEOGLINEARUNITSGEOKEY"                          : value = 2052
    strupcase(key) eq "GEOGLINEARUNITSIZEGEOKEY"                       : value = 2053
    strupcase(key) eq "GEOGANGULARUNITSGEOKEY"                         : value = 2054
    strupcase(key) eq "GEOGANGULARUNITSIZEGEOKEY"                      : value = 2055
    strupcase(key) eq "GEOGELLIPSOIDGEOKEY"                            : value = 2056
    strupcase(key) eq "GEOGSEMIMAJORAXISGEOKEY"                        : value = 2057
    strupcase(key) eq "GEOGSEMIMINORAXISGEOKEY"                        : value = 2058
    strupcase(key) eq "GEOGINVFLATTENINGGEOKEY"                        : value = 2059
    strupcase(key) eq "GEOGAZIMUTHUNITSGEOKEY"                         : value = 2060
    strupcase(key) eq "GEOGPRIMEMERIDIANLONGGEOKEY"                    : value = 2061
    strupcase(key) eq "PROJECTEDCSTYPEGEOKEY"                          : value = 3072
    strupcase(key) eq "PCSCITATIONGEOKEY"                              : value = 3073
    strupcase(key) eq "PROJECTIONGEOKEY"                               : value = 3074
    strupcase(key) eq "PROJCOORDTRANSGEOKEY"                           : value = 3075
    strupcase(key) eq "PROJLINEARUNITSGEOKEY"                          : value = 3076
    strupcase(key) eq "PROJLINEARUNITSIZEGEOKEY"                       : value = 3077
    strupcase(key) eq "PROJSTDPARALLEL1GEOKEY OR PROJSTDPARALLELGEOKEY": value = 3078
    strupcase(key) eq "PROJSTDPARALLEL2GEOKEY"                         : value = 3079
    strupcase(key) eq "PROJNATORIGINLONGGEOKEY OR PROJORIGINLONGGEOKEY": value = 3080
    strupcase(key) eq "PROJNATORIGINLATGEOKEY OR PROJORIGINLATGEOKEY"  : value = 3081
    strupcase(key) eq "PROJFALSEEASTINGGEOKEY"                         : value = 3082
    strupcase(key) eq "PROJFALSENORTHINGGEOKEY"                        : value = 3083
    strupcase(key) eq "PROJFALSEORIGINLONGGEOKEY"                      : value = 3084
    strupcase(key) eq "PROJFALSEORIGINLATGEOKEY"                       : value = 3085
    strupcase(key) eq "PROJFALSEORIGINEASTINGGEOKEY"                   : value = 3086
    strupcase(key) eq "PROJFALSEORIGINNORTHINGGEOKEY"                  : value = 3087
    strupcase(key) eq "PROJCENTERLONGGEOKEY"                           : value = 3088
    strupcase(key) eq "PROJCENTERLATGEOKEY"                            : value = 3089
    strupcase(key) eq "PROJCENTEREASTINGGEOKEY"                        : value = 3090
    strupcase(key) eq "PROJCENTERNORTHINGGEOKEY"                       : value = 3091
    strupcase(key) eq "PROJSCALEATNATORIGINGEOKEY OR PROJSCALEATORIGINGEOKEY" : value = 3092
    strupcase(key) eq "PROJSCALEATCENTERGEOKEY"                        : value = 3093
    strupcase(key) eq "PROJAZIMUTHANGLEGEOKEY"                         : value = 3094
    strupcase(key) eq "PROJSTRAIGHTVERTPOLELONGGEOKEY"                 : value = 3095
    strupcase(key) eq "VERTICALCSTYPEGEOKEY"                           : value = 4096
    strupcase(key) eq "VERTICALCITATIONGEOKEY"                         : value = 4097
    strupcase(key) eq "VERTICALDATUMGEOKEY"                            : value = 4098
    strupcase(key) eq "VERTICALUNITSGEOKEY"                            : value = 4099
    else:value = "Not a recognize key definition!!!"
  endcase

  return, value

End