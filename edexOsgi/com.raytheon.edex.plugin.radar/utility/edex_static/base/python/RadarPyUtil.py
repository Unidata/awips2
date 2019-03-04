##
##
# to recreate the menu files for radar and scan

def generateMenuFiles():
    from com.raytheon.uf.common.dataplugin.radar.util import RadarMenuUtil
    r = RadarMenuUtil()
    from com.raytheon.uf.common.message.response import ResponseMessageGeneric
    return ResponseMessageGeneric("Rebuilt radar menu files")