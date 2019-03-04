##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SiteInfo
#  Information about the NWS sites
#
# Author: mathewson/hansen
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

class SiteInfo:
    def __init__(self):
        pass
    def region(self, siteID):
        return self.getInfo("region", siteID)
    def fullStationID(self, siteID):
        return self.getInfo("fullStationID", siteID)
    def wfoCityState(self, siteID):
        return self.getInfo("wfoCityState", siteID)
    def wfoCity(self, siteID):
        return self.getInfo("wfoCity", siteID)

    def getInfo(self, field, siteID):
        try:
            return SiteInfoDict[siteID][field]
        except:
            return None

from SiteCFG import SiteInfo as SiteInfoDict
