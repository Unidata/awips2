##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
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
