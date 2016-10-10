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

# File auto-generated against equivalent DynamicSerialize Java class
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Oct 10, 2016    5916          bsteffen       Generated

class GetGridLatLonRequest(object):

    def __init__(self):
        self.envelope = None
        self.crsWkt = None
        self.nx = None
        self.ny = None

    def getEnvelope(self):
        return self.envelope

    def setEnvelope(self, envelope):
        self.envelope = envelope

    def getCrsWkt(self):
        return self.crsWkt

    def setCrsWkt(self, crsWkt):
        self.crsWkt = crsWkt

    def getNx(self):
        return self.nx

    def setNx(self, nx):
        self.nx = nx

    def getNy(self):
        return self.ny

    def setNy(self, ny):
        self.ny = ny

