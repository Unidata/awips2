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
#    03/25/14        #2884         randerso       Added xxxid to VTECChange

class VTECChange(object):

    def __init__(self):
        self.site = None
        self.pil = None
        self.phensig = None
        self.xxxid = None

    def getSite(self):
        return self.site

    def setSite(self, site):
        self.site = site

    def getPil(self):
        return self.pil

    def setPil(self, pil):
        self.pil = pil

    def getPhensig(self):
        return self.phensig

    def setPhensig(self, phensig):
        self.phensig = phensig

    def getXxxid(self):
        return self.xxxid

    def setXxxid(self, xxxid):
        self.xxxid = xxxid
