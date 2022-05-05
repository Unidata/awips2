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

class GetVtecAttributeRequest(object):

    def __init__(self):
        self.siteId = None
        self.attribute = None
        self.defaultValue = None
        
    def getSiteId(self):
        return self.siteId
    
    def setSiteId(self, site):
        self.siteId = site

    def getAttribute(self):
        return self.attribute
    
    def setAttribute(self, attribute):
        self.attribute = attribute
        
    def getDefaultValue(self):
        return self.defaultValue
    
    def setDefaultValue(self, default):
        self.defaultValue = default
