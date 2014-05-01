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

class GetActiveTableDictRequest(object):

    def __init__(self):
        self.requestedSiteId = None
        self.mode = None
        self.wfos = None

    def getRequestedSiteId(self):
        return self.requestedSiteId

    def setRequestedSiteId(self, requestedSiteId):
        self.requestedSiteId = requestedSiteId

    def getMode(self):
        return self.mode

    def setMode(self, mode):
        self.mode = mode

    def getWfos(self):
        return self.wfos
    
    def setWfos(self, wfos):
        self.wfos = wfos;
