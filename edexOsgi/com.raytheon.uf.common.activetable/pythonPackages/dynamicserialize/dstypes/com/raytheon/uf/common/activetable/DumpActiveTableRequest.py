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

class DumpActiveTableRequest(object):

    def __init__(self):
        self.actions = None
        self.etns = None
        self.fileContent = None
        self.fileName = None
        self.fromSite = None
        self.ids = None
        self.mode = None
        self.phens = None
        self.pils = None
        self.sigs = None
        self.sites = None

    def getActions(self):
        return self.actions

    def setActions(self, actions):
        self.actions = actions

    def getEtns(self):
        return self.etns

    def setEtns(self, etns):
        self.etns = etns

    def getFileContent(self):
        return self.fileContent

    def setFileContent(self, fileContent):
        self.fileContent = fileContent

    def getFileName(self):
        return self.fileName

    def setFileName(self, fileName):
        self.fileName = fileName

    def getFromSite(self):
        return self.fromSite

    def setFromSite(self, fromSite):
        self.fromSite = fromSite

    def getIds(self):
        return self.ids

    def setIds(self, ids):
        self.ids = ids

    def getMode(self):
        return self.mode

    def setMode(self, mode):
        self.mode = mode

    def getPhens(self):
        return self.phens

    def setPhens(self, phens):
        self.phens = phens

    def getPils(self):
        return self.pils

    def setPils(self, pils):
        self.pils = pils

    def getSigs(self):
        return self.sigs

    def setSigs(self, sigs):
        self.sigs = sigs

    def getSites(self):
        return self.sites

    def setSites(self, sites):
        self.sites = sites

