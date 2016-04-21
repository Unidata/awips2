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
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/22/2015       4522         randerso       Initial creation (hand generated)
#    03/17/2016       5426         randerso       Add issueYear to primary key
#
##    

import ActiveTableKey
import abc

class ActiveTableRecord(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __init__(self):
        self.key = ActiveTableKey.ActiveTableKey()
        self.wmoid = None
        self.pil = None
        self.xxxid = None
        self.countyheader = None
        self.vtecstr = None
        self.productClass = None
        self.act = None
        self.startTime = None
        self.endTime = None
        self.issueTime = None
        self.purgeTime = None
        self.ufn = None
        self.geometry = None
        self.forecaster = None
        self.motdir = None
        self.motspd = None
        self.loc = None
        self.rawmessage = None
        self.seg = None
        self.phensig = None
        self.region = None
        self.overviewText = None
        self.segText = None
        self.locationID = None
        self.floodSeverity = None
        self.immediateCause = None
        self.floodRecordStatus = None
        self.floodBegin = None
        self.floodCrest = None
        self.floodEnd = None
        self.identifier = None

    def getKey(self):
        return self.key
    
    def setKey(self, key):
        self.key = key

    def getWmoid(self):
        return self.wmoid

    def setWmoid(self, wmoid):
        self.wmoid = wmoid

    def getPil(self):
        return self.pil

    def setPil(self, pil):
        self.pil = pil

    def getXxxid(self):
        return self.xxxid

    def setXxxid(self, xxxid):
        self.xxxid = xxxid

    def getCountyheader(self):
        return self.countyheader

    def setCountyheader(self, countyheader):
        self.countyheader = countyheader

    def getUgcZone(self):
        return self.key.getUgcZone()

    def setUgcZone(self, ugcZone):
        self.key.setUgcZone(ugcZone)

    def getVtecstr(self):
        return self.vtecstr

    def setVtecstr(self, vtecstr):
        self.vtecstr = vtecstr

    def getProductClass(self):
        return self.productClass

    def setProductClass(self, productClass):
        self.productClass = productClass

    def getAct(self):
        return self.act

    def setAct(self, act):
        self.act = act

    def getOfficeid(self):
        return self.key.getOfficeid()

    def setOfficeid(self, officeid):
        self.key.setOfficeid(officeid)

    def getPhen(self):
        return self.key.getPhen()

    def setPhen(self, phen):
        self.key.setPhen(phen)

    def getSig(self):
        return self.key.getSig()

    def setSig(self, sig):
        self.key.setSig(sig)

    def getEtn(self):
        return self.key.getEtn()

    def setEtn(self, etn):
        self.key.setEtn(etn)

    def getStartTime(self):
        return self.startTime

    def setStartTime(self, startTime):
        self.startTime = startTime

    def getEndTime(self):
        return self.endTime

    def setEndTime(self, endTime):
        self.endTime = endTime

    def getIssueTime(self):
        return self.issueTime

    def setIssueTime(self, issueTime):
        from datetime import datetime
        date = datetime.utcfromtimestamp(issueTime.getTime()/1000)
        self.key.setIssueYear(date.year)
        self.issueTime = issueTime

    def getPurgeTime(self):
        return self.purgeTime

    def setPurgeTime(self, purgeTime):
        self.purgeTime = purgeTime

    def isUfn(self):
        return self.ufn

    def setUfn(self, ufn):
        self.ufn = ufn

    def getGeometry(self):
        return self.geometry

    def setGeometry(self, geometry):
        self.geometry = geometry

    def getForecaster(self):
        return self.forecaster

    def setForecaster(self, forecaster):
        self.forecaster = forecaster

    def getMotdir(self):
        return self.motdir

    def setMotdir(self, motdir):
        self.motdir = motdir

    def getMotspd(self):
        return self.motspd

    def setMotspd(self, motspd):
        self.motspd = motspd

    def getLoc(self):
        return self.loc

    def setLoc(self, loc):
        self.loc = loc

    def getRawmessage(self):
        return self.rawmessage

    def setRawmessage(self, rawmessage):
        self.rawmessage = rawmessage

    def getSeg(self):
        return self.seg

    def setSeg(self, seg):
        self.seg = seg

    def getPhensig(self):
        return self.phensig

    def setPhensig(self, phensig):
        self.phensig = phensig

    def getRegion(self):
        return self.region

    def setRegion(self, region):
        self.region = region

    def getOverviewText(self):
        return self.overviewText

    def setOverviewText(self, overviewText):
        self.overviewText = overviewText

    def getSegText(self):
        return self.segText

    def setSegText(self, segText):
        self.segText = segText

    def getLocationID(self):
        return self.locationID

    def setLocationID(self, locationID):
        self.locationID = locationID

    def getFloodSeverity(self):
        return self.floodSeverity

    def setFloodSeverity(self, floodSeverity):
        self.floodSeverity = floodSeverity

    def getImmediateCause(self):
        return self.immediateCause

    def setImmediateCause(self, immediateCause):
        self.immediateCause = immediateCause

    def getFloodRecordStatus(self):
        return self.floodRecordStatus

    def setFloodRecordStatus(self, floodRecordStatus):
        self.floodRecordStatus = floodRecordStatus

    def getFloodBegin(self):
        return self.floodBegin

    def setFloodBegin(self, floodBegin):
        self.floodBegin = floodBegin

    def getFloodCrest(self):
        return self.floodCrest

    def setFloodCrest(self, floodCrest):
        self.floodCrest = floodCrest

    def getFloodEnd(self):
        return self.floodEnd

    def setFloodEnd(self, floodEnd):
        self.floodEnd = floodEnd

    def getIdentifier(self):
        return self.identifier

    def setIdentifier(self, identifier):
        self.identifier = identifier

