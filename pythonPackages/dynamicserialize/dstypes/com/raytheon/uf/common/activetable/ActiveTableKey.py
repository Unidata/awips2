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
#    05/22/2015       4522         randerso       Initial creation
#    03/17/2016       5426         randerso       Add issueYear to primary key
#    08/03/2016       19213        ryu            Add pil to primary key
#
##    
class ActiveTableKey(object):

    def __init__(self):
        self.officeid = None
        self.phen = None
        self.sig = None
        self.etn = None
        self.ugcZone = None
        self.issueYear = None
        self.pil = None

    def getOfficeid(self):
        return self.officeid

    def setOfficeid(self, officeid):
        self.officeid = officeid

    def getPhen(self):
        return self.phen

    def setPhen(self, phen):
        self.phen = phen

    def getSig(self):
        return self.sig

    def setSig(self, sig):
        self.sig = sig

    def getEtn(self):
        return self.etn

    def setEtn(self, etn):
        self.etn = etn
        
    def getUgcZone(self):
        return self.ugcZone
    
    def setUgcZone(self, ugcZone):
        self.ugcZone = ugcZone

    def getIssueYear(self):
        return self.issueYear

    def setIssueYear(self, issueYear):
        self.issueYear = issueYear
        
    def getPil(self):
        return self.pil
    
    def setPil(self, pil):
        self.pil = pil
