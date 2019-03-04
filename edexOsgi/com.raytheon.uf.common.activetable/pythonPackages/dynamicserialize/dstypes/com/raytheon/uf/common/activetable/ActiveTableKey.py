##
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
