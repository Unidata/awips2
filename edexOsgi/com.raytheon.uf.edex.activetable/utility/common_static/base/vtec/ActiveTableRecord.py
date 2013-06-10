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
# Provides a wrapper to the Java ActiveTableRecord
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/20/09                      njensen        Initial Creation.
#    02/26/13         1447         dgilling       Implement __eq__() and 
#                                                 __ne__().
#    
# 
#


import ActiveTableVtec
from java.util import Calendar

class ActiveTableRecord(object):
    
    def __init__(self, javaRecord, state="Decoded"):
        self.atr = javaRecord
        self.state = state
        self.pil = ActiveTableVtec.remapPil(self.atr.getPhen(), self.atr.getSig(), self.atr.getPil())
        self.id = self.atr.getUgcZone()
    
    def __getitem__(self, key):        
        if key == 'vtecstr':
            return self.atr.getVtecstr()
        elif key == 'etn':
            return int(self.atr.getEtn())
        elif key == 'sig':
            return self.atr.getSig()
        elif key == 'phen':
            return self.atr.getPhen()
        elif key == 'segText':        
            return self.atr.getSegText()
        elif key == 'overviewText':        
            return self.atr.getOverviewText()
        elif key == 'phensig':
            return self.atr.getPhensig()
        elif key == 'act':
            return self.atr.getAct()
        elif key == 'seg':
            return self.atr.getSeg()
        elif key == 'startTime':                
            return self.atr.getStartTime().getTimeInMillis() / 1000
        elif key == 'endTime':
            return self.atr.getEndTime().getTimeInMillis() / 1000
        elif key == 'ufn':
            return self.atr.isUfn()
        elif key == 'officeid':
            return self.atr.getOfficeid()
        elif key == 'purgeTime':
            return self.atr.getPurgeTime().getTimeInMillis() / 1000
        elif key == 'issueTime':
            return self.atr.getIssueTime().getTimeInMillis() / 1000
        elif key == 'state':
            return self.state
        elif key == 'xxxid':
            return self.atr.getXxxid()
        elif key == 'pil':
            return self.pil
        elif key == 'productClass':
            return self.atr.getProductClass()
        elif key == 'id':
            return self.id
        elif key == 'rawMessage':
            return self.atr.getRawmessage()
        else:
            raise KeyError
    
    
    def __setitem__(self, key, value):
        if key == 'vtecstr':
            self.atr.setVtecstr(value)
        elif key == 'etn':
            self.atr.setEtn(str(value))
        elif key == 'sig':
            self.atr.setSig(value)
        elif key == 'phen':
            self.atr.setPhen(value)
        elif key == 'segText':
            self.atr.setSegText(value)
        elif key == 'overviewText':
            self.atr.setOverviewText(value)
        elif key == 'phensig':
            self.atr.setPhensig(value)
        elif key == 'act':
            self.atr.setAct(value)
        elif key =='seg':
            self.atr.setSeg(value)
        elif key == 'startTime':
            start = Calendar.getInstance()
            start.setTimeInMillis(long(value * 1000))        
            self.atr.setStartTime(start)
        elif key == 'endTime':
            end = Calendar.getInstance()
            end.setTimeInMillis(long(value * 1000))        
            self.atr.setEndTime(end)
        elif key == 'purgeTime':
            purge = Calendar.getInstance()
            purge.setTimeInMillis(long(value * 1000))        
            self.atr.setPurgeTime(purge)
        elif key == 'issueTime':
            issue = Calendar.getInstance()
            issue.setTimeInMillis(long(value * 1000))        
            self.atr.setIssueTime(issue)        
        elif key == 'ufn':
            self.atr.setUfn(value)
        elif key == 'officeid':                
            self.atr.setOfficeid(value)
        elif key == 'state':        
            self.state = value
        elif key == 'xxxid':
            self.atr.setXxxid(value)
        elif key == 'pil':
            self.atr.setPil(value)
            self.pil = value
        elif key == 'productClass':        
            self.atr.setProductClass(value)
        elif key == 'id':                                
            self.id = value
        elif key == 'rawMessage':
            self.atr.setRawmessage(value)
        else:
            raise KeyError   
    
    def __delitem__(self, key):
        pass
    
    def __deepcopy__(self, memo):
        return ActiveTableRecord(self.atr.clone(), self.state)
    
    def __eq__(self, other):
        return self.javaRecord().equals(other.javeRecord())
    
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def javaRecord(self):
        return self.atr
    
    def get(self, key, default):
        val = default
        try:
            val = self[key]
        except KeyError:
            val = default
        return val
    
    def has_key(self, key):
        try:
            self.__getitem__(key)
            return True
        except KeyError:
            return False
