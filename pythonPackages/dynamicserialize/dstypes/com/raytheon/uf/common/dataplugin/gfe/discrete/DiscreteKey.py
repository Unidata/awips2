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

## NOTE: Because the pure python dynamicserialize code does not
#  have a means of accessing the DiscreteDefinition, this class
#  is only really useful as a container for deserialized data
#  from EDEX. I would not recommend trying to use it for anything
#  else.


SUBKEY_SEPARATOR = '^'
AUXDATA_SEPARATOR = ':'

class DiscreteKey(object):

    def __init__(self):
        self.siteId = None
        self.subKeys = None
        self.parmID = None
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return SUBKEY_SEPARATOR.join(self.subKeys)
    
    def __getitem__(self, key):
        try:
            index = int(key)
        except:
            raise TypeError("list indices must be integers, not " + str(type(key)))
        if index < 0 or index > len(self.subKeys):
            raise IndexError("index out of range")
        return self.subKeys[index]
    
    def __hash__(self):
        prime = 31
        result = 1
        result = prime * result + (0 if self.parmID is None else hash(self.parmID))
        result = prime * result + (0 if self.siteId is None else hash(self.siteId))
        result = prime * result + (0 if self.subKeys is None else hash(self.subKeys))
        return result
    
    def __eq__(self, other):
        if not isinstance(other, DiscreteKey):
            return False
        if self.parmID != other.parmID:
            return False
        if self.siteId != other.siteId:
            return False
        return self.subKeys == other.subKeys
    
    def __ne__(self, other):
        return (not self.__eq__(other))
        
    @staticmethod
    def auxData(subkey):
        pos = subkey.find(AUXDATA_SEPARATOR)
        if pos != -1:
            return subkey[pos + 1:]
        else:
            return ""
        
    @staticmethod
    def baseData(subkey):
        pos = subkey.find(AUXDATA_SEPARATOR)
        if pos != -1:
            return subkey[:pos]
        else:
            return subkey

    def getSiteId(self):
        return self.siteId

    def setSiteId(self, siteId):
        self.siteId = siteId

    def getSubKeys(self):
        return self.subKeys

    def setSubKeys(self, subKeys):
        self.subKeys = subKeys

    def getParmID(self):
        return self.parmID

    def setParmID(self, parmID):
        self.parmID = parmID

