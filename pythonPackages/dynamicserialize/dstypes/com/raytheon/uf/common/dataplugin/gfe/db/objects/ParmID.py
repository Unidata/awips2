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
# Modified by njensen to add __repr__

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID

class ParmID(object):

    def __init__(self, parmIdentifier=None, dbId=None, level=None):
        self.parmName = None
        self.parmLevel = None
        self.dbId = None
        self.compositeName = None
        self.shortParmId = None
        self.parmId = None
        
        if (parmIdentifier is not None) and (dbId is not None):
            self.parmName = parmIdentifier
            
            if type(dbId) is DatabaseID:
                self.dbId = dbId
            elif type(dbId) is str:
                self.dbId = DatabaseID(dbId)
            else:
                raise TypeError("Invalid database ID specified.")
            
            if level is None:
                self.parmLevel = self.defaultLevel()
            else:
                self.parmLevel = level
            
            self.__encodeIdentifier()
        
        elif parmIdentifier is not None:
            self.__decodeIdentifier(parmIdentifier)
            self.__encodeIdentifier()

    def getParmName(self):
        return self.parmName

    def getParmLevel(self):
        return self.parmLevel

    def getDbId(self):
        return self.dbId

    def getCompositeName(self):
        return self.compositeName

    def getShortParmId(self):
        return self.shortParmId

    def getParmId(self):
        return self.parmId

    def __decodeIdentifier(self, parmIdentifier):
        parts = parmIdentifier.split(":")
        nameLevel = parts[0].split("_")
        self.dbId = DatabaseID(parts[1])
        if (len(nameLevel) == 2):
            self.parmName = nameLevel[0]
            self.parmLevel = nameLevel[1]
        else:
            self.parmName = nameLevel[0]
            self.parmLevel = self.defaultLevel()
    
    def __encodeIdentifier(self):
        self.compositeName = self.parmName + "_" + self.parmLevel
        self.shortParmId = self.compositeName + ":" + self.dbId.getShortModelId()
        self.parmId = self.compositeName + ":" + self.dbId.getModelId()
            
    def isValid(self):
        if len(self.parmName) is None or len(self.parmLevel) is None or self.dbId is None:
            return False
        if len(self.parmName) < 1 or len(self.parmLevel) < 1 or not self.dbId.isValid():
            return False
        
        if not self.parmName.isalnum():
            return False
        if not self.parmLevel.isalnum():
            return False
        
        return True  
    
    @staticmethod
    def defaultLevel():
        return "SFC"
    
    @staticmethod
    def parmNameAndLevel(composite):
        retValue = []
        
        pos = composite.find('_')
        if pos != -1:
            retValue.append(composite[:pos])
            retValue.append(composite[pos+1:])
        else:
            retValue.append(composite)
            retValue.append("SFC")
        return retValue
    
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return self.parmName + '_' + self.parmLevel + ":" + str(self.dbId)
    
    def __hash__(self):
        return hash(self.parmId)
    
    def __cmp__(self, other):
        if isinstance(other, ParmID):
            nameComp = cmp(self.parmName, other.parmName)
            if nameComp != 0:
                return nameComp
            
            levelComp = cmp(self.parmLevel, other.parmLevel)
            if levelComp != 0:
                return levelComp
            
            return cmp(self.dbId, other.dbId)
        else:
            return NotImplemented
        
    def __eq__(self, other):
        if not isinstance(other, ParmID):
            return False
        if self.dbId != other.dbId:
            return False
        if self.parmLevel != other.parmLevel:
            return False
        if self.parmName != other.parmName:
            return False
        return True
    
    def __ne__(self, other):
        return (not self.__eq__(other))
