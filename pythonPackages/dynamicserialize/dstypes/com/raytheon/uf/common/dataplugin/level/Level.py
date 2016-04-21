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
# and then modified post-generation to add additional features to better
# match Java implementation.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/29/13         2023         dgilling       Initial Creation.
#    02/12/14         2672         bsteffen       Allow String constructor to parse floats.
#    06/29/15         4480         dgilling       Implement __hash__, __eq__,
#                                                 __str__ and rich comparison operators.
#


import numpy
import re

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.level import MasterLevel


LEVEL_NAMING_REGEX = re.compile("^(\d*(?:\.\d*)?)(?:_(\d*(?:\.\d*)?))?([a-zA-Z]+)$")
INVALID_VALUE = numpy.float64(-999999)

class Level(object):

    def __init__(self, levelString=None):
        self.id = 0L
        self.identifier = None
        self.masterLevel = None
        self.levelonevalue = INVALID_VALUE
        self.leveltwovalue = INVALID_VALUE
        
        if levelString is not None:
            matcher = LEVEL_NAMING_REGEX.match(str(levelString))
            if matcher is not None:
               self.levelonevalue = numpy.float64(matcher.group(1))
               self.masterLevel = MasterLevel.MasterLevel(matcher.group(3))
               levelTwo = matcher.group(2)
               if levelTwo:
                   self.leveltwovalue = numpy.float64(levelTwo)

    def __hash__(self):
        # XOR-ing the 3 items in a tuple ensures that order of the 
        # values matters
        hashCode = hash(self.masterLevel) ^ hash(self.levelonevalue) ^ hash(self.leveltwovalue)
        hashCode ^= hash((self.masterLevel, self.levelonevalue, self.leveltwovalue))
        return hashCode
    
    def __eq__(self, other):
        if type(self) != type(other):
            return False
        else:
            return (self.masterLevel, self.levelonevalue, self.leveltwovalue) == \
                (other.masterLevel, other.levelonevalue, other.leveltwovalue)
                
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def __lt__(self, other):
        if type(self) != type(other):
            return NotImplemented
        elif self.masterLevel.getName() != other.masterLevel.getName():
            return NotImplemented
        
        myLevel1 = self.levelonevalue
        myLevel2 = self.leveltwovalue
        otherLevel1 = other.levelonevalue
        otherLevel2 = other.leveltwovalue
        if myLevel1 == INVALID_VALUE and myLevel2 != INVALID_VALUE:
            myLevel1 = myLevel2
            myLevel2 = INVALID_VALUE
        if otherLevel1 == INVALID_VALUE and otherLevel2 != INVALID_VALUE:
            otherLevel1 = otherLevel2
            otherLevel2 = INVALID_VALUE
            
        # We default to descending order to make sorting levels from the DAF easier
        compareType = self.masterLevel.getType() if self.masterLevel.getType() else "DEC"
        if myLevel1 != INVALID_VALUE and otherLevel1 != INVALID_VALUE:
            level1Cmp = self.__compareLevelValues(compareType, myLevel1, otherLevel1)
            if level1Cmp == -1:
                if myLevel2 != INVALID_VALUE and otherLevel2 != INVALID_VALUE:
                    level2Cmp = self.__compareLevelValues(compareType, myLevel2, otherLevel2)
                    return level2Cmp == -1
                elif myLevel2 != INVALID_VALUE:
                    level2Cmp = self.__compareLevelValues(compareType, myLevel2, otherLevel1)
                    return level2Cmp == -1
                else:
                    return True
        return False
    
    def __le__(self, other):
        if type(self) != type(other):
            return NotImplemented 
        elif self.masterLevel.getName() != other.masterLevel.getName():
            return NotImplemented
        
        return self.__lt__(other) or self.__eq__(other)
    
    def __gt__(self, other):
        if type(self) != type(other):
            return NotImplemented
        elif self.masterLevel.getName() != other.masterLevel.getName():
            return NotImplemented
        
        myLevel1 = self.levelonevalue
        myLevel2 = self.leveltwovalue
        otherLevel1 = other.levelonevalue
        otherLevel2 = other.leveltwovalue
        if myLevel1 == INVALID_VALUE and myLevel2 != INVALID_VALUE:
            myLevel1 = myLevel2
            myLevel2 = INVALID_VALUE
        if otherLevel1 == INVALID_VALUE and otherLevel2 != INVALID_VALUE:
            otherLevel1 = otherLevel2
            otherLevel2 = INVALID_VALUE

        # We default to descending order to make sorting levels from the DAF easier
        compareType = self.masterLevel.getType() if self.masterLevel.getType() else "DEC"
        if myLevel1 != INVALID_VALUE and otherLevel1 != INVALID_VALUE:
            level1Cmp = self.__compareLevelValues(compareType, myLevel1, otherLevel1)
            if level1Cmp == 1:
                if myLevel2 != INVALID_VALUE and otherLevel2 != INVALID_VALUE:
                    level2Cmp = self.__compareLevelValues(compareType, myLevel2, otherLevel2)
                    return level2Cmp == 1
                elif otherLevel2 != INVALID_VALUE:
                    level2Cmp = self.__compareLevelValues(compareType, myLevel1, otherLevel2)
                    return level2Cmp == 1
                else:
                    return True
        return False
    
    def __ge__(self, other):
        if type(self) != type(other):
            return NotImplemented
        elif self.masterLevel.getName() != other.masterLevel.getName():
            return NotImplemented
        
        return self.__gt__(other) or self.__eq__(other)

    def __compareLevelValues(self, compareType, val1, val2):
        returnVal = 0
        if val1 < val2:
            returnVal = -1 if compareType == 'INC' else 1
        elif val2 < val1:
            returnVal = 1 if compareType == 'INC' else -1
        return returnVal

    def __str__(self):
        retVal = ""
        if INVALID_VALUE != self.levelonevalue:
            retVal += str(self.levelonevalue)
        if INVALID_VALUE != self.leveltwovalue:
            retVal += "_" + str(self.leveltwovalue)
        retVal += str(self.masterLevel.getName())
        return retVal

    def getId(self):
        return self.id

    def setId(self, id):
        self.id = id

    def getMasterLevel(self):
        return self.masterLevel

    def setMasterLevel(self, masterLevel):
        self.masterLevel = masterLevel

    def getLevelonevalue(self):
        return self.levelonevalue

    def setLevelonevalue(self, levelonevalue):
        self.levelonevalue = numpy.float64(levelonevalue)

    def getLeveltwovalue(self):
        return self.leveltwovalue

    def setLeveltwovalue(self, leveltwovalue):
        self.leveltwovalue = numpy.float64(leveltwovalue)

    def getIdentifier(self):
        return self.identifier

    def setIdentifier(self, identifier):
        self.identifier = identifier
