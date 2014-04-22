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

