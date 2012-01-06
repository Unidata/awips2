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

import warnings

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import GridLocation
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import ParmID
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.db.objects import TimeConstraints


class GridParmInfo(object):

    def __init__(self, id=None, gridLoc=None, gridType="NONE", unit=None,
                 descriptiveName="", minValue=0.0, maxValue=0.0, precision=0,
                 timeIndependentParm=False, timeConstraints=None, rateParm=False):
        self.parmID = id
        self.gridLoc = gridLoc
        self.gridType = gridType
        self.descriptiveName = descriptiveName
        self.unitString = unit
        self.minValue = float(minValue)
        self.maxValue = float(maxValue)
        self.precision = int(precision)
        self.rateParm = rateParm
        self.timeConstraints = timeConstraints
        self.timeIndependentParm = timeIndependentParm
        
#        (valid, errors) = self.__validCheck()
#        if not valid:
#            errorMessage = "GridParmInfo is invalid: " + str(errors)
#            warnings.warn(errorMessage)
#            self.__setDefaultValues()
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        out = ""
        if self.isValid():
            out = "ParmID: " + str(self.parmID) + \
                  " TimeConstraints: " + str(self.timeConstraints) + \
                  " GridLoc: " + str(self.gridLoc) + \
                  " Units: " + self.unitString + \
                  " Name: " + self.descriptiveName + \
                  " Min/Max AllowedValues: " + str(self.minValue) + "," + \
                  str(self.maxValue) + " Precision: " + str(self.precision) + \
                  " TimeIndependent: " + str(self.timeIndependentParm) + \
                  " RateParm: " + str(self.rateParm) + \
                  " GridType: " + self.gridType
        else:
            out = "<Invalid>"
        return out
    
    def __eq__(self, other):
        if not isinstance(other, GridParmInfo):
            return False
        if self.descriptiveName != other.descriptiveName:
            return False
        if self.gridLoc != other.gridLoc:
            return False
        if self.gridType != other.gridType:
            return False
        if self.minValue != other.minValue:
            return False
        if self.maxValue != other.maxValue:
            return False
        if self.parmID != other.parmID:
            return False
        if self.precision != other.precision:
            return False
        if self.rateParm != other.rateParm:
            return False
        if self.timeConstraints != other.timeConstraints:
            return False
        if self.timeIndependentParm != other.timeIndependentParm:
            return False
        if self.unitString != other.unitString:
            return False
        return True
    
    def __ne__(self, other):
        return (not self.__eq__(other))
    
    def __validCheck(self):
        status = []
        
        if not self.parmID.isValid():
            status.append("GridParmInfo.ParmID is not valid [" + str(self.parmID) + "]")
        if not self.timeConstraints.isValid():
            status.append("GridParmInfo.TimeConstraints are not valid [" +
                          str(self.timeConstraints) + "]")
        if not self.gridLoc.isValid():
            status.append("GridParmInfo.GridLocation is not valid")
        if self.timeIndependentParm and self.timeConstraints.anyConstraints():
            status.append("GridParmInfo is invalid. There are time constraints" +
                        " for a time independent parm. Constraints: " +
                        str(self.timeConstraints))
        if len(self.unitString) == 0:
            status.append("GridParmInfo.Units are not defined.")
        if self.precision < -2 or self.precision > 5:
            status.append("GridParmInfo is invalid. Precision out of limits." +
                          " Precision is: " + str(precision) + ". Must be between -2 and 5.")
        
        retVal = True
        if len(status) > 0:
            retVal = False
        return (retVal, status)
    
    def isValid(self):
        (valid, errors) = self.__validCheck()
        return valid
    
    def __setDefaultValues(self):
        self.parmID = ParmID()
        self.gridLoc = GridLocation()
        self.gridType = "NONE"
        self.descriptiveName = ""
        self.unitString = ""
        self.minValue = 0.0
        self.maxValue = 0.0
        self.precision = 0
        self.rateParm = False
        self.timeConstraints = TimeConstraints()
        self.timeIndependentParm = False

    def getParmID(self):
        return self.parmID

    def setParmID(self, parmID):
        self.parmID = parmID

    def getGridLoc(self):
        return self.gridLoc

    def setGridLoc(self, gridLoc):
        self.gridLoc = gridLoc

    def getGridType(self):
        return self.gridType

    def setGridType(self, gridType):
        self.gridType = gridType

    def getDescriptiveName(self):
        return self.descriptiveName

    def setDescriptiveName(self, descriptiveName):
        self.descriptiveName = descriptiveName

    def getUnitString(self):
        return self.unitString

    def setUnitString(self, unitString):
        self.unitString = unitString

    def getMinValue(self):
        return self.minValue

    def setMinValue(self, minValue):
        self.minValue = minValue

    def getMaxValue(self):
        return self.maxValue

    def setMaxValue(self, maxValue):
        self.maxValue = maxValue

    def getPrecision(self):
        return self.precision

    def setPrecision(self, precision):
        self.precision = precision

    def getRateParm(self):
        return self.rateParm

    def setRateParm(self, rateParm):
        self.rateParm = rateParm

    def getTimeConstraints(self):
        return self.timeConstraints

    def setTimeConstraints(self, timeConstraints):
        self.timeConstraints = timeConstraints

    def getTimeIndependentParm(self):
        return self.timeIndependentParm

    def setTimeIndependentParm(self, timeIndependentParm):
        self.timeIndependentParm = timeIndependentParm

