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
# Implements IGridData and wraps around a Java IGridData.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    
# 
#

from ufpy.dataaccess import IGridData
import JData

from com.raytheon.uf.common.geospatial.interpolation.data import FloatArrayWrapper, UnitConvertingDataDestination
from com.raytheon.uf.common.python import PythonNumpyFloatArray
from javax.measure.unit import UnitFormat

class JGridData(IGridData, JData.JData):
    
    def __init__(self, wrappedObject):
        JData.JData.__init__(self, wrappedObject)
        
    def __getitem__(self, key):
        if key == 'parameter':
            return self.getParameter()
        elif key == 'unit':
            return self.getUnit()
        elif key == 'rawData':
            return self.getRawData()
        elif key == 'time':
            return self.getDataTime()
        elif key == 'level':
            return self.getLevel()
    
    def getParameter(self):
        return self.jobj.getParameter()
    
    def getUnit(self):
        return str(self.jobj.getUnit())
    
    def getRawData(self, unit=None):
        dest = FloatArrayWrapper(self.jobj.getGridGeometry())
        pnfa = None
        if unit:
            unitObj = UnitFormat.getUCUMInstance().parseObject(unit)
            converter = self.jobj.getUnit().getConverterTo(unitObj)
            unitDest = UnitConvertingDataDestination(converter, dest)
            filledDest = self.jobj.populateDataDestination(unitDest)
            pnfa = PythonNumpyFloatArray(filledDest.getWrappedDestination().getFloatArray())
        else:
            filledDest = self.jobj.populateDataDestination(dest)
            pnfa = PythonNumpyFloatArray(dest.getFloatArray())
        return pnfa.__numpy__[0]
        
