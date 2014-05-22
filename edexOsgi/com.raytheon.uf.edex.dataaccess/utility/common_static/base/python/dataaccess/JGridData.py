# #
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
# #


#
# Implements IGridData and wraps around a Java IGridData.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    05/01/14        3095          bsteffen      Move numeric data access to new plugin.

#    
# 
#

from ufpy.dataaccess import IGridData
import JData
from jep import jarray

from com.raytheon.uf.common.numeric.buffer import FloatBufferWrapper
from com.raytheon.uf.common.geospatial.data import UnitConvertingDataFilter
from com.raytheon.uf.common.numeric.dest import FilteredDataDestination 
from com.raytheon.uf.common.python import PythonNumpyFloatArray
from com.raytheon.uf.common.geospatial import LatLonReprojection
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
        nx = self.jobj.getGridGeometry().getGridRange().getSpan(0)
        ny = self.jobj.getGridGeometry().getGridRange().getSpan(1)
        dest = FloatBufferWrapper(nx, ny)
        pnfa = None
        if unit:
            unitObj = UnitFormat.getUCUMInstance().parseObject(unit)
            converter = self.jobj.getUnit().getConverterTo(unitObj)
            filter = UnitConvertingDataFilter(converter)
            filter = UnitConvertingDataFilter(converter)
            filters = jarray(1, UnitConvertingDataFilter)
            filters[0] = filter
            unitDest = FilteredDataDestination.addFilters(dest, filters)
            self.jobj.populateData(unitDest)
        else:
            self.jobj.populateData(dest)
        pnfa = PythonNumpyFloatArray(dest.getBuffer().array(), nx, ny)
        return pnfa.__numpy__[0]
    
    def getLatLonCoords(self):
        """
        Gets the lat/lon coordinates of the grid data.
        
        Returns:
            a tuple where the first element is a numpy array of lons, and the 
            second element is a numpy array of lats
        """
        gridGeometry = self.jobj.getGridGeometry()
        if gridGeometry is None :
            return None
        latlons = LatLonReprojection.getLatLons(gridGeometry)
        nx = gridGeometry.getGridRange().getSpan(0)
        ny = gridGeometry.getGridRange().getSpan(1)
        latndarray = PythonNumpyFloatArray(latlons.getLats(), nx, ny).__numpy__[0]
        lonndarray = PythonNumpyFloatArray(latlons.getLons(), nx, ny).__numpy__[0]
        return (lonndarray, latndarray)
        
