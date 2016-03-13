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
# Implements IGridData for use by native Python clients to the Data Access
# Framework.
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/03/13         #2023        dgilling      Initial Creation.
#    
#


import numpy
import warnings

from awips.dataaccess import IGridData
from awips.dataaccess import PyData

NO_UNIT_CONVERT_WARNING = """
The ability to unit convert grid data is not currently available in this version of the Data Access Framework.
"""


class PyGridData(IGridData, PyData.PyData):
    
    def __init__(self, gridDataRecord, nx, ny, latLonGrid):
        PyData.PyData.__init__(self, gridDataRecord)
        nx = nx
        ny = ny
        self.__parameter = gridDataRecord.getParameter()
        self.__unit = gridDataRecord.getUnit()
        self.__gridData = numpy.reshape(numpy.array(gridDataRecord.getGridData()), (nx, ny))
        self.__latLonGrid = latLonGrid
    
    def getParameter(self):
        return self.__parameter
    
    def getUnit(self):
        return self.__unit
    
    def getRawData(self, unit=None):
        # TODO: Find a proper python library that deals will with numpy and
        # javax.measure style unit strings and hook it in to this method to
        # allow end-users to perform unit conversion for grid data.
        if unit is not None:
            warnings.warn(NO_UNIT_CONVERT_WARNING, stacklevel=2)
        return self.__gridData
    
    def getLatLonCoords(self):
        return self.__latLonGrid
