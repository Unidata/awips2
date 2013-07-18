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
# Provides Java implementations of common smart utility functions
# to boost performance.
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/14/13                      njensen       Initial Creation.
#    
# 
#

import jep
from com.raytheon.uf.common.dataplugin.gfe.util import SmartUtils as JavaSmartUtils
import numpy


def __getMaskIndiciesForJava(mask):
    flatMask = mask.flat              #flatten the array
    flatIndicies = numpy.nonzero(flatMask)  # get the indicies of the set cells
    ysize = mask.shape[1]
    indexes = []
    # convert the flat incicies to the x, y indicies
    for i in flatIndicies:
        indexes.append((i / ysize, i % ysize))

    #  Make two new jarrays to hold the final coordinate tuples
    size = len(indexes[0][0])
    xcoords = jep.jarray(size, jep.JINT_ID)
    ycoords = jep.jarray(size, jep.JINT_ID)    

    #===================================================================
    #  Convert the coordinates from a tuple of numpy arrays to a list of
    #  coordinate tuples

    for index in xrange(size):
        try:
            x = indexes[0][0][index]
            y = indexes[0][1][index]
            xcoords[index] = int(x)
            ycoords[index] = int(y)
        except Exception, e:
            print e                

    return xcoords, ycoords
    
    
def fillEditArea(grid, fillMask, borderMask):    
    editPointsX, editPointsY  = __getMaskIndiciesForJava(fillMask)
    borderPointsX, borderPointsY = __getMaskIndiciesForJava(borderMask)
        
    gridObj = JavaSmartUtils.fillEditArea(grid, grid.shape[1], grid.shape[0], \
                                              editPointsY, editPointsX, borderPointsY, borderPointsX)  
                          
    retObj = gridObj.__numpy__[0]
    return retObj


