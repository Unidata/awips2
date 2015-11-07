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
#    Utilities for ensuring a numpy ndarray, list of ndarrays, or tuple of ndarrays
#    match a specific numpy dtype that is compatible with Java.  In theory
#    this should be limited to int8, int16, int32, int64, float32, and float64 based
#    on the bit sizes of Java primitives. 
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Feb 05, 2015    4089          njensen        Initial Creation.
#
#

import numpy

def forceJavaCompatible(grid, correctType):
    """
    Forces a numpy ndarray to a specific dtype
    
    Args: 
            grid: the numpy ndarray
            correctType: a string of the correct dtype, e.g. 'int8'
                          
    Returns:
            a copy of the array as the specified dtype, or the original object
            if the object is not a numpy ndarray
    """
    gridType = type(grid)
    if gridType == numpy.ndarray:
        if grid.dtype.name != correctType:
            return numpy.ascontiguousarray(grid, correctType)
    return grid

def checkdTypes(item, correctType):
    """
    Checks the numpy dtype of the item to ensure it matches the correct dtype.  If
    it does not match, it copies and converts the ndarray.  Also supports enforcing
    a dtype on a list or tuple of ndarrays.    
    
    Args: 
            item: a numpy ndarray or a list or tuple of ndarrays
            correctType: a string of the correct dtype, e.g. 'int8'
                          
    Returns:
            an object similar to item with any ndarrays converted to the correct dtype
    """
    t = type(item)
    if t == numpy.ndarray:
        return forceJavaCompatible(item, correctType)
    elif hasattr(item, '__iter__'):
        return t([forceJavaCompatible(i, correctType) for i in item])                                    
    return item
    