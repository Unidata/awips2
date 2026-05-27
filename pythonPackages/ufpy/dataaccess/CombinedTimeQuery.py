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
# Method for performing a DAF time query where all parameter/level/location
# combinations must be available at the same time.
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/22/16         #5591        bsteffen       Initial Creation.
#

from ufpy.dataaccess import DataAccessLayer

def getAvailableTimes(request, refTimeOnly=False):
    return __getAvailableTimesForEachParameter(request, refTimeOnly)

def __getAvailableTimesForEachParameter(request, refTimeOnly=False):
    parameters = request.getParameters()
    if parameters:
        times = None
        for parameter in parameters:
            specificRequest = __cloneRequest(request)
            specificRequest.setParameters(parameter)
            specificTimes = __getAvailableTimesForEachLevel(specificRequest, refTimeOnly)
            if times is None:
                times = specificTimes
            else:
                times.intersection_update(specificTimes)
            if not times:
                break
        return times
    else:
        return __getAvailableTimesForEachLevel(request, refTimeOnly)

def __getAvailableTimesForEachLevel(request, refTimeOnly=False):
    levels = request.getLevels()
    if levels:
        times = None
        for level in levels:
            specificRequest = __cloneRequest(request)
            specificRequest.setLevels(level)
            specificTimes = __getAvailableTimesForEachLocation(specificRequest, refTimeOnly)
            if times is None:
                times = specificTimes
            else:
                times.intersection_update(specificTimes)
            if not times:
                break
        return times
    else:
        return __getAvailableTimesForEachLocation(request, refTimeOnly)

def __getAvailableTimesForEachLocation(request, refTimeOnly=False):
    locations = request.getLocationNames()
    if locations:
        times = None
        for location in locations:
            specificRequest = __cloneRequest(request)
            specificRequest.setLocationNames(location)
            specificTimes = DataAccessLayer.getAvailableTimes(specificRequest, refTimeOnly)
            if times is None:
                times = set(specificTimes)
            else:
                times.intersection_update(specificTimes)
            if not times:
                break
        return times
    else:
        return DataAccessLayer.getAvailableTimes(request, refTimeOnly)
        
        
def __cloneRequest(request):
    return DataAccessLayer.newDataRequest(datatype      = request.getDatatype(),
                                          parameters    = request.getParameters(),
                                          levels        = request.getLevels(),
                                          locationNames = request.getLocationNames(),
                                          envelope      = request.getEnvelope(),
                                          **request.getIdentifiers())