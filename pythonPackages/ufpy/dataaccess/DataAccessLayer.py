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
# Published interface for ufpy.dataaccess package
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/10/12                      njensen       Initial Creation.
#    Feb 14, 2013    1614          bsteffen       refactor data access framework
#                                                 to use single request.
#    
# 
#


import sys

if sys.modules.has_key('jep'):
    import JepRouter
    router = JepRouter    
else:
    # router = ThriftClientRouter()
    import exceptions
    raise exceptions.NotImplementedError("Must use inside a JVM until ThriftClient support is added")
    

def getAvailableTimes(request):
    return router.getAvailableTimes(request)

def getGridData(request, times):
    return router.getGridData(request, times)

def getGeometryData(request, times):
    return router.getGeometryData(request, times)

def getLatLonCoords(gridData):
    return router.getLatLonCoords(gridData)

def getAvailableLocationNames(request):
    return router.getAvailableLocationNames(request)

def newDataRequest():
    return router.newDataRequest()



