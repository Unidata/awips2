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
# Published interface for ufpy.dataaccess package
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


import sys

if sys.modules.has_key('jep'):
    import JepRouter
    router = JepRouter    
else:
    #router = ThriftClientRouter()
    import exceptions
    raise exceptions.NotImplementedError("Must use inside a JVM until ThriftClient support is added")
    

def getAvailableTimes(request):
    return router.getAvailableTimes(request)

def getData(request, times):
    return router.getData(request, times)

def getLatCoords(gridRequest):
    return router.getLatCoords(gridRequest)

def getLonCoords(gridRequest):
    return router.getLonCoords(gridRequest)

def getAvailableLocationNames(geometryRequest):
    return router.getAvailableLocaitonNames(geometryRequest)

def newGeometryRequest():
    return router.newGeometryRequest()
     
def newGridRequest():
    return router.newGridRequest()



