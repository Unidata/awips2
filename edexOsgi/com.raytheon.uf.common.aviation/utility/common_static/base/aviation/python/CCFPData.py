##
##

import Avn
from com.raytheon.viz.aviation.monitor import CcfpData

#
# Retrieves cached ccfp data
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/09                      njensen       Initial Creation.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##


def retrieve(siteID):
    vals = CcfpData.getReports(siteID)
    ret = []
    sz = vals.size()
    for i in range(sz):
        text = str(vals.get(i))
        ret.append(Avn.Bunch(text=text))
    if len(ret) < 1:
        ret.append(Avn.Bunch(text=''))
    return ret
        
    
