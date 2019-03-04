##
##

import logging
import Avn
import ForecastPointDataRetrieve, NoDataException

#
# Retrieves mos lightning data through pointdata interfaces
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/15/09                      njensen       Initial Creation.
#    26APR2012       14688         rferrel       Use ForecastPointDataRetrieve.
#    03APR2013       1735          rferrel       Limit retrieval for forecast times to the number used.
#    13JUL2015       4544          dgilling      Use new lightning probability parameter.
#    
# 
#

##
# This is a base file that is not intended to be overridden.
##

PARAMETERS = ['ltg2hr', 'stationId', 'refTime', 'fcstHr']

LOOK_AHEAD = 3 # hours
SEC_PER_HOUR = 60*60

_Logger = logging.getLogger(Avn.CATEGORY)

def retrieve(siteID):
    try:
        pdc = ForecastPointDataRetrieve.retrieve('bufrmosLAMP', siteID, PARAMETERS, forecastTimesLimit=3)
    except NoDataException.NoDataException: 
        return None
    pots = []
    try:
        # assume pdc[0] is 0 hour and not used in the forecast.
        vtime = pdc[1]['refTime'] / 1000.0 + LOOK_AHEAD*SEC_PER_HOUR
        for n in range(1, LOOK_AHEAD):
            pdv = pdc[n]
            pots.append(pdv['ltg2hr'])
        data = max([x for x in pots if 0 <= x <= 100] + [0])
        return {'from': vtime-LOOK_AHEAD*SEC_PER_HOUR, 'to': vtime, 'prob': min(data, 100)}
    except KeyError:
        return None
    
