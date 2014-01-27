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
#    
# 
#

PARAMETERS = ['tstorm2hr', 'stationId', 'refTime', 'fcstHr']

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
            pots.append(pdv['tstorm2hr'])
        data = max([x for x in pots if 0 <= x <= 100] + [0])
        return {'from': vtime-LOOK_AHEAD*SEC_PER_HOUR, 'to': vtime, 'prob': min(data, 100)}
    except KeyError:
        return None
    
