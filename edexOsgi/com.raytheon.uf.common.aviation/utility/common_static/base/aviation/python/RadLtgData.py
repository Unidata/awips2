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
#    13JUL2015       4544          dgilling      Use new lightning probability parameter.
#    06MAR2020       DCS21314      mporricelli   Use 1hr lightning probability once available
#
#
#

##
# This is a base file that is not intended to be overridden.
##

PARAMETERS = ['ltg1hr', 'ltg2hr', 'stationId', 'refTime', 'fcstHr']

LOOK_AHEAD = 3 # hours
SEC_PER_HOUR = 60*60

_Logger = logging.getLogger(Avn.CATEGORY)

def retrieve(siteID):
    try:
        pdc = ForecastPointDataRetrieve.retrieve('bufrmosLAMP', siteID, PARAMETERS, forecastTimesLimit=3)
    except NoDataException.NoDataException:
        return None
    pots2hr = []
    pots1hr = []
    try:
        # assume pdc[0] is 0 hour and not used in the forecast.
        vtime = pdc[1]['refTime'] / 1000.0 + LOOK_AHEAD*SEC_PER_HOUR
        for n in range(1, LOOK_AHEAD):
            pdv = pdc[n]
            # both ltg2hr and ltg1hr are stored in the bufrmosLAMP data, but when data
            # is not flowing for one of them, the data for that one is all -9999
            pots2hr.append(pdv['ltg2hr'])
            pots1hr.append(pdv['ltg1hr'])
        data = max([x for x in pots2hr if 0 <= x <= 100] + [y for y in pots1hr if 0 <= y <= 100] + [0])
        return {'from': vtime-LOOK_AHEAD*SEC_PER_HOUR, 'to': vtime, 'prob': min(data, 100)}
    except KeyError:
        return None
