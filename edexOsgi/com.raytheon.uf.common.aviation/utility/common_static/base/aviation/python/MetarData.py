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

import Avn, MetarDecoder
import NoDataException
import HoursRefTimePointDataRetrieve


#
# Retrieves metar data through pointdata interfaces
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/26/09                      njensen       Initial Creation.
#    26APR2012       14688         rferrel       Use HoursRefTimePointDataRetrieve.
#    Jan 19, 2018    6957          tgurney       Return empty list on NoDataException
#    Mar 07, 2018    6933          njensen       Support requesting metars for multiple sites
#                                                 (specifically comma-delimited strings for
#                                                  the MetarViewer)
#
#
#

##
# This is a base file that is not intended to be overridden.
##

PARAMETERS = ['rawMETAR', 'timeObs']

def retrieve(siteID, size=1):
    if type(siteID) is str:
        siteID = [siteID]

    # siteID is a list of strings, there's a chance that an entry could be a
    # comma-delimited string of more than one site
    siteList = []
    for site in siteID:
        sites = site.split(',')
        for s in sites:
            siteList.append(s.strip())
    siteID = siteList

    metars = []
    for site in siteID:
        try:
            pdc = HoursRefTimePointDataRetrieve.retrieve('obs', site, PARAMETERS, keyId='timeObs', maxSize=size)
        except NoDataException.NoDataException:
            continue
        decoder = MetarDecoder.Decoder()
        for key in pdc.keys():
            pdv = pdc[key]
            rawMetar = pdv['rawMETAR'].split('\n')
            header, text = rawMetar[0], '\n'.join(rawMetar[1:])
            if not text.endswith('\n'):
                text += '\n'
            b = Avn.Bunch(header=header, text=text)
            b.dcd = decoder(b.text)
            metars.append(b)
    return metars
