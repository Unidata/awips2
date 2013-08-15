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
#    
# 
#

PARAMETERS = ['rawMETAR', 'timeObs']

def retrieve(siteID, size=1):        
    if type(siteID) is str:
        siteID = [siteID]
    try :
        pdc = HoursRefTimePointDataRetrieve.retrieve('obs', siteID[0], PARAMETERS, keyId='timeObs', maxSize=size)
    except NoDataException.NoDataException:
        raise NoDataException.NoDataException('No METAR data available for site %s' % siteID[0])
    decoder = MetarDecoder.Decoder()
    metars = []    
    for key in pdc.keys():
        pdv = pdc[key]        
        split = pdv['rawMETAR'].split('\n')
        text = ''
        for n in range(1, len(split)):
            text += split[n] +'\n'
        b = Avn.Bunch(header=split[0], text=text)                    
        b.dcd = decoder(b.text)
        metars.append(b)
    return metars
        
        
    
