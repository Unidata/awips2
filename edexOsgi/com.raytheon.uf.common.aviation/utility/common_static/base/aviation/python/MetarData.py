##
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

##
# This is a base file that is not intended to be overridden.
##

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
        
        
    
