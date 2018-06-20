#
# Purpose: Create a class to invoke the TAF Decoder/XMLEncoder for TAC->XML translation
#
# Author: Mark Oberfield MDL/OSTI/NWS/NOAA
#
# Date: 13 October 2016
#

#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/13/2017      6550          tgurney        Add last TAF valid period
#


import TAFXMLEncoder as TXE
import TAFDecoder as TD

class TACtoXML():

    def __init__(self, wwCodesFile, tafStationInfoFile):
        self.decoder = TD.Decoder()
        self.encoder = TXE.XMLEncoder(
            wwCodesFile=wwCodesFile,
            tafStationInfoFile=tafStationInfoFile,
            )

    def __call__(self, tac, lastTafRangeStart=None, lastTafRangeEnd=None):
        return self.encoder(self.decoder(tac), lastTafRangeStart, lastTafRangeEnd)
