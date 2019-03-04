##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# VTECMessageType.py
#
# This module stores the VTEC message type for each product category
#
# Author: lefebvre
# ----------------------------------------------------------------------------
#Products not listed will be considered VTEC disabled.

##
# This is a base file that is not intended to be overridden.
##

VTECMessageTypeDict = {
    'WSW' : 'O',
    'WCN' : 'O',
    'NPW' : 'O',
    'FFA' : 'O',
    'RFW' : 'O',
    'CFW' : 'O',
    'HLS' : 'O',
    'MWW' : 'O',
    'TCV' : 'O',
    }



# This method fetches the message type for the specified product Category
# If not found, return None.
def getVTECMessageType(productCategory):
    if VTECMessageTypeDict.has_key(productCategory):
        return VTECMessageTypeDict[productCategory]
    else:
        return None
