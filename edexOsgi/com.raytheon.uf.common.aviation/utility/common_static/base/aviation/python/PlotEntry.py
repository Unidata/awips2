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

##
# This is a base file that is not intended to be overridden.
##

import logging
import Avn
import TafDecoder
import JUtil

_Logger = logging.getLogger(Avn.CATEGORY)

#
# Entry point for Weather Plot data retrieval
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/24/09                      avarani        Initial Creation.
#    04/28/11        8065          rferrel        Use cached site objects
#
#

def getNam(siteObj):
    o = JUtil.javaUnpickle(siteObj)
    etaData = o['data']
    if etaData is not None:
        data = [{'data': eta.data} for eta in etaData]
    else:
        data = None
    return JUtil.pyValToJavaObj(data)


def getMos(siteObj, model):
    o = JUtil.javaUnpickle(siteObj)
    mosData = o['data']
    if mosData is not None:
        data = [{'data': mos.data} for mos in mosData]
    else:
        data = None
    return JUtil.pyValToJavaObj(data)


def getMetars(siteObj, size=99):
    o = JUtil.javaUnpickle(siteObj)
    data = o['data']
    if data is not None:
        data = [{'header': d.header, 'text': d.text, 'dcd': d.dcd}
                for d in data
                ]
        data.sort(key=lambda x: x['dcd']['itime']['str'], reverse=True)
    return JUtil.pyValToJavaObj(data)


def decodeTaf(taf, wmoHeader):
    decoder = TafDecoder.Decoder()
    try:
        bbb = wmoHeader.split()[3]
    except IndexError:
        bbb = '   '
    dcd = decoder(taf, bbb)
    tafDict = {'header': wmoHeader, 'text': taf, 'dcd': dcd}
    return JUtil.pyValToJavaObj(tafDict)
