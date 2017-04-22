#!/usr/bin/env python
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
#
# Send Message to a list of WFOs
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/22/2016       5374         randerso       Initial Creation.
#    07/11/2016       5774         randerso       Change to send WFO message to all active
#                                                 IFP servers, not just "best" 
# 
##
import os, errno, tempfile

import xml
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement

import IrtAccess

logger = None

def init_logging():
    import iscUtil
    import logging
    global logger
    logger = iscUtil.getLogger("sendWFOMessage", logLevel=logging.INFO)


def runFromJava(siteID, config, destSites, message):
    import siteConfig
    
    host      = str(config.getServerHost())
    port      = str(config.getRpcPort())
    protocol  = str(config.getProtocolVersion())
    mhsid     = str(config.getMhsid())
    ancf      = str(config.iscRoutingTableAddress().get("ANCF"))
    bncf      = str(config.iscRoutingTableAddress().get("BNCF"))
    xmtScript = str(config.transmitScript())
        
    init_logging()

    iscProductsDir = os.path.join(siteConfig.GFESUITE_HOME, "products", "ISC")

    
    # get temporary file name for WFO message
    with tempfile.NamedTemporaryFile(suffix='.sendWFOMessage', dir=iscProductsDir, delete=False) as fp:
        fp.write(message)
        fname = fp.name
        

    sourceServer = {'mhsid'   : mhsid, 
                    'host'    : host, 
                    'port'    : port,
                    'protocol': protocol, 
                    'site'    : siteID}

    try:
        if not destSites:
            raise RuntimeError('No destSites supplied')
        
        if not message:
            raise RuntimeError('No message supplied')

        irt = IrtAccess.IrtAccess(ancf, bncf, logger=logger)
        msgSendDest, xml = irt.createDestinationXML(destSites, sourceServer, findBestMatch=False)
        
        # create the XML file
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=iscProductsDir, delete=False) as fd:
            fnameXML = fd.name
            fd.write(ElementTree.tostring(xml))    
        
        if len(msgSendDest) > 0:
            # Now send the message
            logger.debug("msgSendDest: "+ str(msgSendDest))
            irt.transmitFiles("SEND_WFO_MESSAGE", msgSendDest, mhsid, [fname, fnameXML], xmtScript)
    except:
        logger.exception('Error sending WFO message to sites:' + destSites + "\n" + message)
