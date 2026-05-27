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
import sys
import os

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import IscDataRecRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId
from ufpy import ThriftClient

#
# CLI tool to process incoming ISC requests. Receives incoming request via MHS
# and resends request to EDEX for processing.
#  
#    
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Oct 26, 2010           dgilling  Initial Creation.
# Mar 30, 2015  4103     dgilling  Use shell script to call this script to 
#                                  configure env. variables.
# Jun 30, 2021  8572     randerso  Replace references to SVCBU_HOST and CDSPORT
#                                  With DEFAULT_HOST and DEFAULT_PORT
# 
#

logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    # level=logging.DEBUG)
                    level=logging.INFO)
logger = logging.getLogger("iscDataRec")


def main():
    connectionParams = getConnectionParams()
    
    logger.debug("Connection Parms: %s", connectionParams)
    
    try:
        iscDataRequest = createRequest()
        thriftClient = ThriftClient.ThriftClient(connectionParams[0], connectionParams[1], "/services")
        serverResponse = thriftClient.sendRequest(iscDataRequest)
    except:
        logger.exception("Unhandled exception thrown during iscDataRec processing.")
        sys.exit(1)
    
    if (not serverResponse.isOkay()):
        logger.error("Errors occurred during iscDataRec processing: %s", serverResponse.message())
        sys.exit(1)

def getConnectionParams():
    return (str(os.getenv("DEFAULT_HOST", "localhost")), int(os.getenv("DEFAULT_PORT", "9581")))

def createRequest():    
    obj = IscDataRecRequest()
    
    wsId = WsId(progName="iscDataRec")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID("")
    obj.setArgString(" ".join(sys.argv[1:]))
    return obj

if __name__ == '__main__':
    main()
    