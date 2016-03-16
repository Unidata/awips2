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

import sys
import os

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ProcessReceivedDigitalDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.message import WsId

from awips import ThriftClient, ConfigFileUtil

#
# TODO: ADD DESCRIPTION
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/04/11                      bphillip       Initial Creation.
#    02/12/15         #4103        dgilling       Set site ID field.
#    03/27/15         #4103        dgilling       Read connection values from environment.
#    
# 
#


def main():
    connectionParams = getConnectionParams()
    
    try:
        receiveGridsRequest = createRequest()
        thriftClient = ThriftClient.ThriftClient(connectionParams[0], connectionParams[1], "/services")
        serverResponse = thriftClient.sendRequest(receiveGridsRequest)
    except Exception, e:
        print "Unhandled exception thrown during receive_grids processing: \n", str(e)
        sys.exit(1)
    
    if (not serverResponse.isOkay()):
        print "Errors occurred during iscDataRec processing: ", serverResponse.message()
        sys.exit(1)

def getConnectionParams():
    return (str(os.environ["SVCBU_HOST"]), int(os.environ["CDSPORT"]))

def createRequest():
    print  sys.argv
    obj = ProcessReceivedDigitalDataRequest()
    
    wsId = WsId(progName="receive_grids")
    
    obj.setWorkstationID(wsId)
    obj.setSiteID(str(sys.argv[6]).upper())
    obj.setReceivedDataFile(str(sys.argv[5]))
    return obj

if __name__ == '__main__':
    main()
    
