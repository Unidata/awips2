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
# Returns the client ids of all connections established to qpid.
# 
# 
# SOFTWARE HISTORY
# 
# Date       	Ticket#		Engineer	Description
# ------------	----------	-----------	--------------------------
# 03/21/13      1814        rjpeter     Updated to use rest API for java broker
# 01/15/14      2660        randerso    Log status and reason if request fails
##
import httplib
import json

def getConnections(brokerHost, port=8180):
    # Use rest services to pull connection clientId
    # http://cp1f:9090/rest/connection
    # port needs to be passed as a parameter
    # parse json response for clientId, recommend using a hash of some kind
    if (port is None):
        httpConn = httplib.HTTPConnection(brokerHost)
    else:
        httpConn = httplib.HTTPConnection(brokerHost, port)

    httpConn.connect()
    httpConn.request("GET", "/rest/connection/edex")
    response = httpConn.getresponse()

    if (response.status != 200):
        msg = "Broker %s returned %d %s" % (brokerHost, response.status, response.reason)
        raise Exception(msg)

    jsonStr = response.read()
    jsonObjArray = json.loads(jsonStr)
    resultSet = set()

    for statDict in jsonObjArray:
        clientId = statDict.get("clientId")
        if clientId:
            resultSet.add(clientId)

    return list(resultSet)
