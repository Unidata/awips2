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

import httplib
from dynamicserialize import DynamicSerializationManager
from dynamicserialize.dstypes.com.raytheon.uf.common.serialization.comm.response import ServerErrorResponse
from dynamicserialize.dstypes.com.raytheon.uf.common.serialization import SerializableExceptionWrapper

#
# Provides a Python-based interface for executing Thrift requests.
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/20/10                      dgilling       Initial Creation.
#    
# 
#


class ThriftClient:
    
    # How to call this constructor:
    #   1. Pass in all arguments separately (e.g., 
    #      ThriftClient.ThriftClient("localhost", 9581, "/services"))
    #      will return a Thrift client pointed at http://localhost:9581/services.
    #   2. Pass in all arguments through the host string (e.g., 
    #      ThriftClient.ThriftClient("localhost:9581/services"))
    #      will return a Thrift client pointed at http://localhost:9581/services.
    #   3. Pass in host/port arguments through the host string (e.g., 
    #      ThriftClient.ThriftClient("localhost:9581", "/services"))
    #      will return a Thrift client pointed at http://localhost:9581/services.
    def __init__(self, host, port=9581, uri="/services"):
        hostParts = host.split("/", 1)
        if (len(hostParts) > 1):
            hostString = hostParts[0]
            self.__uri = "/" + hostParts[1]
            self.__httpConn = httplib.HTTPConnection(hostString)
        else:
            if (port is None):
                self.__httpConn = httplib.HTTPConnection(host)
            else:
                self.__httpConn = httplib.HTTPConnection(host, port)
            
            self.__uri = uri
        
        self.__dsm = DynamicSerializationManager.DynamicSerializationManager()
        
    def sendRequest(self, request, uri="/thrift"):
        message = self.__dsm.serializeObject(request)
        
        self.__httpConn.connect()
        self.__httpConn.request("POST", self.__uri + uri, message)
        
        response = self.__httpConn.getresponse()
        if (response.status != 200):
            raise ThriftRequestException("Unable to post request to server")
        
        rval = self.__dsm.deserializeBytes(response.read())
        self.__httpConn.close()
        
        # let's verify we have an instance of ServerErrorResponse
        # IF we do, through an exception up to the caller along
        # with the original Java stack trace
        # ELSE: we have a valid response and pass it back
        try:
           forceError = rval.getException()          
           raise ThriftRequestException(forceError)
        except AttributeError:
            pass
        
        return rval
    
    
class ThriftRequestException(Exception):
    def __init__(self, value):
        self.parameter = value
    
    def __str__(self):
        return repr(self.parameter)
    
        