##
##

import logging
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import GetFourCharSitesRequest
from awips import ThriftClient

##
# Convert a list with 3-char site IDs to a list of 4-char site IDs.
# All the site IDs are sent to the server, which applies a transformation
# to any 3-char strings in the list and returns the modified list.
#
# This method sacrifices some performance and network bandwidth for simplicity. 
# It should be refactored if it is called in tight loops or the input list contains 
# 
# @param threeCharSites: The site IDs to convert.
# @type threeCharSites: list of string 
# @return: The converted site IDs
# @rtype: list of string
# @raise Exception: When raised by ThriftClient
def getFourCharSites(threeCharSites, host='localhost', port=9581):
    thriftClient = ThriftClient.ThriftClient(host, port, "/services")
    request = GetFourCharSitesRequest()
    request.setSites(threeCharSites)
    response = thriftClient.sendRequest(request)
    return response.getSites()
