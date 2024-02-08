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

import os
import logging
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import GetVtecAttributeRequest
from awips import ThriftClient

##
# Ask the server for an attribute from the VTECPartners script for a site.
# Normally, the site is taken from the GFESUITE_SITEID environment variable,
# but the site may also be passed as a parameter, in case GFESUITE_SITEID is
# not set or info for an alternate site is needed. 
# 
# @param host: The host from which to request the site IDs
# @param port: The port to which the request should be sent.
# @param site: The site whose VTECPartners script should be consulted. Defaults
#              to None, which causes os.environ['GFESUITE_SITEID'] to be used.
# @param default: A value to return if VTECPartners does not have the attribute 
#                 (defaults to None).
# @return: The list of default servers, or None.
# @raise Exception: When no site is specified and the GFESUITE_SITEID 
#                   environment variable is not set, or as raised by the thrift 
#                   client.
def getVtecAttribute(attribute, default=None, site=None, host='localhost', port=9581):
    thriftClient = ThriftClient.ThriftClient(host, port, "/services")
    request = GetVtecAttributeRequest()
    if site is None:
        try:
            request.setSiteId(os.environ['GFESUITE_SITEID'])
        except KeyError:
            raise Exception("The site has not been specified and os.environ['GFESUITE_SITEID'] is not set.")
    else:
        request.setSiteId(site)
    request.setAttribute(attribute)
    request.setDefaultValue(default)
    response = thriftClient.sendRequest(request)
    value = response.getValue()
        
    return value
        
