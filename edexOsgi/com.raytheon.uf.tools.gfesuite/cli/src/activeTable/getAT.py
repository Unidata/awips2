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
from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import GetActiveTableDictRequest
from awips import ThriftClient
from getVtecAttribute import getVtecAttribute

logging.basicConfig(level=logging.INFO)
log = logging.getLogger("getAT")
def getActiveTable(tableName, sites, ourSite=None, host="localhost", port=9581):
    if len(sites) == 0:
        sites = getDefaultVtecSites(host,port)
        
    log.info('Getting active table for Sites:'+ str(sites))
    
    request = GetActiveTableDictRequest()
    
    if ourSite is None:
        if 'GFESUITE_SITEID' in os.environ.keys():
            ourSite = os.environ['GFESUITE_SITEID']
        elif len(sites) > 0:
            ourSite = sites[0];
            
    if ourSite is None or ""==ourSite:
        raise Exception("The site has not been specified and os.environ['GFESUITE_SITEID'] is not set.")
        
    request.setRequestedSiteId(ourSite)
    tableName = tableName.upper()
    if tableName != 'PRACTICE':
        tableName = 'OPERATIONAL'
    request.setMode(tableName)
    request.setWfos(sites);

    thriftClient = ThriftClient.ThriftClient(host, port, "/services")
    
    try:
        response = thriftClient.sendRequest(request)
    except:
        log.exception('Error getting table for sites %s:', sites)
        response = None
    
    return response.getActiveTable()

##
# Ask the server for the default list of sites to use when no sites have been
# specified by the user.
# 
# @param host: The host from which to request the site IDs
# @param port: The port to which the request should be sent.
# @return: The list of default servers, or None.
# @raise Exception: as raised by getVtecAttribute
def getDefaultVtecSites(host, port):
    backupDict = getVtecAttribute('BackupDict', host=host, port=port)
    # Server-side, Jep may have returned BackupDict as a string.
    # If so, convert it back to a dict.
    if type(backupDict)==str:
        backupDict = eval(backupDict)
    sites = backupDict.keys()
        
    return sites
        
