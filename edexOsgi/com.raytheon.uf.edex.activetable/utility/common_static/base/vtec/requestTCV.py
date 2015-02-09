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
# Request TCV Advisory Files
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/05/14        4953          randerso       Initial Creation.
# 
##
import os, errno, tempfile

import xml
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement

import IrtAccess
import VTECPartners

logger = None

def init_logging():
    import iscUtil
    import logging
    global logger
    logger = iscUtil.getLogger("requestTCV", logLevel=logging.DEBUG)

def createDestinationXML(siteID, host, port, protocol, mhsid, ancf, bncf, logger):
    #--------------------------------------------------------------------
    # Assemble XML source/destination document
    #--------------------------------------------------------------------
    msgSendDest = []   #list of mhs sites to send request

    irt = IrtAccess.IrtAccess(ancf, bncf)
    iscE = ElementTree.Element('isc')
    # this is the requestor of the data
    sourceServer = {'mhsid'   : mhsid, 
                    'host'    : host, 
                    'port'    : port,
                    'protocol': protocol, 
                    'site'    : siteID}
    irt.addSourceXML(iscE, sourceServer)
    logger.info("Requesting Server: " + irt.printServerInfo(sourceServer))

    # who is running the domains requested?
    sites = VTECPartners.VTEC_TABLE_REQUEST_SITES
    if not sites:
        logger.error('No sites defined for VTEC_TABLE_REQUEST_SITES')
        sys.exit(1)

    status, xml = irt.getServers(sites)
    if not status:
        logger.error('Failure to getServers from IRT')
        sys.exit(1)

    # decode the XML
    try:
        serverTree = ElementTree.ElementTree(ElementTree.XML(xml))
        serversE = serverTree.getroot()
    except:
        logger.exception("Malformed XML on getServers()")
        sys.exit(1)

    if serversE.tag != "servers":
        logger.error("Servers packet missing from web server")
        sys.exit(1)

    # process each requested domain returned to us
    chosenServers = []
    matchingServers = []
    for domainE in serversE:
        if domainE.tag != "domain":
            continue
        servers = []  #list of servers for this domain

        # decode each server in the domain
        for addressE in domainE.getchildren():
            info = irt.decodeXMLAddress(addressE)
            if info is None:
                continue   #not address tag
            
            # remove unneeded keys
            for key in ['parms', 'area', 'domain']:
                if info.has_key(key):
                    del info[key]
            
            servers.append(info)
            matchingServers.append(info)

        # server search list in priority.  The px3 entries are used for
        # dual domain for AFC.
        hp = [('dx4','98000000'),('px3', '98000000'), ('dx4','98000001'),
          ('px3', '98000001')]

        # choose one server from this domain, find first dx4, 98000000
        # try to use one with the same mhsidDest as the site, which
        # would be the primary operational GFE. Note that the px3 entries
        # are for AFC.
        found = False
        for matchServer, matchPort in hp:
            for server in servers:
                if server['host'][0:3] == matchServer and \
                  server['port'] == matchPort and server['mhsid'] == siteID:
                    chosenServers.append(server)
                    if server['mhsid'] not in msgSendDest:
                        msgSendDest.append(server['mhsid'])
                    found = True
                    break

        # find first dx4, 98000000, but perhaps a different mhsid
        # this is probably not the primary operational GFE
        if not found:
            for matchServer, matchPort in hp:
                for server in servers:
                    if server['host'][0:3] == matchServer and \
                      server['port'] == matchPort:
                        chosenServers.append(server)
                        if server['mhsid'] not in msgSendDest:
                            msgSendDest.append(server['mhsid'])
                        found = True
                        break

        # if didn't find standard one, then take the first one, but don't
        # take ourselves unless we are the only one.
        if not found and servers:
            for server in servers:
                if server['mhsid'] != mhsid and server['host'] != host \
                  and server['port'] != port and \
                  server['mhsid'] != siteID:
                    chosenServers.append(server)
                    if server['mhsid'] not in msgSendDest:
                        msgSendDest.append(server['mhsid'])
                    found = True
            if not found:
                chosenServers.append(server)
                if servers[0]['mhsid'] not in msgSendDest:
                    msgSendDest.append(servers[0]['mhsid'])

    # Display the set of matching servers
    s = "Matching Servers:"
    for x in matchingServers:
        s += "\n" + irt.printServerInfo(x)
    logger.info(s)

    # Display the chosen set of servers
    s = "Chosen Servers:"
    for x in chosenServers:
        s += "\n" + irt.printServerInfo(x)
    logger.info(s)

    irt.addDestinationXML(iscE, chosenServers)

    return msgSendDest, iscE

def runFromJava(siteID, config):
    import siteConfig
    host      = str(config.getServerHost())
    port      = str(config.getRpcPort())
    protocol  = str(config.getProtocolVersion())
    mhsid     = str(config.getMhsid())
    ancf      = str(config.iscRoutingTableAddress().get("ANCF"))
    bncf      = str(config.iscRoutingTableAddress().get("BNCF"))
    xmtScript = str(config.transmitScript())

    init_logging()
    
    tempdir = os.path.join(siteConfig.GFESUITE_HOME, "products", "TCV")
    try:
        os.makedirs(tempdir, 0755)
    except OSError, e:
        if e.errno != errno.EEXIST:
            logger.warn("%s: '%s'" % (e.strerror,e.filename))

    try:
        msgSendDest, xml = createDestinationXML(siteID, host, port, protocol, mhsid, ancf, bncf, logger)
        # create the XML file
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=tempdir, delete=False) as fd:
            fnameXML = fd.name
            fd.write(ElementTree.tostring(xml))    
    
        # don't send to ourselves
        if mhsid in msgSendDest:
            msgSendDest.remove(mhsid)
            
        if len(msgSendDest) > 0:
            # Now send the message
            irt = IrtAccess.IrtAccess(ancf, bncf)
            irt.transmitFiles("GET_TCV_FILES", msgSendDest, mhsid, [fnameXML], xmtScript)
    except:
        logger.exception('Error requesting TCV files for site: ' + siteID)
