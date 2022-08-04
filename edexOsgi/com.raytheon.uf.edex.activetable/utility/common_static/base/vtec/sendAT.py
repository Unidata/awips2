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
# Port of sendAT code from AWIPS1
#
#    
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Feb 08, 2013  1447     dgilling  Initial Creation.
# Jan 24, 2014  2504     randerso  change to use iscUtil.getLogger for consistency
# May 15, 2014  3157     dgilling  Support multiple TPC and SPC sites.
# Mar 10, 2015  4129     randerso  Removed sys.exit() call
# Jul 12, 2017  19898    ryu       Include request site's own in VTEC records sent.
# Dec 02, 2020  8294     randerso  Set pickle protocol=2 for backward
#                                  compatibility with Python2
#
#

##
# This is a base file that is not intended to be overridden.
##



import pickle
import gzip
import os
import time
import tempfile
import stat
from xml.etree import ElementTree

import IrtAccess
import JUtil
import siteConfig
import VTECPartners
import VTECTableSqueeze
import iscUtil

from com.raytheon.uf.common.activetable import VTECPartners as JavaVTECPartners


# Configuration Item for Test Purposes
FORCE_SEND = False   #Set to True to always send even if no updates required.

logger = None

def init_logging():
    import logging
    global logger
    logger = iscUtil.getLogger("sendAT", logLevel=logging.INFO)
    
def execute_send_at(myServerHost, myServerPort, myServerProtocol, 
                    myServerMHSID, myServerSite, sites, filterSites, mhsSites, 
                    issueTime, countDict, fname, xmlIncoming, xmtScript):
    logger.info('reqSite= ' + repr(sites))
    logger.info('filterSites= ' + repr(filterSites))
    logger.info('mhsSite= ' + repr(mhsSites))
    logger.info('reqCountDict= ' + repr(countDict))
    if issueTime is None:
        logger.info('reqIssueTime= None')
    else:
        logger.info('reqIssueTime= ' + str(issueTime) + ' ' +
                  time.asctime(time.gmtime(issueTime)))

    irt = IrtAccess.IrtAccess("")
    myServer = {'mhsid': myServerMHSID, 'host': myServerHost, 'port': myServerPort,
                'protocol': myServerProtocol, 'site': myServerSite}
    logger.info('MyServer: ' + irt.printServerInfo(myServer))
    
    #--------------------------------------------------------------------
    # Prepare the file for sending
    #--------------------------------------------------------------------
    with open(fname, 'rb') as fd:
        buf = fd.read()
    os.remove(fname)
    table = pickle.loads(buf)   #unpickle it
    logger.info("Local Table Length= " +  str(len(table)))
    
    filtTable = []
    # filter by sites listing
    if filterSites:
        tpcSites = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(myServerSite).getTpcSites())
        spcSites = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(myServerSite).getSpcSites())
        
        sites4 = [VTECPartners.get4ID(s) for s in sites]
        for t in table:
            if t['oid'] in filterSites or t['oid'] in sites4 or \
              t['oid'] in tpcSites or t['oid'] in spcSites:
                filtTable.append(t)
    else:
        filtTable = table   #no filtering
    logger.info("Site Filtered Table Length= " + str(len(filtTable)))
    
    # eliminate obsolete records
    ctime = time.time()  #now time
    vts = VTECTableSqueeze.VTECTableSqueeze(ctime)
    filtTable = rename_fields_for_A2(filtTable)
    actTable, tossRecords = vts.squeeze(filtTable)
    actTable = rename_fields_for_A1(actTable)
    logger.info("Squeezed Table Length= " + str(len(actTable)))
    
    # check issuance time - any times newer in remote table (this table) than
    # the local table (requesting site)?
    if issueTime is not None:
        newerRec = False
        newestRec = 0.0
        for t in actTable:
            if newestRec < t['issueTime']:
                newestRec = t['issueTime']
        if issueTime < newestRec:
            newerRec = True
            
        logger.info("NewestFound= " + str(newestRec) + ' ' + 
                 time.asctime(time.gmtime(newestRec)))
        logger.info("IssueTime check.  Newer record found=  " + str(newerRec))
    else:
        newerRec = True   #just assume there are newer records
    
    # check "counts" for number of records.  Any more records means that
    # we may be missing some records.
    if countDict:
        missingRec = False
        localCountDict = {}
        for t in actTable:
            if t['oid'] in localCountDict:
                localCountDict[t['oid']] = localCountDict[t['oid']] + 1
            else:
                localCountDict[t['oid']] = 1
        for site in localCountDict:
            reqCount = countDict.get(site, 0)  #number in requesting site
            if reqCount != localCountDict[site]:  #records different in request site
                missingRec = True
                break
        logger.info("MissingRec check. Missing record found= " + str(missingRec))
        logger.info("lclCountBySite= " + repr(localCountDict))
        logger.info("reqCountBySite= " + repr(countDict))
    else:
        missingRec = True   #just assume there are
    
    #should we send?
    if missingRec or newerRec or FORCE_SEND:
        sendIt = True
    else:
        sendIt = False
    
    if sendIt:
        # Must use protocol=2 for backward compatibility with Python2,
        actTablePickled = pickle.dumps(actTable, protocol=2)  #repickle it
        rawSize = len(actTablePickled)
    
        #output it as gzipped
        fname = fname + ".gz"
        with gzip.open(fname, 'wb', 6) as fd:
            fd.write(actTablePickled)

        gzipSize = os.stat(fname)[stat.ST_SIZE]
        logger.info('#dataSize: ' + str(rawSize) + ', #gzipSize: ' + str(gzipSize))

        #--------------------------------------------------------------------
        # Create the destination XML file
        #--------------------------------------------------------------------
        iscOut = ElementTree.Element('isc')
        irt.addSourceXML(iscOut, myServer)

        destServers = []

        if xmlIncoming is not None:
            with open(xmlIncoming,'rb') as fd:
                xml = fd.read()
            os.remove(xmlIncoming)
            reqTree = ElementTree.ElementTree(ElementTree.XML(xml))
            sourceE = reqTree.find('source')
            for addressE in sourceE.getchildren():
                destServer = irt.decodeXMLAddress(addressE)
                if destServer is None:
                    continue
                destServers.append(destServer)
                break

        # no XML received on request, this is probably from an older site.
        # create a dummy response XML file. Try all combinations.  Only
        # the mhsid is important for older sites.
        else:
            for mhss in mhsSites:
                for port in range(98000000, 98000002):
                    for site in sites:
                        for host in ['dv4', 'dx4f', 'pv3', 'px3']:
                            destServer = {'mhsid': mhss, 'host': host,
                              'port': port, 'protocol': "20070723", 
                              'site': site}
                            destServers.append(destServer)

        irt.addDestinationXML(iscOut, destServers)   #add the dest server xml

        # print out destinations
        s = "Destinations:"
        for destServer in destServers:
            s += "\n" + irt.printServerInfo(destServer)
        logger.info(s)

        # create XML file
        tempdir = os.path.join(siteConfig.GFESUITE_HOME, "products", "ATBL")
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=tempdir, delete=False) as fd:
            fnameXML = fd.name
            fd.write(ElementTree.tostring(iscOut, encoding="utf-8"))

        #--------------------------------------------------------------------
        # Send it 
        #--------------------------------------------------------------------
        irt.transmitFiles("PUT_ACTIVE_TABLE2", mhsSites, myServerSite,
          [fname, fnameXML], xmtScript)
    
    else:
        logger.info("Send has been skipped")
    
def rename_fields_for_A1(table):
    newTable = []
    for record in table:
        record['oid'] = record['officeid']
        del record['officeid']
        record['vstr'] = record['vtecstr']
        del record['vtecstr']
        record['end'] = record['endTime']
        del record['endTime']
        record['start'] = record['startTime']
        del record['startTime']
        record['key'] = record['phensig']
        del record['phensig']
        if 'segText' in record:
            record['text'] = record['segText']
            del record['segText']
        newTable.append(record)
        
    return newTable

def rename_fields_for_A2(table):
    newTable = []
    for record in table:
        record['officeid'] = record['oid']
        del record['oid']
        record['vtecstr'] = record['vstr']
        del record['vstr']
        record['endTime'] = record['end']
        del record['end']
        record['startTime'] = record['start']
        del record['start']
        record['phensig'] = record['key']
        del record['key']
        if 'text' in record:
            record['segText'] = record['text']
            del record['text']
        newTable.append(record)
        
    return newTable

def runFromJava(myServerHost, myServerPort, myServerProtocol, myServerMHSID, 
                myServerSite, sites, filterSites, mhsSites, issueTime, 
                countDict, fname, xmlIncoming, xmtScript):
    init_logging()
    
    logger.info('*********** sendAT ****************')
    startT = time.time()
    
    try:
        sites = JUtil.javaObjToPyVal(sites)
        filterSites = JUtil.javaObjToPyVal(filterSites)
        mhsSites = JUtil.javaObjToPyVal(mhsSites)
        countDict = JUtil.javaObjToPyVal(countDict)
        
        execute_send_at(myServerHost, myServerPort, myServerProtocol, 
                        myServerMHSID, myServerSite, sites, filterSites, 
                        mhsSites, issueTime, countDict, fname, xmlIncoming, 
                        xmtScript)
    except:
        logger.exception('Error in sendAT:')
        raise
    
    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = time.time()
    logger.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(endT - startT, time.clock())) 
