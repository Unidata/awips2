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
# Port of requestAT code from AWIPS1
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Feb 06, 2013  1447     dgilling  Initial Creation.
# Jan 24, 2014  2504     randerso  change to use iscUtil.getLogger for
#                                  consistency
# May 15, 2014  3157     dgilling  Support multiple TPC and SPC sites.
# Mar 10, 2015  4129     randerso  Refactored server selection code into a
#                                  reusable method
# Apr 28, 2015  4027     randerso  Expunged Calendar from ActiveTableRecord
# Apr 29, 2020  8151     randerso  Use SiteMap.getSite4LetterId()
# Dec 02, 2020  8294     randerso  Set pickle protocol=2 for backward
#                                  compatibility with Python2
#
##

##
# This is a base file that is not intended to be overridden.
##

import pickle
import os
import tempfile
import time
from xml.etree import ElementTree

import IrtAccess
import JUtil
import VTECPartners
from com.raytheon.uf.common.activetable import ActiveTableMode
from com.raytheon.uf.common.activetable import VTECPartners as JavaVTECPartners
from com.raytheon.uf.common.time.util import TimeUtil
from com.raytheon.uf.edex.activetable import ActiveTable
import iscUtil
import siteConfig

logger = None


def init_logging():
    import logging
    global logger
    logger = iscUtil.getLogger("requestAT", logLevel=logging.INFO)


def execute_request_at(serverHost, serverPort, serverProtocol, mhsid, siteID, ancf,
                         bncf, xmtScript):
    #--------------------------------------------------------------------
    # Create a message - pickled
    # (MHSsiteID, mySiteID, listOfVTECMergeSites, countDict, issueTime)
    # Note that VTEC_MERGE_SITES does not contain our site or SPC, TPC.
    #--------------------------------------------------------------------

    # determine my 4 letter siteid
    from com.raytheon.uf.common.site import SiteMap
    mysite4 = SiteMap.getInstance().getSite4LetterId(siteID)

    otherSites = [mysite4]
    tpcSites = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(siteID).getTpcSites())
    spcSites = JUtil.javaObjToPyVal(JavaVTECPartners.getInstance(siteID).getSpcSites())
    otherSites.extend(tpcSites)
    otherSites.extend(spcSites)

    # connect to ifpServer and retrieve active table
    actTab = ActiveTable.getActiveTable(mysite4, ActiveTableMode.OPERATIONAL)

    # analyze active table to get counts
    countDict = {}
    issueTime = 0
    for i in range(actTab.size()):
        rec = actTab.get(i)
        # only care about our own sites that we merge
        if rec.getOfficeid() not in VTECPartners.VTEC_MERGE_SITES and \
          rec.getOfficeid() not in otherSites:
            continue

        recIssueTime = float(rec.getIssueTime().getTime() // TimeUtil.MILLIS_PER_SECOND)
        #track latest
        issueTime = max(recIssueTime, issueTime)

        cnt = countDict.get(rec.getOfficeid(), 0)  #get old count
        countDict[rec.getOfficeid()] = cnt + 1

    data = (mhsid, siteID, VTECPartners.VTEC_MERGE_SITES, countDict, issueTime)
    logger.info("Data: " + repr(data))

    tempdir = os.path.join(siteConfig.GFESUITE_HOME, "products", "ATBL")
    with tempfile.NamedTemporaryFile(suffix='.reqat', dir=tempdir, delete=False) as fp:
        fname = fp.name
        # Must use protocol=2 for backward compatibility with Python2,
        pickle.dump(data, fp, protocol=2)

    sourceServer = {'mhsid'   : mhsid,
                    'host'    : serverHost,
                    'port'    : serverPort,
                    'protocol': serverProtocol,
                    'site'    : siteID}

    destSites = VTECPartners.VTEC_TABLE_REQUEST_SITES
    if not destSites:
        raise Exception('No destSites defined for VTEC_TABLE_REQUEST_SITES')

    irt = IrtAccess.IrtAccess(ancf, bncf, logger=logger)
    msgSendDest, xml = irt.createDestinationXML(destSites, sourceServer)

    # create the XML file
    with tempfile.NamedTemporaryFile(suffix='.xml', dir=tempdir, delete=False) as fd:
        fnameXML = fd.name
        fd.write(ElementTree.tostring(xml, encoding="utf-8"))

    #--------------------------------------------------------------------
    # Now send the message
    #--------------------------------------------------------------------
    irt.transmitFiles("GET_ACTIVE_TABLE2", msgSendDest, mhsid,
      [fname, fnameXML], xmtScript)


def runFromJava(serverHost, serverPort, serverProtocol, mhsid, siteID, ancf,
                bncf, xmtScript):
    init_logging()

    logger.info('*********** requestAT ******************')
    startT = time.time()

    try:
        execute_request_at(serverHost, serverPort, serverProtocol, mhsid,
                             siteID, ancf, bncf, xmtScript)
    except:
        logger.exception('Error requesting active table')

    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = time.time()
    logger.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(endT - startT, time.clock()))

