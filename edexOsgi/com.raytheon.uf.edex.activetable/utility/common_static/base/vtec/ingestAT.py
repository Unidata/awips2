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
# Port of ingestAT code from AWIPS1
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/13/13        1447          dgilling       Initial Creation.
#    01/24/14        2504          randerso       change to use iscUtil.getLogger for consistency 
#


import os
import time
import xml.etree.ElementTree as ET

import IrtAccess
import MergeVTEC
import siteConfig
import iscUtil

logger = None

def init_logging():
    import logging
    global logger
    logger = iscUtil.getLogger("ingestAT", logLevel=logging.INFO)


def execute_ingest_at(incomingRecords, activeTable, atName, ztime, makeBackups, xmlIncoming):
    # log the source of this data
    if xmlIncoming is not None:
        irt = IrtAccess.IrtAccess("")
        xmlTree = ET.ElementTree(ET.XML(xmlIncoming))
        sourceE = xmlTree.find('source')
        for addressE in sourceE.getchildren():
            sourceServer = irt.decodeXMLAddress(addressE)
            if sourceServer is None:
                continue
            logger.info("Source Server: " + irt.printServerInfo(sourceServer))
    
    results = None        
    try:
        results = MergeVTEC.merge(activeTable, atName, incomingRecords, ztime, makeBackups,
          logger)
    except:
        logger.exception("MergeVTEC fail:")
    return results

def runFromJava(activeTable, activeTableMode, newRecords, drt, makeBackups,
                xmlIncoming):
    init_logging()
    
    logger.info('************* ingestAT ************************')
    startT = time.time()
    
    results = execute_ingest_at(newRecords, activeTable, activeTableMode, drt,
                      makeBackups, xmlIncoming)
    
    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = time.time()
    logger.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(endT - startT, time.clock()))
    
    return results

