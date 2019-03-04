##
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
#    03/04/2015      4129          randerso       Pass active table change logger through to MergeVtec.merge
#
##

##
# This is a base file that is not intended to be overridden.
##



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


def execute_ingest_at(incomingRecords, activeTable, atName, ztime, makeBackups, xmlIncoming, atChangeLog):
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
          logger, atChangeLog)
    except:
        logger.exception("MergeVTEC fail:")
    return results

def runFromJava(activeTable, activeTableMode, newRecords, drt, makeBackups,
                xmlIncoming, atChangeLog=None):
    init_logging()
    
    logger.info('************* ingestAT ************************')
    startT = time.time()
    
    results = execute_ingest_at(newRecords, activeTable, activeTableMode, drt,
                      makeBackups, xmlIncoming, atChangeLog=atChangeLog)
    
    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = time.time()
    logger.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(endT - startT, time.clock()))
    
    return results

