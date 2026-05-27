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
#    03/04/2015      4129          randerso       Pass active table change logger through to MergeVtec.merge
#    11/10/2021      8698          njensen        Replace timing calls with os.times()
#    07/14/2023      2035938       dgilling       Update xml.etree.ElementTree calls to remove 
#                                                 functions deprecated in python 3.11
#
##

##
# This is a base file that is not intended to be overridden.
##



import os
import xml.etree.ElementTree as ET

import IrtAccess
import MergeVTEC
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
        for addressE in sourceE:
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
    startT = os.times()
    
    results = execute_ingest_at(newRecords, activeTable, activeTableMode, drt,
                      makeBackups, xmlIncoming, atChangeLog=atChangeLog)
    
    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = os.times()
    elapsed = endT.elapsed - startT.elapsed
    cpu = endT.system + endT.user - startT.system - startT.user
    logger.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(elapsed, cpu))
    
    return results

