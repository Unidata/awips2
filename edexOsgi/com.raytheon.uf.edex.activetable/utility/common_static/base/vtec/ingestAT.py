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
# 
#


import errno
import logging
import os
import sys
import time
import xml.etree.ElementTree as ET

import IrtAccess
import MergeVTEC
import siteConfig


log = None

def init_logging():
    logPath = os.path.join(siteConfig.GFESUITE_LOGDIR, 
                           time.strftime("%Y%m%d", time.gmtime()), 'ingestAT.log')
    try:
        os.makedirs(os.path.dirname(logPath))
    except OSError as e:
        if e.errno != errno.EEXIST:
            sys.stderr.write("Could not create log directory " + os.path.dirname(logPath))
            sys.exit(-1)
    
    logging.basicConfig(filename=logPath, 
                        format="%(levelname)s  %(asctime)s [%(process)d:%(thread)d] %(filename)s: %(message)s", 
                        datefmt="%H:%M:%S", 
                        level=logging.INFO)
    global log
    log = logging.getLogger("ingestAT")


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
            log.info("Source Server: " + irt.printServerInfo(sourceServer))
    
    results = None        
    try:
        results = MergeVTEC.merge(activeTable, atName, incomingRecords, ztime, makeBackups,
          logging.getLogger('MergeVTEC'))
    except:
        log.exception("MergeVTEC fail:")
    return results

def runFromJava(activeTable, activeTableMode, newRecords, drt, makeBackups,
                xmlIncoming):
    init_logging()
    
    log.info('************* ingestAT ************************')
    startT = time.time()
    
    results = execute_ingest_at(newRecords, activeTable, activeTableMode, drt,
                      makeBackups, xmlIncoming)
    
    #--------------------------------------------------------------------
    # Finish
    #--------------------------------------------------------------------
    endT = time.time()
    log.info("Final: wctime: {0:-6.2f}, cputime: {1:-6.2f}".format(endT - startT, time.clock()))
    
    return results

