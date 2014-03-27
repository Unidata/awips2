#! /bin/sh
# -*-python-*-

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

import iscMosaic,iscUtil
import os, stat, sys, re, string, traceback, types
import time, xml, LogStream, IrtAccess
import IrtServer
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
from java.util import ArrayList

#
# Port of iscDataRec.py
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    01/29/13        1447          dgilling       Implement VTEC table sharing.
#    03/12/13        1759          dgilling       Bypass command-line processing
#                                                 for iscMosaic, support changes
#                                                 to IscReceiveSrv.
#    01/24/14        2504          randerso       removed obsolete A1 comments 
#    
# 
#

iscDataRecLogger=None

## Logging methods ##
def initLogger(logFile=None):
    global iscDataRecLogger
    import logging, siteConfig
    iscDataRecLogger = iscUtil.getLogger("iscDataRec",logFile)

def logEvent(*msg):
    iscDataRecLogger.info(iscUtil.tupleToString(*msg))

def logProblem(*msg):
    iscDataRecLogger.error(iscUtil.tupleToString(*msg))
    
def logException(*msg):
    iscDataRecLogger.exception(iscUtil.tupleToString(*msg))    

def logVerbose(*msg):
    iscDataRecLogger.debug(iscUtil.tupleToString(*msg))
    
def logDebug(*msg):
    logVerbose(iscUtil.tupleToString(*msg))

# File Purging Routing
def purgeFiles(msgid, files):
    for file in files:
        try:
            os.remove(file)
        except:
            logEvent("iscDataRec Failed to remove: ",file)


def execIscDataRec(MSGID,SUBJECT,FILES):
    import siteConfig
    
    try:
       # logEvent('*** iscDataRec ***', sys.argv[1:]) 
        logEvent('SUBJECT:', SUBJECT, 'MSGID:', MSGID,"FILES:",FILES)
     
        time1 = time.clock() 
        
        #get our MHS id
        ourMhsID = siteConfig.GFESUITE_MHSID
        
        # for most transactions, first attachment is the data file, 2nd file is the
        # XML destinations.  The ISCREQUEST is unique with only 1 file being the
        # XML destinations.  We simulate two files naming them the same.
        if SUBJECT == "ISCREQUEST":
            FILES.append(FILES[0])
        
        dataFile = FILES[0]  #first attachment is always the data file
        if len(FILES) > 1:
            xmlFile = FILES[1]  #second attachment is the xml destinations file
            fd = open(xmlFile,'rb')
            xmlFileBuf = fd.read()
            fd.close()
            try:
                destTree = ElementTree.ElementTree(ElementTree.XML(xmlFileBuf))
                iscE = destTree.getroot()
            except:
                logProblem("Malformed XML received")
                return
        
        #no XML destination information. Default to dx4f,px3 98000000, 98000001
        else:
            # create a xml element tree to replace the missing one.  This will
            # occur when OB8.2 sites send ISC data to OB8.3 sites, and also when
            # active table exchanges occur.  We default to 98000000 and 98000001
            # on dx4 since that is where the primary and svcbu servers are located.
            # This will cause log errors until everyone is on OB8.3.
            iscE = Element('isc')
            destinationsE = SubElement(iscE, 'destinations')
            for x in xrange(98000000, 98000002):
                for shost in ['dx4f','px3f']:
                    addressE = SubElement(destinationsE, 'address')
                    serverE = SubElement(addressE, 'server')
                    serverE.text = shost
                    portE = SubElement(addressE, 'port')
                    portE.text = str(x)
                    protocolE = SubElement(addressE, 'protocol')
                    protocolE.text = "20070723"   #match this from IFPProtocol.C
                    mhsE = SubElement(addressE, 'mhsid')
                    mhsE.text = siteConfig.GFESUITE_MHSID
        
        irt = IrtAccess.IrtAccess("")
        
        # find source xml
        found = False
        for srcE in iscE.getchildren():
            if srcE.tag == "source":
                for addressE in srcE:
                    srcServer = irt.decodeXMLAddress(addressE)
                    if srcServer is None:
                        continue
                    found = True
                    logEvent("Source:",irt.printServerInfo(srcServer))
                    break
        if not found:
            logEvent("Source: <unknown>")
        
        # find destinations xml
        found = False
        for destE in iscE.getchildren():
            if destE.tag == "destinations":
                found = True
                break
        if not found:
            logProblem("destinations packet missing from xml")
            return
        
        # decode and print the source server (if present)
        for addressE in destE:
            if addressE.tag != "address":
                continue
        
            destServer = irt.decodeXMLAddress(addressE)
        
            # find destination server information
            mhsidDest=serverDest=portDest=protocolDest=None
            for attrE in addressE.getchildren():
                if attrE.tag == "mhsid":
                    mhsidDest = attrE.text
                elif attrE.tag == "server":
                    serverDest = attrE.text
                elif attrE.tag == "port":
                    portDest = attrE.text
                elif attrE.tag == "protocol":
                    protocolDest = attrE.text
                    
            if destServer['mhsid'].upper() != ourMhsID.upper():
                logDebug(SUBJECT, 'Not our mhs of ' + ourMhsID + \
                  ', so skipped:', irt.printServerInfo(destServer))
                continue   #this destination is for someone else.
        
            # transmit the data to the ifpServer
            with open(dataFile, "rb") as fp:
                fpData = fp.read()
            time2 = time.clock()
    
            if SUBJECT == 'PUT_ACTIVE_TABLE':
                IrtServer.putVTECActiveTable(fpData, None)
            elif SUBJECT == 'PUT_ACTIVE_TABLE2':
                IrtServer.putVTECActiveTable(fpData, xmlFileBuf)
            elif SUBJECT == 'GET_ACTIVE_TABLE':
                IrtServer.getVTECActiveTable(fpData, None)
            elif SUBJECT == 'GET_ACTIVE_TABLE2':
                IrtServer.getVTECActiveTable(fpData, xmlFileBuf) 
            elif SUBJECT in ['ISCGRIDS', 'ISCGRIDS2']:
                args = {"siteID": siteConfig.GFESUITE_SITEID, 
                        "userID": 'SITE', 
                        "databaseID": siteConfig.GFESUITE_SITEID+"_GRID__ISC_00000000_0000",
                        "parmsToProcess": [], 
                        "blankOtherPeriods": True, 
                        "startTime": None,
                        "endTime": None, 
                        "altMask": None,
                        "replaceOnly": False, 
                        "eraseFirst": False,
                        "announce": "ISC: ", 
                        "renameWE": True,
                        "iscSends": False, 
                        "inFiles": [dataFile],
                        "ignoreMask": False, 
                        "adjustTranslate": True,
                        "deleteInput": True, 
                        "parmsToIgnore": [],
                        "gridDelay": 0.0, 
                        "logFileName": None}                
                mosaic = iscMosaic.IscMosaic(args)
                mosaic.execute() 
    
            elif SUBJECT == 'ISCREQUEST':
                IrtServer.serviceISCRequest(fpData)
            else:
                nosend = True
                logProblem("unknown subject: ", SUBJECT)
                continue
            time3 = time.clock()
            delta1 = time2-time1
            delta2 = time3-time2
            logEvent('Sent to:', 
              irt.printServerInfo(destServer), "connectT=", delta1, "xmtT=", delta2)   
    except:
        logProblem("iscDataRec failed!",traceback.format_exc())
    finally:    
        # cleanup
        purgeFiles(MSGID, FILES)

#--------------------------------------------------------------------
# Main Routine
#--------------------------------------------------------------------

#           The following keywords prefaced by the '%' character cause the
#           value from the current message to be inserted into the
#           specification string.
#
#               MSGID:      Unique ID of message
#               MSGTYPE:    Sender assigned message type
#               MSGCODE:    Sender assigned message code
#               SUBJECT:    Sender assigned message subject
#               SENDER:     Sending site name
#               PRIORITY:   Sender assigned message priority
#               BODY:       Path to file containing body of message.
#               ENCLIST:    List of paths to files for each enclosure.
#               ENCLOSE(N): Path to file containing Nth enclosure.
#               NUMENC:     Total number of enclosures.
#               X400DOC:    Path to X.400 document file.
#
# /data/adapt/GFESuite/iscDataRec SYS /awips/adapt/GFESUITE/bin/iscDataRec
#                             %MSGID %SUBJECT %ENCLIST
def main(argv):
    initLogger()
    try:
        if type(argv) is not list:
            import JUtil
            argv = JUtil.javaObjToPyVal(argv)
        logEvent('*** iscDataRec ***', argv)
        try:
            MSGID = argv[0]
            SUBJECT = argv[1]
            FILES = argv[2].split(',')
        
            logEvent('SUBJECT:', SUBJECT, 'MSGID:', MSGID)
            #log the incoming files and size
            for file in FILES:
                filesize = os.stat(file)[stat.ST_SIZE]
                logEvent('Received:', file, filesize, "bytes")
            execIscDataRec(MSGID,SUBJECT,FILES)
        
        except:
            logProblem('Failure:', traceback.format_exc())
    
    except:
        logProblem("FAIL: ", traceback.format_exc())
        
