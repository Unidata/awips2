#! /bin/sh
# -*-python-*-

##
##
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
#    12/08/2014      4953          randerso       Added support for sending/receiving TCV files
#                                                 Additional code cleanup
#    04/08/2015      4383          dgilling       Support FireWx ISC.
#    04/23/2015      4383          randerso       Fixed exception logging
#    02/22/2016      5374          randerso       Added support for sendWFOMessage
#    11/21/2016      5959          njensen        Removed unused imports and made more pythonic
#    10/19/2017      6279          randerso       Only process requests for our site id.
#    10/25/2017      6495          randerso       Only process requests for our site id.
#    04/16/2018      7267          mapeters       Correctly log filename in purge error messages
#
##

##
# This is a base file that is not intended to be overridden.
##



from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
import os, stat, time

import IrtAccess, IrtServer
import iscMosaic, iscUtil


iscDataRecLogger = None

## Logging methods ##
def initLogger(logFile=None):
    global iscDataRecLogger
    import logging, siteConfig
    iscDataRecLogger = iscUtil.getLogger("iscDataRec", logFile)

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
def purgeFiles(files):
    for f in files:
        try:
            os.remove(f)
        except:
            logException("iscDataRec Failed to remove file %s: " % str(f))


def execIscDataRec(MSGID, SUBJECT, FILES):
    import siteConfig

    try:
       # logEvent('*** iscDataRec ***', sys.argv[1:])
        logEvent('SUBJECT:', SUBJECT, 'MSGID:', MSGID, "FILES:", FILES)

        time1 = time.clock()

        #get our MHS id
        ourMhsID = siteConfig.GFESUITE_MHSID
        ourSiteID = siteConfig.GFESUITE_SITEID

        # for most transactions, first attachment is the data file, 2nd file is the
        # XML destinations.  ISCREQUEST and GET_TCV_FILES have only 1 file being the
        # XML destinations.  We simulate two files naming them the same.
        if SUBJECT in ["ISCREQUEST", "GET_TCV_FILES"]:
            FILES.append(FILES[0])

        dataFile = FILES[0]  #first attachment is always the data file
        if len(FILES) > 1:
            xmlFile = FILES[1]  #second attachment is the xml destinations file
            fd = open(xmlFile, 'rb')
            xmlFileBuf = fd.read()
            fd.close()
            try:
                destTree = ElementTree.ElementTree(ElementTree.XML(xmlFileBuf))
                iscE = destTree.getroot()
            except:
                logException("Malformed XML received")
                return

        #no XML destination information. Default to localhost 98000000
        else:
            # create a xml element tree to replace the missing one.  This will
            # occur when OB8.2 sites send ISC data to OB8.3 sites, and also when
            # active table exchanges occur.  We default to 98000000.
            iscE = Element('isc')
            destinationsE = SubElement(iscE, 'destinations')
	    x = 98000000
            shost = 'localhost'
            addressE = SubElement(destinationsE, 'address')
            serverE = SubElement(addressE, 'server')
            serverE.text = shost
            portE = SubElement(addressE, 'port')
            portE.text = str(x)
            protocolE = SubElement(addressE, 'protocol')
            protocolE.text = "20070723"  #match this from IFPProtocol.C
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
                    logEvent("Source:", irt.printServerInfo(srcServer))
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

        # Handle GET_TCV_FILES out side the loop as it needs to do it's own loop
        if SUBJECT == 'GET_TCV_FILES':
            IrtServer.getTCVFiles(ourMhsID, srcServer, destE)
        else:
            # decode and print the source server (if present)
            for addressE in destE:
                if addressE.tag != "address":
                    continue

                destServer = irt.decodeXMLAddress(addressE)

                # find destination server information
                if destServer['mhsid'].upper() != ourMhsID.upper():
                    logDebug(SUBJECT, 'Not our mhs ID of ' + ourMhsID + \
                      ', so skipped:', irt.printServerInfo(destServer))
                    continue  #this destination is for someone else.

                if destServer['site'].upper() != ourSiteID.upper():
                    logDebug(SUBJECT, 'Not our site ID of ' + ourSiteID + \
                      ', so skipped:', irt.printServerInfo(destServer))
                    continue  #this destination is for someone else.

                # transmit the data to the ifpServer
                time2 = time.clock()

                if SUBJECT == 'PUT_ACTIVE_TABLE':
                    IrtServer.putVTECActiveTable(dataFile, None)
                elif SUBJECT == 'PUT_ACTIVE_TABLE2':
                    IrtServer.putVTECActiveTable(dataFile, xmlFileBuf)
                elif SUBJECT == 'GET_ACTIVE_TABLE':
                    IrtServer.getVTECActiveTable(dataFile, None)
                elif SUBJECT == 'GET_ACTIVE_TABLE2':
                    IrtServer.getVTECActiveTable(dataFile, xmlFileBuf)
                elif SUBJECT in ['ISCGRIDS', 'ISCGRIDS2']:
                    import serverConfig

                    additionalISCRouting = []
                    if serverConfig.AdditionalISCRouting:
                        additionalISCRouting = serverConfig.AdditionalISCRouting
                    putISCGrids(dataFile, siteConfig.GFESUITE_SITEID, srcServer.get('site'), additionalISCRouting)
                elif SUBJECT == 'ISCREQUEST':
                    IrtServer.serviceISCRequest(dataFile)
                elif SUBJECT == 'PUT_TCV_FILES':
                    IrtServer.putTCVFiles(srcServer.get('site'), dataFile)
                else:
                    logProblem("unknown subject: ", SUBJECT)
                    continue
                time3 = time.clock()
                delta1 = time2 - time1
                delta2 = time3 - time2
                logEvent('Sent to:',
                  irt.printServerInfo(destServer), "connectT=", delta1, "xmtT=", delta2)
    except:
        logException("iscDataRec failed!")

    finally:
        # cleanup
        purgeFiles(FILES)

def putISCGrids(dataFile, destSite, srcSite, additionalISCRouting):
    # iscMosaic now executes multiple times--once for the primary ISC database,
    # and once more for each additional ISC database defined in the localConfig
    args = {"siteID": destSite,
            "userID": 'SITE',
            "databaseID": destSite + "_GRID__ISC_00000000_0000",
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
            "deleteInput": False,
            "parmsToIgnore": [],
            "gridDelay": 0.0,
            "logFileName": None}
    mosaic = iscMosaic.IscMosaic(args)
    mosaic.execute()

    for entry in additionalISCRouting:
        (parms, dbName, editAreaPrefix) = entry
        parmNameList = [parm[0] + "_SFC" for parm in parms]
        args['parmsToProcess'] = parmNameList
        args['databaseID'] = destSite + "_GRID__" + dbName + "_00000000_0000"
        args['altMask'] = editAreaPrefix + srcSite
        mosaic = iscMosaic.IscMosaic(args)
        mosaic.execute()


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
        logEvent('*** iscDataRec ***', argv)
        try:
            MSGID = argv[0]
            SUBJECT = argv[1]
            FILES = argv[2].split(',')

            logEvent('SUBJECT:', SUBJECT, 'MSGID:', MSGID)
            #log the incoming files and size
            for f in FILES:
                filesize = os.stat(f)[stat.ST_SIZE]
                logEvent('Received:', f, filesize, "bytes")
            execIscDataRec(MSGID, SUBJECT, FILES)

        except:
            logException('Failure:')

    except:
        logException("FAIL: ")

