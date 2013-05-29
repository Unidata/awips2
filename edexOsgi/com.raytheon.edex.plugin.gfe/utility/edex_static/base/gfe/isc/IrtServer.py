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

import cPickle

import LogStream, tempfile, os, sys, JUtil, subprocess, traceback
import time, copy, string, iscUtil

from com.raytheon.edex.plugin.gfe.isc import IRTManager



#
# Port of IRT functionality from legacy ifpServer
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/14/09        1995          bphillip       Initial Creation.
#    01/25/13        1447          dgilling       Implement routines needed by
#                                                 iscDataRec for VTEC table 
#                                                 sharing.
#    03/13/13        1759          dgilling       Move siteConfig imports into
#                                                 functions where module is used
#                                                 to interact better with IscScript.
#    05/22/13        1759          dgilling       Add missing import to 
#                                                 makeISCrequest().
#    
# 
#
# starts the IRT thread and registers.
StopIRT = 0   #flag to shut down the 2nd thread
IRTthread = None   #flag to hold the IRTthread object

def logEvent(*msg):
    iscUtil.getLogger("irtServer").info(iscUtil.tupleToString(*msg))

def logProblem(*msg):
    iscUtil.getLogger("irtServer").error(iscUtil.tupleToString(*msg))
    
def logException(*msg):
    iscUtil.getLogger("irtServer").exception(iscUtil.tupleToString(*msg))    

def logVerbose(*msg):
    iscUtil.getLogger("irtServer").debug(iscUtil.tupleToString(*msg))
    
def logDebug(*msg):
    logVerbose(iscUtil.tupleToString(*msg))
    
# called by iscDataRec when another site has requested the active table
# returns the active table, filtered, pickled.
def getVTECActiveTable(siteAndFilterInfo, xmlPacket):
    import siteConfig
    import VTECPartners
    
    if not VTECPartners.VTEC_RESPOND_TO_TABLE_REQUESTS:
        return   #respond is disabled

    #decode the data (pickled)
    info = cPickle.loads(siteAndFilterInfo)
    (mhsSite, reqsite, filterSites, countDict, issueTime) = info

    #get the active table, and write it to a temporary file
    from com.raytheon.uf.common.site import SiteMap
    from com.raytheon.uf.edex.activetable import ActiveTable
    from com.raytheon.uf.common.activetable import ActiveTableMode
    from com.raytheon.uf.common.activetable import ActiveTableUtil
    site4Id = SiteMap.getInstance().getSite4LetterId(siteConfig.GFESUITE_SITEID)
    javaTable = ActiveTable.getActiveTable(site4Id, ActiveTableMode.OPERATIONAL)
    dictTable = ActiveTableUtil.convertToDict(javaTable, siteConfig.GFESUITE_SITEID)
    
    # we must convert this to a python hash using the A1 field naming conventions
    # for cross-version compatibility
    table = []
    for i in xrange(dictTable.size()):
        convRecord = JUtil.javaObjToPyVal(dictTable.get(i))
        convRecord['oid'] = convRecord['officeid']
        convRecord['vstr'] = convRecord['vtecstr']
        convRecord['end'] = convRecord['endTime']
        convRecord['start'] = convRecord['startTime']
        convRecord['key'] = convRecord['phensig']
        # remove new fields so we don't pickle two copies
        del convRecord['officeid']
        del convRecord['vtecstr']
        del convRecord['endTime']
        del convRecord['phensig']
        del convRecord['startTime']
        if convRecord.has_key('segText'):
            convRecord['text'] = convRecord['segText']
            del convRecord['segText']
        table.append(convRecord)
        
    # additionally, we'll need to pickle our output to match the A1 file
    # format
    pickledTable = cPickle.dumps(table)
    outDir = os.path.join(siteConfig.GFESUITE_PRDDIR, "ATBL")
    with tempfile.NamedTemporaryFile(suffix='.ato', dir=outDir, delete=False) as fp:
        fname = fp.name
        fp.write(pickledTable)

    #write the xmlpacket to a temporary file, if one was passed
    if xmlPacket is not None:
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=outDir, delete=False) as fp:
            fnameXML = fp.name
            fp.write(xmlPacket)

    from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
    config = IFPServerConfigManager.getServerConfig(siteConfig.GFESUITE_SITEID)
    ServerHost = siteConfig.GFESUITE_SERVER
    ServerPort = str(siteConfig.GFESUITE_PORT)
    ServerProtocol = str(config.getProtocolVersion())
    ServerMHS = siteConfig.GFESUITE_MHSID
    ServerSite = siteConfig.GFESUITE_SITEID
    XmtScript = config.transmitScript()

    #call sendAT to send the table to the requestor
    cmd = os.path.join(siteConfig.GFESUITE_HOME, "bin", "sendAT")
    args = [cmd, '-s', reqsite, '-a', mhsSite, '-H', ServerHost,
      '-P', ServerPort, '-L', ServerProtocol, '-M', ServerMHS,
      '-S', ServerSite, '-x', XmtScript]
    if filterSites is not None:
        for fs in filterSites:
            args.append('-f')
            args.append(fs)
    if countDict is not None:
            args.append('-c')
            args.append(`countDict`)
    if issueTime is not None:
        args.append('-t')
        args.append(`issueTime`)
    args.append('-v')
    args.append(fname)
    if xmlPacket is not None:
        args.append('-X')
        args.append(fnameXML)
    try:
        output = subprocess.check_output(args, stderr=subprocess.STDOUT)
    except:
        logProblem("Error executing sendAT: ", traceback.format_exc())
    logEvent("sendAT command output: ", output)

#when we receive a requested active table from another site, this function
#is called from iscDataRec
def putVTECActiveTable(strTable, xmlPacket):
    import siteConfig
    
    #write the xmlpacket to a temporary file, if one was passed
    inDir = os.path.join(siteConfig.GFESUITE_PRDDIR, "ATBL")
    if xmlPacket is not None:
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=inDir, delete=False) as fp:
            fnameXML = fp.name
            fp.write(xmlPacket)
    with tempfile.NamedTemporaryFile(suffix='.ati', dir=inDir, delete=False) as fp:
         fname = fp.name
         fp.write(strTable)
    
    cmd = os.path.join(siteConfig.GFESUITE_HOME, "bin", "ingestAT")
    args = []
    args.append(cmd)
    args.append("-s")
    args.append(siteConfig.GFESUITE_SITEID)
    args.append("-f")
    args.append(fname)
    if xmlPacket is not None:
        args.append('-X')
        args.append(fnameXML)
    try:
        output = subprocess.check_output(args, stderr=subprocess.STDOUT)
    except:
        logProblem("Error executing ingestAT: ", traceback.format_exc())
    logEvent("ingesAT command output: ", output)
    
def initIRT(ancfURL, bncfURL, mhsid, serverHost, serverPort, serverProtocol,
  site, parmsWanted, gridDims, gridProj, gridBoundBox, iscWfosWanted):
    global IRTthread
    import threading
    IRTthread = threading.Thread(target=irtReg, args=[ancfURL, bncfURL, mhsid,
      serverHost, serverPort, serverProtocol, site, parmsWanted, gridDims,
      gridProj, gridBoundBox, iscWfosWanted])
    IRTthread.setDaemon(True)
    IRTthread.start()

# IRT registration thread
def irtReg(ancfURL, bncfURL, mhsid, serverHost, serverPort, serverProtocol,
  site, parmsWanted, gridDims, gridProj, gridBoundBox, iscWfosWanted):
    import IrtAccess, threading
    irt = IrtAccess.IrtAccess(ancfURL, bncfURL)

    # do initial registration, keep trying until successful
    while True:
        okay = irt.register(mhsid, serverHost, serverPort, serverProtocol,
          site, parmsWanted, gridDims, gridProj, gridBoundBox, iscWfosWanted)
        if okay:
            break
        elif StopIRT:
            return False#stop this thread
        else:
            return False

    # if we are here, we had a successful registration, check for re-register
    # every few seconds, check the StopIRT flag every few seconds
    while IRTManager.getInstance().isRegistered(mhsid,site) == True:
        time.sleep(3.0)   #wait 3 seconds
        irt.checkForReregister()

    # if we get here, we have been told to stop IRT, so we unregister.  We
    # try only once.
    irt.unregister()
    return True

# call from C++ to Python to tell IRT thread to shut itself down
def irtStop():
    global StopIRT
    StopIRT = True   #tells irt thread to exit
    if IRTthread:
        IRTthread.join()  #wait till thread returns then return to caller

# get servers direct call for IRT
def irtGetServers(ancfURL, bncfURL, iscWfosWanted):
    import IrtAccess
    irt = IrtAccess.IrtAccess(ancfURL, bncfURL)
    xml = None
    status, xml = irt.getServers(iscWfosWanted)
    return xml

# make a request for ISC (support for IFPServer.C)
# xmlRequest is the original request from the GFE's ISCRequestDialog.
def makeISCrequest(xmlRequest, gridDims, gridProj, gridBoundBox, mhs, host, port, protocol, site, xmtScript):
    import IrtAccess
    import siteConfig
    import xml
    from xml.etree import ElementTree
    from xml.etree.ElementTree import Element, SubElement
    
    ServerMHS = mhs
    ServerHost = host
    ServerPort = port
    ServerProtocol = protocol
    ServerSite = site
    
    
    if type(gridDims) != "list":
        pylist = []
        size = gridDims.size() 
        for i in range(size):
            pylist.append(gridDims.get(i).intValue())
        gridDims = pylist
    
    if type(gridBoundBox) != "tuple":
        gridBoundBox = ((gridBoundBox.get(0).doubleValue(),gridBoundBox.get(1).doubleValue()),(gridBoundBox.get(2).doubleValue(),gridBoundBox.get(3).doubleValue()))

    irt = IrtAccess.IrtAccess(None)
    logEvent("ISC Request (makeISCrequest)")

    # we need to modify the incoming xmlRequest and add the <source>
    # and move the <welist> into the <source> <address>
    requestTree = ElementTree.ElementTree(ElementTree.XML(xmlRequest))
    requestE = requestTree.getroot()
    ElementTree.tostring(requestE) 
    ourServer =  {'mhsid': ServerMHS, 'host': ServerHost, 'port': ServerPort,
      'protocol': ServerProtocol, 'site': ServerSite,
      'area': {'xdim': gridDims[0], 'ydim': gridDims[1]},
      'domain': {'proj': gridProj,
      'origx': gridBoundBox[0][0], 'origy': gridBoundBox[0][1],
      'extx': gridBoundBox[1][0], 'exty': gridBoundBox[1][1]}}
    sourcesE, addressE = irt.addSourceXML(requestE, ourServer)

    #find the <welist> and move it
    welistE = requestE.find('welist')
    requestE.remove(welistE)
    addressE.append(welistE)

    # we need to decode the <destinations> to determine which sites should
    # get this request
    mhsSites = []
    for destE in requestE.getchildren():
        if destE.tag == "destinations":
            for addrE in destE:
                if addrE.tag != "address":
                    continue   #not expecting something different, so ignore
                serverInfo = irt.decodeXMLAddress(addrE)
                logEvent("Destination:",
                  irt.printServerInfo(serverInfo))
                if serverInfo['mhsid'] not in mhsSites:
                    mhsSites.append(serverInfo['mhsid'])

    # convert XML tree to a string and write out packet to send via MHS
    dir = siteConfig.GFESUITE_PRDDIR + "/ISC"
    #create the directories if they don't exist
    try:
        os.makedirs(dir)
    except:  
        pass
    tempfile.tempdir = dir
    fname = tempfile.mktemp(".iscRequest")
    fp = open(fname, "wb")
    buf = ElementTree.tostring(requestE)
    fp.write(buf)
    fp.close()

    # Transmit the request -- do string substitution
    #if XmtScript is not None:
        # create the required wfmoid
    wmoid = "TTAA00 "
    if ServerMHS in ['SJU']:
        wmoid += "TJSJ"
    elif ServerMHS in ['AFG', 'AJK', 'HFO', 'GUM']: 
        wmoid += "P" + ServerMHS
    elif ServerMHS in ['AER', 'ALU']:
        wmoid += "PAFC"
    elif len(ServerMHS) == 3:
        wmoid += "K" + ServerMHS
    elif len(ServerMHS) == 4:
        wmoid += ServerMHS
    else:
        wmoid = "XXXX"
    wmoid += " " + time.strftime("%d%H%M", time.gmtime(time.time()))

    cmd = copy.deepcopy(xmtScript)
    args = cmd.split(" ")  #break down into separate entries
    for s1, s2 in [("%SUBJECT", "ISCREQUEST"),
      ("%ADDRESSES", ",".join(mhsSites)), ("%WMOID", wmoid),
      ("%ATTACHMENTS", fname)]:
        for x in xrange(len(args)):
            args[x] = string.replace(args[x], s1, s2)
    logEvent("ISCRequest xml: ", args)

    # start subprocess to actually make the call
    pid = os.fork()
    if pid == 0:
        try:
            os.execvp(args[0], args)
        except:
            pass
        finally:
            os.remove(fname)
            os._exit(0)


def serviceISCRequest(xmlRequest):
    # function called by iscDataRec with an isc request to be serviced.
    # We take this information, convert it into a different format,
    # and queue the request via the IFPServer to the SendISCMgr
    import IrtAccess
    import siteConfig
    import xml
    from xml.etree import ElementTree
    from xml.etree.ElementTree import Element, SubElement
    irt = IrtAccess.IrtAccess(None)

    logEvent("serviceISCRequest.....")

    # validate xml
    inTree = ElementTree.ElementTree(ElementTree.XML(xmlRequest))
    inE = inTree.getroot()
    if inE.tag != "iscrequest":
        raise Exception, "iscrequest packet missing from request"

    # prepare output XML file - emulating the IRT getSendAddr() format
    iscE = Element('destinations') 

    # process the input request file.  Look for source, destinations.
    sourceE = None
    for sdwE in inE:
        if sdwE.tag == "source":
            for addrE in sdwE:
                if addrE.tag == "address":   #should only be 1
                    info = irt.decodeXMLAddress(addrE)  #requestor information
                    logEvent("ISC requestor:",
                      irt.printServerInfo(info))
                    irt.addAddressXML(iscE, info)  #into the destinations
        elif sdwE.tag == "destinations":
            continue   #we don't care about these destinations since they are
                       #where the request went (which is our own server)

    # pass request into C++ IFPServer
    xmlDestinations = ElementTree.tostring(iscE)  #convert to string 
    from com.raytheon.edex.plugin.gfe.isc import ServiceISCRequest
    ServiceISCRequest.serviceRequest(JUtil.pyValToJavaObj(info['parms']),xmlDestinations,siteConfig.GFESUITE_SITEID)
   # ifpServer.serviceISCRequest(info['parms'], xmlDestinations)
   
# get servers direct call for IRT
def irtGetServers(ancfURL, bncfURL, iscWfosWanted):
    import IrtAccess
    irt = IrtAccess.IrtAccess(ancfURL, bncfURL)
    xml = None
    status, xml = irt.getServers(iscWfosWanted)
    return xml

