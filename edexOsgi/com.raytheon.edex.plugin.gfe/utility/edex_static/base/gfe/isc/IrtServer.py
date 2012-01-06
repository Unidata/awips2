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

import LogStream, siteConfig, tempfile, os, sys, JUtil, subprocess
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

