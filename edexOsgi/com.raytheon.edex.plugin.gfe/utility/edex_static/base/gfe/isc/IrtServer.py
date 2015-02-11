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

import LogStream, tempfile, os, sys, JUtil, subprocess, traceback, errno
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
#    10/16/13        2475          dgilling       Remove unneeded code to handle
#                                                 registration with IRT.
#    12/08/2014      4953          randerso       Added support for sending/receiving TCV files
#                                                 Additional code clean up
#
##
PURGE_AGE = 30 * 24 * 60 * 60  # 30 days in seconds

def getLogger():
    import logging
    return iscUtil.getLogger("irtServer", logLevel=logging.DEBUG)
    
def logEvent(*msg):
    getLogger().info(iscUtil.tupleToString(*msg))

def logProblem(*msg):
    getLogger().error(iscUtil.tupleToString(*msg))
    
def logException(*msg):
    getLogger().exception(iscUtil.tupleToString(*msg))    

def logDebug(*msg):
    getLogger().debug(iscUtil.tupleToString(*msg))
    
# called by iscDataRec when another site has requested the active table
# returns the active table, filtered, pickled.
def getVTECActiveTable(dataFile, xmlPacket):
    import siteConfig
    import VTECPartners
    
    if not VTECPartners.VTEC_RESPOND_TO_TABLE_REQUESTS:
        return   #respond is disabled

    #decode the data (pickled)
    with open(dataFile, "rb") as fp:
        info = cPickle.load(fp)
        
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
def putVTECActiveTable(dataFile, xmlPacket):
    import siteConfig

    with open(dataFile, "rb") as fp:
        strTable = fp.read()
    
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

def putTCVFiles(siteID, tarFile):
    import LocalizationSupport
    import glob
    import TCVUtil
    
    logEvent("Receiving TCV files from " + siteID)
    
    siteDir = LocalizationSupport.getLocalizationFile(LocalizationSupport.CAVE_STATIC, 
                                                     LocalizationSupport.SITE, 
                                                     siteID, "gfe").getFile()
    siteDir = siteDir.getParentFile().getParentFile().getAbsolutePath()
    logDebug("siteDir: "+siteDir)

    try:
        tmpDir = tempfile.mkdtemp(dir="/tmp")
        logDebug("tmpDir: "+tmpDir)
        subprocess.check_call(["cd " + tmpDir + "; tar xvzf " + tarFile], shell=True)
    except:
        logException('Error untarring TCV files from site: ' + siteID)
        raise
        
    TCVUtil.purgeAllCanFiles(getLogger())
    
    # create the new allCAN files
    for tmpFile in glob.iglob(os.path.join(tmpDir, "*/gfe/tcvAdvisories/*.allCAN")):
        # create tcvDir if necessary
        tcvDir = os.path.dirname(tmpFile).replace(tmpDir, siteDir)
        logDebug("tcvDir: "+tcvDir)
        try:
            os.makedirs(tcvDir, 0755)
        except OSError, e:
            if e.errno != errno.EEXIST:
                logProblem("%s: '%s'" % (e.strerror,e.filename))
        
        basename = os.path.basename(tmpFile)
        stormName = basename.replace(".allCAN", "")
        allCanPath = os.path.join(tcvDir, basename)
        logDebug("copying "+tmpFile+" to "+allCanPath)
        try:
            # just create the empty allCan file
            with open(allCanPath, 'w'):
                pass
        except:
            logException("Error creating: "+ allCanPath)

        try:            
            # delete all JSON files starting with stormName
            for fn in glob.iglob(os.path.join(tcvDir, stormName + "*.json")):
                try:
                    site = fn.replace(siteDir,"").split("/")[1]
                    basename = os.path.basename(fn)
                    logDebug("removing canceled file: ", os.path.join(site, "gfe/tcvAdvisories", basename))
                    LocalizationSupport.deleteFile(LocalizationSupport.CAVE_STATIC, 
                                                   LocalizationSupport.SITE, site, 
                                                   "gfe/tcvAdvisories/" + basename)
                except:
                    logException("Error removing " + fn)
 
             
            os.remove(tmpFile)
        except:
            logException("Error removing JSON files for " + stormName)
    
    # copy in the json files
    for tmpFile in glob.iglob(os.path.join(tmpDir, "*/gfe/tcvAdvisories/*.json")):
        site = tmpFile.replace(tmpDir,"").split("/")[1]
        jsonFile = "gfe/tcvAdvisories/" + os.path.basename(tmpFile)
        logDebug("copying "+tmpFile+" to "+jsonFile)
        try:
            with open(tmpFile, 'r') as tf:
                jsonData = tf.read()
            LocalizationSupport.writeFile(LocalizationSupport.CAVE_STATIC, 
                                          LocalizationSupport.SITE, 
                                          site, jsonFile, jsonData)
            os.remove(tmpFile)
        except:
            logException("Error copying JSON file: "+jsonFile)
        
    # delete tmpDir
    try:
        for dirpath, dirs, files in os.walk(tmpDir, topdown=False):
            os.rmdir(dirpath)
    except:
        logException("Unable to remove "+ tmpDir)
    
    
def getTCVFiles(ourMhsID, srcServer, destE):
    import IrtAccess
    import TCVUtil
    import siteConfig
    
    irt = IrtAccess.IrtAccess("")
    localSites = [srcServer['site']]
    for addressE in destE:
        if addressE.tag != "address":
            continue
    
        destServer = irt.decodeXMLAddress(addressE)
        if destServer['mhsid'] == ourMhsID:
            localSites.append(destServer['site'])
    
    
    logEvent("Sending TCV files for " + str(localSites) + " to " + srcServer['mhsid'])

    tcvProductsDir = os.path.join(siteConfig.GFESUITE_HOME, "products", "TCV")
    
    # create tcvProductsDir if necessary
    try:
        os.makedirs(tcvProductsDir, 0755)
    except OSError, e:
        if e.errno != errno.EEXIST:
            logger.warn("%s: '%s'" % (e.strerror,e.filename))

    # get temporary file name for packaged TCV files
    with tempfile.NamedTemporaryFile(suffix='.sendtcv', dir=tcvProductsDir, delete=False) as fp:
        fname = fp.name
        
    try:    
        TCVUtil.packageTCVFiles(localSites, fname, getLogger())
        
        from xml.etree import ElementTree
        from xml.etree.ElementTree import Element, SubElement
        iscE = ElementTree.Element('isc')
        irt.addSourceXML(iscE, destServer)
        irt.addDestinationXML(iscE, [srcServer])

        # create the XML file
        with tempfile.NamedTemporaryFile(suffix='.xml', dir=tcvProductsDir, delete=False) as fd:
            fnameXML = fd.name
            fd.write(ElementTree.tostring(iscE))    

        # send the files to srcServer
        sendMHSMessage("PUT_TCV_FILES", srcServer['mhsid'], [fname, fnameXML])
    except:
        logException('Error sending TCV files for ' + str(localSites))

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
    requestE = ElementTree.fromstring(xmlRequest)
    ourServer =  {'mhsid':    ServerMHS, 
                  'host':     ServerHost, 
                  'port':     ServerPort,
                  'protocol': ServerProtocol, 
                  'site':     ServerSite,
                  'area': {'xdim': gridDims[0], 
                           'ydim': gridDims[1]
                           },
                  'domain': {'proj':  gridProj,
                             'origx': gridBoundBox[0][0], 
                             'origy': gridBoundBox[0][1],
                             'extx':  gridBoundBox[1][0], 
                             'exty':  gridBoundBox[1][1]
                             }
                  }
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


def serviceISCRequest(dataFile):
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
    inTree = ElementTree.parse(dataFile)
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

def sendMHSMessage(subject, adressees, attachments, xmtScript=None):
    # Transmit the request -- do string substitution
    import siteConfig
    from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
    config = IFPServerConfigManager.getServerConfig(siteConfig.GFESUITE_SITEID)
    ourMHS = siteConfig.GFESUITE_MHSID

    if xmtScript is None:
        xmtScript = config.transmitScript()
        
    # create the required wmoid
    wmoid = "TTAA00 "
    if ourMHS in ['SJU']:
        wmoid += "TJSJ"
    elif ourMHS in ['AFG', 'AJK', 'HFO', 'GUM']: 
        wmoid += "P" + ourMHS
    elif ourMHS in ['AER', 'ALU']:
        wmoid += "PAFC"
    elif len(ourMHS) == 3:
        wmoid += "K" + ourMHS
    elif len(ourMHS) == 4:
        wmoid += ourMHS
    else:
        wmoid = "XXXX"
    wmoid += " " + time.strftime("%d%H%M", time.gmtime(time.time()))

    if type(adressees) in [list, tuple]:
        adressees = ",".join(addresses)
        
    if type(attachments) in [list, tuple]:
        attachments  = ",".join(attachments)

    cmd = copy.deepcopy(xmtScript)
    for s1, s2 in [("%SUBJECT", subject),
                   ("%ADDRESSES", adressees), 
                   ("%WMOID", wmoid),
                   ("%ATTACHMENTS", attachments)]:
        cmd = cmd.replace(s1, s2)

    logDebug("cmd: "+ cmd)

    # start subprocess to actually make the call
    try:
        subprocess.check_call([cmd], shell=True)
    except:
        logException("Error running cmd: " + cmd)