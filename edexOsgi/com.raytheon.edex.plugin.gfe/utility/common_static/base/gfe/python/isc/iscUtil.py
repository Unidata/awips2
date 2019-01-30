#
# Utility module of isc functions
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    02/19/13        1637          randerso       Removed unused import
#    03/11/13        1759          dgilling       Move siteConfig import into
#                                                 methods where it's needed.
#    11/07/13        2517          randerso       Allow getLogger to override logLevel
#    01/22/14/       2504          randerso       Added hostname to log path
#    04/10/2014      17241         dgilling       (code checked in by zhao)
#    04/25/2015      4952          njensen        Updated for new JEP API
#    08/14/15        4750          dgilling       Stop pickling ISC domainDicts.
#    09/12/2016      5861          randerso       Remove references to IFPServerConfigManager
#                                                 which was largely redundant with IFPServer.
#    06/06/2017      19967         bwhunder       Correct deleteEditArea() to delete file via Localization
#    07/31/2017      6342          randerso       Use ReferenceMgr to load/save/delete edit areas
#
#

import string, IrtAccess, JUtil, logging
import xml, pickle, tempfile, os, socket
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
import LogStream
from time import gmtime, strftime
from com.raytheon.uf.common.time import TimeRange
from com.raytheon.edex.plugin.gfe.server import IFPServer


def getEditArea(name, siteID):
    ifpServer = IFPServer.getActiveServer(siteID)
    if ifpServer is None:
            raise Exception("No active IFPServer for site: " + siteId)
    referenceMgr = ifpServer.getReferenceMgr()
    
    refData = None
    sr = referenceMgr.getEditArea(name)

    if sr.isOkay():
        refData = sr.getPayload()
        return refData
    else:
        raise KeyError(" ".join(["EDIT AREA NOT FOUND:", name, "for site", siteID]))


def saveEditAreaGrid(name, refData, siteID):
    ifpServer = IFPServer.getActiveServer(siteID)
    if ifpServer is None:
            raise Exception("No active IFPServer for site: " + siteId)
    referenceMgr = ifpServer.getReferenceMgr()
    
    referenceMgr.saveEditArea(name, refData)

def deleteEditArea(name, siteID):
    ifpServer = IFPServer.getActiveServer(siteID)
    if ifpServer is None:
            raise Exception("No active IFPServer for site: " + siteId)
    referenceMgr = ifpServer.getReferenceMgr()
    
    referenceMgr.deleteEditArea(name)

def transformTime(tr):
    return (int(tr.getStart().getTime() / 1000), int(tr.getEnd().getTime() / 1000))

def toJavaTimeRange(tr):
    return TimeRange(long(tr[0]) * long(1000), long(tr[1]) * long(1000))

def swapCoord(coord):
    temp = coord.y
    coord.y = coord.x
    coord.x = temp
    return coord

def serverBoxText(server):
    hostport = server['host'] + "/" + server['port']
    return server['site'] + "->  " + hostport + "@" + \
      server['mhsid'].lower()

def createDomainDict(xml):
        irt = IrtAccess.IrtAccess("")
        #decodes the packet of information from the ISC_REQUEST_QUERY call
        #to the ifpServer.  This info will be used for creating the dialog.
        # Returns the domainDict, which is keyed by domain, and contains
        # a list of servers (each server in a dictionary with keys of
        # mhsid, host, port, protocol, site.
        try:
            serverTree = ElementTree.ElementTree(ElementTree.XML(xml))
            serversE = serverTree.getroot()
        except:
            LogStream.logProblem('Malformed XML in createDomainDict')
            return None
        if serversE.tag != "servers":
            LogStream.logEvent('servers tag not found in createDomainDict')
            return None   #invalid xml

        #decode XML and create dictionary and parms list
        domains = {}
        welist = []
        serverDictS2T = {}   #key=serverinfo, value=text on GUI
        serverDictT2S = {}   #key=text on GUI, value=serverinfo
        for domainE in serversE:
            if domainE.tag == "domain":
                site = None
                for name, value in domainE.items():
                    if name == "site":
                        site = value
                        break
                if site is None:
                    LogStream.logProblem('Malformed domain site XML')
                    continue
                for addressE in domainE.getchildren():
                    info = irt.decodeXMLAddress(addressE)
                    if not domains.has_key(site):
                        domains[site] = []
                    list = domains[site]
                    list.append(info)
                    guiText = serverBoxText(info)
                    serverDictT2S[guiText] = info
                    serverDictS2T[str(info)] = guiText
                    domains[site] = list

            elif domainE.tag == "welist":
                for parmE in domainE.getchildren():
                    welist.append(parmE.text)
                welist.sort()

        retVal = {}
        retVal['serverDictS2T'] = serverDictS2T
        retVal['serverDictT2S'] = serverDictT2S
        retVal['domains'] = domains
        return retVal

def getRequestXML(xml, selectedServers, selectedWEList):
    irt = IrtAccess.IrtAccess("")
    selectedServers = JUtil.javaStringListToPylist(selectedServers)
    selectedWElist = JUtil.javaStringListToPylist(selectedWEList)

    response = createDomainDict(xml)
    serverDictT2S = response['serverDictT2S']
    domainDict = response['domains']

    iscReqE = Element('iscrequest')
    servers = []
    for serverT in selectedServers:
        server = serverDictT2S[serverT]
        servers.append(server)
    irt.addDestinationXML(iscReqE, servers)
    welistE = SubElement(iscReqE, 'welist')
    for we in selectedWElist:
        weE = SubElement(welistE, 'parm')
        weE.text = we

    # output the list of servers and their priority
    s = '\n'
    for key in domainDict.keys():
        s += "DOMAIN=" + key + '\n'
        servers = selectedServers
        for serverT in servers:
            server = serverDictT2S[serverT]
            if server['site'] == key:
                s += "  mhs=" + server['mhsid'] + " host=" + \
                  server['host'] + " port=" + server['port'] + "\n"
    #LogStream.logEvent("Chosen request servers:", s)

    # send to ifpServer
    xmlreq = ElementTree.tostring(iscReqE)

    return xmlreq;

def getLogger(scriptName, logName=None, logLevel=logging.INFO):
    # be relocating this import here we allow
    # com.raytheon.edex.plugin.gfe.isc.IscScript to dynamically
    # modify its include path with the proper siteConfig just before
    # execution time
    import siteConfig
    hostname = socket.gethostname().split('.')[0]
    logPath = os.path.join(siteConfig.GFESUITE_LOGDIR, strftime("%Y%m%d", gmtime()), hostname) 
    if logName is None:
        logName = scriptName + ".log"
    else:
        logDir = os.path.dirname(logName)
        if len(logDir) > 0:
            logPath = logDir
        logName = os.path.basename(logName)

    logFile = os.path.join(logPath, logName)

    try:
        os.makedirs(logPath)
    except OSError as e:
        import errno
        if e.errno != errno.EEXIST:
            raise e

    theLog = logging.getLogger(scriptName)
    theLog.setLevel(logLevel)
    ch = logging.FileHandler(logFile)

    ch.setLevel(logLevel)
    formatter = logging.Formatter("%(levelname)s  %(asctime)s [%(process)d:%(thread)d] %(filename)s: %(message)s")
    ch.setFormatter(formatter)
    for h in theLog.handlers:
        theLog.removeHandler(h)
    theLog.addHandler(ch)
    return theLog

def tupleToString(*msg):
    concatMsg = ""
    for m in msg:
        concatMsg = concatMsg + " " + str(m)
    return concatMsg
