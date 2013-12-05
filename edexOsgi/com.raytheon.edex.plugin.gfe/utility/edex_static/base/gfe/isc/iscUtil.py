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

import string, IrtAccess, JUtil, logging
import xml, pickle, tempfile, os
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
import LogStream
from datetime import datetime
from time import gmtime, strftime
from java.io import File
from com.raytheon.uf.common.time import TimeRange
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridLocation
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
from com.raytheon.edex.plugin.gfe.config import IFPServerConfig
from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
from com.raytheon.uf.common.localization import LocalizationFile
from com.raytheon.uf.common.localization import PathManagerFactory
from com.raytheon.uf.common.localization import LocalizationContext
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel

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
#
#
#

def getEditArea(name, siteID):

    pathMgr = PathManagerFactory.getPathManager();

    commonStaticConfig = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.SITE)
    commonStaticConfig.setContextName(siteID)
    file = pathMgr.getFile(commonStaticConfig, "gfe/editAreas" + File.separator + name + ".xml")

    if not os.path.exists(file.getPath()):
        commonStaticConfig = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED)
        commonStaticConfig.setContextName(siteID)
        file = pathMgr.getFile(commonStaticConfig, "gfe/editAreas" + File.separator + name + ".xml")

    refData = None

    try:
        if os.path.exists(file.getPath()):
            refData = ReferenceData.getJAXBManager().unmarshalFromXmlFile(file.getPath());
        else:
            LogStream.logProblem("EDIT AREA NOT FOUND: ", name, " for site ", siteID)
    except:
        LogStream.logProblem("Unable to unmarshal " + name + " in iscExtract")

    return refData

def saveEditAreaGrid(maskName, iscMask, siteID):
    iscMask.getPolygons(CoordinateType.LATLON);

    pathMgr = PathManagerFactory.getPathManager();
    commonStaticConfig = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED)
    commonStaticConfig.setContextName(siteID)
    sitePath = pathMgr.getFile(commonStaticConfig, "gfe/editAreas").getPath()
    editAreaPath = str(sitePath) + "/" + maskName + ".xml"
    ReferenceData.getJAXBManager().marshalToXmlFile(iscMask, editAreaPath)

def deleteEditArea(name, siteID):
    pathMgr = PathManagerFactory.getPathManager()
    commonStaticConfig = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED)
    commonStaticConfig.setContextName(siteID)
    file = pathMgr.getFile(commonStaticConfig, "gfe/editAreas" + File.separator + name + ".xml")
    file.delete()

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
    #returns text based on the server dictionary that should be placed
    #into the dialog
    hostport = None
    if server['host'][0:3] in ['dx4', 'px3'] and server['port'] in \
      ['98000000', '98000001']:
        if server['port'] == "98000000":
            hostport = server['host'] + "-primary"
        elif server['port'] == "98000001":
            hostport = server['host'] + "-svcbu"

    if hostport is None:
        hostport = server['host'] + "/" + server['port']

    return server['site'] + "->  " + hostport + "@" + \
      server['mhsid'].lower()

def sortServers(a, b):
# sort function for the list of servers.  Sorts in priority order for
# most likely to have the data.  Order is:
# dx4 or px3 98000000 site==mhsid
# dx4 or px3 98000001 site==mhsid
# dx4 or px3 98000000 site!=mhsid
# dx4 or px3 98000001 site!=mhsid
# all others in random order.
    sameSiteA = (a['mhsid'] == a['site'])
    sameSiteB = (b['mhsid'] == b['site'])
    if sameSiteA and not sameSiteB:
        return -1
    elif not sameSiteA and sameSiteB:
        return 1
    #both are same sites, check for host next
    else:
        regHostA = (a['host'][0:3] in ['dx4', 'px3'])
        regHostB = (b['host'][0:3] in ['dx4', 'px3'])
        if regHostA and not regHostB:
            return -1
        elif not regHostA and regHostB:
            return 1
        # same host, but not preferred host
        else:
            regPortA = (a['port'] == "98000000")
            regPortB = (b['port'] == "98000000")
            if regPortA and not regPortB:
                return -1
            elif not regPortA and regPortB:
                return 1
            return 1   #must be non-standard, put at end of list

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
                    list.sort(sortServers)
                    domains[site] = list

            elif domainE.tag == "welist":
                for parmE in domainE.getchildren():
                    welist.append(parmE.text)
                welist.sort()

        retVal = {}
        retVal['serverDictS2T'] = serverDictS2T
        retVal['serverDictT2S'] = serverDictT2S
        retVal['domains'] = domains

        tempfile.tempdir = "/tmp/"
        fname = tempfile.mktemp(".bin")
        FILE = open(fname, "w")
        pickle.dump(retVal, FILE)
        FILE.close()

        FILE = open(fname, "r")
        lines = FILE.readlines()
        FILE.close()
        os.remove(fname)

        pickledFile = ""
        for line in lines:
            pickledFile += line

        return pickledFile

def unPickle(str):
    import pickle, tempfile, os, JUtil
    tempfile.tempdir = "/tmp/"
    fname = tempfile.mktemp(".bin")
    FILE = open(fname, "w")
    FILE.write(str)
    FILE.close()

    FILE = open(fname, "r")
    retVal = pickle.load(FILE)
    FILE.close()
    return retVal


def getRequestXML(xml, selectedServers, selectedWEList):
    irt = IrtAccess.IrtAccess("")
    selectedServers = JUtil.javaStringListToPylist(selectedServers)
    selectedWElist = JUtil.javaStringListToPylist(selectedWEList)

    response = unPickle(createDomainDict(xml))
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

    if logName is None:
        logPath = siteConfig.GFESUITE_LOGDIR + "/" + strftime("%Y%m%d", gmtime())
        logName = scriptName + ".log"
    else:
        logPath = os.path.dirname(logName)
        if len(logPath) == 0:
            logPath = siteConfig.GFESUITE_LOGDIR + "/" + strftime("%Y%m%d", gmtime())
        logName = os.path.basename(logName)

    logFile = logPath + "/" + logName

    if not os.path.exists(logPath):
        os.makedirs(logPath)

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
