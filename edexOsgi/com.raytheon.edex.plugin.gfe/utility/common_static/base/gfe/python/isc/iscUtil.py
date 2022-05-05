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
#    08/18/2020      22148         ryu            Release file handles to log files.
#

##
# This is a base file that is not intended to be overridden.
##


import functools
import logging
import os
import socket
from time import gmtime, strftime
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement

import IrtAccess
import JUtil
import LogStream

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
    return (tr.getStart().getTime() // 1000, tr.getEnd().getTime() // 1000)

def toJavaTimeRange(tr):
    return TimeRange(int(tr[0]) * 1000, int(tr[1]) * 1000)

def swapCoord(coord):
    temp = coord.y
    coord.y = coord.x
    coord.x = temp
    return coord

def serverBoxText(server):
    #returns text based on the server dictionary that should be placed
    #into the dialog
    hostport = None
    if server['host'][0:3] in ['dv4', 'dx4', 'pv3', 'px3'] and server['port'] in \
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
# dv4 or pv3 98000000 site==mhsid
# dv4 or pv3 98000001 site==mhsid
# dv4 or pv3 98000000 site!=mhsid
# dv4 or pv3 98000001 site!=mhsid
# all others in random order.
    sameSiteA = (a['mhsid'] == a['site'])
    sameSiteB = (b['mhsid'] == b['site'])
    if sameSiteA and not sameSiteB:
        return -1
    elif not sameSiteA and sameSiteB:
        return 1
    #both are same sites, check for host next
    else:
        regHostA = (a['host'][0:3] in ['dv4', 'dx4', 'pv3', 'px3'])
        regHostB = (b['host'][0:3] in ['dv4', 'dx4', 'pv3', 'px3'])
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
                    if site not in domains:
                        domains[site] = []
                    list = domains[site]
                    list.append(info)
                    guiText = serverBoxText(info)
                    serverDictT2S[guiText] = info
                    serverDictS2T[str(info)] = guiText
                    list.sort(key=functools.cmp_to_key(sortServers))
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
    for key in domainDict:
        s += "DOMAIN=" + key + '\n'
        servers = selectedServers
        for serverT in servers:
            server = serverDictT2S[serverT]
            if server['site'] == key:
                s += "  mhs=" + server['mhsid'] + " host=" + \
                  server['host'] + " port=" + server['port'] + "\n"
    LogStream.logEvent("Chosen request servers:", s)

    # send to ifpServer
    xmlreq = ElementTree.tostring(iscReqE, encoding="unicode")

    return xmlreq

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
        h.close()
    theLog.addHandler(ch)
    return theLog

def tupleToString(*msg):
    concatMsg = ""
    for m in msg:
        concatMsg = concatMsg + " " + str(m)
    return concatMsg
