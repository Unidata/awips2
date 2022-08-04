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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Author: mathewson
# ----------------------------------------------------------------------------
##
#
# This class provides interfaces to the ISC Routing Table Web Service.
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer        Description
# ------------- -------- --------------- ---------------------------------------
# Dec 10, 2014  4953     randerso        Cleaned up imports, improved spawning
#                                        of shell cmd
# Mar 10, 2015  4129     randerso        Refactored server selection code into a
#                                        reusable method
# Jul 11, 2016  5774     randerso        Change to send WFO message to all
#                                        active IFP servers, not just "best"
# Oct 31, 2016  5979     njensen         Cast to primitives for compatibility
# May 14, 2019  21081    dfriedman       Roll over log file on a new date
# May 17, 2017  20105    mgamazaychikov  Updated GFESUITE_MHSID assignment
# Apr 29, 2020  8151     randerso        Use SiteMap.getSite4LetterId()
# Jan 05, 2022  8726     dgilling        Fix python 3 issue with __baseURL.
#
##

##
# This is a base file that is not intended to be overridden.
##

from xml.etree import ElementTree
from xml.etree.ElementTree import SubElement
import datetime
import socket
import urllib.request, urllib.parse, urllib.error
import time, os, copy
import subprocess

import JUtil
import iscUtil


class IrtAccess():

    #Constructor taking the web URL for the ISC Routing Table
    def __init__(self, ancfURL=None, bncfURL=None, logger=None):
        self.__addrs = {}
        self.__addrs['ANCF'] = ancfURL
        self.__addrs['BNCF'] = bncfURL
        self.__registered = None  #flag to indicate whether we registered
        # If a logger is supplied, set __loggerDate to the maximum value
        # so that __logger will not be replaced in getLogger.
        if logger is not None:
            self.__logger = logger
            self.__loggerDate = datetime.date.max
        else:
            self.__logger = None
            self.__loggerDate = datetime.date.min

    def logEvent(self, *msg):
        self.getLogger().info(iscUtil.tupleToString(*msg))

    def logProblem(self, *msg):
        self.getLogger().error(iscUtil.tupleToString(*msg))

    def logException(self, *msg):
        self.getLogger().exception(iscUtil.tupleToString(*msg))

    def logVerbose(self, *msg):
        self.getLogger().debug(iscUtil.tupleToString(*msg))

    def logDebug(self, *msg):
        self.logVerbose(iscUtil.tupleToString(*msg))

    # If the current date is a later date than __loggerDate, create a new
    # logger to roll over to a new log file.
    def getLogger(self):
        today = datetime.date.today()
        if today > self.__loggerDate:
            self.__logger = iscUtil.getLogger("irtAccess", "irtServer.log")
            self.__loggerDate = today
        return self.__logger

    def __checkArgs(self, parmsWanted, gridDims, gridBoundBox, iscWfosWanted):

        if type(parmsWanted) is not list:
            parmsWanted = JUtil.javaStringListToPylist(parmsWanted)

        if type(gridDims) is not list:
            pylist = []
            size = gridDims.size()
            for i in range(size):
                pylist.append(int(gridDims.get(i)))
            gridDims = pylist

        if type(gridBoundBox) is not tuple:
            gridBoundBox = ((float(gridBoundBox.get(0)), float(gridBoundBox.get(1))), (float(gridBoundBox.get(2)), float(gridBoundBox.get(3))))

        if type(iscWfosWanted) is not list:
            iscWfosWanted = JUtil.javaStringListToPylist(iscWfosWanted)

        return parmsWanted, gridDims, gridBoundBox, iscWfosWanted

    # Registration call for the ISC Routing Table.  Returns True if okay, i.e.,
    # you are registered.
    def register(self, mhsid, serverHost, serverPort, serverProtocol,
      site, parmsWanted, gridDims, gridProj, gridBoundBox, iscWfosWanted):

        parmsWanted, gridDims, gridBoundBox, iscWfosWanted = self.__checkArgs(parmsWanted, gridDims, gridBoundBox, iscWfosWanted)

        if self.__registered is not None:
            self.unregister(self.__registered)  #unregister, then reset table
            self.__regInfo = None
            self.__timeToReRegister = None

        # shorten parmsWanted list, i.e., don't need the "_SFC".
        for x in range(len(parmsWanted)):
            idx = parmsWanted[x].find("_SFC")
            if idx != -1:
                parmsWanted[x] = parmsWanted[x][0:idx]

        # set up registration information
        self.__regInfo = {'mhsid': mhsid, 'server': serverHost,
          'port': serverPort, 'protocol': serverProtocol, 'site': site,
          'parms': ",".join(parmsWanted), 'dims': self.__listConvert(gridDims),
          'proj': gridProj, 'Bbox': self.__nestedTupleConvert(gridBoundBox),
          'wfos': ",".join(iscWfosWanted)}

        # set up unregistration information
        self.__unregInfo = {'mhsid': mhsid, 'server': serverHost,
          'port': serverPort, 'protocol': serverProtocol,
          'wfos': ",".join(iscWfosWanted), 'site': site}

        okay = self.__doRegister()  #perform registration
        return okay

    # Call made by ifpServer to check and perform re-registration.
    # Returns True if no errors.
    def checkForReregister(self):
        if not self.__registered:
            return True  #do nothing if not registered
        # check for change of IRT web service
        if self.__baseURL() != self.__registered:
            self.unregister(self.__registered)
            return self.__doRegister()
        # check for time to re-register
        if time.time() >= self.__timeToReRegister:
            return self.__doRegister()
        return True

    # Call to unregister from the ISC Routing Table.  Returns True if
    # successful, false otherwise. The irtAddress overrides the calculated
    # IRT web address if present.
    def unregister(self, irtAddress=None):
        if not self.__registered:
            return True  #do nothing if never registered

        #now unregister
        status, xml, transIRT = self.__callIRT('unregister', self.__unregInfo,
          irtAddress)
        if status is False:
            return False

        # decode XML, read status
        try:
            tree = ElementTree.ElementTree(ElementTree.XML(xml))
        except:
            self.logProblem("Malformed XML on unregister: ", xml)
            return False

        element = tree.getroot()
        if element is None:
            self.logProblem("status tag missing in XML for unregister")
            return False
        status = None
        for attr, value in element.items():
            if attr == 'ok':
                status = value
                break
        if status is None:
            self.logProblem("ok attr missing in status tag for unregister")
            return False
        if status == "1":
            self.__registered = None  #reset registration flag
            self.logEvent("Unregistered from IRT")
            return True
        else:
            self.logProblem("Error on unregistration", element.text)
            return False

    # routing to get the list of destination servers that are active for
    # the given domain. Returns status flag and XML string.
    def getSendAddrs(self, sourceDomain):
        sourceDomainDict = {'wfoid': sourceDomain}
        status, xml, transIRT = self.__callIRT('getaddrs', sourceDomainDict)
        return status, xml

    # routine to get the list of servers that are active for the given list
    # of domains. Returns status flag and XML string.
    def getServers(self, wfos):
        if type(wfos) is not list:
            wfos = JUtil.javaStringListToPylist(wfos)
        wfoDict = {'wfoids': ",".join(wfos)}
        status, xml, transIRT = self.__callIRT('getservers', wfoDict)
        return status, xml

    # registers with the ISC routing table.  Returns True if successful,
    # False otherwise.
    def __doRegister(self):
        status, xml, transIRT = self.__callIRT('register', self.__regInfo)
        if status is False:
            return False

        # decode XML
        try:
            tree = ElementTree.ElementTree(ElementTree.XML(xml))
        except:
            self.logProblem("Malformed XML on register: ", xml)
            return False

        element = tree.getroot()  #status tag is the root tag
        if element is None:
            self.logProblem("status tag missing in XML for register")
            return False
        ok = None
        for attr, value in element.items():
            if attr == 'ok':
                ok = value
                break
        if ok is None:
            self.logProblem("ok field missing in status tag for register")
            return False

        # get the ok flag
        if ok == "1":
            isoTimeStr = element.text
            idx = isoTimeStr.find(".")
            if idx != -1:
                isoTimeStr = isoTimeStr[0:idx]  #eliminate sub-seconds if any
            try:
                # switch to GMT0 for time conversions
                prevTZ = os.environ.get('TZ', None)
                os.environ['TZ'] = "GMT0"
                time.tzset()

                # Fix to correct importing of the time.strptime method
                importError = True
                while importError:
                    try:
                        self.__timeToReRegister = time.mktime(time.strptime(isoTimeStr, "%Y-%m-%dT%H:%M:%S"))
                        importError = False
                    except ImportError:
                        importError = True

                # reset TZ environment variable to previous state
                if prevTZ:
                    os.environ['TZ'] = prevTZ
                    time.tzset()
            except ValueError:
                self.logProblem("time string has bad format", isoTimeStr)
                return False
            self.__registered = transIRT  #set registration flag
            self.logEvent("IRT Registration Successful. ",
              "Re-register time: ",
              time.asctime(time.gmtime(float(self.__timeToReRegister))))
            return True  #okay registration
        else:
            self.logProblem("Error on registration: ", element.text)
            return False

    # returns the appropriate (id, url) for the IRT web service.
    def __baseURL(self):
        statusFile = '/data/mhs/ncfstatus'
        #statusFile = '/scratch/ncfstatus'
        ncf = "ANCF"
        try:
            with open(statusFile) as fp:
                ncf = fp.read()
                ncf = ncf.strip().upper()
        except IOError:
            pass
            #self.logProblem("Can't read NCF status file: ", statusFile,
            #  "assuming ANCF...")
        return ncf, self.__addrs.get(ncf)

    # makes call to ISC routing Table service, calling the given function,
    # with the given attributes (dictionary). Returns the status of the
    # call (bool), the XML returned, and the IRT address (id, url).
    # The optional IRTAddress is used to force the call to a specific
    # IRT (id, url).
    def __callIRT(self, function, attributes, irtAddress=None):
        retries = 0
        tDuration = 0.000
        startT = time.time()
        #use normal method to calculate IRT address
        if irtAddress is None:
            irtid, url = self.__baseURL()
            acturl = url + "/" + function
        else:
            irtid, url = irtAddress
            acturl = url + "/" + function

        data = urllib.parse.urlencode(attributes).encode("utf-8")
        while True:
            try:
                prevtimeout = socket.setdefaulttimeout(60.0)
                #check for update of ANCF/BNCF
                if irtAddress is None:
                    irtid, url = self.__baseURL()
                    acturl = url + "/" + function
                with urllib.request.urlopen(acturl, data) as response:
                    xml = response.read().decode()
                socket.setdefaulttimeout(prevtimeout)
                break
            except urllib.error.URLError as e:
                problem = "URLError"
                problem1 = e
            except urllib.error.HTTPError as e:
                problem = "HTTPError"
                problem1 = e
            except IOError as e:
                problem = "IOError"
                problem1 = e
            except Exception as e:
                problem = "Exception"
                problem1 = e

            #failed transaction
            endT = time.time()
            tDuration = endT - startT
            self.logProblem("IRT access: ", problem, function,
              "t=%-7.3f" % tDuration, "retries=%-1d" % retries,
              "IRT=[%s %s]" % (irtid, url), attributes, problem1)

            #try again?
            retries += 1
            if retries > 5:
                socket.setdefaulttimeout(prevtimeout)
                return False, "", (irtid, url)  #complete failure

        #successful transaction
        endT = time.time()
        tDuration = endT - startT
        self.logEvent("IRT access: okay", function,
          "t=%-7.3f" % tDuration, "retries=%-1d" % retries,
          "IRT=[%s %s]" % (irtid, url), attributes)
        self.logDebug("XML: ", xml)
        return True, xml, (irtid, url)

    # list convert to comma-deliminated string
    def __listConvert(self, a):
        s = ""
        for x in range(len(a)):
            if len(s):
                s += "," + repr(a[x])
            else:
                s += repr(a[x])
        return s

    # domain (x,y),(xe,ye) convert to comma-deliminated string
    def __nestedTupleConvert(self, a):
        s = ''
        for x in range(len(a)):
            for y in range(len(a[x])):
                if len(s):
                    s += "," + repr(a[x][y])
                else:
                    s += repr(a[x][y])
        return s

    #----------------------------------------------------------------------
    # Utility Routines ----------------------------------------------------
    #----------------------------------------------------------------------
    def addAddressXML(self, root, serverInfo):
        #adds the address XML with the source server information
        #to the root XML tree. Input server information is a dict with
        #following keys: "mhsid",'host','port','protocol','site'
        # Returns element for address (in case additional info is required)
        addressE = SubElement(root, 'address')
        mhsidE = SubElement(addressE, 'mhsid')
        mhsidE.text = serverInfo.get('mhsid', "?")
        serverE = SubElement(addressE, 'server')
        serverE.text = serverInfo.get('host', "?")
        portE = SubElement(addressE, 'port')
        portE.text = str(serverInfo.get('port', "?"))
        protocolE = SubElement(addressE, 'protocol')
        protocolE.text = str(serverInfo.get('protocol', "?"))
        siteE = SubElement(addressE, 'site')
        siteE.text = serverInfo.get('site', "?")

        #optional components "location" "area" "welist"
        if 'domain' in serverInfo and serverInfo['domain'] is not None:
            d = serverInfo['domain']
            SubElement(addressE, 'location', proj=d['proj'],
              origx=str(d['origx']), origy=str(d['origy']),
              extx=str(d['extx']), exty=str(d['exty']))
        if 'area' in serverInfo and serverInfo['area'] is not None:
            d = serverInfo['area']
            SubElement(addressE, 'area', xdim=str(d['xdim']),
              ydim=str(d['ydim']))
        if 'parms' in serverInfo and serverInfo['parms'] is not None:
            parms = serverInfo['parms']
            self.addWelistXML(addressE, parms)

        return addressE

    def addSourceXML(self, root, serverInfo):
        #adds the source XML with the source server information to the root
        #XML tree. Input server information is a dict with
        #following keys: "mhsid",'host','port','protocol','site'
        #Returns the "source" element and the "address" element.
        sourcesE = SubElement(root, 'source')
        addressE = self.addAddressXML(sourcesE, serverInfo)
        return sourcesE, addressE

    def addDestinationXML(self, root, serverInfos):
        #adds the destinationXML and server information to the XML root.
        # Input server information is a list of dicts with
        #following keys: "mhsid",'host','port','protocol','site'
        # Returns the destinations elment.
        destinationsE = SubElement(root, 'destinations')
        for serverInfo in serverInfos:
            self.addAddressXML(destinationsE, serverInfo)
        return destinationsE

    def addWelistXML(self, root, parms):
        #adds the welist and parms to the XML root. Returns the welist
        #element.
        welistE = SubElement(root, 'welist')
        for parm in parms:
            parmE = SubElement(welistE, 'parm')
            parmE.text = parm
        return welistE

    def decodeXMLAddress(self, element):
        #decodes the address element which identifies the server
        #Returns None if not address tag, returns None as part of the
        #tuple return if that value is not defined. Otherwise returns
        #a dict with keys "mhsid",'host','port','protocol','site','parms',
        #'domain'.
        server = {}
        if element.tag != "address":
            return None  #not address tag
        parms = None
        for attrE in element:
            if attrE.tag == "mhsid":
                server['mhsid'] = attrE.text
            elif attrE.tag == "server":
                server['host'] = attrE.text
            elif attrE.tag == "port":
                server['port'] = attrE.text
            elif attrE.tag == "protocol":
                server['protocol'] = attrE.text
            elif attrE.tag == "site":
                server['site'] = attrE.text
            elif attrE.tag == "welist":
                parmsE = attrE.getchildren()
                for parmE in parmsE:
                   if parms is None:
                       parms = []
                   if parmE.tag not in parms:
                       parms.append(parmE.text)
            elif attrE.tag == "location":
                domain = {}
                for key, value in attrE.items():
                    domain[key] = value
                server['domain'] = domain
            elif attrE.tag == "area":
                size = {}
                for key, value in attrE.items():
                    size[key] = value
                server['area'] = size
        server['parms'] = parms
        return server

    def transmitFiles(self, subject, addresses, sourceWfo, attachments,
      xmtScript):
        mhsid = os.environ['SITE_IDENTIFIER'].upper()
        # assembles the command and executes it.
        # determine MHS WMO id for this message
        from com.raytheon.uf.common.site import SiteMap
        if len(sourceWfo) == 4:
            site4 = sourceWfo
        elif len(sourceWfo) == 3:
            site4 = SiteMap.getInstance().getSite4LetterId(sourceWfo)
        else:
            site4 = "XXXX"

        wmoid = "TTAA00 " + site4 + " " + time.strftime("%d%H%M", time.gmtime(time.time()))

    # Transmit files - do string substitution
        if xmtScript is not None:
            cmd = copy.deepcopy(xmtScript)
            cmd = cmd.replace("%SUBJECT", subject)
            cmd = cmd.replace("%ADDRESSES", ",".join(addresses))
            cmd = cmd.replace("%WMOID", "'" + wmoid + "'")
            cmd = cmd.replace("%ATTACHMENTS", ",".join(attachments))

            self.logEvent("Transmit: ", cmd)
            import siteConfig
            processInfo = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, encoding='utf-8') 
            output = processInfo.stdout
            err = processInfo.stderr
            if mhsid + "-" not in output:
                alertMsg = "ISC Send failed transmission to : " + ",".join(addresses) + " --> " + output + " " + err
                self.logProblem(alertMsg)
            else:
                self.logEvent(output.rstrip())
                if len(err) > 0:
                    self.logProblem(err.rstrip())
                alertMsg = "ISC data successfully transmitted to: " + ",".join(addresses)
                self.logEvent(alertMsg)

        for aFile in attachments: 
            try:
                os.remove(aFile)
            except OSError:
                self.logException("Error removing file: " + aFile)

    def printServerInfo(self, serverInfo):
        # assembles a string to print out the server information.  serverInfo
        # is a dict with keys "mhsid",'host','port','protocol','site'.
        # Returns the string to print. All input values are strings.
        mhsid = serverInfo.get('mhsid', '?')
        host = serverInfo.get('host', '?')
        port = serverInfo.get('port', '?')
        protocol = serverInfo.get('protocol', '?')
        site = serverInfo.get('site', '?')
        s = "mhs=" + mhsid + ",host=" + host + ",port=" + port + \
          ",proto=" + protocol + ",site=" + site
        return s

    def createDestinationXML(self, destSites, requestingServer, findBestMatch=True):
        #--------------------------------------------------------------------
        # Assemble XML source/destination document
        #--------------------------------------------------------------------
        iscE = ElementTree.Element('isc')
        self.addSourceXML(iscE, requestingServer)
        self.logEvent("Requesting Server:", self.printServerInfo(requestingServer))

        # who is running the domains requested?
        status, xml = self.getServers(destSites)
        if not status:
            raise Exception('Failure to getServers from IRT')

        # decode the XML
        try:
            serverTree = ElementTree.ElementTree(ElementTree.XML(xml))
            serversE = serverTree.getroot()
        except:
            self.logException("Malformed XML from getServers()")
            raise

        if serversE.tag != "servers":
            raise Exception("Servers packet missing from web server")

        # process each requested domain returned to us
        msgSendDest = []
        chosenServers = []
        matchingServers = []
        for domainE in serversE:
            if domainE.tag != "domain":
                continue

            domain = domainE.get('site')
            servers = []  #list of servers for this domain

            # decode each server in the domain
            for addressE in domainE.getchildren():
                info = self.decodeXMLAddress(addressE)
                if info is None:
                    continue  #not address tag
                servers.append(info)
                matchingServers.append(info)

            # server search list in priority.  The px3/pv3 entries are used for
            # dual domain for AFC.
            hp = [('dx4', '98000000'), ('dv4', '98000000'),
                  ('px3', '98000000'), ('pv3', '98000000'),
                  ('dx4', '98000001'), ('dv4', '98000001'),
                  ('px3', '98000001'), ('pv3', '98000001')
                  ]

            if findBestMatch:
                # choose one server from this domain, find first dv4, 98000000
                # try to use one with the same mhsidDest as the site, which
                # would be the primary operational GFE. Note that the pv3 entries
                # are for AFC.
                found = False
                for matchServer, matchPort in hp:
                    if found:
                        break
                    for server in servers:
                        if server['host'][0:3] == matchServer and \
                          server['port'] == matchPort and server['mhsid'] == domain:
                            chosenServers.append(server)
                            if server['mhsid'] not in msgSendDest:
                                msgSendDest.append(server['mhsid'])
                            found = True
                            break

                # find first dv4, 98000000, but perhaps a different mhsid
                # this is probably not the primary operational GFE
                for matchServer, matchPort in hp:
                    if found:
                        break
                    for server in servers:
                        if server['host'][0:3] == matchServer and \
                          server['port'] == matchPort:
                            chosenServers.append(server)
                            if server['mhsid'] not in msgSendDest:
                                msgSendDest.append(server['mhsid'])
                            found = True
                            break

                # if didn't find standard one, then take the first one, but don't
                # take ourselves unless we are the only one.
                if not found and servers:
                    for server in servers:
                        if server['mhsid'] != requestingServer['mhsid'] \
                          and server['host'] != requestingServer['host'] \
                          and server['port'] != requestingServer['port'] \
                          and server['site'] != requestingServer['site']:
                            chosenServers.append(server)
                            if server['mhsid'] not in msgSendDest:
                                msgSendDest.append(server['mhsid'])
                            found = True
                            break

                    if not found:
                        chosenServers.append(servers[0])
                        if servers[0]['mhsid'] not in msgSendDest:
                            msgSendDest.append(servers[0]['mhsid'])
            else:
                for server in matchingServers:
                    chosenServers.append(server)
                    if server['mhsid'] not in msgSendDest:
                        msgSendDest.append(server['mhsid'])

        # Display the set of matching servers
        s = "Matching Servers:"
        for x in matchingServers:
            s += "\n" + self.printServerInfo(x)
        self.logEvent(s)

        # Display the chosen set of servers
        s = "Chosen Servers:"
        for x in chosenServers:
            s += "\n" + self.printServerInfo(x)
        self.logEvent(s)

        self.addDestinationXML(iscE, chosenServers)

        return msgSendDest, iscE
