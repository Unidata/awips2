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

#
# This class provides interfaces to the ISC Routing Table Web Service.
#

import xml
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
import socket
import urllib, urllib2, time, os, copy, string
import LogStream,JUtil

import JUtil, iscUtil
from java.util import ArrayList

class IrtAccess():

    #Constructor taking the web URL for the ISC Routing Table
    def __init__(self, ancfURL=None, bncfURL=None):
        self.__addrs = {}
        self.__addrs['ANCF'] = ancfURL
        self.__addrs['BNCF'] = bncfURL
        self.__registered = None   #flag to indicate whether we registered
        self.__logger=iscUtil.getLogger("irtAccess","irtServer.log")

    def logEvent(self,*msg):
        self.__logger.info(iscUtil.tupleToString(*msg))
    
    def logProblem(self,*msg):
        self.__logger.error(iscUtil.tupleToString(*msg))
        
    def logException(self,*msg):
        self.__logger.exception(iscUtil.tupleToString(*msg))    
    
    def logVerbose(self,*msg):
        self.__logger.debug(iscUtil.tupleToString(*msg))
        
    def logDebug(self,*msg):
        self.logVerbose(iscUtil.tupleToString(*msg))


    def __checkArgs(self,parmsWanted, gridDims,gridBoundBox, iscWfosWanted):
        
        if type(parmsWanted) is not list:
            parmsWanted = JUtil.javaStringListToPylist(parmsWanted)

        if type(gridDims) is not list:
            pylist = []
            size = gridDims.size() 
            for i in range(size):
                pylist.append(gridDims.get(i).intValue())
            gridDims = pylist
        
        if type(gridBoundBox) is not tuple:
            gridBoundBox = ((gridBoundBox.get(0).doubleValue(),gridBoundBox.get(1).doubleValue()),(gridBoundBox.get(2).doubleValue(),gridBoundBox.get(3).doubleValue()))

        if type(iscWfosWanted) is not list:
            iscWfosWanted = JUtil.javaStringListToPylist(iscWfosWanted)
        
        
        return parmsWanted, gridDims, gridBoundBox, iscWfosWanted
        
    # Registration call for the ISC Routing Table.  Returns True if okay, i.e.,
    # you are registered.
    def register(self, mhsid, serverHost, serverPort, serverProtocol,
      site, parmsWanted, gridDims, gridProj, gridBoundBox, iscWfosWanted):
        
        parmsWanted, gridDims,gridBoundBox, iscWfosWanted = self.__checkArgs(parmsWanted, gridDims,gridBoundBox, iscWfosWanted)
        
        if self.__registered is not None:
            self.unregister(self.__registered)  #unregister, then reset table
            self.__regInfo = None
            self.__timeToReRegister = None

        # shorten parmsWanted list, i.e., don't need the "_SFC".
        for x in xrange(len(parmsWanted)):
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

        okay = self.__doRegister()   #perform registration
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
            return True   #do nothing if never registered

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
        wfoDict  = {'wfoids': ",".join(wfos)}
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

        element = tree.getroot()   #status tag is the root tag
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
                        self.__timeToReRegister = time.mktime(time.strptime(isoTimeStr,"%Y-%m-%dT%H:%M:%S"))
                        importError=False
                    except ImportError:
                        importError = True
                
                # reset TZ environment variable to previous state
                if prevTZ:
                    os.environ['TZ'] = prevTZ
                    time.tzset()
            except ValueError:
                self.logProblem("time string has bad format", isoTimeStr)
                return False
            self.__registered = transIRT   #set registration flag
            self.logEvent("IRT Registration Successful. ",
              "Re-register time: ",
              time.asctime(time.gmtime(float(self.__timeToReRegister))))
            return True   #okay registration
        else:
            self.logProblem("Error on registration: ", element.text)
            return False

    # returns the appropriate (id, url) for the IRT web service.
    def __baseURL(self):
        statusFile = '/data/mhs/ncfstatus'
        #statusFile = '/scratch/ncfstatus'
        ncf = "ANCF"
        try:
            fp = open(statusFile, 'rb')
            ncf = fp.read()
            ncf = ncf.strip().upper()
        except IOError, e:
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

        data = urllib.urlencode(attributes)
        while True:
            try:
                prevtimeout = socket.setdefaulttimeout(60.0)
                #check for update of ANCF/BNCF
                if irtAddress is None:
                    irtid, url = self.__baseURL()
                    acturl = url + "/" + function
                fd = urllib2.urlopen(acturl, data)
                xml = fd.read()
                fd.close()
                socket.setdefaulttimeout(prevtimeout)
                break
            except urllib2.URLError, e:
                problem = "URLError"
                problem1 = e
            except urllib2.HTTPError, e:
                problem = "HTTPError"
                problem1 = e
            except IOError, e:
                problem = "IOError"
                problem1 = e
            except Exception, e:
                problem = "Exception"
                problem1 = e

            #failed transaction
            endT = time.time()
            tDuration = endT - startT
            self.logProblem("IRT access: ", problem, function,
              "t=%-7.3f" % tDuration, "retries=%-1d" % retries, 
              "IRT=[%s %s]" % (irtid, url), attributes, problem1)

            #try again?
            retries = retries + 1
            if retries > 5:
                socket.setdefaulttimeout(prevtimeout)
                return False, "", (irtid, url)    #complete failure


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
        for x in xrange(len(a)):
            if len(s):
                s += "," + `a[x]`
            else:
                s += `a[x]`
        return s

    # domain (x,y),(xe,ye) convert to comma-deliminated string
    def __nestedTupleConvert(self, a):
        s = ''
        for x in xrange(len(a)):
            for y in xrange(len(a[x])):
                if len(s):
                    s += "," + `a[x][y]`
                else:
                    s += `a[x][y]`
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
        if serverInfo.has_key('domain') and serverInfo['domain'] is not None:
            d = serverInfo['domain']
            locationE = SubElement(addressE, 'location', proj=d['proj'],
              origx=str(d['origx']), origy=str(d['origy']),
              extx=str(d['extx']), exty=str(d['exty']))
        if serverInfo.has_key('area') and serverInfo['area'] is not None:
            d = serverInfo['area']
            areaE = SubElement(addressE, 'area', xdim=str(d['xdim']),
              ydim=str(d['ydim']))
        if serverInfo.has_key('parms') and serverInfo['parms'] is not None:
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
        destinationsE= SubElement(root, 'destinations')
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
        dict = {}
        if element.tag != "address":
            return None   #not address tag
        parms = None
        for attrE in element:
            if attrE.tag == "mhsid":
                dict['mhsid'] = attrE.text
            elif attrE.tag == "server":
                dict['host']  = attrE.text
            elif attrE.tag == "port":
                dict['port']  = attrE.text
            elif attrE.tag == "protocol":
                dict['protocol']  = attrE.text
            elif attrE.tag == "site":
                dict['site']  = attrE.text
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
                dict['domain'] = domain
            elif attrE.tag == "area":
                size = {}
                for key, value in attrE.items():
                    size[key] = value
                dict['area'] = size
        dict['parms'] = parms
        return dict

    def transmitFiles(self, subject, addresses, sourceWfo, attachments,
      xmtScript):
        # assembles the command and executes it.
        # determine MHS WMO id for this message
        wmoid = "TTAA00 "
        if sourceWfo in ['SJU']:
            wmoid += "TJSJ"
        elif sourceWfo in ['AFG', 'AJK', 'HFO', 'GUM']:
            wmoid += "P" + sourceWfo
        elif sourceWfo in ['AER', 'ALU']:
            wmoid += "PAFC"
        elif len(sourceWfo) == 3:
            wmoid += "K" + sourceWfo
        elif len(sourceWfo) == 4:
            wmoid += sourceWfo
        else:
            wmoid += "XXXX"
        wmoid += " " + time.strftime("%d%H%M", time.gmtime(time.time()))

    # Transmit files - do string substitution
        if xmtScript is not None: 
            cmd = copy.deepcopy(xmtScript)
            cmd = string.replace(cmd, "%SUBJECT", subject)
            cmd = string.replace(cmd, "%ADDRESSES", ",".join(addresses))
            cmd = string.replace(cmd, "%WMOID", "'" + wmoid + "'")
            cmd = string.replace(cmd, "%ATTACHMENTS", ",".join(attachments))

            self.logEvent("Transmit: ", cmd) 
            import siteConfig
            from subprocess import Popen,PIPE
            output,err = Popen(cmd, shell=True, stdout=PIPE,stderr=PIPE).communicate()
            if output.find(siteConfig.GFESUITE_MHSID+"-") == -1:
                alertMsg = "ISC Send failed transmission to : "+",".join(addresses)+" --> "+output+" "+err
                self.logProblem(alertMsg)
            else:
                self.logEvent(output.rstrip())
                if len(err) > 0:
                    self.logProblem(err.rstrip())
                alertMsg="ISC data successfully transmitted to: "+",".join(addresses)
                self.logEvent(alertMsg)

        for file in attachments: 
            try:
                os.remove(file)
            except OSError:
                pass

    def printServerInfo(self, serverInfo):
        # assembles a string to print out the server information.  serverInfo
        # is a dict with keys "mhsid",'host','port','protocol','site'.
        # Returns the string to print. All input values are strings.
        mhsid = serverInfo.get('mhsid', '?')
        host = serverInfo.get('host', '?')
        port = serverInfo.get('port', '?')
        protocol = serverInfo.get('protocol', '?')
        site = serverInfo.get('site', '?')
        s = "mhs=" + mhsid + ",host=" + host + ",port=" + port +\
          ",proto=" + protocol + ",site=" + site
        return s
