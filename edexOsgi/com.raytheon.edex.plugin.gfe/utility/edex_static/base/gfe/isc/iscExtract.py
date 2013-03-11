#!/common/bphillip/awips/bin/python

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


import JUtil
import ifpnetCDF, iscUtil 
import numpy
import  tempfile, os, stat, getopt, sys, cPickle, siteConfig
import LogStream, time, traceback, string, IrtAccess, urllib, urllib2
import xml, copy, string
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
from java.io import File
from java.awt import Point
from com.vividsolutions.jts.geom import Coordinate
from java.util import ArrayList
from com.raytheon.edex.plugin.gfe.config import IFPServerConfig
from com.raytheon.edex.plugin.gfe.config import IFPServerConfigManager
from com.raytheon.edex.plugin.gfe.util import CartDomain2D
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridLocation
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType

from com.vividsolutions.jts.geom import Coordinate

#
# Port of iscExtract.py
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    03/12/13        1759          dgilling       Change how ifpnetCDF is called.
#    
# 
#

parms = []             #parm names
dbid = None            #database identifier
startTR = None          #ifpnetCDF start time
endTR = None            #ifpnetCDF start time
xmlDestinations = None #XML destinations information
ancf = None         #IRT web address 
bncf = None
xmtScript = None       #transmit script
serverHost = None      #server host 
serverPort = None      #server port
serverProtocol = None  #serverProtocol
mhsid = None           #MHS site identifier
siteID = None          #our site id
startT = None

iscExtractLogger=None

def logEvent(*msg):
    iscUtil.getLogger("iscExtract").info(iscUtil.tupleToString(*msg))

def logProblem(*msg):
    iscUtil.getLogger("iscExtract").error(iscUtil.tupleToString(*msg))
    
def logException(*msg):
    iscUtil.getLogger("iscExtract").exception(iscUtil.tupleToString(*msg))    

def logVerbose(*msg):
    iscUtil.getLogger("iscExtract").debug(iscUtil.tupleToString(*msg))
    
def logDebug(*msg):
    logVerbose(iscUtil.tupleToString(*msg))


def executeIscExtract(parmNames, databaseName, startTime, endTime,
                      irtTableAddressA, irtTableAddressB, transmitScript, ourServerHost,
                      ourServerPort, ourServerProtocol, ourMHSid, ourSiteID, destinations=None):
    
    startT = time.time()
    parms = parmNames
    dbid = databaseName
    startTR = startTime
    endTR = endTime
    xmlDestinations = destinations
    ancf = irtTableAddressA
    bncf = irtTableAddressB
    xmtScript = transmitScript
    serverHost = ourServerHost
    serverPort = ourServerPort
    serverProtocol = ourServerProtocol
    mhsid = ourMHSid
    siteID = ourSiteID
    
    myOfficeType = IFPServerConfigManager.getServerConfig(siteID).officeType()
    
    
    #--------------------------------------------------------------------
    # determine the list of destination servers
    #--------------------------------------------------------------------
    try:
        nowT = time.time()   #current time
        useUntilTime = None  #cached use until time
        cacheFilename = "/tmp/" + serverHost + serverPort + ".iscExtract"
        cachedXmlDestinations = None
        #if xmlDestinations is None:    #destinations not on command line
        #    # check the cache 
        #    try:
        #        fd = open(cacheFilename, 'rb')
        #        buf = fd.read()
        #        fd.close()
        #        useUntilTime, cachedXmlDestinations = cPickle.loads(buf)
        #        nowT = time.time()   #current time
        #        if nowT > useUntilTime:
        #            xmlDestinations = None   #cache is too old
        #            useUntilTime = None
        #        else:
        #            logEvent('Using xmlDestinations cache')
        #            xmlDestinations = cachedXmlDestinations
        #    except:
        #        pass
            
            
        # need to contact IRT to get destinations
        irt = IrtAccess.IrtAccess(ancf, bncf)
        if xmlDestinations is None:
            logEvent('contacting IRT to get destinations')
            count = 1
            while True:
                status, xmlDestinations = irt.getSendAddrs(siteID)
                logEvent('IRT getSendAddrs status:', status)
                if status:
                    # if we obtained XML destinations from IRT, then decode
                    # the useUntilTime field
                    try:
                        d = ElementTree.ElementTree(ElementTree.XML(xmlDestinations))
                        dE = d.getroot()
                        for e in dE:
                            if e.tag == "useuntil":
                                isoTimeStr = e.text
                                idx = isoTimeStr.find(".") 
                                if idx != - 1:
                                    isoTimeStr = isoTimeStr[0:idx]  #eliminate subseconds
                                useUntilTime = time.mktime(time.strptime(isoTimeStr,
                                  "%Y-%m-%dT%H:%M:%S"))
                                logEvent("Use Until: ", isoTimeStr)
                    except:
                        logProblem("Malformed XML on getSendAddrs()")
                        logProblem("XML=", xmlDestinations)
                        return
                    if useUntilTime is None:
                        useUntilTime = time.time() + 180.0   #3 minutes default
                        logEvent("Using default 180 second useUntilTime")
    
                    # write the cache
                    fd = open(cacheFilename, 'wb')
                    buf = cPickle.dumps((useUntilTime, xmlDestinations))
                    fd.write(buf)
                    fd.close()
                    break   #success from the irt
                else:
                    # try again and again for 10 minutes, then use cache
                    # if available and alert GFE users
                    if time.time() - nowT > 600.00: 
                        logProblem("Unable to access IRT for send addrs")
                        if cachedXmlDestinations is None:
                            s = "Unable to access IRT for send addrs. Previous" + \
                              " cache not available."
                            logProblem(s)
                            return
                        # use cached value, even if out of date
                        else:
                            xmlDestinations = cachedXmlDestinations
                            if useUntilTime is not None:
                                s = time.asctime(time.gmtime(useUntilTime))
                            else:
                                s = "Unknown"
                            logProblem("Using expired cache. Date=", s)
    
                            #determine when we issued our last GFE alert
                            #we alert every 30 minutes.
                            try:
                                fd = open(cacheFilename + "-warn", 'rb')
                                buf = fd.read()
                                fd.close()
                                lastAlertTime = cPickle.loads(buf)
                            except:
                                lastAlertTime = 0  #for way long ago
                            if time.time() - lastAlertTime > 1800.0:
                                logProblem("Sending GFE notification")
                                msg = """
    Contact NCF. ifpServer is unable to contact IRT central server.  ISC
    traffic routing information is old and possibly incorrect."""
                                os.system("sendGfeMessage -u -c GFE -m '" + \
                                  msg + "'")
                                fd = open(cacheFilename + "-warn", 'wb')
                                fd.write(cPickle.dumps(time.time()))
                                fd.close()
                            break
    
                    time.sleep(15.0)   #sleep awhile and then try again
                    count = count + 1
                    logProblem("Retrying to getSendAddrs()", count)
        # qc the XML
        try:
            destTree = ElementTree.ElementTree(ElementTree.XML(xmlDestinations))
            destE = destTree.getroot()
        except:
            logProblem("Malformed XML on getSendAddrs() or provided xmlDest")
            logProblem("XML=", xmlDestinations)
            return
        #--------------------------------------------------------------------
        # determine how many transmissions are necessary
        #--------------------------------------------------------------------
        xmt = []

        logEvent("XML dest:", xmlDestinations)
        if destE.tag != "destinations":
            logProblem("Destinations packet missing from web service")
            return
            
        # create list of individual transmissions (before attempting to combine
        doClip = 1  #0 to send entire domain, 1 to do destination clipping (default)
        destStr = "Destination Servers:\n"
        for addressE in destE:
            if addressE.tag == "doclip":
                for name, value in addressE.items():
                    if name == "clip":
                        if value == "1":
                            doClip = 1
                        elif value == "0":
                            doClip = 0
    
        logEvent("Clipping State: ", doClip)
        
        for addressE in destE:

            if addressE.tag != "address":
                continue
            
            # find destination server info and domain information
            serverInfo = irt.decodeXMLAddress(addressE)
            
            if doClip == 0:
                serverInfo['domain'] = None
            keycheckfail = False
            for key in ['mhsid', 'host', 'port', 'protocol', 'site']:
                if not serverInfo.has_key(key):
                    logProblem("Fail to decode XML. Skipping serverInfo:",
                      serverInfo)
                    keycheckfail = True
                    continue
            if keycheckfail:
                continue   #skipping this destination due to insufficient info
           
            # get the destination office type
            try: 
                siteIndex = IFPServerConfigManager.getServerConfig(siteID).allSites().indexOf(serverInfo['site'])
                destOfficeType = str(IFPServerConfigManager.getServerConfig(siteID).officeTypes().get(siteIndex))
            except:
                logProblem("Unknown site id to get office type. ",
                  "Skipping serverInfo:", serverInfo)
                continue   #skipping this destination due to unknown site id

            # find weather elements that remote ifpServer wants
            # that is available in our server and in the -p parm switches
            any = False
            for parm in serverInfo['parms']:
                p1 = string.replace(parm, "_SFC", "")  #remove _SFC if exists

                # translation of parm name needed, also if no office type, then
                # not wanted from this office.
                #   example: changes QPFwfo to QPF if we are wfo
                #   example: discards T if we are wfo and site is non-wfo
                if myOfficeType != destOfficeType:
                    if p1.find(myOfficeType) != - 1:
                        p1 = string.replace(p1, myOfficeType, "") #remove type
                    else:
                        continue   #no type, so not intended for our type
                # see if parm was listed in the command line switches
                if parms.contains(p1):
                    xmt.append({'serverInfo':[serverInfo], 'parms':[p1],
                      'domain': serverInfo['domain'], 'area': serverInfo['area']})
                    if not any:
                        destStr += irt.printServerInfo(serverInfo) + "\n"
                        any = True
    
        logEvent(destStr)
        
        # now combine transmissions
        # find same domains, same parms, to combine servers/destinations
        i = 0
        while i < len(xmt):
            j = i + 1
            while j < len(xmt):
                if xmt[i]['domain'] == xmt[j]['domain'] and \
                  xmt[i]['area'] == xmt[j]['area'] and \
                  xmt[i]['parms'] == xmt[j]['parms']:
                    for si in xmt[j]['serverInfo']:
                        if si not in xmt[i]['serverInfo']:
                            dests = xmt[i]['serverInfo']
                            dests.append(si)
                            xmt[j]['serverInfo'] = dests
                    del xmt[j] #delete the entry
                    j = j - 1  #redo this entry index next loop
                j = j + 1
            i = i + 1
    
        # now try to combine common parm lists (same domain, same servers/destinations)
        i = 0
        while i < len(xmt):
            j = i + 1
            while j < len(xmt):
                if xmt[i]['domain'] == xmt[j]['domain'] and \
                  xmt[i]['area'] == xmt[j]['area'] and \
                  xmt[i]['serverInfo'] == xmt[j]['serverInfo'] :
                    iparms = xmt[i]['parms']
                    for p in xmt[j]['parms']:
                        if p not in iparms:
                            iparms.append(p)
                    xmt[i]['parms'] = iparms 
                    del xmt[j]  #delete the entry
                    j = j - 1  #redo this entry index for next loop
                j = j + 1
            i = i + 1

        # if doClip, gather some required information
        if doClip:
            #get the isc send area and grid domain from the ifpServer
            iscSendAreaGrid = iscUtil.getEditArea("ISC_Send_Area",siteID)           
            sourceDomain = IFPServerConfigManager.getServerConfig(siteID).dbDomain()
            
            iscSendAreaGrid.setGloc(sourceDomain)
            iscSendAreaGrid = iscSendAreaGrid.getGrid()

        
        #--------------------------------------------------------------------
        # prepare output files
        #--------------------------------------------------------------------
        for dest in xmt:
            s = "Processing Xmt Pass:\n"
            for sv in dest['serverInfo']:
                s += irt.printServerInfo(sv) + '\n'
            s += "Domain:" + `dest['domain']` + '\n'
            s += "Area:" + `dest['area']` + '\n'
            s += "Parms:" + `dest['parms']` + '\n\n'
            logEvent(s)
            # extract the data using ifpnetCDF
            if os.path.exists(siteConfig.GFESUITE_HOME + "/products/ISC") == False:
                os.makedirs(siteConfig.GFESUITE_HOME + "/products/ISC")
                
            tempfile.tempdir = siteConfig.GFESUITE_HOME + "/products/ISC" 
            fname = tempfile.mktemp(".isc") 
    
            # Determine domain edit area.
            
            if doClip == 1 and dest['domain'] is not None and \
              dest['domain']['proj'] == sourceDomain.getProjection().getProjectionID():
                #make a GridLocation for our domain
                gridSize = Coordinate(float(str(sourceDomain.getNx())), float(str(sourceDomain.getNy())))
                origin = sourceDomain.getOrigin()
                extent = sourceDomain.getExtent()
                domain = CartDomain2D(origin, extent)
                gloc = sourceDomain
                
                #make a GridLocation covering the area for the destination, expanded
                #by 1/2 grid cell 
                dd = dest['domain']
                da = dest['area']
                cellsizeX = float(dd['extx']) / (float(da['xdim']) - 1.0)
                cellsizeY = float(dd['exty']) / (float(da['ydim']) - 1.0)
                originD = Coordinate(float(dd['origx']) - cellsizeX / 2.0,
                  float(dd['origy']) - cellsizeY / 2.0)
                extentD = Coordinate(float(dd['extx']) + cellsizeX,
                  float(dd['exty']) + cellsizeY)
                domainD = CartDomain2D(originD, extentD) 

                #check for overlap
                if not domainD.overlaps(domain):
                    logEvent("No intersection of domain box, skipping....")
                    continue   #no bits set in the resulting mask, no intersect
    
                domainD.trim(domain)   #trim it to just the overlapping section
                
                
                gridSize  = Point(int(da['xdim']),int(da['ydim']))
                destGridLocation = GridLocation("Dest",sourceDomain.getProjection(),
                                                gridSize,domainD.getOrigin(),domainD.getExtent(),"GMT")
                                            
                # make a Reference Set
                refid = ReferenceID("jibberish")
                refSet = ReferenceData(gloc, refid, destGridLocation.getGeometry(), CoordinateType.LATLON)

                # convert destination site's domain to gridpoints
                iscMask = refSet.getGrid()
                
                # "and" it with our ISC_Send_Area
                iscMask.andEquals(iscSendAreaGrid)

                if not iscMask.isAnyBitsSet():
                    logEvent("No intersection of domain points, skipping....")
                    continue   #no bits set in the resulting mask, no intersect
    
                # store the grid back into the ifpServer
                maskName = "iscExtract" + `time.time()`
                refSet.setGrid(iscMask)
                iscUtil.saveEditAreaGrid(maskName, refSet, siteID)

            else:  #no clipping, or different projection
                maskName = "ISC_Send_Area"
    
            # Run ifpnetCDF for the data
            argv = {"outputFilename": fname, 
                    "parmList": dest['parms'],
                    "databaseID": dbid, 
                    "startTime": startTR,
                    "endTime": endTR, 
                    "mask": maskName, 
                    "geoInfo": False, 
                    "compressFileFlag": True, 
                    "configFileName": "iscSendSampleDef", 
                    "compressFileFactor": 6, 
                    "trim": True, 
                    "krunch": True, 
                    "userID": "iscExtract",
                    "logFileName": None}
            ifpnetCDF.main(**argv)
            
            fname = fname + '.gz'
            size = os.stat(fname)[stat.ST_SIZE]
            endT = time.time()
            logEvent('File Size: ', size)
            logEvent('After ifpnetCDF, ,wctime:', "%-6.2f" % (endT - startT),
                                   ',cputime:', "%-6.2f" % time.clock())
    
            # create XML destinations file for this output
            iscE = Element('isc')  #create the XML tree root
            sourceServer = {'mhsid': mhsid, 'host': serverHost, 'port': serverPort,
              'protocol': serverProtocol, 'site': siteID}
            irt.addSourceXML(iscE, sourceServer)
            irt.addDestinationXML(iscE, dest['serverInfo']) 
    
            #get the unique list of mhs sites 
            mhsSites = []
            for si in dest['serverInfo']:
                if si['mhsid'] not in mhsSites: 
                    mhsSites.append(si['mhsid']) 
    
            # create the XML file
            fnameXML = tempfile.mktemp(".xml")
            fd = open(fnameXML, 'wb')
            fd.write(ElementTree.tostring(iscE))   
            fd.close()
    
            # Transmit files - do string substitution
            irt.transmitFiles("ISCGRIDS2", mhsSites, mhsid, [fname,fnameXML], xmtScript)
            # Delete temporary files
            if maskName != "ISC_Send_Area": 
               iscUtil.deleteEditArea(maskName,siteID)
    
            endT = time.time() 
            logEvent('After transmission pass, ,wctime:',
              "%-6.2f" % (endT - startT), ',cputime:', "%-6.2f" % time.clock())
    
    except:
        logProblem("Failure", traceback.format_exc()) 
        