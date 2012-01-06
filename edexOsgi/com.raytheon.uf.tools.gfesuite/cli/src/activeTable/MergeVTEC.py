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
import gzip
import cPickle
import os
import stat
import time
import logging
import getAT
#import JUtil
import dynamicserialize.dstypes.com.raytheon.uf.common.activetable.UpdateActiveTableRequest as UpdateActiveTableRequest
from ufpy import ThriftClient
from ufpy import TimeUtil
from getVtecAttribute import getVtecAttribute
from getFourCharSites import getFourCharSites

logging.basicConfig(level=logging.DEBUG)
log = logging.getLogger("MergeVTEC")
def merge(removeRemote, remoteATName, atName, inputIsGZIP=False, drt=None,
      makeBackups=True, xmlIncoming=None, ourSites=None, host='localhost', port=9581):

    oldTable = None
    siteFilter = _getFilterSites(ourSites, host, port)
    
    if drt is None:
        drtOffset = 0.0;
    else:
        drtOffset = TimeUtil.determineDrtOffset(drt)[0]
        
    atName = atName.upper()
    if "PRACTICE" != atName:
        atName = "OPERATIONAL"
        
    try:
        oldTable = getAT.getActiveTable(atName, siteFilter, host=host, port=port)
    except:
        log.error('Error getting old table for backup :', exc_info=True)
    
    log.info("Active Table size: %d", len(oldTable))
        
    if inputIsGZIP:
        fd = gzip.open(remoteATName)
    else:
        fd = open(remoteATName)

    try:
        newTable = cPickle.load(fd)
    finally:
        fd.close()

    log.info("Other Table size: %d", len(newTable))

    if len(newTable) > 0 and newTable[0].has_key('oid'):
        convertToNewFormat(newTable)
        
    # Filter out any extra sites in the new table.
    if siteFilter is not None:
        newTable = [x for x in newTable if x['officeid'] in siteFilter]
        
    log.info("Other Table filtered size: %d", len(newTable))

    # Send the new active table to the server.
    # The real merge is done server-side.
    request = UpdateActiveTableRequest()
    request.setActiveTable(newTable)
    request.setMode(atName)
    request.setXmlSource(xmlIncoming)
    request.setTimeOffset(drtOffset)
    response = None
    
    thriftClient = ThriftClient.ThriftClient(host, port, "/services")
    try:
        response = thriftClient.sendRequest(request)
        log.info('Other table merged.')
    except:
        log.error("Error sending other table to server:", exc_info=True) 

    if response is None:
        log.error("No response was received from the server.")
        return
        
    errMsg = response.getMessage()
    if errMsg is not None and "" != errMsg:
        log.error("Error on server: %s", errMsg)
        return
        
    sourceInfo = response.getSourceInfo()
    if sourceInfo is not None:
        if type(sourceInfo) != list:
            log.info("Converting sourceInfo from " + str(type(sourceInfo)))
            # sourceInfo = JUtil.javaStringListToPylist(sourceInfo)
        for source in sourceInfo:
            log.info("Source Server: %s", str(source))
    else:
        log.info("sourceInfo is None")
        
    if makeBackups and oldTable:
        convertToOldFormat(oldTable)
        dirname = os.path.dirname(os.path.abspath(remoteATName))
        format = "%Y%m%d_%H%M%S"
        gmtime = time.gmtime(time.time() + drtOffset)
        bname = time.strftime(format, gmtime) + 'activeTable.gz'
        bdirName = os.path.join(dirname, "backup")

        if not os.path.exists(bdirName):
            # create the dir and give everyone R/W access
            os.mkdir(bdirName, stat.S_IRWXU | stat.S_IRWXG | stat.S_IROTH | 
                     stat.S_IWOTH)

        fname = os.path.join(bdirName, bname)
        saveFile = gzip.open(fname, "wb")
        save_start = time.time()
        try:
            cPickle.dump(oldTable, saveFile)
        finally:
            saveFile.close()
            
        # Give the user and user's group all access, others read access
        os.chmod(fname, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH)
        tdiff = time.time() - save_start
        log.info('Saved Previous Active Table: %s t=%.3f sec. ', fname, tdiff)
    elif makeBackups:
        log.info('No old active table to back up.')
        
    if removeRemote:
        os.remove(remoteATName)
        log.info('Input file ' + remoteATName + ' deleted.')

def _getFilterSites(ourSites, host, port):
    if ourSites is None:
        ourSite = os.getenv('GFESUITE_SITEID')
        if ourSite is None:
            raise Exception("Specify at least one site or set GFESUITE_SITEID.")
    else:
        ourSite = ourSites[0]
    sites = getVtecAttribute('VTEC_MERGE_SITES', [], ourSite, host, port)
    if sites is not None:
        spcSite = getVtecAttribute("VTEC_SPC_SITE", "KWNS", ourSite, host, port) 
        tpcSite = getVtecAttribute("VTEC_TPC_SITE", "KNHC", ourSite, host, port)
        sites.extend([spcSite, tpcSite])
        if ourSites is None:
            sites.append(ourSite)
        else:
            sites.extend(ourSites)  

    sites = getFourCharSites(sites, host, port)

    log.debug("Filter Sites: %s", str(sites))
    return sites

def convertToNewFormat(newTable):
    '''Convert an AWIPS I table to AWIPS 2 internally'''
    import re
    maxFutureTime = long(float(2**31-1)) 
    for dct in newTable:
        dct['officeid'] = dct['oid']
        dct['vtecstr'] = dct['vstr']
        dct['endTime'] = dct['end']
        dct['startTime'] = dct['start']
        dct['phensig'] = dct['key']
        dct['state'] = 'Decoded'
        if dct['endTime'] >= maxFutureTime:
            dct['ufn'] = True
        else:
            dct['ufn'] = False
        dct['productClass'] = dct['vtecstr'][1]
        if not dct.has_key('rawMessage'):
            dct['rawMessage'] = ''
        
        if dct.has_key('text'):
            dct['segText'] = dct['text']
            # adapted from WarningDecoder...
            lines = dct['segText'].split('\n')
            for count in xrange(len(lines)-1):
                dtg_search = re.search(r' ([0123][0-9][012][0-9][0-5][0-9])', 
                                       lines[count])
                if dtg_search:
                    pil_search = re.search(r'^([A-Z]{3})(\w{3}|\w{2}|\w{1})', 
                                           lines[count+1])
                    if pil_search:
                        dct['xxxid'] = pil_search.group(2)
                        break

def convertToOldFormat(backupTable):
    'Convert an AWIPS 2 table to a table usable by AWIPS 1, in-place.'
    for dct in backupTable:
        dct['oid'] = dct['officeid']
        dct['vstr'] = dct['vtecstr']
        dct['end'] = dct['endTime']
        dct['start'] = dct['startTime']
        dct['key'] = dct['phensig']
        # remove new fields so we don't pickle two copies
        del dct['officeid']
        del dct['vtecstr']
        del dct['endTime']
        del dct['phensig']
        del dct['startTime']
        
        if dct.has_key('segText'):
            dct['text'] = dct['segText']
            del dct['segText']
