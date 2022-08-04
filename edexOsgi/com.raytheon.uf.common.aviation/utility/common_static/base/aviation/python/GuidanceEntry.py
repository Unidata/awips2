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
# Entry point for Java on TAF Guidance
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/22/09                      njensen       Initial Creation.
#    02/13/2013      1549          rferrel        Change to properly display grid data.
#    06/06/2014      3252          rferrel       tampgen no longer thows exception when missing data.
#
#

##
# This is a base file that is not intended to be overridden.
##

import Avn
import AvnLib
import AvnParser
import EtaData
import GridData
import JUtil
import MetarData
import MetarViewer
import MosData
import NoDataException
import TafDecoder
import TafGen
import time
import logging
import pickle

_Logger = logging.getLogger(Avn.CATEGORY)

# The alerts that need to be tagged
_Tags = [['sky', 'sky'],
         ['wx', 'pcp,obv,vcnty'],
         ['vsby', 'vsby'],
         ['wind', 'wind']]

# Number of seconds to keep a cached item.
_TIMEOUT = 5.0 * 60.0

# Custom except class
class NoCacheEntry(Exception):
    "Used when no cache entry is found."

class Cache(object):
    "Class to handle data cache enters for siteID, key"

    def __init__(self):
        self.cache = {}

    def addData(self, siteID, key, data):
        '''Add data for a given site and key to the cache along with the time of entry.'''
        self.cache.setdefault(siteID,[]).append({'key': key, 'data': data, 'time': time.time()})

    def delSite(self, siteID):
        '''Delete all of siteID's entries'''
        try:
            del self.cache[siteID]
        except KeyError:
            pass

    def delData(self, siteID, key):
        '''Delete the entry for siteID and key from cache.
        Quietly do nothing if entry does not exist.'''
        if siteID in self.cache:
            for f in self.cache[siteID]:
                if f['key'] == key:
                    self.cache[siteID].remove(f)
                    break

    def getData(self, siteID, key):
        '''Method used to obtain an entry from a cache.
         This checks and removes stale entries.
         NoCacheEntry thrown when unable to find an entry'''
        if siteID in self.cache:
            for f in self.cache[siteID]:
                if f['key'] == key:
                    if (time.time() - f['time']) <= _TIMEOUT:
                        return f['data']
                    self.cache[siteID].remove(f)
                    break
        raise NoCacheEntry

# Cache for the various methods
_gridgenDataCache = Cache()
_metargenDataCache = Cache()
_tafgenDataCache = Cache()
_tampgenDataCache = Cache()


def clearMetarCache(siteIDs, model=None, format=None):
    '''Clears all metar cached information for the sites.
    Returns the list of siteIDs.
    '''
    siteIDs = JUtil.javaStringListToPylist(siteIDs)
    for siteID in siteIDs:
        _metargenDataCache.delSite(siteID)
    return siteIDs


def clearCache(siteIDs, model=None, format=None):
    '''Clear all cache information for the sites.
    Returns the list of siteIDs.
    '''
    siteIDs = JUtil.javaStringListToPylist(siteIDs)
    for siteID in siteIDs:
        _gridgenDataCache.delSite(siteID)
        _tafgenDataCache.delSite(siteID)
        _tampgenDataCache.delSite(siteID)
        _metargenDataCache.delSite(siteID)
    return siteIDs


def metargenRetrieve(siteID, size=1):
    try:
        data = MetarData.retrieve([siteID], size)
    except NoDataException.NoDataException:
        data = None
    o = {}
    o['siteID'] = siteID
    o['data'] = data
    return JUtil.javaPickle(o)


def metargen(siteObjs, all, header, decoded, size=1):
    def __highlightFltCat(data, text):
        fc = AvnLib.flightCategory(data['dcd'])
        startTag = '<' + fc + '>'
        endTag = '</' + fc + '>'

        return startTag + text.strip() + endTag

    def __addAlertTagsToRaw(data):
        dcd = data['dcd']
        metar = data['text'] + ' '
        for it in _Tags:
            tag = it[0]
            keys = it[1].split(',')
            for key in dcd.keys():
                if key in keys:
                    str = dcd[key]['str']
                    newStr = ' <' + tag + '>'
                    newStr += str.strip()
                    newStr += '</' + tag + '> '
                    metar = metar.replace(str, newStr)
        return metar

    def __showDecoded(data, addTages):
        formatted = MetarViewer._format(d['dcd'], addTages)
        formatted = __highlightFltCat(d, formatted)
        return formatted

    def __showRaw(data, headers, addTages):
        if addTages:
            metar = __addAlertTagsToRaw(data)
        else:
            metar = data['text']
        raw = metar.split()
        text = ''
        for i in range(len(raw)):
            text += raw[i] + ' '

        text = __highlightFltCat(data, text)

        if headers:
            text = data['header'] + '\n' + text
        return text

    cutoffTime = time.time() - int(size) * 3600
    DataList = []
    for siteObj in siteObjs:
        obj = JUtil.javaUnpickle(siteObj)
        siteID = obj['siteID']
        data = obj['data']
        if data is None:
            if len(siteObjs) == 1:
                return ['Cannot retrieve data for %s' % siteID]
        else:
            data = [{'header': d.header, 'text': d.text, 'dcd': d.dcd}
                    for d in data
                    if d.dcd['itime']['value'] >= cutoffTime
                    ]
            data.sort(key=lambda x: x['dcd']['itime']['value'], reverse=True)
            DataList.append(data)
    metars = []
    if len(DataList) == 1:
        addTages = True
        data = DataList[0]
        if len(data) == 0:
            metars.append('')
            metars.append('No METARS for site %s for the hours specified' % siteID)
            return metars

        metars.append(__makeHeader('%s METAR' % siteID, data[0]['dcd']))
        if decoded:
            MetarViewer._adjustLengths(data)
            metars[0] += '\n' + (MetarViewer._makeHeaderLine())
            for d in data:
                metars.append(__showDecoded(d, addTages))
                addTages = False
        else:
            for d in data:
                metars.append(__showRaw(d, header, addTages))
                addTages = False
    else:
        metars.append('')
        noData = True
        for data in DataList:
            if data:
                noData = False
                for d in data:
                    metars.append(__showRaw(d, header, False))
        if noData:
            metars.append('No METARS for the sites for the hours specified')
    return metars


def tafgenRetrieve(siteID, model, format, refTime=None):
    includeReport = True
    try:
        # This can also be used by Plot which needs all the data.
        if model == 'etabuf':
            data = EtaData.retrieve(model, [siteID], includeReport, refTime=refTime)
        else:
            data = MosData.retrieve(model, [siteID], includeReport, refTime=refTime)
    except IndexError:
        data = None

    if data is not None and len(data) == 0:
        data = None

    if includeReport and data is not None:
        # Assume formatting only needs to be done for the first entry.
        data0 = data[0]
        if data0 is not None and  data0.data is not None and len(data0.data) > 1:
            __highlightTableFlightCat(data0, 9, 3)  # 9 is the length of the row label
    o = {}
    o['siteID'] = siteID
    o['data'] = data
    return JUtil.javaPickle(o)


def tafgen(siteObjs, model, format='short', routine = False, highlightFlightCat=True):
    if routine:
        bbb = '   '
    else:
        bbb = 'AAX'

    includeReport = (format == 'table')

    if len(siteObjs) > 1 and includeReport:
        msg = 'Table display for all sites is not supported'
        return [msg]

    DataList = []
    for siteObj in siteObjs:
        o = JUtil.javaUnpickle(siteObj)
        siteID = o['siteID']
        data = o['data']
        if data is not None:
            data = data[0]
        if data is not None and data.data is not None and len(data.data) > 1:
            DataList.append(data)

    if len(siteObjs) == 1:
        if data is None or data.data is None:
            return ["'No data available for site %s'" % siteID]
        elif len(data.data) == 1:
            # Assume error message
            return data.data
    if includeReport:
        return data.rpt
    else:
        tafList = []
        tafWithHeader = []
        if len(DataList) > 0:
            donot_group = (format == 'long')
            for data in DataList:
                tc = TafGen.TafGen(model,data.data,bbb)
                taf=tc.createTaf(donot_group)
                if highlightFlightCat:
                    taf = __highlightFlightCat(taf)
                tafList.append(taf)
            tafWithHeader.append(__makeHeader(model, DataList[0].data))
            for taf in tafList:
                tafWithHeader.append('')
                tafWithHeader += taf
        else:
            if len(siteObjs) == 1:
                tafWithHeader.append('No data available for the site %s' % siteID)
            else:
                tafWithHeader.append('No data available for the sites')
        return tafWithHeader


def __makeHeader(model, data):
    if data and 'itime' in data and 'value' in data['itime']:
        date = time.strftime('%m/%d/%y %H%M UTC', time.gmtime(data['itime']['value']))
    else:
        date = 'Unable to determine data/time'
    return '%s Guidance  %s' % (model.upper(), date)


def __highlightFlightCat(taf):
    decoder = TafDecoder.Decoder()
    dcd = decoder(taf)
    if 'fatal' in dcd:
        return taf
    previousLine = 0
    previousStartChar = 0
    previousEndChar = 0
    for ix, g in AvnLib.getTafPeriods(dcd):
        fc = AvnLib.flightCategory(g)
        startLine, startChar = ix[0].split('.')
        endLine, endChar = ix[1].split('.')
        startLine = int(startLine) - 1
        startChar = int(startChar)
        endLine = int(endLine) - 1
        endChar = int(endChar)
        if startLine == endLine:
            if previousLine == startLine:
                if startChar >= previousStartChar:
                    startChar += len(startTag)
                if startChar >= previousEndChar:
                    startChar += len(endTag)
                if endChar >= previousStartChar:
                    endChar += len(startTag)
                if endChar >= previousEndChar:
                    endChar += len(endTag)
            startTag = '<' + fc + '>'
            endTag = '</' + fc + '>'
            line = taf[startLine]
            replaceLine = line[0:startChar] + startTag + line[startChar:]
            replaceLine = replaceLine[0:endChar+len(startTag)] + endTag + replaceLine[endChar+len(startTag):]
            taf[startLine] = replaceLine
            previousLine = startLine
            previousStartChar = startChar
            previousEndChar = endChar
    return taf


def __highlightTableFlightCat(data, startIndex, columnWidth):
    nrows = len(data.rpt) - 2
    fcList = []
    for i in range(len(data.data['group'])):
        fc = AvnLib.flightCategory(data.data['group'][i])
        fcList.append(fc)
    for n in range(2, nrows+2):
        index = startIndex
        for fc in fcList:
            startTag = '<' + fc + '>'
            endTag = '</' + fc + '>'
            replaceLine = data.rpt[n]
            replaceLine = replaceLine[0:index] + startTag + replaceLine[index:]
            index += len(startTag) + columnWidth
            replaceLine = replaceLine[0:index] + endTag + replaceLine[index:]
            index += len(endTag)
            index += 1  # column width + space
            data.rpt[n] = replaceLine
    return data.rpt


def tampgenRetrieve(siteID, model):
    data = MosData.retrieve(model, [siteID], False)
    if not data:
        data = None
    else:
        data = data[0]
    o = {'siteID': siteID, 'data': data}
    return JUtil.javaPickle(o)


def tampgen(selSite, siteObjs, model, format, taf, tafHeader, cvOnly, routine=False, highlightFlightCat=True):
    if routine:
        bbb = '   '
    else:
        bbb = 'AAX'

    LAMPs = []
    siteIDs = []
    for siteObj in siteObjs:
        o = JUtil.javaUnpickle(siteObj)
        siteID = o['siteID']
        LAMP = o['data']
        if LAMP is not None:
            LAMPs.append(LAMP)
            siteIDs.append(siteID)
        if selSite == siteID:
            selLAMP = LAMP
    if len(LAMPs) == 0 or selLAMP is None:
        if len(siteObjs) == 1 or selLAMP is None:
            msg = 'No data available for site %s' % selSite
        else:
            msg = 'No data available for the sites'
        return [msg]

    tafWithHeader = [__makeHeader('%s TAF/%s' % (selSite,model), selLAMP.data)]
    if str(tafWithHeader).find('Unable to determine data/time') > 0:
        tafWithHeader.append('')
        return tafWithHeader

    import TAMPGenerator, ProbReader
    decoder = TafDecoder.Decoder()
    for siteID, LAMP in zip(siteIDs, LAMPs):
        tmtuple = time.gmtime(LAMP.data['itime']['value'])
        thresholds = ProbReader.prob_reader(siteID,[tmtuple[7],('%02d'%tmtuple[3])])
        if thresholds is None:
            msg = 'Missing LAMP thresholds at this hour for %s' % siteID
            return [msg]
        header = tafHeader
        text = taf

        dcd = decoder(text, bbb)
        TAF = Avn.Bunch(header=header, text=text, dcd=dcd)

        try:
            siteinfo = AvnParser.getTafSiteCfg(LAMP.data['ident']['str'])
            tafDuration = int(siteinfo['thresholds'].get('tafduration','24'))
            newTaf = TAMPGenerator.TAMPGenerator(LAMP,TAF.dcd['group'],thresholds,
                                                 bbb,cvOnly, (format == 'long'),
                                                 tafDuration)
            if highlightFlightCat:
                newTaf = __highlightFlightCat(newTaf)
            tafWithHeader.append('')
            tafWithHeader += newTaf
        except (KeyError,TypeError,AttributeError, Avn.AvnError) as e:
            msg = "%s - %s" % (siteID, e)
            _Logger.exception('Error with TAMPGenerator: %s', msg)
            if len(siteObjs) == 1:
                return [msg]
    return tafWithHeader


def gridgenRetrieve(siteID):
    currentTime = time.gmtime()
    requestTimeTuple = (*currentTime[0:4], 0, 0, *currentTime[6:9])
    requestTime = time.mktime(requestTimeTuple) - time.timezone
    # The GridData make table also does the make data
    try:
        data = GridData.makeTable(siteID, requestTime)
    except:
        data = None
    if data is not None:
        __highlightTableFlightCat(data, 6, 4)
    o = {}
    o['siteID'] = siteID
    o['data'] = (requestTime, data)
    return JUtil.javaPickle(o)

def gridgen(siteObjs, format='short', routine=False, highlightFlightCat=True):
    currentTime = time.gmtime()
    requestTimeTuple = (*currentTime[0:4], 0, 0, *currentTime[6:9])
    requestTime = time.mktime(requestTimeTuple) - time.timezone
    if len(siteObjs) > 1 and format == 'table':
        msg = 'Table display for all sites is not supported'
        return [msg]

    if format == 'table':
        siteObj = siteObjs[0]
        o = JUtil.javaUnpickle(siteObj)
        siteID = o['siteID']
        cacheRequestTime, data = o['data']
        if cacheRequestTime != requestTime:
            return ['++Cache out of date :%s' % siteID]
        if data:
            return data.rpt
        else:
            return ['Cannot retrieve data for site %s.' % siteID]
    else:
        if routine:
            bbb = '   '
        else:
            bbb = 'AAX'
        DataList = []
        cacheList = []
        firstSiteID  = None
        for siteObj in siteObjs:
            o = JUtil.javaUnpickle(siteObj)
            siteID = o['siteID']
            cacheRequestTime, data = o['data']
            if data is not None:
                data = list(data.values())[0]

            if not firstSiteID:
                firstSiteID = siteID
            if cacheRequestTime != requestTime:
                cacheList.append('++Cache out of date :%s' % siteID)
            if data:
                DataList.append(data)
        if len(cacheList) > 0:
            return cacheList
        if len(DataList) == 0:
            if len(siteObjs) == 1:
                msg = 'No data available for site %s.' % firstSiteID
            else:
                msg = 'No data available for the sites'
            return [msg]

        tafWithHeader = [__makeHeader("NDFD", DataList[0])]

        donot_group = (format == 'long')
        for data in DataList:
            tc = TafGen.TafGen('grid',data,bbb)
            taf = tc.createTaf(donot_group)
            if highlightFlightCat:
                taf = __highlightFlightCat(taf)
            tafWithHeader.append('')
            tafWithHeader += taf
        return tafWithHeader
