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
#     SOFTWARE HISTORY
#
#    Date          Ticket#     Engineer     Description
#    ------------  ----------  -----------  --------------------------
#    02/20/2016    6110        tgurney      Extract and refactor from AvnFPS
#                                           MosData.py
#
#

##
# This is a base file that is not intended to be overridden.
##

from com.raytheon.uf.edex.aviation.aag import AAGData
import time
import AvnLib
import TafGen

UNLIMITED = 99999
TO_KT = 3600.0/1852.0
FILL_VALUE = -9999.0

class AAGTafGen(TafGen.TafGen):
    def __init__(self, allFcst):
        self.model = 'gfslamp'
        self.ident = allFcst['ident']['str']
        self.fcst = allFcst['group']
        self.startTimes = [t['time']['from'] for t in self.fcst]
        self.endTimes = [t['time']['to'] for t in self.fcst]
        self.grpTaf = TafGen.Config(self.ident, 'gfslamp').grpTaf()
        self.fltCat = TafGen.Config(self.ident, 'gfslamp').fltCat()
        self.tafTime = time.time()
        self.tafDuration = 24
        nBeg, nEnd = self.getTafPrd(self.tafDuration)
        self.projData = [
            TafGen.LampProjection(self.ident, self.grpTaf, self.fltCat,
                           dat, self.tafTime).getData()
            for dat in self.fcst[nBeg:nEnd]
            ]
        self.subStartTimes = self.startTimes[nBeg:nEnd]
        self.subEndTimes = self.endTimes[nBeg:nEnd]


class _PointDataView:
    def __init__(self, java_PointDataView):
        self.__javaPdv = java_PointDataView
        self.__keys = []
        keyset = self.__javaPdv.getContainer().getParameters()
        itr = keyset.iterator()
        while itr.hasNext():
            self.__keys.append(str(itr.next()))

    def __getitem__(self, key):
        result = None
        strValType = self.getType(key)
        if strValType == 'FLOAT':
            result = self.__javaPdv.getFloat(key)
        elif strValType == 'STRING':
            result = self.__javaPdv.getString(key)
        elif strValType == 'INT':
            result = self.__javaPdv.getInt(key)
        elif strValType == 'LONG':
            result = self.__javaPdv.getLong(key)
        return result

    def getType(self, key):
        val = self.__javaPdv.getType(key)
        if val:
            val = str(val)
        return val

    def has_key(self, key):
        return self.__keys.__contains__(key)

    def keys(self):
        return self.__keys

    def __contains__(self, key):
        return self.has_key(key)

###############################################################################

def accumulate(iterable):
    s = 0
    for v in iterable:
        s += v
        yield s

###############################################################################

def _getCigProbs(v,element,numCategories):
    try:
        probs = [v[element+str(x)] for x in xrange(1,numCategories+1)]
        return [min(x,100) for x in accumulate(probs)]
    except KeyError:
        return [0]*(numCategories-1)+[100]

def _getVisProbs(v,element,numCategories):
    try:
        return [v[element+str(x)] for x in xrange(1,numCategories+1)]+[100]
    except KeyError:
        return [0]*(numCategories-1)+[100]

###############################################################################

class _GfsLampData:
    VSBY_VALUE = {
        8: 0.25,
        9: 0.5,
        10: 1.5,
        11: 2.5,
        5: 4.0,
        6: 6,
        7: 10.0
        }
    CIG_VALUE = {
        1: 100,
        2: 300,
        3: 700,
        8: 1500,
        9: 2500,
        5: 5000,
        6: 10000,
        7: 25000
        }
    COVER_STR = {
        0: 'SKC',
        13: 'FEW',
        11: 'SCT',
        12: 'BKN',
        8: 'OVC'
        }
    numData = 25    # 25 forecast hours
    PRECIP_TYPE = {
        13: 'SHPL',
        11: 'FZDZ',
        12: 'FZRA',
        23: 'SHSN',
        21: 'DZSN',
        22: 'SN',
        33: 'SHRA',
        31: 'DZ',
        32: 'RA'
        }
    OBV_TYPE = {
        1: 'BL',
        2: 'HZ',
        3: 'FG',
        4: '',
        5: 'BR'
        }
    CIG_COVT = {1:1, 2:2, 3:3, 8:4, 9:5, 5:6, 6:7, 7:8}
    VIS_COVT = {8:1, 9:2, 10:3, 11:4, 5:5, 6:6, 7:7}

    def cigBestCat(self,t):
        try:
            return '%3.0f' % self.CIG_COVT[int(t+0.1)]
        except:
            return ''

    def visBestCat(self,t):
        try:
            return '%3.0f' % self.VIS_COVT[int(t+0.1)]
        except:
            return ''

    def makeObv(self, v):
        t = int(v['obVis_bestCat'])
        s = self.OBV_TYPE.get(t, '')
        if s:
            return {'str': s}
        else:
            return None

    def makePcp(self, v, vsby, pdc, n, fcstHrList):
        d = {}
        p = v['POP_hour']
        if p != FILL_VALUE:
            d['pop'] = int(p)
        p = int(v['POP_hour_bestCat'])
        if p != FILL_VALUE:
            d['pcat'] = p

        # tstm has overlapped 2-hour forecasts in the first five hours,then 2 hour
        if n < self.numData - 1:
            p = _PointDataView(pdc.readRandom(fcstHrList[n+1]))['ltg2hr']
            if p == FILL_VALUE and n < self.numData-2:
                try:
                    p = _PointDataView(pdc.readRandom(fcstHrList[n+2]))['ltg2hr']
                except:
                    p = FILL_VALUE
        else:
            p = FILL_VALUE
        if p != FILL_VALUE:
            d['pot'] = int(p)

        if n < self.numData-1:
            p = int(_PointDataView(pdc.readRandom(fcstHrList[n+1]))['ltg_bestCat'])
            if p == FILL_VALUE and n < self.numData-2:
                try:
                    #p = int(v['ltg_bestCat'][recno,n+2])
                    p = int(_PointDataView(pdc.readRandom(fcstHrList[n+2]))['ltg_bestCat'])
                except:
                    p = FILL_VALUE
        else:
            p = FILL_VALUE

        if p != FILL_VALUE:
            d['tcat'] = p
        ptype = int(v['precipType'])
        #if ptype is missing, it's rain
        if ptype == FILL_VALUE:
            ptype = 3   # rain
        pchar = int(v['POP_bestCat'])
        if pchar == FILL_VALUE:
            pchar = 2
        intensity = ''
        if ptype == 2 or pchar == 1:  # SN or DZ
            if vsby:
                if vsby < 0.245:
                    intensity = '+'
                elif vsby > 0.50:
                    intensity = '-'
        else:
            intensity = '-'
        pcp = self.PRECIP_TYPE[ptype * 10 + pchar]
        d.update({'str': intensity + pcp, 'int': intensity})
        return d

    def makeSky(self, ceiling_bestCat, clouds_bestCat):
        cig = self.CIG_VALUE.get(int(ceiling_bestCat), None)
        cover = int(clouds_bestCat)
        #if sky cover is not BKN or OVC, set cig to unlimited.
        if cover in [0, 13, 11]:
            cig = UNLIMITED

        if cig is not None:
            if cover == 0:
                d = {'str': 'SKC', 'cover': 0, 'cig': UNLIMITED}
            elif cover in self.COVER_STR.keys():
                if cig != UNLIMITED:
                    d = {
                        'str': '%s%03d' % (self.COVER_STR[cover], cig/100),
                        'cover': cover,
                        'cig': cig
                        }
                else:
                    d = {'str': '%s%03d' % (self.COVER_STR[cover], 250),
                        'cover': cover,
                        'cig': cig
                        }
            else:
                return None
            return d
        else:
            return None

    def makeWind(self, v, noToKt):
        d = {}
        gg = 0
        dd = int(v['windDir'])
        if dd != FILL_VALUE:
            dd = 10* ((dd + 5) // 10)
            if dd == 0:
                dd = 360
            d['dd'] = dd
        if 'windSpeedInflated' in v:
            ff = float(v['windSpeedInflated'])
            fillValue = FILL_VALUE
        else:
            ff = float(v['windSpeed'])
            fillValue = FILL_VALUE
        if ff != fillValue:
            if noToKt:
                d['ff'] = int(ff + 0.5)
            else:
                d['ff'] = int(ff * TO_KT + 0.5)
            if d['ff'] == 0:
                d['dd'] = 0
        if 'MaxWindSpeed' in v:
            gg = int(v['MaxWindSpeed'] * TO_KT + 0.5)
        if 'dd' in d and 'ff' in d and 9998 > gg > 0:
            d['gg'] = int(gg)
            d['str'] = '%03d%02dG%02dKT' % (d['dd'], d['ff'], d['gg'])
        else:
            if 'dd' in d and 'ff' in d:
                d['str'] = '%03d%02dKT' % (d['dd'], d['ff'])
            else:
                d['str'] = '??????KT'
        return d

    def makeVsby(self, var):
        # returns mid point of category range
        tmp = self.VSBY_VALUE.get(int(var), None)
        if tmp:
            return AvnLib.fixTafVsby(tmp)
        else:
            return None

    def makeData(self, pdc, ident):
        self.numData = min(self.numData, pdc.getCurrentSz())
        self.issuetime = pdc.readRandom(0).getDataTime(False).getRefTime().getTime() / 1000
        fcstHrList = range(pdc.getCurrentSz())
        fcstHrList.sort()
        self._validTimeList = []
        for f in fcstHrList:
            self._validTimeList.append(self.issuetime + (f * 3600))
        d = {'itime': {'value': self.issuetime,
            'str': time.strftime('%d%H%MZ', time.gmtime(self.issuetime))},
            'ident': {'str': ident}}
        d['group'] = [self.makePeriod(pdc, n, fcstHrList) for n in range(self.numData)]
        return d

    def makePeriod(self, pdc, n, fcstHrList):
        v = _PointDataView(pdc.readRandom(fcstHrList[n]))
        try:
            f, t = self._validTimeList[n:n+2]
        except ValueError:
            # LAMP only has 25 projections, so need to consider running out of pairs
            f, t = self._validTimeList[n], self._validTimeList[n]+3600

        g = {'time': {'from': f, 'to': t}}
        d = self.makeWind(v, 0)
        if d:
            g['wind'] = d

        d = self.makeVsby(v['vis_bestCat'])
        if d:
            g['vsby'] = d
            vsby = d['value']
        else:
            vsby = None

        d = self.makeVsby(v['cvis_bestCat'])
        if d:
            g['cvsby'] = d
            cvsby = d['value']
        else:
            cvsby = None

        if v['POP_hour']*100 > 40:
            vsby = cvsby

        d = self.makePcp(v, vsby, pdc, n, fcstHrList)
        if d:
            g['pcp'] = d

        d = self.makeObv(v)
        if d:
            g['obv'] = d
            #cobv is the same as obv until 'FG' and 'BR' is switched based vis
            g['cobv'] = d

        d = self.makeSky(v['ceiling_bestCat'], v['clouds_bestCat'])
        if d:
            g['sky'] = d

        try:
            d = self.makeSky(v['c_ceiling_bestCat'], v['clouds_bestCat'])
            if d:
                g['csky'] = d
        except:
            pass

        # fix visibility and obstruction to vision
        if 'vsby' in g and 'obv' in g and g['obv']['str'] in ['BR', 'FG']:
            vsby = g['vsby']['value']
            if vsby > 6.1:
                g['vsby'] = {'str': '6SM', 'value': 6.0}
            if vsby < 0.6:
                g['obv']['str'] = 'FG'
            elif vsby <= 6.1:
                g['obv']['str'] = 'BR'

        # fix conditional visibility and obstruction to vision
        if 'cvsby' in g and 'obv' in g and g['obv']['str'] in ['BR', 'FG']:
            vsby = g['cvsby']['value']
            if vsby > 6.1:
                g['cvsby'] = {'str': '6SM', 'value': 6.0}
            if vsby < 0.6:
                g['cobv']['str'] = 'FG'
            elif vsby <= 6.1:
                g['cobv']['str'] = 'BR'
        #
        # include the probabilities
        # Look ahead for the 6hr QPF POP
        #
        g['pop6hr'] = -1
        try:
            for i in range(n, 25):
                if _PointDataView(pdc.readRandom(fcstHrList[i]))['PQPF_6hr'] < 100:
                    g['pop6hr'] = _PointDataView(pdc.readRandom(fcstHrList[i]))['PQPF_6hr']
                    break
        except KeyError:
            pass
        # Probability of ceiling categories including best category
        g['cprob'] = _getCigProbs(v,'ceiling_cat',8)
        g['ccprob'] = _getCigProbs(v,'c_ceiling_cat',8)
        try:
            g['cig_bestCat'] = int(self.cigBestCat(v['ceiling_bestCat']))
            g['ccig_bestCat'] =int(self.cigBestCat(v['c_ceiling_bestCat']))
        except ValueError:
            pass
        # Probability of visibility categories including best category
        g['vprob'] = _getVisProbs(v,'vis_cat',6)
        g['cvprob'] = _getVisProbs(v,'cvis_cat',6)
        try:
            g['vis_bestCat'] = int(self.visBestCat(v['vis_bestCat']))
            g['cvis_bestCat'] = int(self.visBestCat(v['cvis_bestCat']))
        except ValueError:
            pass
        return g


def tafPartToAAGData(tafPart, fcstType):
    aagData = None
    if fcstType in tafPart:
        aagData = AAGData()
        pcpObv = ""
        if 'time' in tafPart[fcstType]:
            aagData.setTimeFromSeconds(tafPart[fcstType]['time']['from'])
            aagData.setTimeToSeconds(tafPart[fcstType]['time']['to'])
        if 'vsby' in tafPart[fcstType]:
            aagData.setVisibility(tafPart[fcstType]['vsby']['str'])
        if 'wind' in tafPart[fcstType]:
            aagData.setWind(tafPart[fcstType]['wind']['str'])
        if 'sky' in tafPart[fcstType]:
            aagData.setSky(tafPart[fcstType]['sky']['str'])
        if 'pcp' in tafPart[fcstType]:
            pcpObv += tafPart[fcstType]['pcp']['str']
        if 'obv' in tafPart[fcstType]:
            if pcpObv != "":
                pcpObv += " "
            pcpObv += tafPart[fcstType]['obv']['str']
        if pcpObv != "":
            aagData.setWeather(pcpObv)
    return aagData


def getAAGData(siteID, pdc):
    data = _GfsLampData().makeData(pdc, siteID)
    tafParts = AAGTafGen(data).formNewDic(False)
    aagDatas = []
    for tafPart in tafParts:
        # If not one of "prev","ocnl" then it is junk, ignore it
        for fcstType in ('ocnl', 'prev'):
            aagData = tafPartToAAGData(tafPart, fcstType)
            if aagData:
                if fcstType == 'ocnl':
                    aagData.setForecastType(tafPart['ocnl']['type'])
                elif fcstType == 'prev':
                    aagData.setForecastType('FM')
                aagDatas.append(aagData)
    return aagDatas
