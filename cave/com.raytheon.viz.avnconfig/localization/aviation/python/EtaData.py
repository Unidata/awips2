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
#    Name:
#       EtaData.py
#       GFS1-NHD:A7794.0000-SCRIPT;1.12
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.12 (DELIVERED)
#         Created:  01-MAR-2007 11:43:52      OBERFIEL
#           Replace occurrences of ETA to NAM-WRF
#       
#       Revision 1.11 (DELIVERED)
#         Created:  06-SEP-2005 20:17:40      TROJAN
#           spr 7014
#       
#       Revision 1.10 (DELIVERED)
#         Created:  06-SEP-2005 19:09:57      TROJAN
#           spr 7009
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-JUL-2005 12:36:34      TROJAN
#           spr 6912
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:32:51      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  04-APR-2005 15:51:05      TROJAN
#           spr 6775
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-MAR-2005 22:51:39      TROJAN
#           spr 6703
#       
#       Revision 1.5 (APPROVED)
#         Created:  15-FEB-2005 18:12:20      TROJAN
#           spr 6561
#       
#       Revision 1.4 (DELIVERED)
#         Created:  27-JAN-2005 18:35:21      BLI
#           Modified to use new taf formatter
#       
#       Revision 1.3 (APPROVED)
#         Created:  24-JAN-2005 15:51:13      TROJAN
#           spr 6259
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 20:22:10      TROJAN
#           stdr 873
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:39:27      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7241
#       	Action Date:       20-MAR-2007 09:50:26
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Update installation and staging script for OB8.1
#       
#
import cPickle, itertools, logging, math, os, time
import numpy
import pupynere
import Avn, AvnLib

ToKt = 3600.0/1852.0
ToDeg = 180.0/math.pi
ToSm = 1.0/1609.0
ToFt = 1.0/0.305
ToIn = 1.0/25.4
Rd_g2 = 287.05/9.80616/2.0

#_Logger = logging.getLogger(__name__)
import NoDataException
_Logger = logging.getLogger(Avn.CATEGORY)

# All available parameters
#PARAMETERS = ["maxLevels", "stationId", "refTime", "forecastHr", "staLat",
#              "staLon", "staElev", "staName", "wmoStaNum", "refTime",
#              "validTime", "landSea", "sfcPress", "seaLvlPress", "lowCld",
#              "midCld", "hiCld", "prCloud", "vsby", "uStorm", "vStorm", 
#              "srHel", "skinTemp", "minTemp", "maxTemp", "sensHeat",
#              "subSfcHeat", "snowFlux", "totPrecip", "convPrecip", "snowFall",
#              "snowWater", "snowMelt", "u10", "v10", "Theta10", "q10", "temp2",
#              "q2", "snowTyp", "iceTyp", "frzgRainTyp", "rainType", 
#              "numProfLvls", "pressure", "temperature", "specHum", "omega",
#              "uComp", "vComp", "cldCvr"]

# Parameters in use
PARAMETERS = ["temp2", "q2", "u10", "v10", "vsby", "totPrecip",
              "snowTyp", "iceTyp", "frzgRainTyp", "rainType", "pressure",
              "temperature", "cldCvr", "prCloud"]

###############################################################################
def _groupToText(g):
    d = {}
    try:
        d['TMP'] = '%2.0f' % (g['temp']['tt']*1.8 + 32.0)
    except KeyError:
        d['TMP'] = ''
    try:
        d['DPT'] = '%2.0f' % (g['temp']['td']*1.8 + 32.0)
    except KeyError:
        d['DPT'] = ''
    try:
        d['WDR'] = '%03.0f' % g['wind']['dd']
    except KeyError:
        d['WDR'] = ''
    try:
        d['WSP'] = '%03.0f' % g['wind']['ff']
    except KeyError:
        d['WSP'] = ''
    try:
        d['VIS'] = '%.1f' % min(g['vsby']['value'], 9.9)
    except KeyError:
        d['VIS'] = ''
    try:
        s = g['pcp']['str']
        if 'FZ' in  s:
            d['PTYPE'] = 'FZ'
        elif 'PL' in  s:
            d['PTYPE'] = 'PL'
        elif 'SN' in  s:
            d['PTYPE'] = 'SN'
        elif 'RA' in  s:
            d['PTYPE'] = 'RA'
        else:
            d['PTYPE'] = ''
    except KeyError:
        d['PTYPE'] = ''
    try:
        d['PAMT'] = '%03d' % g['pcp']['amt']
    except KeyError:
        d['PAMT'] = ''
    for n in range(1,4):
        d['CLD%d'%n] = ''
        d['HGT%d'%n] = ''
    try:
        sky = g['sky']['str'].split()
        for n, s in enumerate(sky):
            if s == 'SKC':
                d['CLD%d'%(n+1)] = 'SKC'
                break
            else:
                d['CLD%d'%(n+1)] = s[:3]
                d['HGT%d'%(n+1)] = s[3:]
    except (KeyError, IndexError):
        pass
    return d

###############################################################################
def _makeTemp(v):
    d = {}
    tt = float(v['temp2'])
    if tt != v.getFillValue('temp2'):
        d['tt'] = tt - 273.15
    q = float(v['q2'])
    if q != v.getFillValue('q2'):
        # Check to prevent divide by 0.
        if q > 0.0:
            # formula 2.28 in "Short Course in Cloud Physics", R.R. Rogers,
            # M.K. Yau
            d['td'] = 5.42e3/math.log(2.53e11*0.622/(q*1.0e5)) - 273.15
        else:
            # Skip calculation and made very low value but valid Celsius value.
            d['td'] = -100.0
    return d

def _makePcp(v):
    tp = float(v['totPrecip'])
    if tp == v.getFillValue('totPrecip'):
        tp = 0
    st = int(v['snowTyp'])
    if st == v.getFillValue('snowTyp'):
        st = 0
    it = int(v['iceTyp'])
    if it == v.getFillValue('iceTyp'):
        it = 0
    ft = int(v['frzgRainTyp'])
    if ft == v.getFillValue('frzgRainTyp'):
        ft = 0
    rt = int(v['rainType'])
    if rt == v.getFillValue('rainType'):
        rt = 0
    plst = []
    if ft == 1:
        plst.append('FZRA')
    if rt == 1 and ft == 0:
        plst.append('RA')
    if st == 1:
        plst.append('SN')
    if it == 1:
        plst.append('PL')
    s = ''.join(plst)
    if not s:
        return {}
    intensity = ''
    if rt == 1:
        if 0 < tp < 25:
            intensity = '-'
        elif tp > 75:
            intensity = '+'
    return {'str': intensity+s, 'amt': 100*tp*ToIn,'pop':99}

def _makeSky(nLevels, v):
    _Cld = ['SKC', 'FEW', 'SCT', 'BKN', 'OVC']
    base_pres = float(v['prCloud'])
    if not 0.0 <= base_pres:
        base_pres = None
    def _makeHgt(p, t, c):
        # no checks for missing data
        h = 0.0
        base = base_pres
        height = [h]
        for (p0, t0, c0), (p1, t1, c1) in \
            Avn.window(itertools.izip(p, t, c)):
            h += Rd_g2 * (t1+t0) * math.log(p0/p1)
            height.append(h*ToFt)
        return height[:nLevels]

    def _map(c):
        # returns sky cover code given percentage
        if c < 12:
            return 0
        elif c < 25:
            return 1
        elif c < 50:
            return 2
        elif c < 90:
            return 3
        else:
            return 4

    hgt = _makeHgt(v.getNumberAllLevels('pressure')[:nLevels], v.getNumberAllLevels('temperature')[:nLevels],
        v.getNumberAllLevels('cldCvr')[:nLevels]) 
#   hgt = _makeHgt(pressure[n,:nLevels], temperature[n,:nLevels])   
    cvr = [_map(c) for c in v.getNumberAllLevels('cldCvr')]
    total = 0
    last = cvr[0]
    sky = []
    for h, c in zip(hgt[1:], cvr[1:]):
        if h > 25000:
            break
        if c <= last:
            continue
        if c == total and total > 0:    # same cloud cover
            if h > 1.2 * sky[-1][0]:
                sky.append((h, c))
        elif c > total:
            try:
                if h < 1.2 * sky[-1][0]:
                    sky.pop()
            except IndexError:
                pass
            sky.append((h, c))
            if c == 4:
                break
            total = c
        last = c
    if not sky:
        return {'str': 'SKC', 'cover': 0, 'cig': Avn.UNLIMITED}
    s = ['%s%03d' % (_Cld[c], AvnLib.fixCldBase(h/100)) for h, c in sky]
    cover = sky[-1][1]
    for h, c in sky:
        if c >= 3:
            cig = h
            break
    else:
        cig = Avn.UNLIMITED
    return {'str': ' '.join(s), 'cover': cover, 'cig': cig}

def _makeVsby(vv):
    v = float(vv['vsby'])
    if v == vv.getFillValue('vsby'):
        return {}
    v *= ToSm
    return AvnLib.fixTafVsby(v)

def _makeObv(v):
    if v < 0.6:
        return {'str': 'FG'}
    elif v < 6.5:
        return {'str': 'BR'}
    else:
        return {}

def _makeWind(vv):
    d = {}
    u, v = float(vv['u10']), float(vv['v10'])
    if u == vv.getFillValue('u10') or v == vv.getFillValue('v10'):
        return d
    dd = 180+math.degrees(math.atan2(u, v))
    ff = ToKt*math.sqrt(u*u+v*v)
    if ff < 1.0:
        d['dd'], d['ff'] = 0, 0
    else:
        dd = int(10*((dd+5)//10))
        if dd == 0:
            dd = 360
        d['dd'], d['ff'] = dd, int(ff)
    d['str'] = '%03d%02dKT' % (d['dd'], d['ff'])
    return d

###############################################################################
class _NetCDFFile:
    MaxData = 36    # 36 hours
    NumData = MaxData
    MaxLevels = 40  # ~25000 ft
        
    def __init__(self):
        pass

    def __makePeriod(self, n, pdc):
        v = pdc[n]
        t = (pdc.refTime.getTime() / 1000) + (3600.0 * n)
        g = {'time': {'from': long(t), 'to': long(t+3600.0)}}
        d = _makeTemp(v)
        if d:
            g['temp'] = d
        d = _makeWind(v)
        if d:
            g['wind'] = d
        d = _makeVsby(v)
        if d:
            g['vsby'] = d
            d = _makeObv(d['value'])
            if d:
                g['obv'] = d
        d = _makePcp(v)
        if d:
            g['pcp'] = d
        d = _makeSky(self.MaxLevels, v)
        if d:
            g['sky'] = d
        #return {'prev': g}
        return g

    def close(self):
        try:
            self._fh.close()
        except:
            _Logger.error('Failed to close data file')

    def getFile(self, path):
        try:
            self._path = path
            self._fh = pupynere.NetCDFFile(self._path, 'r')
            var = self._fh.variables['staName']
            # get record numbers for all idents
            self._sitedict = {}
            for n, s in enumerate(var[:]):
                ident = s.tostring().split('\x00')[0].rstrip()
                try:
                    self._sitedict[ident].append(n)
                except KeyError:
                    self._sitedict[ident] = [n]
            # sort wrt valid time
            for i in self._sitedict:
                v = self._fh.variables['validTime']
                tmp = [(int(v[n]), n) for n in self._sitedict[i]]
                tmp.sort()
                self._sitedict[i] = [t[1] for t in tmp][:self.NumData+2]
            return True
        except IOError:
            _Logger.error('Error accessing %s', path)
            return False

    def makeData(self, ident, refTime=None):
#        print 'start EtaData.makeData, ident(%s), refTime(%s) - %s' % (ident, refTime, type(refTime))
#        try:
#            records = self._sitedict[ident]
#        except KeyError:
#            raise Avn.AvnError('%s not in %s' % (ident, self._path))
#        v = self._fh.variables['refTime']
#        itime = int(v[records[0]])

        import ForecastPointDataRetrieve
        self.Model = 'ETA'
        pdc = ForecastPointDataRetrieve.retrieve('modelsounding', ident, PARAMETERS, refTime=refTime, constraint={'reportType':self.Model})
        self.NumData = min(self.MaxData, len(pdc.keys()))
        keys = pdc.keys()
        keys.sort()
        keys = keys[:self.NumData]
        itime = long(pdc.refTime.getTime()) / 1000
        d = {'itime': {'value': itime, 'str': time.strftime('%d%H%MZ', \
            time.gmtime(itime))}, 'ident': {'str': ident}}
        d['group'] = [self.__makePeriod(n, pdc) for n in keys]
        return d

    def makeTable(self, data):
        itime = data['itime']['value']
        rpt = ['%s    DMO Guidance   %s' % (data['ident']['str'], 
            time.strftime('%x  %H%M UTC', time.gmtime(itime)))]
        #vtimes = [itime+3600.0*n for n in range(self.NumData)]
        vtimes = [g['time']['from'] for g in data['group']]
        rpt.append('hour     ' + ' '.join([time.strftime(' %H', \
            time.gmtime(t)) for t in vtimes]))
        tmp = [_groupToText(g) for g in data['group']]
        for k in ['TMP', 'DPT', 'WDR', 'WSP', 'VIS', 'PTYPE', 'PAMT', \
            'CLD1', 'HGT1', 'CLD2', 'HGT2', 'CLD3', 'HGT3']:
            tok = tuple([t[k] for t in tmp])
            rpt.append('%-8s' % k + '%+4s' * self.NumData % tok)
        rpt.append('')
        return rpt

###############################################################################
def _cleanup(path, nhours):
    tstamp = Avn.time2string(time.time()-nhours*3600.0)
    for f in os.listdir(path):
        if f[:10] < tstamp:
            fname = os.path.join(path, f)
            try:
                os.unlink(fname)
            except OSError:
                _Logger.exception('Cannot remove %s', fname)

###############################################################################
def retrieve(model, idlist, retrieveReport, refTime=None):
    nc = _NetCDFFile()
    ids = []
    for ident in idlist:
        try:
#            print 'EtaData.retrieve ident(%s), retrieveReport(%s), refTime(%s)' % (ident, retrieveReport, refTime)
            table = None
            data = nc.makeData(ident, refTime=refTime)
            tstamp = Avn.time2string(data['itime']['value'])
            if retrieveReport:
                table = nc.makeTable(data) # table view
            ids.append(Avn.Bunch(data=data, rpt=table))
            _Logger.info('Retrieved data for %s', ident)
        except Avn.AvnError, e:
            _Logger.error(str(e))
        except NoDataException.NoDataException, e:
            msg = [str(e)]
            ids.append(Avn.Bunch(data=msg, rpt=msg))
        except Exception, e:
            _Logger.exception('Unexpected error: %s', e)
    return ids
