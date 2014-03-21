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
#       TafQC.py
#       GFS1-NHD:A8832.0000-SCRIPT;16
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 16 (DELIVERED)
#         Created:  06-AUG-2009 18:40:20      OBERFIEL
#           Added cloud string for examination and evaluation.
#       
#       Revision 15 (REVIEW)
#         Created:  21-APR-2009 20:26:26      OBERFIEL
#           Updated to include wind direction,speed and gusts
#           in expressions to be evaluated.
#       
#       Revision 14 (DELIVERED)
#         Created:  29-OCT-2008 12:55:29      OBERFIEL
#           Missing climate files does not halt QC checks.  Debug
#           statements added to log file.
#       
#       Revision 13 (DELIVERED)
#         Created:  01-OCT-2008 22:06:55      OBERFIEL
#           Removed call to data request server
#       
#       Revision 12 (DELIVERED)
#         Created:  16-SEP-2008 11:35:02      GILMOREDM
#           Refreshed changes where needed for TafQC
#       
#       Revision 11 (REVIEW)
#         Created:  10-JUN-2008 11:44:09      GILMOREDM
#           Fixed curWx definition to accept 2 arguments
#       
#       Revision 10 (REVIEW)
#         Created:  22-MAY-2008 10:06:41      GILMOREDM
#           Fixed code related to latest change
#       
#       Revision 9 (REVIEW)
#         Created:  15-MAY-2008 15:20:34      GILMOREDM
#           Now handles QC configuration
#       
#       Revision 8 (DELIVERED)
#         Created:  04-MAY-2006 14:16:17      TROJAN
#           SPR7125: fixed weather check in TafQC, changes to
#           checkTEMPO() in MetarMonitor
#       
#       Revision 7 (DELIVERED)
#         Created:  04-MAY-2006 14:02:11      TROJAN
#           SPR 7126: fixed weather check in TafQC, changes to
#           checkTEMPO() in MetarMonitor
#       
#       Revision 6 (DELIVERED)
#         Created:  21-OCT-2005 19:34:49      TROJAN
#           spr 7046
#       
#       Revision 5 (DELIVERED)
#         Created:  19-SEP-2005 13:46:06      TROJAN
#           spr 7012
#       
#       Revision 4 (APPROVED)
#         Created:  15-SEP-2005 18:23:13      TROJAN
#           spr 7026
#       
#       Revision 3 (DELIVERED)
#         Created:  08-AUG-2005 13:13:56      TROJAN
#           spr 6971
#       
#       Revision 2 (DELIVERED)
#         Created:  28-JUL-2005 21:53:22      TROJAN
#           beautified message formatting
#       
#       Revision 1 (APPROVED)
#         Created:  10-JUL-2005 18:06:56      TROJAN
#           spr 6911
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
#   21Mar2014    #2925       lvenable    Fixed global name not defined error in run() method.
#
import itertools, logging, math, os, sets, time
import ConfigParser
import numpy, pupynere
import Avn, AvnLib, AvnParser, Globals, MetarMonitor
import UFStatusHandler

_Logger = logging.getLogger(Avn.CATEGORY)
_MetarMonitorDict = None

ConfigDir = 'etc'
try:
    from com.raytheon.uf.common.localization import PathManagerFactory
    from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType, LocalizationContext_LocalizationLevel as LocalizationLevel
    pathMgr = PathManagerFactory.getPathManager()
    ctx = pathMgr.getContext(LocalizationType.valueOf('CAVE_STATIC'), LocalizationLevel.valueOf('BASE'))
    ConfigDir = pathMgr.getFile(ctx, os.path.join('aviation', 'config')).getPath()               
except:
    _Logger.exception("Error determining AvnFPS config directory")
    pass 

###############################################################################
# weather check
def curWx(tafs, items):
    result = {}
    for taf in tafs:
        
        if items[taf['ident']['str']]['currentwx'] == 0:
            continue            
        if not taf['group']:
            continue
        
        try:
            _Logger.debug('curWx QC invoked for %s' % taf['ident']['str'])
            hourly = AvnLib.TafData(taf['group'])
            info = AvnParser.getTafSiteCfg(taf['ident']['str'])
            metar_monitor = MetarMonitor.Monitor(info, _MetarMonitorDict)
            r = metar_monitor.compare(Avn.Bunch(hourly=hourly, dcd=taf))
            msg = [r['status'][i].msg for i in _MetarMonitorDict['items'][1:] \
                if r['status'][i].severity > 1]
            index = AvnLib.getGroupIndex(taf['group'][0]['prev'])
            if msg:
                result[index] = {'level': 1, 'text': '\n'.join(msg)}
            else:
                result[index] = {'level': 0, 'text': 'OK'}
        except:
            _Logger.error('curWx() failed for %s',taf['ident']['str'])
            pass
        
    return result

###############################################################################
# impact check
def _getCriteria(ident):
    fname = Avn.getTafPath(ident, 'impact.cfg')
    cp = ConfigParser.SafeConfigParser()
    cp.read(fname)
    clist = AvnParser.split(cp.get('conditions', 'items'))
    criteria = [dict(cp.items(c)) for c in clist]
    for d in criteria:
        d['level'] = int(d['level'])
    return criteria

def _impact(prevp, p, siteinfo, criteria):
    def _isTS():
        return 'pcp' in p and 'TS' in p['pcp']['str']
    def _wind(alpha):
        wind = Avn.Bunch(shift=False, runway=0, cross=0, dd=0, ff=0, gg=0)
        w = p.get('wind', {})
        dd, ff, gg = w.get('dd'), w.get('ff', 0), w.get('gg', 0)
        if dd is None or dd == 0:
            return wind
        elif dd == 'VRB':
            wind.ff = wind.runway = wind.cross = ff
            wind.gg = gg
            return wind        
        wind.dd, wind.ff, wind.gg = dd, ff, gg
        if alpha <= 0:
            return wind
        fi = math.radians(alpha-dd)
        rff, xff = math.cos(fi), math.sin(fi)
        wind.runway, wind.cross = ff*abs(rff), ff*abs(xff)
        if prevp is None:
            return wind
        pw = prevp.get('wind', {})
        pdd, pff = pw.get('dd'), pw.get('ff')
        if pdd is None or pdd == 'VRB' or pff == 0:
            return wind
        prff = math.cos(math.radians(alpha-pdd))
        wind.shift = rff * prff < 0
        return wind
    d = {}
    try:
        vsby = p['vsby']['value']
        cig = p['sky']['cig']
        skystr = p['sky']['str']
        ts = _isTS()
        wind = [_wind(alpha) for alpha in siteinfo['geography']['runway']]
        for cri in criteria:
            try:
                if eval(cri['expr']):
                    level = cri['level']
                    tag = cri['tag']
                    if tag not in d or d[tag]['level'] < level:
                        d[tag] = {'text': cri['text'], 'level': level}
            except Exception, e:
                msg = 'Cannot evaluate %s:\nerror %s' % (cri['expr'], str(e))
                d[cri['tag']] = {'text': msg, 'level': 3}
                _Logger.exception(msg)
    except KeyError:
        pass
    if not d:
        return {'level': 0, 'text': 'OK'}
    level = max([d[tag]['level'] for tag in d])
    text = '\n'.join([d[tag]['text'] for tag in d])
    return {'level': level, 'text': text}

def impact(qcdata, siteinfo, items):
    result = {}
    for ident, periods in qcdata:
        if items[ident]['impact'] == 0:
            continue
        try:
            _Logger.debug('impact QC invoked for %s' % ident)
            cri = _getCriteria(ident)
            if not cri or not periods:
                continue
            ix, p = periods[0]
            d = _impact(None, p, siteinfo[ident], cri)
            if d:
                result[ix] = d
            for (ix0, p0), (ix, p) in Avn.window(periods):
                d = _impact(p0, p, siteinfo[ident], cri)
                if d:
                    result[ix] = d
        except:
            _Logger.error('impact() failed for %s',ident)
            pass
        
    return result

###############################################################################
# climate check
# dimension indices 
DD, FF, OBV, INT, PCP, CIG, VSBY = range(7)

def _analyze(ncfh, off, details):
    counter = {'1': {}, 'n-1': {}} 
    counter['#'] = ncfh.variables['total'][0]
    var = ncfh.variables['all']
    if var[off] == 0:
        return 0.0, ['No similar events in the database'], off
    counter['n'] = var[off]
    counter['1']['dd'] = ncfh.variables['dd'][off[0]]
    counter['n-1']['dd'] = sum(var[:,off[1],off[2],off[3],off[4],off[5],
        off[6]])
    counter['1']['ff'] = ncfh.variables['ff'][off[1]]
    counter['n-1']['ff'] = sum(var[off[0],:,off[2],off[3],off[4],off[5],
        off[6]])
    counter['1']['obv'] = ncfh.variables['obv'][off[2]]
    counter['n-1']['obv'] = sum(var[off[0],off[1],:,off[3],off[4],off[5],
        off[6]])
    counter['1']['int'] = ncfh.variables['int'][off[3]]
    counter['n-1']['int'] = sum(var[off[0],off[1],off[2],:,off[4],off[5],
        off[6]])
    counter['1']['pcp'] = ncfh.variables['pcp'][off[4]]
    counter['n-1']['pcp'] = sum(var[off[0],off[1],off[2],off[3],:,off[5],
        off[6]])
    counter['1']['cig'] = ncfh.variables['cig'][off[5]]
    counter['n-1']['cig'] = sum(var[off[0],off[1],off[2],off[3],off[4],:,
        off[6]])
    counter['1']['vsby'] = ncfh.variables['vsby'][off[6]]
    counter['n-1']['vsby'] = sum(var[off[0],off[1],off[2],off[3],off[4],
        off[5],:])
    if details:
        output = ['Occurences']
        output.append('total:\t%d' % counter['#'])
        output.append('n:\t%d' % counter['n'])
        output.append('n-1:\t%s' % str(counter['n-1']))
        output.append('1:\t%s' % str(counter['1']))
        output.append('Probabilities')
    else:
        if counter['n'] == 1:
            output = ['%d match for %d events in database' % \
                (counter['n'], counter['#'])]
        else:
            output = ['%d matches for %d events in database' % \
                (counter['n'], counter['#'])]
    alpha = [0.0]*len(counter['1'])
    for n, i in enumerate(counter['1']):
        pcond = counter['n']/float(counter['n-1'][i])
        prob = counter['1'][i]/float(counter['#'])
        alpha[n] = pcond/prob
        if details:
            output.append('P(%4s|other) = %.3f\tP(%4s) = %.3f' % 
                (i, pcond, i, prob))
    output.append('')
    alpha.sort()
    return alpha[0], output, off

def _getLevel(alpha, threshold):
    if alpha >= threshold:
        return 0
    elif alpha > 0.0:
        return 1
    else:
        return 2

def _getOffset(cfg, period):
    offObv = 0
    if 'obv' in period:
        obv = period['obv']['str']
        if 'HZ' in obv:
            offObv |= 1
        if 'BR' in obv or 'FG' in obv:
            offObv |= 2
        if 'BL' in obv:
            offObv |= 4
    offInt, offPcp = 0, 0
    if 'pcp' in period:
        pcp = period['pcp']['str']
        if '-' in pcp:
            offInt = 1
        elif '+' in pcp:
            offInt = 3
        else:
            offInt = 2
        if 'SN' in pcp:
            offPcp = 1
        elif 'DZ' in pcp:
            offPcp = 2
        else:
            offPcp = 3
    offDD, offFF = Avn.wind_category(period['wind']['dd'], 
        period['wind']['ff'], cfg['dd'], cfg['ff'])
    offVsby = Avn.category(period['vsby']['value'], cfg['vsby'])
    offCig = Avn.category(period['sky']['cig'], cfg['cig'])
    return offDD, offFF, offObv, offInt, offPcp, offCig, offVsby

def _neighbors(ncfh, off):
    var = ncfh.variables['all']
    ret = []
    def neighbor_of(elem, ix):
        if off[ix] > 0:
            tmpoff = list(off[:])
            tmpoff[ix] -= 1
            # fix special cases: no precipitation and calm wind
            if elem == 'int':
                if tmpoff[ix] == 0:
                    tmpoff[PCP] = 0
            elif elem == 'ff':
                if tmpoff[ix] == 0:
                    tmpoff[DD] = 0
            toff = tuple(tmpoff)
            num_n = var[toff]
            if num_n > 0:
                ret.append(toff)
        if off[ix] < ncfh.dimensions[elem]-1:
            tmpoff = list(off[:])
            tmpoff[ix] += 1
            toff = tuple(tmpoff)
            num_n = var[toff]
            if num_n > 0:
                ret.append(toff)
    neighbor_of('int', INT)
    neighbor_of('cig', CIG)
    neighbor_of('vsby', VSBY)
    neighbor_of('ff', FF)
    if off[DD] > 0:   # special case for wind direction
        tmpoff = list(off[:])
        tmpoff[DD] -= 1
        if tmpoff[DD] == 0:
            tmpoff[DD] = ncfh.dimensions['dd']-1
        toff = tuple(tmpoff)
        num_n = var[toff]
        if num_n > 0:
            ret.append(toff)
        tmpoff = list(off[:])
        tmpoff[DD] += 1
        if tmpoff[DD] == ncfh.dimensions['dd']:
            tmpoff[DD] = 1
        toff = tuple(tmpoff)
        num_n = var[toff]
        if num_n > 0:
            ret.append(toff)
    return ret

def _suggest(off, toff):
    i = toff[FF] - off[FF]
    if i != 0:
        if toff[FF] == 0:
            return 'Try calm wind'
        elif i > 0:
            return 'Increase wind speed'
        else:
            return 'Decrease wind speed'
    i = toff[DD] - off[DD]
    if i == 1 or i < -1:
        return 'Veer wind direction'
    elif i == -1 or i > 1:
        return 'Back wind direction'
    i = toff[INT] - off[INT]
    if i != 0:
        if toff[INT] == 3:
            return 'Try heavy precipitation'
        elif toff[INT] == 2:
            return 'Try moderate precipitation'
        elif toff[INT] == 1:
            return 'Try light precipitation'
    i = toff[CIG] - off[CIG]
    if i > 0:
        return 'Try higher ceiling'
    elif i < 0:
        return 'Try lower ceiling'
    i = toff[VSBY] - off[VSBY]
    if i > 0:
        return 'Try better visibility'
    elif i < 0:
        return 'Try worse visibility'
    return ''

def _climate(ident, periods, dataDir):
    
    result = {}
    try:
        month = time.gmtime(periods[0][1]['time']['from']).tm_mon
        path = os.path.join(dataDir, '%s.%02d.nc' % (ident, month))
        if not os.path.isfile(path):
            raise Avn.AvnError('File %s does not exist' % path)
        ncfh = pupynere.NetCDFFile(path)
        cfg = AvnParser.getClimQCConfig(ident)
    except IndexError:
        return result
    except IOError:
        raise IOError, 'Cannot access %s' % path

    for ix, p in periods:
        off = _getOffset(cfg, p)
        a0, text, off = _analyze(ncfh, off, cfg['showdetails'])
        level = _getLevel(a0, cfg['alpha'])
        if a0 < cfg['alpha']:
            suggestions = [_analyze(ncfh, toff, cfg['showdetails']) \
                for toff in _neighbors(ncfh, off)]
            suggestions.sort()
            suggestions.reverse()
            best = filter(None, [_suggest(off, x[2]) for x in suggestions \
                if x[0] >= cfg['alpha']])
            if best:
                text.extend(best)
            else:
                text.append('Cannot figure it out')
                
        result[ix] = {'level': level, 'text': '\n'.join(text)}
        
    ncfh.close()    
    return result

def climate(qcdata, items, dataDir):
    result = {}
    for ident, periods in qcdata:
        if items[ident]['climate'] == 0:
            continue
        try:
            _Logger.debug('climate QC invoked for %s' % ident)
            result.update(_climate(ident, periods, dataDir))
        except IOError, msg:
            if 'error' in result:
                result['error']['text'] += '\n%s' % msg.args[0]
            else:
                result['error'] = {'level':0, 'text': msg.args[0]}
        except Exception, e:
            _Logger.error('climate() failed for %s',ident)
            pass
        
    return result

###############################################################################
def run(tafs, siteinfo, items, dataDir):
    import JUtil
    from com.raytheon.viz.aviation.xml import TafMonitorCfg, MonitorCfg
     
    global _MetarMonitorDict
    label = {'curwx': 'Current WX', 'impact': 'Impact', 'clim': 'Climate'}
    qcdata = [(taf['ident']['str'], AvnLib.getTafPeriods(taf)) for taf in tafs]
#    if _MetarMonitorDict is None:
#        _MetarMonitorDict = {'tempograceperiod': 3600.0, 'menu': 'METAR', 'labels': {'sky': 'cig', 'tempo': 'tpo', 'wind': 'wnd', 'vsby': 'vis', 'wx': 'wx'}, 'module': 'MetarMonitor', 'items': ['tempo', 'vsby', 'wind', 'wx', 'sky']}
    
    d = {}
    path = os.path.join(ConfigDir, 'gui', 'TafMonitorCfg.xml')
    if not os.path.isfile(path):
        raise Avn.AvnError('File %s does not exist' % path)
        return None    
    tafMonCfg = TafMonitorCfg.unmarshal(path)
    jMonCfgs = tafMonCfg.getMonitorCfgs()
    monCfgs = []
    for i in range (jMonCfgs.size()):
        monCfgs.append(jMonCfgs.get(i))
    
    for monCfg in monCfgs:
        className = monCfg.getClassName()
        if className == 'MetarMonitor':
            monItems = [s.strip() for s in monCfg.getMonitorItems().split(',')]
            labels = [s.strip() for s in monCfg.getMonitorLabels().split(',')]
            d['labels'] = {}
            d['module'] = className
            d['menu'] = 'METAR'
            d['tempograceperiod'] = 3600.0
            d['items'] = []
            for i in range(len(labels) - 1):
                d['items'].append(monItems[i])
                d['labels'][labels[i]] = monItems[i]
    _MetarMonitorDict = d
        
    d = {}
    d['curwx'] = curWx(tafs, items)
    d['impact'] = impact(qcdata, siteinfo, items)
    d['clim'] = climate(qcdata, items, dataDir)
    
    keys = sets.Set(itertools.chain(*[d[k].keys() for k in d]))
    result = {}
    for ix in keys:
        text = '\n\n'.join(['--- %s ---\n%s' % (label[k], d[k][ix]['text']) \
            for k in d if ix in d[k]])
        level = max([d[k][ix]['level'] for k in d if ix in d[k]])
        result[ix] = {'level': level, 'text': text}
    return result
###############################################################################
class TafQC():
    """TAFQC class"""
    def __call__(self, tafs, siteinfo, items, dataDir):
        """Runs the quality control procedure"""
        return run(tafs, siteinfo, items, dataDir)

##############################################################################
# java interface part ... added to support calling python from java

    def qcFromJava(self, text, sites, items, bbb, dataDir):
        import JUtil
        import TafDecoder
        
        # tafs
        # convert the ArrayList of tafs into a python list
        fcsts = []
        size = text.size()
        for i in range(size):
            fcsts.append(str(text.get(i)))
        
        # decode the tafs for use in the QC check
        taf_decoder = TafDecoder.Decoder()
        pytafs = []
        offset = 0
        for fcst in fcsts:
            taf = taf_decoder(fcst, bbb, offset, strict=True)
            offset += fcst.count('\n')+1
            if 'fatal' in taf:
                continue
            pytafs.append(taf)
            
            
        # convert the siteinfo Hashmap into python dictionary
        pysiteinfo = {}
        itr = sites.iterator()
        while itr.hasNext():  
            key = itr.next()
            ident = str(key)
            pysite = AvnParser.getTafSiteCfg(ident)
            pysiteinfo[ident] = pysite
            
        
        # convert the items HashMap into a python dictionary
        pyitems = {}
        keys = items.keySet()
        itr = keys.iterator()
        while itr.hasNext():
            key = itr.next()
            val = items.get(key)
            wx = eval(str(val.get('currentwx')))
            climate = eval(str(val.get('climate')))
            impact = eval(str(val.get('impact')))
            pyitem = {}
            pyitem['currentwx'] = wx
            pyitem['climate'] = climate
            pyitem['impact'] = impact
            pyitems[str(key)] = pyitem
        
        result = self(pytafs, pysiteinfo, pyitems, dataDir)

        return JUtil.pyValToJavaObj(result)
