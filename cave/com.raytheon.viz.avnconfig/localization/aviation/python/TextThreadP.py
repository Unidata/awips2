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
#       TextThreadP.py
#       GFS1-NHD:A8833.0000-SCRIPT;17
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 17 (DELIVERED)
#         Created:  15-AUG-2008 13:54:57      OBERFIEL
#           Fixed logic
#       
#       Revision 16 (DELIVERED)
#         Created:  01-AUG-2008 15:44:47      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 15 (REVIEW)
#         Created:  19-JUN-2008 14:33:24      GILMOREDM
#           Fixed code causing an error while testing for chiming
#       
#       Revision 14 (DELIVERED)
#         Created:  11-JUN-2008 14:53:55      OBERFIEL
#           Fixed CCFP logic to ignore LINE in file.
#       
#       Revision 13 (REVIEW)
#         Created:  16-MAY-2008 13:34:40      GILMOREDM
#           Added import of MetarDecoder
#       
#       Revision 12 (REVIEW)
#         Created:  15-MAY-2008 15:22:41      GILMOREDM
#           Changed so that if a new METAR is received with the same
#           timestamp, it is checked against the current METAR and if
#           there is any difference in the wind, wx, vis, or cig the
#           METAR is updated with the new values. This will hopefully
#           reduce the chiming when duplicate METARs are received.
#       
#       Revision 11 (DELIVERED)
#         Created:  17-MAR-2008 13:41:55      OBERFIEL
#           Fixed typo with _findCCFPArea function that stopped
#           monitoring of CCFP products.
#       
#       Revision 10 (DELIVERED)
#         Created:  06-MAR-2008 17:41:38      OBERFIEL
#           Fixed line-crossing bug that generated spurious alerts at
#           beign sites.
#       
#       Revision 9 (DELIVERED)
#         Created:  14-APR-2006 13:25:18      TROJAN
#           spr 7117
#       
#       Revision 8 (DELIVERED)
#         Created:  14-APR-2006 13:17:51      TROJAN
#           spr 7118
#       
#       Revision 7 (DELIVERED)
#         Created:  09-MAR-2006 13:41:55      TROJAN
#           spr 7105 - CCFP fix
#       
#       Revision 6 (DELIVERED)
#         Created:  09-MAR-2006 13:08:03      TROJAN
#           fixed handling of reports without convection
#       
#       Revision 5 (DELIVERED)
#         Created:  07-SEP-2005 12:47:29      TROJAN
#           spr 7011
#       
#       Revision 4 (DELIVERED)
#         Created:  29-AUG-2005 19:56:25      TROJAN
#           fixed Metar regular expression
#       
#       Revision 3 (APPROVED)
#         Created:  26-AUG-2005 18:10:53      TROJAN
#           spr 7006
#       
#       Revision 2 (APPROVED)
#         Created:  18-AUG-2005 15:32:19      TROJAN
#           spr 6993
#       
#       Revision 1 (DELIVERED)
#         Created:  10-JUL-2005 18:12:29      TROJAN
#           spr 6548
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7382
#       	Action Date:       11-OCT-2008 12:55:41
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Persistent chiming on AvnWatch
#       
#
import logging, os, Queue, re, shutil, stat, string, time
import Avn, AvnParser, MetarDecoder

_Logger = logging.getLogger(__name__)

##############################################################################
def _addmtrheader(kind, rpt):
    # needed to deal with collectives
    if rpt.startswith('METAR') or rpt.startswith('SPECI'):
        return rpt
    elif rpt and kind:
        # prepend METAR or SPECI
        return '%s %s' % (kind, rpt)
    else:
        return None

def _cleanup(direct, nhours):
    # removes files older than "nhours"
    cutoff = time.time() - nhours * 3600.0
    for f in os.listdir(direct):
        try:
            path = os.path.join(direct, f)
            if Avn.string2time(f) < cutoff:
                os.unlink(path)
                _Logger.info('Removed %s', path)
        except (ValueError, OSError):
            _Logger.exception('Cannot remove %s', path)

def _cleanupBad(nhours=6):
    # removes files from "bad" directory older than "nhours"
    direct = os.path.join('data', 'bad')
    cutoff = time.time() - nhours * 3600.0
    for f in os.listdir(direct):
        path = os.path.join(direct, f)
        if os.path.getmtime(path) < cutoff:
            try:
                os.unlink(path)
            except OSError:
                _Logger.exception('Cannot remove %s', path)

def _gettype(header):
    # returns forecast type, or "=" for routine forecasts
    try:
        t = header.split()[3][0]
        if t == 'R':
            t = '='
        return t
    except IndexError:
        return '='

def _mktstamp(day, hour, minute, year=None, month=None):
    # creates file name in a format used by AvnFPS
    ts = list(time.gmtime())
    if year and month:
        ts[:5] = int(year), int(month), int(day), int(hour), int(minute)
    else:
        ts[2:5] = int(day), int(hour), int(minute)
        if time.mktime(ts) > time.time() + 10800.0:
            if ts[1] > 1:
                ts[1] -= 1
            else:
                ts[0] -= 1
                ts[1] = 12
    return time.strftime('%y%m%d%H%M', ts)

def _mkCCFPReport(ident, vtime, area):
    vtime = time.strftime('%y%m%d%H00', time.gmtime(vtime))
    tops = 'TOPS %d' % area['tops']
    gwth = 'GWTH %d' % area['growth']
    conf = 'CONF %d' % area['conf']
    cvrg = 'CVRG %d' % area['coverage']
    return ' '.join([ident, vtime, tops, gwth, conf, cvrg])

def _findCCFPArea(xtaf, ytaf, arealist):
    # areas can overlap, select one with highest confidence, coverage
    def _match(area):
        inside=False
        for (x0, y0), (x1, y1) in Avn.window(area['latlon']):
            #
            # Make sure x1-x0 > 0
            if x1 < x0:
                (x1,y1),(x0,y0) = (x0,y0),(x1,y1)
            #
            # If segment endpoints lie on either side and
            # y intercept lies 'north' of TAF position, then
            # count it.
            #
            if (x1 < xtaf) == (xtaf <= x0) and ((y0-ytaf)*(x1-x0) > (x0-xtaf)*(y1-y0)):
                inside = not inside      
        return inside
    
    alist = filter(_match, arealist)
    if alist:
        tmp = [(a['conf'], a['coverage'], a) for a in alist]
        tmp.sort()
        return tmp[0][-1]
    else:
        return None

def _decodeCCFP(data):
    def _gettime(s):
        y, m, d, h = int(s[:4]), int(s[4:6]), int(s[6:8]), int(s[9:11])
        return time.mktime((y, m, d, h, 0, 0, 0, 0, 0))

    dcd = {}
    for line in data:
        if line.startswith('CCFP'):
            tok = line.split()
            dcd['itime'] = _gettime(tok[1])
            dcd['vtime'] = _gettime(tok[2])
        elif line.startswith('AREA'):
            tok = [int(x) for x in line.split()[1:]]
            d = {'coverage': tok[0], 'conf': tok[1], 'growth': tok[2], \
                'tops': tok[3], 'speed': tok[4], 'direction': tok[5]}
            d['latlon'] = Avn.pairs(tok[7:-2])
            dcd.setdefault('area', []).append(d)

    tmp = [(d['coverage'], d) for d in dcd.get('area', [])] 
    tmp.sort()
    tmp.reverse()
    dcd['area'] = [x[1] for x in tmp]   # sorted w.r. coverage, highest first
    return dcd

##############################################################################
class Server(object):
    """Processes TAFs, TWEBs, METARs and CCFP reports
Expects files from acqserver. File name determines data type.
"""
    _Tmout = 5.0    # minimum age of a file before processing (seconds)
    _Pat = re.compile(r'[=\n]\n', re.MULTILINE) # to split collective
    _Ident = string.maketrans('', '')   # needed to remove chars below
    _Delchars = '\003\r'    # characters to remove from data files
    _MtrRe = re.compile(r'(METAR |SPECI )?\w{4} .+', re.DOTALL)
    _TafRe = re.compile(r'[A-Z][A-Z0-9]{3} \d{6}Z .+', re.DOTALL)
    
    def __init__(self, inqueue, outqueue, info):
        self.inqueue = inqueue
        self.outqueue = outqueue
        self.datapath = info['source']
        self.numhours = int(info['nhours'])

        self._tafinfo = []
        self.metarDecoder = MetarDecoder.Decoder()

    def __writeFile(self, text, directory, fname, checkSize=True):
        # Writes files to AvnFPS data directory
        if not os.path.isdir(directory):
            os.mkdir(directory)
        else:
            _cleanup(directory, self.numhours)
        path = os.path.join(directory, fname)
        if checkSize and os.path.exists(path):
            size = os.stat(path)[stat.ST_SIZE]
            if size >= len(text):
                _Logger.info('Skipping possible duplicate %s', path)
                return None
        file(path, 'w').write(text)
    
    def dispose(self, rcode, directory, fname):
        # Removes files after processing
        # If there was an error, the file is copied to "bad" directory
        path = os.path.join(directory, fname)
        try:
            if rcode:
                shutil.copy2(path, os.path.join('data', 'bad'))
            os.unlink(path)
        except OSError:
            _Logger.exception('Cannot remove %s', path)

    def doMetar(self, bulletin):
        # Processes METARs from a collective
        try:
            for n in [1, 2, 3]:
                if self._MtrRe.match(bulletin[n]):
                    head = bulletin[:n]
                    break
            else:
                raise IndexError
        except IndexError:
            _Logger.warning('Cannot recognize %s', bulletin[0])
            return 1

        ids = self._ids['metar']
        if head[-1] in ['METAR', 'SPECI']:
            # a collective
            kind = head[-1]
        else:
            kind = ''
        mtrlist = filter(None, [_addmtrheader(kind, m) for m in \
            self._Pat.split('\n'.join(bulletin[n:])) if len(m) > 6])
        
        errflag = 0
        for mtr in mtrlist:
            try:
                ident, tstamp = mtr.split(None, 3)[1:3]
                if tstamp[0] in 'SN':   # SA, SP or NIL
                    continue
                if ids and not ident in ids:
                    continue
                
                day, hour, minute = tstamp[:2], tstamp[2:4], tstamp[4:6]
                fname = _mktstamp(day, hour, minute)
                directory = os.path.join('data', 'mtrs', ident)
                data = '\n'.join([bulletin[0], mtr, ''])
                
		if os.path.exists(os.path.join(directory,fname)):
                    #
                    # A file with this timestamp exists, let's compare it to the incoming
		    incomingMetarDecoded = self.metarDecoder(mtr)
		    previousMetar = file(os.path.join(directory,fname)).readlines()[1:]
		    previousMetarDecoded = self.metarDecoder(' '.join([el.strip() for el in previousMetar]))
                    #
                    # Compare aviation elements for differences
                    doWrite, doAlert = self.differentMetars(incomingMetarDecoded,
                                                            previousMetarDecoded)
                    if doAlert:
                        self.outqueue.put(Avn.Bunch(src='mtrs', ident=ident))     
                    if doWrite:
                        self.__writeFile(data, directory, fname, False)
                        _Logger.info('Processed %s from %s', ident, bulletin[0])
                else:
                    self.outqueue.put(Avn.Bunch(src='mtrs', ident=ident))     
                    self.__writeFile(data, directory, fname, False)
                    
            except Exception:
                _Logger.exception('Cannot process %s:\n%s', bulletin[0], mtr)
                errflag = 1
                
        return errflag

    def differentMetars(self, newmetar, oldmetar):
        """Compare new observation with the old and decide whether to write and alert"""
        
        write, alert = False, False
        #
        # Differences in the sky string insufficient for alerts
        for n,(element,attribute) in enumerate([('sky','str'),('sky','cig'),
                                                ('vsby','value'),('wind','str'),
                                                ('pcp','str'),('obv','str')]):
            if newmetar.has_key(element) == oldmetar.has_key(element):
                try:
                    if newmetar[element][attribute] != oldmetar[element][attribute]:
                        if n == 0:
                            write = True
                        else:
                            write = alert = True
                            _Logger.debug('Significant differences in %s METAR: (%s,%s)' %
                                          (newmetar['ident']['str'],
                                           oldmetar[element][attribute],
                                           newmetar[element][attribute]))
                            break

                except KeyError:
                    pass
            else:
                _Logger.debug('Significant differences found in %s METAR: %s' %
                              (newmetar['ident']['str'],element))
                write = alert = True
                break
        
        else:
            _Logger.debug('No significant differences found in %s METAR' %
                          newmetar['ident']['str'])
        return write, alert

    def doTaf(self, bulletin):
        # Processes TAFs from a collective
        try:
            for n in [1, 2, 3]:
                if self._TafRe.match(bulletin[n]):
                    head = bulletin[:n]
                    break
            else:
                raise IndexError
        except IndexError:
            _Logger.warning('Cannot recognize %s', bulletin[0])
            return 1
        taflist = [taf+[''] for taf in [t.split('\n') for t in \
            self._Pat.split('\n'.join(bulletin[n:]))] if taf[0]]
        ids = self._ids['taf']
        typ = _gettype(bulletin[0])
        errflag = 0
        for taf in taflist:
            try:
                ident, tstamp = taf[0].split(None, 2)[:2]
                if ids and not ident in ids:
                    continue
                day, hour, minute = tstamp[:2], tstamp[2:4], tstamp[4:6]
                fname = _mktstamp(day, hour, minute) + typ
                directory = os.path.join('data', 'tafs', ident)
                data = '\n'.join(head+taf)
                path = self.__writeFile(data, directory, fname)
                self.outqueue.put(Avn.Bunch(src='tafs', ident=ident))
                _Logger.info('Processed %s from %s', ident, bulletin[0])
            except Exception:
                _Logger.exception('Cannot process %s\n%s', head, taf)
                errflag = 1
        return errflag
        
    def doCCFP(self, bulletin):
        try:
            for n in [1, 2]:
                if bulletin[n][:4] == 'CCFP':
                    head = bulletin[:n]
                    break
            else:
                raise IndexError
        except IndexError:
            _Logger.warning('Cannot recognize %s', bulletin[0])
            return 1
        cfplist = [cfp+[''] for cfp in [t.split('\n') for t in \
            self._Pat.split('\n'.join(bulletin[n:]))] if cfp[0]]
        errflag = 0
        for cfp in cfplist:
            try:
                dcd = _decodeCCFP(cfp)
                year, month, day, hour = time.gmtime(dcd['itime'])[:4]
                fhour = int((dcd['vtime']-dcd['itime']+10.0)//3600.0)
                fname = _mktstamp(day, hour, 0, year, month) + ('+%02d' % fhour)
                directory = os.path.join('data', 'ccfp')
                report = [head[0]]
                for info in self._tafinfo:
                    xsi0, eta0 = info['lat']*10.0, -info['lon']*10.0
                    area = _findCCFPArea(xsi0, eta0, dcd['area'])
                    if area is None:
                        continue
                    report.append(_mkCCFPReport(info['id'], dcd['vtime'], area))
                report.append('')
                data = '\n'.join(report)
                self.__writeFile(data, directory, fname)
                self.outqueue.put(Avn.Bunch(src='ccfp', ident='ALL'))
                _Logger.info('Processed %s from %s', info['id'], head[0])
            except Exception:
                _Logger.exception('Cannot process %s', cfp)
                errflag = 1
        return errflag

    def getSites(self):
        self._ids = AvnParser.getAllSiteIds()
        for ident in self._ids['taf']:
            info = AvnParser.getTafSiteCfg(ident)
            if info is None:
                continue
            lat = float(info['geography']['lat'])
            lon = float(info['geography']['lon'])
            d = {'id': ident, 'lat': lat, 'lon': lon}
            self._tafinfo.append(d)

    def run(self):
        # main loop
        _cleanupBad()
        self.onStartup()
        lasttime = time.time()
        while True:
            try:
                code, fname, direct = self.inqueue.get(True, self._Tmout)
                if code == 0:       # end thread
                    _Logger.info('Got exit request')
                    raise SystemExit
                self.processFile(code, direct, fname)
                now = time.time()
                if now > lasttime + 21600:  # every 6 hours
                    _cleanupBad()
                    lasttime = now
            except Queue.Empty:
                pass
            except Exception:
                _Logger.exception('Unexpected error')
                break
