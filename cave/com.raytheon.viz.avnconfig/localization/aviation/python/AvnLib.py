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
#       AvnLib.py
#       GFS1-NHD:A6626.0000-SCRIPT;1.45
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.45 (DELIVERED)
#         Created:  05-NOV-2009 20:42:46      OBERFIEL
#           makeTempo() function 'promotes' all items to 
#           prevailing conditions in dictionary.
#       
#       Revision 1.44 (DELIVERED)
#         Created:  25-SEP-2009 14:30:06      OBERFIEL
#           Change to support hacked code.
#       
#       Revision 1.43 (DELIVERED)
#         Created:  24-AUG-2009 14:35:19      OBERFIEL
#           Added code to support "CB not in TAF" when evaluating CCFP
#           guidance.
#       
#       Revision 1.42 (DELIVERED)
#         Created:  20-JUL-2009 10:02:28      GILMOREDM
#           Fixed error that gave wrong flight cat when vsby or cig was
#           missing
#       
#       Revision 1.41 (DELIVERED)
#         Created:  22-APR-2009 19:30:15      OBERFIEL
#           Added exception handling when BUFR model data is being
#           processed.
#       
#       Revision 1.40 (REVIEW)
#         Created:  20-MAR-2009 18:22:14      OBERFIEL
#           Removed code cruft. ETA changed to NAM. NGMMOS removed.
#       
#       Revision 1.39 (UNDER WORK)
#         Created:  17-MAR-2009 13:33:51      GILMOREDM
#           Code now properly checks TEMPO wx
#       
#       Revision 1.38 (DELIVERED)
#         Created:  31-DEC-2008 10:14:27      OBERFIEL
#           Changes to support amending TAFs prior to valid period.
#       
#       Revision 1.37 (DELIVERED)
#         Created:  31-OCT-2008 10:35:15      GILMOREDM
#           fixed issue in flightCategory
#       
#       Revision 1.36 (DELIVERED)
#         Created:  18-SEP-2008 13:34:09      OBERFIEL
#           Expanded grace period of the transmission window slightly
#       
#       Revision 1.35 (DELIVERED)
#         Created:  17-SEP-2008 20:33:32      OBERFIEL
#           Lengthened the grace period for regularly issued TAF a tiny
#           bit.
#       
#       Revision 1.34 (DELIVERED)
#         Created:  02-SEP-2008 13:08:50      OBERFIEL
#           Updated rule to account for 30-h length TAF and allow COR
#           on the first line
#       
#       Revision 1.33 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.32 (DELIVERED)
#         Created:  19-JUN-2008 14:18:09      OBERFIEL
#           Fixed getFmtValidTime to allow for 30-h header.
#       
#       Revision 1.31 (INITIALIZE)
#         Created:  10-JUN-2008 12:34:27      OBERFIEL
#           Fixed getFmtHeaderTime() routine and removed references to
#           TWEBs
#       
#       Revision 1.30 (DELIVERED)
#         Created:  14-MAR-2008 10:10:45      OBERFIEL
#           Adjusted visibility thresholds downward slightly in LIFR to
#           VLIFR categories
#       
#       Revision 1.29 (DELIVERED)
#         Created:  26-FEB-2008 14:25:35      OBERFIEL
#           Fixed _talk notification (unimplemented) and DTG for Nov
#           2008
#       
#       Revision 1.28 (DELIVERED)
#         Created:  25-MAY-2007 14:27:09      OBERFIEL
#           Update to support additional information in remarks
#       
#       Revision 1.27 (DELIVERED)
#         Created:  14-MAY-2007 10:04:48      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.26 (DELIVERED)
#         Created:  02-JUN-2006 10:28:08      TROJAN
#           spr 7160: changed regular expression for visibility
#       
#       Revision 1.25 (DELIVERED)
#         Created:  02-JUN-2006 09:04:21      TROJAN
#           spr 7159: changed regular expression for visibility
#       
#       Revision 1.24 (DELIVERED)
#         Created:  16-MAY-2006 10:36:40      TROJAN
#           spr 7148: fixes TEMPO without sky/visibility
#       
#       Revision 1.23 (DELIVERED)
#         Created:  09-MAY-2006 16:04:35      TROJAN
#           SPR 7145: fixes TEMPO without sky/visibility
#       
#       Revision 1.22 (DELIVERED)
#         Created:  09-MAR-2006 13:42:58      TROJAN
#           spr 7107: TEMPO elements fix
#       
#       Revision 1.21 (DELIVERED)
#         Created:  09-MAR-2006 12:48:49      TROJAN
#           fix in findIndex() for missing elements
#       
#       Revision 1.20 (DELIVERED)
#         Created:  13-FEB-2006 10:09:20      TROJAN
#           fix wrong indentantion of PROB groups
#       
#       Revision 1.19 (APPROVED)
#         Created:  23-JAN-2006 08:23:10      TROJAN
#           stdr 956
#       
#       Revision 1.18 (APPROVED)
#         Created:  12-OCT-2005 18:27:31      TROJAN
#           spr 7039
#       
#       Revision 1.17 (DELIVERED)
#         Created:  09-SEP-2005 13:53:20      TROJAN
#           spr 7011
#       
#       Revision 1.16 (DELIVERED)
#         Created:  16-AUG-2005 13:53:04      TROJAN
#           spr 6988
#       
#       Revision 1.15 (APPROVED)
#         Created:  09-AUG-2005 15:02:46      TROJAN
#           spr 6975
#       
#       Revision 1.14 (DELIVERED)
#         Created:  29-JUL-2005 18:55:09      TROJAN
#           spr 6956
#       
#       Revision 1.13 (APPROVED)
#         Created:  06-JUL-2005 21:01:41      TROJAN
#           spr 6909
#       
#       Revision 1.12 (DELIVERED)
#         Created:  12-MAY-2005 14:05:53      TROJAN
#           spr 6833
#       
#       Revision 1.11 (REVIEW)
#         Created:  07-MAY-2005 11:52:17      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.10 (DELIVERED)
#         Created:  04-APR-2005 15:20:32      TROJAN
#           spr 6780
#       
#       Revision 1.9 (APPROVED)
#         Created:  02-APR-2005 17:02:15      TROJAN
#           spr 6763
#       
#       Revision 1.8 (DELIVERED)
#         Created:  14-FEB-2005 20:54:46      TROJAN
#           spr 6649
#       
#       Revision 1.7 (APPROVED)
#         Created:  23-JAN-2005 19:05:30      TROJAN
#           spr 6586
#       
#       Revision 1.6 (APPROVED)
#         Created:  07-DEC-2004 14:13:59      TROJAN
#           spr 6521
#       
#       Revision 1.5 (APPROVED)
#         Created:  16-NOV-2004 20:12:09      PCMS
#           Restoring history
#       
#       Revision 1.4 (DELIVERED)
#         Created:  19-MAR-2004 18:32:39      TROJAN
#           spr 5922
#       
#       Revision 1.3 (DELIVERED)
#         Created:  15-JAN-2004 22:30:10      PCMS
#           Fixed AdjustTimes tool which didn't work in second half of
#           the month
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:39:50      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:19      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7432
#       	Action Date:       06-NOV-2009 08:30:19
#       	Relationship Type: In Response to
#       	Status:           NEXTRELEASE
#       	Title:             OB9.2 AvnFPS - TPO/FuelAlternate Rule Doesn't work
#       
#    Date          Ticket#       Engineer       Description
#    ----------    ----------    -----------    --------------------------
#    08/03/2015    17540         zhao           Modified to make default issue time configurable
#
import itertools, os, time
import Avn, Globals
import ConfigParser

# transmission times
_Fcst_Times = (6*3600, 12*3600, 18*3600, 24*3600)
_Xmit_Windows = (2400, 1200)

###############################################################################
# common functions used by forecast editors
###############################################################################
def _startHour(bbb, t):
    if bbb and bbb[0] != ' ':   # 1731Z -> 1818
        t += 1800.0

    tsec = t % 86400.0
    for i in range(len(_Fcst_Times)):
        if tsec < _Fcst_Times[i]:
            break
    else:
        i = 0

    if bbb and bbb[0] != ' ':   # not regular: previous period
        i -= 1
    return (_Fcst_Times[i] // 3600) % 24

##############################################################################
def getFmtHeaderTime(kind, bbb, t=None):
    # Returns header time string: one hour less than start of valid time
    if t is None:
        t = time.time()
    tms = list(time.gmtime(t))
    try:
        if bbb[0] != ' ':
            tms[4:6] = 0, 0
        else:
            raise IndexError
    except IndexError:
        hour = (_startHour(bbb, t) - 1) % 24
        thour = tms[3]
        if thour < hour - 12:       # previous day
            tms = list(time.gmtime(t-86400.0))
        elif thour > hour + 12:     # next day
            tms = list(time.gmtime(t+86400.0))
        tms[3:6] = hour, 0, 0
        
    return time.strftime('%d%H00', tuple(tms))

def getValidTime(kind, bbb, t=None):
    # returns start of valid time
    if t is None:
        t = time.time()
    starthour = _startHour(bbb, t)
    if bbb and bbb[0] != ' ':
       # amendments and delayed forecasts: use closest hour
       starthour = int(((t+1800.0)//3600))%24
    if starthour < time.gmtime(t)[3]:
        t += 86400.0
           
    tms = list(time.gmtime(t))
    tms[3:6] = starthour, 0, 0
    return Avn.mkgmtime(tms)

def getFmtValidTime(kind, bbb, t=None, tafDuration=24, evtime=None):
    # Returns valid time as yymmddHHhh for 'bbb'
    if t is None:
        t = time.time()
    #
    # Get the ending day and hour first
    tms = list(time.gmtime(t))
    tms[3:6] = _startHour(bbb, t), 0, 0
    if tms[3] == 0:
        try:
            if bbb[0] == ' ':
                tms[2] += 1
        except IndexError:
            tms[2] += 1

    tms = list(time.gmtime(Avn.mkgmtime(tms) + tafDuration*3600))
    endday,endhour = tms[2:4]
    if tms[3] == 0:
        tms[3] = -1
        endday,endhour = time.gmtime(Avn.mkgmtime(tms))[2],24
    #
    # for amendments and delayed forecasts: use closest hour
    if bbb and bbb[0] != ' ':
        starthour = int(((t+1800.0)//3600))%24
        #
        # If amendment needed before valid period starts (up to 40 minutes
        # prior (2400s)), preserve previous ending valid time
        #
        if evtime and (_Fcst_Times[0] - t%_Fcst_Times[0]) < 2400:
            endday,endhour = int(evtime[:2]),int(evtime[2:])
    #
    # Otherwise, find the next regular issuance time
    else:
        starthour = _startHour(bbb,t)
        
    tms = list(time.gmtime(t))
    if starthour < tms[3]:
        t += 86400.0
        tms = list(time.gmtime(t))

    tms[3:6] = starthour, 0, 0
    year, month, startday = tms[:3]
    
    return '%02d%02d%02d%02d/%02d%02d' % (year-2000, month, startday, starthour, endday, endhour)
        
def getIssueTime(kind, bbb, t=None):
    if t is None:
        t = time.time()
    if not bbb or bbb[0] == ' ':    # regular issue forecast
        itime = Avn.string2time('%s00' % getFmtValidTime(kind, bbb, t)[:8])
        
        minutesBeforeForecastTime = getMinutesBeforeForecastTime()
        
        if minutesBeforeForecastTime != None: 
            itime -= 60*int(minutesBeforeForecastTime)
        else:
            itime -= _Xmit_Windows[0]
            
        if itime > t:
            return itime
    return t

def getMinutesBeforeForecastTime():
    try:
        f = Avn.PATH_MGR.getStaticFile(Avn.ConfigDir)
        fname = f.getPath()
        fname = os.path.join(fname, 'default_issue_time.cfg')
        if not (os.path.exists(fname)):
            return None
        cp = ConfigParser.RawConfigParser()
        cp.read(fname)
        d = cp.get('minutesBeforeForecastTime', 'minutes')
        return d
    
    except Exception:
        raise

def getFmtIssueTime(kind, bbb, t=None):
    if t is None:
        t = time.time()
    if kind == 'taf':
        return time.strftime('%d%H%MZ', \
            time.gmtime(getIssueTime(kind, bbb, t)))
    else:
        raise Avn.AvnError, 'Programming bug'

# the next 2 functions use current time
def getXmitTime(kind, headertime):
    t = Avn.string2time('%s%s' % (Avn.time2string()[:6], headertime[2:])) \
        + 3600.0 - _Xmit_Windows[0]
    now = time.time()
    if t < now - 43200.0:
        t += 86400.0
    return max(t, now)

def getBBB(kind):
    # Returns either '' or 'RRX'
    eow = _Xmit_Windows[1]
    now = time.time() % 86400.0
    #
    # Allow some leeway here
    for t in _Fcst_Times:
        if 0 < t - now < (eow-59):
            return 'RRX'
    else:
        return ''

def printForecast(filelist):
    content = ['Printed by %s on %s' % \
        (Globals.Forecaster, time.strftime('%x %X')), '']
    for f in filelist:
        content.append(' '.join(os.path.basename(f).split('-')[2:]))
        content.append(Globals.DRC.getFile(f))
    (chldin, chldout) = os.popen4('lpr', -1)
    chldin.write('\n'.join(content))
    chldin.close()
    return chldout.read()

###############################################################################
def configureFlightCatColors(text):
    for tag in [Avn.LIFR, Avn.IFR, Avn.MVFR, Avn.VFR]:
        text.tag_configure(tag, \
            background=text.option_get('%sColor' % tag, ''))

###############################################################################
# functions used to produce forecasts
##############################################################################
def _split_line(line, indent=0):
    # used by indentTaf()
    maxlen = 68 - indent
    if indent > 0:
        lst1 = [' '*indent]
    else:
        lst1 = []
    lst2 = [' '*5]
    total = 0
    for word in line.split():
        if total + len(word) < maxlen:
            lst1.append(word)
        else:
            lst2.append(word)
        total += len(word)+1
    return ' '.join(lst1), ' '.join(lst2).rstrip()

def indentTaf(lines):
    taf = []
    lines = filter(None, [x.strip() for x in lines])
    if not lines:
        return taf
    k = 0
    if lines[0][:3] == 'TAF':
        taf.append(lines[0])
        k += 1
    l1, l2 = _split_line(lines[k])
    taf.append(l1)
    if l2:
        taf.append(l2)
    k += 1
    for line in lines[k:]:
        if line.startswith('FM'):
            l1, l2 = _split_line(line, 4)
        elif line.startswith('TEMPO'):
            l1, l2 = _split_line(line, 5)
        elif line.startswith('PROB'):
            prevline = taf.pop()
            if taf:
                indent = 4 
            else:
                indent = 0
            l1, l2 = _split_line('%s %s' % (prevline, line), indent)
        else:
            l1, l2 = _split_line(line, 5)
        taf.append(l1)
        if l2:
            taf.append(l2)
    if not taf[-1].endswith('='):
        taf[-1] += '='
    return taf

def adjustTimes(bbb, taf):
    # removes forecast periods that passed, adjust start of valid time
    if not 'group' in taf:
        # NIL TAF
        return
    stime = getValidTime('taf', bbb)
    groups = [g for g in taf['group'] if g['prev']['time']['to'] > stime]
    for g in groups:
        if 'ocnl' in g and g['ocnl']['type'] == 'PROB' and \
            g['ocnl']['time']['from'] < stime+32400:
            del g['ocnl']
    g = groups[0]
    if g['prev']['time']['from'] < stime:
        g['prev']['time']['from'] = stime
        if 'ocnl' in g:
            if g['ocnl']['time']['from'] < stime:
                if g['ocnl']['time']['to'] - stime < 3600.0:
                    del g['ocnl']
                else:
                    g['ocnl']['time']['from'] = stime
    taf['group'] = groups

def formatRecForMAIN(rec, ident, bbb, itime=None, tafDuration=24, evtime=None):
    # returns formated MAIN group as a list of strings
    kind = 'taf'
    lst = [ident, getFmtIssueTime(kind, bbb, itime), \
           getFmtValidTime(kind, bbb, itime, tafDuration, evtime)[4:]]
    if not rec :
        # no data or too early for forecast
        return lst 
    try:
        if 'wind' in rec: 
            lst.append(rec['wind']['str'])
        if 'vsby' in rec: 
            lst.append(rec['vsby']['str'])
        if 'pcp' in rec:    
            lst.append(rec['pcp']['str'])
        if 'obv' in rec:    
            lst.append(rec['obv']['str'])
        if 'vcnty' in rec:    
            lst.append(rec['vcnty']['str'])
        if 'sky' in rec:    
            lst.append(rec['sky']['str'])
        if 'llws' in rec:
            lst.append(rec['llws']['str'])
    except KeyError:
        pass
    return lst

def formatRecForFM(rec):
    # returns formatted FM group as a list of strings
    if not rec:
        return []
    lst = [time.strftime('FM%d%H%M', time.gmtime(rec['time']['from']))]
        
    try:
        lst.extend([rec[k]['str'] for k in \
            ['wind', 'vsby', 'pcp', 'obv', 'vcnty', 'sky', 'llws'] if k in rec])
    except KeyError:
        pass
    return lst

def formatRecForOCNL(rec):
    # returns formated TEMPO group as a list of strings
    # or an empty list if TEMPO not needed
    if not rec or rec['type'] not in ['TEMPO', 'PROB']:
        return []

    start,end = time.gmtime(rec['time']['from']),\
                list(time.gmtime(rec['time']['to']))
    
    if end[3] == 0:
        end[2] = start[2]
        end[3] = 24
        
    lst = []
    typ = rec['type']
    if typ == 'TEMPO':
        lst = ['TEMPO']
    elif typ == 'PROB':
        lst = ['PROB30']

    if len(lst):
        lst.append('%02d%02d/%02d%02d' % (start[2],start[3],end[2],end[3]))
        
    try:
        lst.extend([rec[k]['str'] for k in \
            ['wind', 'vsby', 'pcp', 'obv', 'nsw', 'sky'] if k in rec])
    except KeyError:
        pass
    
    if len(lst) > 1:
        return lst
    else:
        return []

def _filterPeriods(bbb, periods, t, kind, tafDuration):
    starttime = getValidTime(kind, bbb, t)
    tms = list(time.gmtime(starttime))
    tms[3] = _startHour(bbb, t)
    endtime = Avn.mkgmtime(tms)
    if endtime <= starttime:
        endtime += tafDuration*3600.0
    def _within(p):
        return p['prev']['time']['to'] > starttime and \
            p['prev']['time']['from'] < endtime
    return itertools.ifilter(_within, periods)

def makeTafFromPeriods(ident, bbb, periods, t=None, tafDuration=24, evtime=None):
    # returns formatted TAF given sequence of periods
    if bbb is None:
        bbb = 'RRA'
    if t is None:
        t = time.time()
    if bbb.startswith('A'):
        lines = ['TAF AMD']
    else:
        lines = ['TAF']
        
    inperiods = _filterPeriods(bbb, periods, t, 'taf', tafDuration)
    try:
        p = inperiods.next()
        if getValidTime('taf', bbb, t) < p['prev']['time']['from']-1800.0:
           # start time of the first group in the future
	   tmp=formatRecForMAIN(None, ident, bbb, t, tafDuration, evtime)
           lines.append(' '.join(' '.join(tmp).split()))
           tmp = formatRecForFM(p['prev'])
        else:
           tmp = formatRecForMAIN(p['prev'], ident, bbb, t, tafDuration, evtime)
        if not tmp:
            raise StopIteration
        lines.append(' '.join(' '.join(tmp).split()))
        if 'ocnl' in p:
            tmp = formatRecForOCNL(p['ocnl'])
            if tmp:
                lines.append(' '.join(' '.join(tmp).split()))
        while 1:
            p = inperiods.next()
            tmp = formatRecForFM(p['prev'])
            lines.append(' '.join(' '.join(tmp).split()))
            if 'ocnl' in p:
                tmp = formatRecForOCNL(p['ocnl'])
                if tmp:
                    lines.append(' '.join(' '.join(tmp).split()))
    except StopIteration:
        pass
    return lines
    
# default categories, used for eliminating unneccessary weather from TEMPO
_VsbyCat = [0.5, 1.0, 2.0, 3.0, 5.0]
_CigCat = [2, 6, 10, 20, 31]

def fixTafVsby(v):
    # returns valid TAF visibility as a dictionary {val, str}
    if v < 0.0:
        return {}
    elif v < 0.12:
        return {'value': 0.0, 'str': '0SM'}
    elif v < 0.37:
        return {'value': 0.25, 'str': '1/4SM'}
    elif v < 0.62:
        return {'value': 0.5, 'str': '1/2SM'}
    elif v < 0.87:
        return {'value': 0.75, 'str': '3/4SM'}
    elif v < 1.3:
        return {'value': 1.0, 'str': '1SM'}
    elif v < 1.8:
        return {'value': 1.5, 'str': '1 1/2SM'}
    elif v < 6.5:
        iv = (v+0.49)//1.0
        return {'value': iv, 'str': '%.0fSM' % iv}
    else:
        return {'value': 12.0, 'str': 'P6SM'}

def fixCldBase(h):
    # returns valid TAF cloud base
    if h < 30:
        return h
    elif h < 50:
        return (h+2)//5 * 5
    else:
        return (h+5)//10 * 10

def fixTafSky(sky):
    # returns valid TAF sky conditions as a tuple (cig, string)
    # replaces CLR by SKC, rounds cloud bases 
    _Cover = {'FEW': 1, 'SCT': 2, 'BKN': 3, 'OVC': 4}
    nsky = []
    s = sky['str']
    if s.find('CLR') != -1:
        return {'cig': Avn.UNLIMITED, 'str': 'SKC'}
    elif s.find('VV') != -1:
        return sky
    cover = 0
    for c, h, cb in [(x[:3], int(x[3:6]), x[6:]) for x in s.split()]:
        if cb == 'TCU':
            cb = ''
        cover = max(cover, _Cover.get(c, 0))
        nsky.append('%s%03d%s' % (c, fixCldBase(h), cb))
    return {'cig': fixCldBase(sky['cig']), 'cover': sky['cover'], \
            'str': ' '.join(nsky)}

def updateTafWithMetar(tafgrp, mtr):
    # replaces TAF group with METAR data
    if 'vsby' in mtr:
        tafgrp['vsby'] = fixTafVsby(mtr['vsby']['value'])
    if 'sky' in mtr:
        tafgrp['sky'] = fixTafSky(mtr['sky'])
    for k in ['wind', 'pcp', 'obv']:
        if k in mtr:
            tafgrp[k] = mtr[k]
        elif k in tafgrp:
            del tafgrp[k]

def fixTafTempo(p, t):
    # eliminates TAF TEMPO elements that match those in FM group
    if not ('obv' in t or 'pcp' in t) and 'vsby' in t:
        tcat = Avn.category(t['vsby']['val'], _VsbyCat)
        pcat = Avn.category(p['vsby']['val'], _VsbyCat)
        if tcat == pcat:
            del t['vsby']
        else:
            t['obv'] = {'str': 'NSW'}
    if 'wind' in t:
        tdd, tff = t['wind']['dd'], t['wind']['ff'] 
        pdd, pff = p['wind']['dd'], p['wind']['ff']
        if Avn.VARIABLE in (tdd, pdd):
            delta = 0
        else:
            delta = abs(pdd - tdd)
            if delta > 180:
                delta = 360 - delta
        if abs(tff-pff) < 10 and (max(tff, pff) < 12 or delta < 30):
            del t['wind']
    if 'sky' in t:
        tcat = Avn.category(t['sky']['cig'], _CigCat)
        pcat = Avn.category(p['sky']['cig'], _CigCat)
        if tcat == pcat:
            del t['sky']

###############################################################################
# utility functions
##############################################################################
def getGroupIndex(g):
    ixlist = [g[i]['index'] for i in g if 'index' in g[i]]
    line = int(ixlist[0][0].split('.')[0])
    fchar = min([int(x[0].split('.')[1]) for x in ixlist])
    lchar = max([int(x[1].split('.')[1]) for x in ixlist])
    return '%d.%d' % (line, fchar), '%d.%d' % (line, lchar)

def getTafPeriods(taf):
    periods = []
    indices = []
    for p in taf['group'][:]:
        pp = p['prev']
        periods.append(pp)
        indices.append(getGroupIndex(pp))
        if 'ocnl' in p:
            po = pp.copy()
            po.update(p['ocnl'])
            periods.append(po)
            indices.append(getGroupIndex(p['ocnl']))
    return zip(indices, periods)

def flightCategory(dcd):
    # dcd: dictionary d = {'cig': (cig, ...), 'vsby': (vsby, ...)}
    sky, vis = dcd.get('sky',{}), dcd.get('vsby',{})
    cig, vsby = sky.get('cig',-1), vis.get('value',-1)
    if cig < 0: #cig is missing, vsby may be valid
	if vsby < 0: #vsby missing, default to VFR 
	    return Avn.VFR
	elif vsby < 1.0:
	    return Avn.LIFR
	elif vsby < 3.0:
	    return Avn.IFR
	elif vsby < 6.0:
	    return Avn.MVFR
	return Avn.VFR
    elif vsby < 0: #vsby is missing, cig is valid (took care of cig & vsby both missing already)
	if cig < 500:
	    return Avn.LIFR
	elif cig < 1000:
	    return Avn.IFR
	elif cig < 3100:
	    return Avn.MVFR
        return Avn.VFR
    elif cig < 500 or vsby < 1.0:
        return Avn.LIFR
    elif cig < 1000 or vsby < 3.0:
        return Avn.IFR
    elif cig < 3100 or vsby < 6.0:
        return Avn.MVFR
    else:
        return Avn.VFR

##############################################################################
# functions used by monitors
##############################################################################
def findIndex(e, dcd, hlen):
    def _update(ix):
        if hlen == 0:
            return ix
        l0, c0 = ix[0].split('.')
        l1, c1 = ix[1].split('.')
        return '%d.%s' % (int(l0)+hlen,c0), '%d.%s' % (int(l1)+hlen,c1)

    if e == 'wx':
        ix = []
        if 'pcp' in dcd:
            ix.append(_update(dcd['pcp']['index']))
        if 'obv' in dcd:
            ix.append(_update(dcd['obv']['index']))
        return ix
    elif e == 'cat':
        ix = []
        if 'vsby' in dcd:
            ix.append(_update(dcd['vsby']['index']))
        if 'sky' in dcd:
            ix.append(_update(dcd['sky']['index']))
        return ix
    elif e in dcd:
        return [_update(dcd[e]['index'])]
    else:
        return []

def _makeWx(g):
    """Returns wx string"""
    try:
        return ' '.join([g[k]['str'] for k in ['pcp', 'obv'] if k in g]) or None
    except KeyError:
        return None
    
def makeMetarData(metar):
    d = {'time': metar['itime']['value']}
    try:
        tmp = metar['wind']
        d['wind'] = {'dd': tmp['dd'], \
            'ff': {'lo': tmp['ff'], 'hi': tmp.get('gg', tmp['ff'])}}
    except KeyError:
        pass
    try:
        d['vsby'] = {'vsby': metar['vsby']['value']}
    except KeyError:
        pass
    s = _makeWx(metar)
    if s:
        d['wx'] = {'str': s}
        
    try:
        d['sky'] = {'cig': metar['sky']['cig']}
    except KeyError:
        pass
    #
    # Optional information from remarks section
    try:
        d['vvsby'] = {'lo': metar['vvsby']['lo'],
                      'hi': metar['vvsby']['hi']}
    except KeyError:
        pass
    
    try:
        d['vsky'] = {'cvr1': metar['vsky']['cvr1'],
                     'cvr2': metar['vsky']['cvr2'],
                     'cig': metar['vsky']['cig']}
    except KeyError:
        pass
    
    try:
        d['vcig'] = {'lo': metar['vcig']['lo'],
                     'hi': metar['vcig']['hi']}
    except KeyError:
        pass
    
    return d

##############################################################################
# TAF structure used by monitors
##############################################################################
class TafData:
    def _makeTafWind(g, items):
        d = {}
        try:
            if 'p' in items and 'wind' in g['prev']:
                tmp = g['prev']['wind']
                d['dd'] = {'prev': tmp['dd']}
                d['ff'] = {'prev': tmp.get('gg', tmp['ff'])}
            if 'o' in items and g['ocnl'] and 'wind' in g['ocnl']:
                tmp = g['ocnl']['wind']
                if 'dd' in d:
                    d['dd']['ocnl'] = tmp['dd']
                else:
                    d['dd'] = {'ocnl': tmp['dd']}
                if 'ff' in d:
                    d['ff']['ocnl'] = tmp.get('gg', tmp['ff'])
                else:
                    d['ff'] = {'ocnl': tmp.get('gg', tmp['ff'])}
            lo = d['ff'].get('prev', 0)
            hi = d['ff'].get('ocnl', lo)
            if lo > hi:
                lo, hi = hi, lo
            d['ff']['lo'] = lo
            d['ff']['hi'] = hi
        except KeyError:
            pass
        return d
    _makeTafWind = staticmethod(_makeTafWind)

    def _makeTafVsby(g, items):
        d = {}
        try:
            if 'p' in items and 'vsby' in g['prev']:
                d['hi'] = g['prev']['vsby']['value']
                d['prev'] = g['prev']['vsby']['value']
                
            if 'o' in items and 'ocnl' in g:
                if 'vsby' in g['ocnl']:
                    d['lo'] = g['ocnl']['vsby']['value']
                    d['ocnl'] = g['ocnl']['vsby']['value']
                elif 'vsby' in g['prev']:
                    d['hi'] = g['prev']['vsby']['value']
                    d['prev'] = g['prev']['vsby']['value']
                else:
                    return d
                
            if 'hi' in d and not 'lo' in d:
                d['lo'] = d['hi']
            if 'lo' in d and not 'hi' in d:
                d['hi'] = d['lo']
            if d['lo'] > d['hi']:
                d['lo'], d['hi'] = d['hi'], d['lo']
        except KeyError:
            pass
        return d
    
    _makeTafVsby = staticmethod(_makeTafVsby)

    def _makeTafWx(g, items):
        pstr, ostr = None, None
        if 'p' in items:
            pstr = _makeWx(g['prev'])
        if 'o' in items and 'ocnl' in g:
            g = g['ocnl']
            if 'nsw' in g:
                ostr = None
            elif 'pcp' in g or 'obv' in g:
                ostr = _makeWx(g)
            elif pstr:
                ostr = pstr
        d = {}
        if pstr:
            d['pstr'] = pstr
        if ostr:
            d['ostr'] = ostr
        return d
    _makeTafWx = staticmethod(_makeTafWx)

    def _makeTafSky(g, items):
        d = {}
        try:
            if 'p' in items and 'sky' in g['prev']:
                d['hi'] = g['prev']['sky']['cig']
                d['prev'] = g['prev']['sky']['cig']

            if 'o' in items: 
                if 'ocnl' in g and 'sky' in g['ocnl']:
                    d['lo'] = g['ocnl']['sky']['cig']
                    d['ocnl'] = g['ocnl']['sky']['cig']
                elif 'sky' in g['prev']:
                    d['hi'] = g['prev']['sky']['cig']
                    d['prev'] = g['prev']['sky']['cig']
                else:
                    return d
                
            if 'hi' in d and not 'lo' in d:
                d['lo'] = d['hi']
            if 'lo' in d and not 'hi' in d:
                d['hi'] = d['lo']
            if d['lo'] > d['hi']:
                d['lo'], d['hi'] = d['hi'], d['lo']
        except KeyError:
            pass
        return d
    _makeTafSky = staticmethod(_makeTafSky)

    def _makeTafCover(g, items):
        d = {}
        try:
            if 'p' in items and 'sky' in g['prev']:
                d['hi'] = g['prev']['sky']['cover']
                d['prev'] = g['prev']['sky']['cover']
                
            if 'o' in items: 
                if 'ocnl' in g and 'sky' in g['ocnl']:
                    d['lo'] = g['ocnl']['sky']['cover']
                    d['ocnl'] = g['ocnl']['sky']['cover']
                elif 'sky' in g['prev']:
                    d['hi'] = g['prev']['sky']['cover']
                    d['prev'] = g['prev']['sky']['cover']
                else:
                    return d
                
            if 'hi' in d and not 'lo' in d:
                d['lo'] = d['hi']
            if 'lo' in d and not 'hi' in d:
                d['hi'] = d['lo']
            if d['lo'] > d['hi']:
                d['lo'], d['hi'] = d['hi'], d['lo']
        except KeyError:
            pass
        return d
    _makeTafCover = staticmethod(_makeTafCover)

    def _makeTafTS(g, items):
        d = {}
        try:
            if 'p' in items:
                if 'pcp' in g['prev'] and 'TS' in g['prev']['pcp']['str']:
                    d['prev'] = 'Y'
                elif 'vcnty' in g['prev'] and 'TS' in g['prev']['vcnty']['str']:
                    d['prev'] = 'VC'
            if 'o' in items and 'ocnl' in g:
                if 'pcp' in g['ocnl'] and 'TS' in g['ocnl']['pcp']['str']:
                    d['ocnl'] = g['ocnl']['type']
        except KeyError:
            pass
        return d
    _makeTafTS = staticmethod(_makeTafTS)

    def _makeTafCB(g, items):
        d = {}
        try:
            if 'p' in items and g['prev']['sky']['str'].find('CB') > -1:
                d['prev']='Y'
                raise KeyError
            
            if 'o' in items and 'ocnl' in g and g['ocnl']['sky']['str'].find('CB') > -1:
                d['prev']='Y'
        except KeyError:
            pass
        return d
    
    _makeTafCB = staticmethod(_makeTafCB)

    def _makeTafLLWS(g, items):
        d = {}
        if 'p' in items:
            if 'llws' in g['prev']:
                d['prev'] = True
        return d
    _makeTafLLWS = staticmethod(_makeTafLLWS)

    def _make(g, t1, t2, items):
        d = {}
        for key, f in [('wind', TafData._makeTafWind), \
            ('vsby', TafData._makeTafVsby), \
            ('wx', TafData._makeTafWx), \
            ('sky', TafData._makeTafSky), \
            ('cover', TafData._makeTafCover), \
            ('ts', TafData._makeTafTS), \
            ('cb', TafData._makeTafCB), \
            ('llws', TafData._makeTafLLWS)]:
            tmp = f(g, items)
            if tmp:
                d[key] = tmp
            d['from'], d['to'] = t1, t2
        return d
    _make = staticmethod(_make)

    def makeTempo(grp):
        try:
            to1, to2 = grp['ocnl']['time']['from'], grp['ocnl']['time']['to']
            tempo = TafData._make(grp, to1, to2, ['o'])
            tempo['tempoCheck'] = True
            #
            # Promote TEMPO items to prevailing
            try:
                tempo['wx']['pstr'] = tempo['wx']['ostr']
                del tempo['wx']['ostr']
            except KeyError:
                pass
                
            for item in tempo.keys():
                try:
                    tempo[item]['prev']=tempo[item]['ocnl']
                    del tempo[item]['ocnl']
                except (TypeError,KeyError):
                    pass
                
	    return tempo
        except KeyError:
            return None
        
    makeTempo = staticmethod(makeTempo)
    
    def __init__(self, groups):
        self._data = []
        for g in groups:
            tp1, tp2 = g['prev']['time']['from'], g['prev']['time']['to']
            if 'ocnl' in g:
                to1, to2 = g['ocnl']['time']['from'], g['ocnl']['time']['to']
                if tp1 < to1:
                    d = TafData._make(g, tp1, to1, ['p'])
                    self._data.append(d)
                    tp1 = to1
                d = TafData._make(g, tp1, to2, ['p', 'o'])
                self._data.append(d)
                if to2 < tp2:
                    d = TafData._make(g, to2, tp2, ['p'])
                    self._data.append(d)
            else:
                d = TafData._make(g, tp1, tp2, ['p'])
                self._data.append(d)

    def get(self, t):
        if t < self._data[0]['from'] or t >= self._data[-1]['to']:
            return None
        for d in self._data:
            if d['from'] <= t < d['to']:
                return d
        return None
