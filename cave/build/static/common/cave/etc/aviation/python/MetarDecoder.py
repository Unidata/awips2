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
#       MetarDecoder.py
#       GFS1-NHD:A7806.0000-SCRIPT;1.25
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.25 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.24 (DELIVERED)
#         Created:  19-JUN-2008 14:28:30      OBERFIEL
#           Fixed problem with visibility when given in meters
#       
#       Revision 1.23 (DELIVERED)
#         Created:  22-JUN-2007 12:42:57      OBERFIEL
#           Removed superflous __index method and used tpg information
#           to determine location of token in index method.
#       
#       Revision 1.22 (DELIVERED)
#         Created:  20-JUN-2007 08:56:55      OBERFIEL
#           Updates to take care of PIT DRs
#       
#       Revision 1.21 (DELIVERED)
#         Created:  25-MAY-2007 14:27:10      OBERFIEL
#           Update to support additional information in remarks
#       
#       Revision 1.20 (REVIEW)
#         Created:  15-MAY-2007 14:19:17      OBERFIEL
#           Added ability to find and decode SFC VIS in RMK section of
#           METAR.
#       
#       Revision 1.19 (DELIVERED)
#         Created:  14-APR-2006 13:17:48      TROJAN
#           spr 7118
#       
#       Revision 1.18 (DELIVERED)
#         Created:  14-APR-2006 08:24:41      TROJAN
#           spr 7117
#       
#       Revision 1.17 (DELIVERED)
#         Created:  03-NOV-2005 13:16:32      TROJAN
#           spr 7051
#       
#       Revision 1.16 (APPROVED)
#         Created:  12-OCT-2005 18:26:35      TROJAN
#           spr 7040
#       
#       Revision 1.15 (DELIVERED)
#         Created:  07-JUL-2005 12:57:10      TROJAN
#           spr 6879
#       
#       Revision 1.14 (DELIVERED)
#         Created:  13-MAY-2005 18:58:15      TROJAN
#           spr 6841
#       
#       Revision 1.13 (REVIEW)
#         Created:  07-MAY-2005 11:35:32      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.12 (DELIVERED)
#         Created:  18-APR-2005 17:32:08      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.11 (DELIVERED)
#         Created:  04-APR-2005 15:51:07      TROJAN
#           spr 6775
#       
#       Revision 1.10 (APPROVED)
#         Created:  21-MAR-2005 14:02:06      TROJAN
#           spr 6734
#       
#       Revision 1.9 (DELIVERED)
#         Created:  02-MAR-2005 14:38:31      TROJAN
#           spr 6691
#       
#       Revision 1.8 (DELIVERED)
#         Created:  14-FEB-2005 21:08:13      TROJAN
#           spr 6651
#       
#       Revision 1.7 (APPROVED)
#         Created:  24-JAN-2005 17:49:55      TROJAN
#           spr 6608
#       
#       Revision 1.6 (APPROVED)
#         Created:  19-JAN-2005 15:17:43      TROJAN
#           spr 6565
#       
#       Revision 1.5 (APPROVED)
#         Created:  01-OCT-2004 13:34:49      TROJAN
#           spr 6398
#       
#       Revision 1.4 (APPROVED)
#         Created:  19-AUG-2004 20:49:15      OBERFIEL
#           Code chage
#       
#       Revision 1.3 (REVIEW)
#         Created:  09-JUL-2004 19:48:12      OBERFIEL
#           Fixed problem with VCTS and temperature decoding
#       
#       Revision 1.2 (UNDER WORK)
#         Created:  09-JUL-2004 19:42:15      OBERFIEL
#           Fixed problem with VCTS and temperature decoding
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:41:54      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7385
#       	Action Date:       11-OCT-2008 12:56:11
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Handle missing LLWS sources better
#       
#
import exceptions, time, types
import tpg
import Avn

IN_TO_MB = 33.8622
M_TO_SM = 1.0/1609.0
MPS_TO_KT = 3600.0/1852.0

_ValidVcnty = dict.fromkeys(['TS', 'SH', 'FG'])

###############################################################################
# local exceptions
class Error(exceptions.Exception): pass

##############################################################################
# parser stuff
_SkyCov = 'FEW|SCT|BKN|OVC'
_Cld = '(%s)\d{3}(CB|TCU)?' % _SkyCov
_Fract = '([1-4] |[1-4])?[1357]/([248]|16)'
_Obv = 'BR|FG|FU|VA|DU|SA|HZ|PY|PO|SQ|[+]?FC|SS|DS|SN'
_ObvQ = 'MI|PR|BC|DR|BL|FZ'
_Pcp = 'DZ|RA|SN|SG|IC|PE|GR|GS|UP|PL'
_PcpQ = 'SH|TS|FZ'

_Options = r"""
set lexer = ContextSensitiveLexer 
set lexer_dotall = True
"""

_Separator = r"separator spaces:    '\s+' ;"

_pcptok = '[+-]?(%s)?(%s)+' % (_PcpQ, _Pcp)
_obvtok = '(%s)?(%s)' % (_ObvQ, _Obv)
_vsbytok = '(?P<%s>\d{1,2}(?!/)\s*)?(?P<%s>[1357]/1?[2468])?'

_TokList = [
    # mandatory part
    ('type', r'METAR|SPECI'),
    ('ident', r'[A-Z][A-Z0-9]{3}'),
    ('itime', r'\d{6}Z'),
    ('autocor', r'AUTO|COR|RTD'),
    ('wind', r'(VRB|\d{3})\d{2,3}(G\d{2,3})?(KT|MPS)'),
    ('wind_vrb', r'\d{3}V\d{3}'),
    ('vsby', r'(M\d/\d|%s|\d{1,3})SM|\d{1,4}[NEWS]{0,2}' % _Fract),
    ('rvr', r'R\w+/[MP]?\d{3,4}(V?P?\d{4})?(FT)?'),
    ('funnel', r'[+]?FC'),
    ('pcp', r'%s|TS(\s+%s)?' % (_pcptok, _pcptok)),
    ('obv', r'%s(\s+%s)*' % (_obvtok, _obvtok)),
    ('vcnty', r'VC\w+'),
    ('sky', r'SKC|CLR|VV\d{3}|(%s(\s+%s)*)' % (_Cld, _Cld)),
    ('temp', r'M?\d{2}/(M?\d{2})?'),
    ('alt', r'[AQ]\d{3,4}'),
    # US remarks
    ('pcp1h', r'P\d{4}'),
    ('tempdec', r'T[01]\d{3}[01]\d{3}'),
    ('mslp', r'SLP\d{3}'),
    ('sfcvis', r'SFC\s+VIS\s+'+_vsbytok % ('visint','visfrac')),
    ('vvis', r'VIS\s+'+_vsbytok % ('vintlo','vfraclo') +'V'+_vsbytok % ('vinthi','vfrachi')),
    ('vcig', r'CIG\s+(\d{3})V(\d{3})'),
    ('vsky', r'(%s)(\d{3})?\s+V\s+(%s)' % ( _SkyCov, _SkyCov )),
    # is this needed?
    ('any', r'\S+'),
]

_Tokens = '\n'.join([r"token %s: '%s' ;" % tok for tok in _TokList])

_Rules = r"""
START/e -> METAR/e $ e=self._metar $ ;
METAR -> Type Ident ('NIL' '.*' | ITime autocor? Body (Remark | any*)) ;
Body -> Wind? wind_vrb? Vsby? rvr* Funnel? Vcnty? Pcp? Obv? Vcnty? Sky? Temp? Alt? ;
Remark -> 'RMK' (Pcp1h | TempDec | Slp | SfcVis | VVis | VCig | VSky | any)+ ;

# PcpG -> (Pcp vcnty?) | (vcnty Pcp) ;

Type -> type/x $ self.type(x) $ ;
Ident -> ident/x $ self.ident(x) $ ;
ITime -> itime/x $ self.itime(x) $ ;
Wind -> wind/x $ self.wind(x) $ ;
Vsby -> vsby/x $ self.vsby(x) $ ;
Funnel -> funnel/x $ self.obv(x) $ ;
Pcp -> pcp/x $ self.pcp(x) $ ;
Obv -> obv/x $ self.obv(x) $ ;
Vcnty -> vcnty/x $ self.vcnty(x) $ ;
Sky -> sky/x $ self.sky(x) $ ;
Temp -> temp/x $ self.temp(x) $ ;
Alt -> alt/x $ self.alt(x) $ ;
SfcVis -> sfcvis/x $ self.sfcvsby(x) $ ;
VVis -> vvis/x $ self.vvis(x) $ ;
VCig -> vcig/x $ self.vcig(x) $ ;
VSky -> vsky/x $ self.vsky(x) $ ;
Pcp1h -> pcp1h/x $ self.pcp1h(x) $ ;
TempDec -> tempdec/x $ self.tempdec(x) $ ;
Slp -> mslp/x $ self.mslp(x) $ ;
"""

##############################################################################
# local functions
def valid_day(tms):
    """Checks if day of month is valid"""
    year, month, day = tms[:3]
    if day > 31:
        return 0
    if month in [4, 6, 9, 11] and day > 30:
        return 0
    if month == 2 and (year%4 == 0 and day > 29 or day > 28):
        return 0
    return 1

##############################################################################
# decoder class
class Decoder(tpg.VerboseParser):
    """METAR decoder class"""
    __doc__ = '\n'.join([_Options, _Separator, _Tokens, _Rules])
    verbose = 0
#   print __doc__

    def __call__(self, metar, year=None, month=None):
        self._year, self._month = year, month
        self._metar = {}
        self.expected = []
        self._first = 0
        if type(metar) == types.ListType:
            metar = '\n'.join(metar)
        try:
            return super(Decoder, self).__call__(metar)
        except tpg.SyntacticError:
            return {'fatal': {'index': self.index(), \
                'error': 'Invalid token. Expecting one of:\n%s' % \
                    '\n'.join(self.expected)}}
        except Exception, e:
            # highlight only METAR/SPECI
            row = self._first+1
            index = ('%d.%d' % (row, 0), '%d.%d' % (row, 5))
            return {'fatal': {'index': index, 'error': 'METAR decoder bug'}}

    def index(self):
        ti = self.lexer.cur_token
        return ('%d.%d' % (ti.line+self._first, ti.start),
                '%d.%d' % (ti.end_line+self._first, ti.end_column-1))

    def eatCSL(self, name):
        """Overrides super definition"""
        try:
            value = super(Decoder, self).eatCSL(name)
            self.expected = []
            return value
        except tpg.WrongToken:
            self.expected.append(name)
            raise

    def fix_date(self, tms):
        """Tries to determine month and year from report timestamp.
tms contains day, hour, min of the report, current year and month
"""
        if self._year is not None and self._month is not None:
            tms[:2] = self._year, self._month
        else:
            now = time.time()
            t = time.mktime(tms)  - time.timezone
            if t > now + 86400.0:       # previous month
                if tms[1] > 1:
                    tms[1] -= 1
                else:
                    tms[1] = 12
                    tms[0] -= 1
            elif t < now - 25*86400.0:  # next month
                if tms[1] < 12:
                    tms[1] += 1
                else:
                    tms[1] = 1
                    tms[0] += 1

    #######################################################################
    # Methods called by the parser
    def alt(self, s):
        d = self._metar['alt'] = {'str': s, 'index': self.index()}
        v = int(s[1:])
        if s[0] == 'A':
            v *= IN_TO_MB/100.0;
        if 900 < v < 1080:
            d['value'] = v
        else:
            d['error'] = 'Invalid value %.f' % v

    def ident(self, s):
        self._metar['ident'] = {'str': s, 'index': self.index()}

    def itime(self, s):
        d = self._metar['itime'] = {'str': s, 'index': self.index()}
        mday, hour, min = int(s[:2]), int(s[2:4]), int(s[4:6])
        try:
            if mday > 31 or hour > 23 or min > 59:
                raise Error('Invalid time')
            tms = list(time.gmtime())
            tms[2:6] = mday, hour, min, 0
            self.fix_date(tms)
            if not valid_day(tms):
                raise Error('Invalid day')
            d['value'] = time.mktime(tms)  - time.timezone
        except Error, e:
            d['value'] = time.time()    # should be fatal?
            d['error'] = str(e)

    def type(self, s):
        self._metar['type'] = {'str': s, 'index': self.index()}

    def vvis(self, s):
        d = self._metar['vvsby'] = {'str': s, 'index': self.index()}
        v = self.lexer.tokens[self.lexer.cur_token.name][0].search(s)
        vis = 0.0
        
        try:
            vis += float(v.group('vintlo').strip())
        except (AttributeError, ValueError):
            pass
            
        try:
            num, den = v.group('vfraclo').split('/', 1)
            vis += float(num)/float(den)
        except (AttributeError, ValueError):
            pass

        if vis > 50.0:
            if vis > 9998.:
                vis = 7.0
            else:
                vis = vis*M_TO_SM

        d['lo'] = vis
        
        vis = 0.0
        try:
            vis += float(v.group('vinthi').strip())
        except (AttributeError, ValueError):
            pass
            
        try:
            num, den = v.group('vfrachi').split('/', 1)
            vis += float(num)/float(den)
            metric = False            
        except (AttributeError, ValueError):
            pass

        if vis > 50.0:
            if vis > 9998.:
                vis = 7.0
            else:
                vis = vis*M_TO_SM

        d['hi'] = vis
        #
        # Bad token processed
        if d['hi'] < d['lo']:
            del self._metar['vvsby']
            raise tpg.WrongToken
    #
    # SFC VIS always overrides the prevailing visibility
    def sfcvsby(self, s):
        v = self.lexer.tokens[self.lexer.cur_token.name][0].search(s)
        vis = 0.0
        metric = True
        
        try:
            vis += float(v.group('visint').strip())
        except (AttributeError, ValueError):
            pass
            
        try:
            num, den = v.group('visfrac').split('/', 1)
            vis += float(num)/float(den)
            metric = False            
        except (AttributeError, ValueError):
            pass
        #
        # It would be unusual if visibility wasn't already in the
        # dictionary
        try:
            d = self._metar['vsby']
            if 'SM' in d['str']:
                metric = False
                
            d['str'] = s
            d['index'] = self.index()

        except KeyError:
            d = self._metar['vsby'] = {'str': s,
                                       'index': self.index()}
            if vis > 50.0:
                metric = True
                
        if metric:
            if vis > 9998.:
                vis = 7.0
            else:
                vis = vis*M_TO_SM

        d['value'] = vis
            
    def vsby(self, s):
        d = self._metar['vsby'] = {'str': s, 'index': self.index()}
        
        if s[0] == 'M':         # M1/4SM
            v = 0.0
        elif 'SM' in s:         # miles
            tok = s[:-2].split()
            if len(tok) > 1:
                v = float(tok[0])
                num, den = tok[1].split('/', 1)
                v += float(num)/float(den)
            else:
                if '/' in tok[0]:
                    num, den = tok[0].split('/', 1)
                    if len(num) > 1: # 11/4 (missing space)
                        v = float(num[0])
                        num = num[1:]
                    else:
                        v = 0.0
                    v += float(num)/float(den)
                else:
                    v = float(tok[0])
        else:                   # meters
            for n, x in enumerate(s):
                if not x.isdigit():
                    v = float(s[:n])*M_TO_SM
                    break
            else:
                v = float(s)*M_TO_SM
                
            if v > 6.2:
                v = 7.0
                
        d['value'] = v

    def wind(self, s):
        d = self._metar['wind'] = {'str': s, 'index': self.index()}
        try:
            if s.startswith('VRB'):
                dd = 'VRB'
            else:
                if s[2] != '0':
                    raise Error('Invalid direction')
                dd = int(s[:3])
            if 'MPS' in s:
                tok = s[3:-3].split('G', 1)
                factor = MPS_TO_KT 
            else:
                tok = s[3:-2].split('G', 1)
                factor = 1
            ff = int(tok[0]) * factor
            if ff > 100:
                raise Error('Invalid speed')
            d.update({'dd': dd, 'ff': ff})
            if len(tok) > 1:
                gg = int(tok[1]) * factor
                if gg <= ff or gg - ff > 40:
                    raise Error('Invalid gust')
                d['gg'] = gg
            else:
                gg = None
        except Error, e:
            d['error'] = str(e)

    def obv(self, s):
        if 'obv' in self._metar:
            return
        self._metar['obv'] = {'str': s, 'index': self.index()}

    def pcp(self, s):
        self._metar['pcp'] = {'str': s, 'index': self.index()}

    def vcnty(self, s):
        d = self._metar['vcnty'] = {'str': s, 'index': self.index()}
        if not s[2:] in _ValidVcnty:
            d['error'] = 'Invalid weather in vicinity'

    def sky(self, s):
        cig = Avn.UNLIMITED
        cover = 0
        for n, tok in enumerate(s.split()):
            if tok.startswith('VV'):
                cig = int(tok[2:])
                cover = 4
            elif tok == 'CLR':
                cig = Avn.CLEAR
                break
            elif tok == 'SKC':
                cig = Avn.UNLIMITED
                break
            elif tok.startswith('FEW'):
                cover = 1
            elif tok.startswith('SCT'):
                cover = 2
            elif tok.startswith('BKN'):
                cover = 3
                cig = min(cig, int(tok[3:6]))
            elif tok.startswith('OVC'):
                cover = 4
                cig = min(cig, int(tok[3:6]))
        self._metar['sky'] = {'str': s, 'index': self.index(), \
            'cover': cover, 'cig': 100*cig}

    def vsky(self,s):
        d = self._metar['vsky'] = {'str': s, 'index': self.index()}
        v = self.lexer.tokens[self.lexer.cur_token.name][0].search(s)
        d['cvr1'] = {'FEW':1,'SCT':2,'BKN':3,'OVC':4}.get(v.group(1),4)
        d['cvr2'] = {'FEW':1,'SCT':2,'BKN':3,'OVC':4}.get(v.group(3),4)
        
        try:
            d['cig'] = int(v.group(2))*100
        except (AttributeError,TypeError):
            try:
                d['cig'] = self._metar['sky']['cig']                
            except KeyError:
                pass

    def vcig(self,s):
        d = self._metar['vcig'] = {'str': s, 'index': self.index()}
        v = self.lexer.tokens[self.lexer.cur_token.name][0].search(s)
        d['lo']=int(v.group(1))*100
        d['hi']=int(v.group(2))*100
        
    def temp(self, s):
        d = self._metar['temp'] = {'str': s, 'index': self.index()}
        tok = s.split('/')
        if tok[0][0] == 'M':
            tt = -int(tok[0][1:])
        else:
            tt = int(tok[0])
        try:
            if tok[1][0] == 'M':
                td = -int(tok[1][1:])
            else:
                td = int(tok[1])
        except IndexError:
            td = None
        if -60 < tt < 50:
            d['tt'] = tt
        else:
            d['error'] = 'Invalid temperature'
        if td is not None and -60 < td <= tt:
            d['td'] = td
        else:
            d['error'] = 'Invalid temperature'

    def tempdec(self, s):
        d = self._metar['tempdec'] = {'str': s, 'index': self.index()}
        tt = float(s[2:5])/10.0
        if s[1] == '1':
            tt = -tt
        td = float(s[6:9])/10.0
        if s[5] == '1':
            td = -td
        if -60.0 < tt < 50.0 and -60.0 < td <= tt:
            d.update({'tt': tt, 'td': td})
        else:
            d['error'] = 'Invalid temperature'

    def pcp1h(self, s):
        self._metar['pcp1h'] = {'str': s, 'index': self.index(), \
            'value': float(s[1:])/100.0}

    def mslp(self, s):
        p = float(s[3:])/100.0
        if p >= 50.0:
            p += 900.0
        else:
            p += 1000.0
        try:
            if p - self._metar['alt']['value'] > 80.0:
                p -= 100.0
            elif p - self._metar['alt']['value'] < -80.0:
                p += 100.0
            self._metar['mslp'] = {'str': s, 'index': self.index(), 'value': p}
        except KeyError:
            pass    # missing/bad altimeter

##############################################################################
# public part
def errors(decoded):
    """Returns list of elements that have 'error' or 'warning' key"""
    d = {'error': [], 'warning': []}
    def walk(item, k=None):
        if type(item) == types.ListType:
            for i in item:
                walk(i)
        elif type(item) == types.DictType:
            if 'error' in item:
                d['error'].append((k, item))
            elif 'warning' in item:
                d['warning'].append((k, item))
            else:
                for k, v in item.iteritems():
                    walk(v, k)
    if 'fatal' in decoded:
        return d
    walk(decoded)
    return d

##############################################################################
# test
def main(report):
    import Avn
    for n, line in enumerate(report):
        if line.startswith('METAR') or line.startswith('SPECI'):
            metar = ''.join(report[n:])
            break
    else:
        raise Avn.AvnError('Cannot find METAR')
    print metar
    decoder = Decoder()
    decoded = decoder(metar)
    if 'fatal' in decoded:
        print 'Fatal error at', decoded['fatal']
    for key in decoded:
        print key, decoded[key]
    errlist = errors(decoded)
    for k, d in errlist['error']:
        print k, d['index'], d['error']
    # decoder does not produce warnings

if __name__ == '__main__':
    import sys
    main(sys.stdin.readlines())
