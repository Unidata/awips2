#!/usr/bin/env python
#
# Name: TAFDecoder.py
# Purpose: To decode both US and International civilian TAF reports.
#          This program converts TAC TAF messages into python dictionaries
#          for encoding in XML.
#
# Author: Mark Oberfield
# Organization: NOAA/NWS/OSTI/MDL
# Date: 08 October 2016

#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/17/2017      6065          tgurney        Change timestamps to UTC, and
#                                                 add lastTafText parameter
#    12/13/2017      6550          tgurney        Remove lastTafText parameter
#
#
import calendar, re, time, tpg
#
# Conversion from statute miles to metres.
_Vsby = { \
    '0': '0',
    'M1/4': '0',
    '1/4': '402',
    '1/2': '805',
    '3/4': '1207',
    '1': '1609',
    '11/4': '2012',
    '1 1/4': '2012',
    '11/2': '2414',
    '1 1/2': '2414',
    '2': '3219',
    '3': '4828',
    '4': '6437',
    '5': '8046',
    '6': '9656',
    'P6':'10000'
}

_ValidCover = {'FEW': 1, 'SCT': 2, 'BKN': 3, 'OVC': 4, '0VC': 4}

##############################################################################
# parser stuff
_Cld = '((FEW|SCT|BKN|[0O]VC|VV)\d{3}(CB|TCU)?|VV///)'
_Fract = '(1\s*)?[13]/[24]'
_Obv = 'BR|FG|FU|VA|DU|SA|HZ|PY|PO|SQ|\+?(FC|SS|DS)|SN'
_ObvQ = 'MI|PR|BC|DR|BL|FZ'
_Pcp = 'DZ|RA|SN|SG|IC|PE|GR|GS|UP|PL'
_PcpQ = 'SH|TS|FZ'
_TimePhrase = '(AFT|TIL)\s+(\d{6})|(\d{4}/\d{4})'

_Options = r"""
set lexer = ContextSensitiveLexer
set lexer_dotall = True
"""
_Separator = r"separator spaces:    '\s+' ;"

_pcptok = '[+-]?(%s)?(%s)+' % (_PcpQ, _Pcp)
_obvtok = '(%s)?(%s)' % (_ObvQ, _Obv)

_TokList = [
    ('prefix', r'TAF(\s+(AMD|COR|CC[A-Z]|RTD))?'),
    ('ident', r'[A-Z][A-Z0-9]{3}'),
    ('itime', r'\d{6}Z'),
    ('nil', r'NIL'),
    ('vtime', r'\d{4}/\d{4}|\d{6}'),
    ('cnl', r'CNL'),
    ('ftime', r'FM\d{6}'),
    ('btime', r'BECMG\s+\d{4}/\d{4}'),
    ('ttime', r'TEMPO\s+\d{4}/\d{4}'),
    ('ptime', r'PROB\d{2}\s+\d{4}/\d{4}|PROB\d{2}(\s+\S+)\s+\d{4}/\d{4}'),
    ('wind', r'((VRB|\d{3})\d{2,3}(G\d{2,3})?|/////)(KT|MPS)'),
    ('cavok',r'CAVOK'),
    ('vsby', r'\d{4}|((%s|\d|P6)SM)|////' % _Fract),
    ('pcp', r'%s|TS(\s+%s)?' % (_pcptok, _pcptok)),
    ('obv', r'%s(\s+%s)*' % (_obvtok, _obvtok)),
    ('vcnty', r'VC\w+'),
    ('nsw', r'NSW'),
    ('sky', r'SKC|CLR|NSC|(%s(\s+%s)*)' % (_Cld, _Cld)),
    ('valyr',r'VA\d{6}'),
    ('icng',r'6\d{5}'),
    ('turb',r'5[0-9X]\d{4}'),
    ('altim', r'(QFE|QNH)\d{4}(INS)?'),
    ('temp', r'(T[NX]([M-]?\d{2})/\d{4}Z)'),
    ('llws', r'WS\d{3}/\d{5,6}KT'),
    ('pkdir',r'GST\s+DRCTN\s+\d{3}'),
    ('amd', r'AMD\s+(NOT\s|LTD\s).+'),
    ('amdcor', r'(AMD|COR)\s+\d{4,6}.+'),
    ('filterNewGrp',r'(?!(FM|TEMPO|BECMG|PROB|AMD|COR))\S+'),
    ('any', r'\S+'),
    #
    # Consume any unrecognized tokens except AMD and COR
    ('noAMDCOR', r'(?!(AMD|COR))\S+'),
]

_Tokens = '\n'.join([r"token %s: '%s' ;" % tok for tok in _TokList])

_Rules = r"""
START/e -> TAF/e $ e=self.taf() $ ;
TAF -> Prefix Main (BGroup|TGroup|PGroup)? (FGroup|BGroup|TGroup|PGroup|noAMDCOR)* (Amd|AmdCor)? any* ;
Main -> Ident ITime? ( Nil | (VTime ( Cnl | OWeather ))) $ self.add_group('FM') $ ;
FGroup -> FTime OWeather $ self.add_group('FM') $ ;
BGroup -> BTime OWeather $ self.add_group('BECMG') $ ;
TGroup -> TTime OWeather $ self.add_group('TEMPO') $ ;
PGroup -> PTime OWeather $ self.add_group('PROB') $ ;

OWeather -> (Wind|Cavok|Vsby|Pcp|Obv|Vcnty|Nsw|Sky|Temp|Altim|VALyr|Turb|Shear|Icng|PkDir|filterNewGrp)+ ;

Prefix -> prefix/x $ self.prefix(x) $ ;
Ident -> ident/x $ self.ident(x) $ ;
ITime -> itime/x $ self.itime(x) $ ;
VTime -> vtime/x $ self.vtime(x) $ ;
FTime -> ftime/x $ self.ftime(x) $ ;
BTime -> btime/x $ self.ttime(x) $ ;
TTime -> ttime/x $ self.ttime(x) $ ;
PTime -> ptime/x $ self.ptime(x) $ ;
Nil -> nil $ self._nil = True $ ;
Cnl -> cnl $ self._canceled = True $ ;
Wind -> wind/x $ self.wind(x) $ ;
Cavok -> cavok/x $ self.cavok(x) $ ;
Vsby -> vsby/x $ self.vsby(x) $ ;
Pcp -> pcp/x $ self.pcp(x) $ ;
Obv -> obv/x $ self.obv(x) $ ;
Nsw -> nsw/x $ self.nsw(x) $ ;
Vcnty -> vcnty/x $ self.vcnty(x) $ ;
Sky -> sky/x $ self.sky(x) $ ;
Temp -> temp/x $ self.temp(x) $ ;
Altim -> altim/x $ self.altim(x) $ ;
VALyr -> valyr/x $ self.valyr(x) $ ;
Turb -> turb/x $ self.turbicng(x,'turb') $ ;
Icng -> icng/x $ self.turbicng(x,'icng') $ ;
PkDir -> pkdir/x $ self.pkdir(x) $ ;
Shear -> llws/x $ self.llws(x) $ ;
Amd -> amd/x $ self.amd(x) $ ;
AmdCor -> amdcor/x $ self.amdcor(x) $ ;
"""

alist = [r'AMD\s+NOT\s+SKED(\s+(%s))?$' % _TimePhrase,
         r'AMD\s+LTD\s+TO(\s+(CLD|VIS|WX|AND|WIND)){1,5}(\s+(%s))?$' % _TimePhrase,
         ]

_AmdPat = re.compile('|'.join(alist))
_ConvectionPat = re.compile(r'TS|SH')
_AmdCorTime = re.compile(r'\d{4,6}')

##############################################################################
# local function
def fix_date(tms):
    """Tries to determine month and year from report timestamp"""
    now = time.time()
    t = calendar.timegm(tms)
    # tms contains day, hour, min of the report, current year and month
    if t > now + 3*86400.0:     # previous month
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

##############################################################################
# decoder class
class Decoder(tpg.VerboseParser):
    """TAF decoder class"""
    verbose = 0
    __doc__ = '\n'.join([_Options, _Separator, _Tokens, _Rules])

    def __call__(self, taf):

        self._taf = {'group': []}
        self._group = {'cavok' : 'false'}
        self._nil = False
        self._canceled = False

        i = taf.find('RMK')
        if i > 0:
            taf = taf[:i]

        self._text = taf.strip().replace('=','')
        return super(Decoder, self).__call__(self._text)

    def __index(self, pos, token):
        tmp = self.lexer.input[:pos]
        line = tmp.count('\n')
        row = pos - tmp.rfind('\n') - 1
        return ('%d.%d' % (line, row), '%d.%d' % (line, row+len(token)))

    def index(self):
        token = self.lexer.token()
        return self.__index(token.start, token.text)

    def taf(self):
        """Called by the parser at the end of work"""
        try:
            self._taf['itime']
        except KeyError:
            self._taf['itime'] = { 'value': self._taf['vtime']['from']}

        if self._nil:
            self._taf['bbb'] = 'NIL'

        elif self._canceled:
            self._taf['bbb'] = 'canceled'
            self._taf['group'] = []
            self._taf['prevtime'] = self._taf['vtime'].copy()
            self._taf['vtime']['from'] = self._taf['itime']['value']

        else:
            p = self._taf['group'][-1]
            p['prev']['time']['to'] = self._taf['vtime']['to']


        self.unparsed()

        return self._taf

    def whiteOut(self,index):
        #
        # Starting, ending line and character positions
        slpos,scpos = map(int,index[0].split('.'))
        elpos,ecpos = map(int,index[1].split('.'))

        if slpos == elpos:
            self.unparsedText[slpos][scpos:ecpos] = ' ' * (ecpos-scpos)

        else:
            self.unparsedText[slpos][scpos:] = ' ' * len(self.unparsedText[slpos][scpos:])
            self.unparsedText[elpos][:ecpos+1] = ' ' * (ecpos+1)

        #print 'removed',key
        #print '\n'.join([''.join(x) for x in self.unparsedText])

    def recurse_keys(self, d):
        if 'index' in d:
            self.whiteOut(d['index'])
        for k in d:
            if isinstance(d[k], dict):
                self.recurse_keys(d[k])
            if isinstance(d[k], list):
                for i in d[k]:
                    self.recurse_keys(i)

    def unparsed(self):

        self.unparsedText = [list(x) for x in self.lexer.input.split('\n')]

        #
        # Remove all tokens from input string that were successfully parsed.
        self.recurse_keys(self._taf)

        #
        # Before the RMK token, if there is one, should be considered an error
        # After the RMK token, it is considered text added by the observer
        #
        rmk_pos = -1
        additiveText = []
        unrecognized = []

        for lne in self.unparsedText:
            try:
                pos = lne.index('R')
                if pos > -1 and lne[pos:pos+3] == ['R','M','K']:
                    rmk_pos = pos
                    unrecognized.append(''.join(lne[:pos]))
                    additiveText.append(''.join(lne[pos+3:]))
                    break

            except ValueError:
                pass
            #
            # RMK not found yet
            if rmk_pos == -1:
                unrecognized.append(''.join(lne))
            else:
                additiveText.append(''.join(lne))
        #
        # Reassemble and remove superfluous whitespaces
        try:
            text = ' '.join(additiveText).strip()
            if len(text):
                self._taf['additive'] = {'str':text}

        except IndexError:
            pass

        try:
            text = ' '.join(unrecognized).strip()
            text = ' '.join(text.split())
            if len(text):
                self._taf['unparsed'] = {'str':text}

        except IndexError:
            pass

    def tokenOK(self, pos=0):
        """Checks whether token ends with a blank"""
        try:
            return self._text[self.lexer.token().stop+pos] == ' '
        except IndexError:
            return True

    def add_group(self, ctype):

        self._group['type'] = ctype
        if self._group['type'] in ['FM','BECMG']:
            if self._taf['group']:
                p = self._taf['group'][-1]
                p['prev']['time']['to'] = self._group['time']['from']

            self._taf['group'].append({'prev': self._group})

        else:
            period = self._taf['group'][-1]
            period.setdefault('ocnl',[]).append(self._group)

        self._group = { 'cavok' : 'false' }

    ###################################################################
    # Element checks
    def prefix(self,s):
        self._taf['type'] = {'str': s, 'index': self.index()}
        try:
            self._taf['bbb'] = s.split()[1]
        except IndexError:
            self._taf['bbb'] = ' '

    def ident(self, s):
        self._taf['ident'] = {'str': s, 'index': self.index()}

    def itime(self, s):

        self._group['type']= 'FM'
        d = self._taf['itime'] = {'str': s, 'index': self.index()}
        mday, hour, minute = int(s[:2]), int(s[2:4]), int(s[4:6])
        tms = list(time.gmtime())
        tms[2:6] = mday, hour, minute, 0
        fix_date(tms)
        d['value'] = calendar.timegm(tms)

    def vtime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}

        tms = list(time.gmtime())
        tms[2:6] = int(s[0:2]),int(s[2:4]),0,0
        fix_date(tms)

        if len(s) == 6:
            mday, shour, ehour = int(s[:2]), int(s[2:4]), int(s[4:6])
        else:
            mday, shour, eday, ehour = int(s[:2]), int(s[2:4]), int(s[5:7]), int(s[7:9])

        tms[2:6] = mday, shour, 0, 0
        fix_date(tms)
        d['from'] = calendar.timegm(tms)

        if len(s) == 6:
            period = ehour - shour
            if period <= 0:
                period += 24
            d['to'] = d['from'] + 3600*period
        else:
            tms[2:6] = eday, ehour, 0, 0
            fix_date(tms)
            d['to'] = calendar.timegm(tms)

        self._taf['vtime'] = self._group['time'].copy()
        try:
            d['from'] = min(self._taf['vtime']['from'],
                            self._taf['itime']['value'])
        except KeyError:
            d['from'] = self._taf['vtime']['from']

    def ftime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}

        mday, hour, minute = int(s[2:4]), int(s[4:6]), int(s[6:8])
        try:
            tms = list(time.gmtime(self._taf['vtime']['from']))
            tms[2:5] = mday, hour, minute

            t = calendar.timegm(tms)
            if t <= self._taf['vtime']['from']-1800:
                fix_date(tms)
                t = calendar.timegm(tms)

            d.update({'from': t, 'to': self._taf['vtime']['to']})

        except KeyError:
            pass

    def ttime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}
        tmp = s.split()[1]
        sday, shour, eday, ehour = int(tmp[:2]), int(tmp[2:4]),\
                                   int(tmp[5:7]),int(tmp[7:9])

        tms = list(time.gmtime(self._taf['vtime']['from']))
        tms[2:4] = sday,shour
        t = calendar.timegm(tms)
        if t < self._taf['vtime']['from']:
            fix_date(tms)

        t = calendar.timegm(tms)

        tms[2:4] = eday, ehour
        if eday < sday:
            tms[1] += 1

        d.update({'from': t, 'to': calendar.timegm(tms)})

    def ptime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}
        tokens = s.split()
        if len(tokens) == 3:
            #
            # Only PROB%% TEMPO is allowed in IWXXM
            if tokens[1] != 'TEMPO':
                d['str'] = '%s %s' % (tokens[0],tokens[2])

        tmp = tokens[-1]
        sday, shour, eday, ehour = int(tmp[:2]), int(tmp[2:4]),\
                                   int(tmp[5:7]),int(tmp[7:9])

        tms = list(time.gmtime(self._taf['vtime']['from']))
        tms[2:4] = sday, shour
        t = calendar.timegm(tms)
        if t < self._taf['vtime']['from']:
            fix_date(tms)

        t = calendar.timegm(tms)
        tms[2:4] = eday,ehour
        if eday < sday:
            tms[1] += 1

        d.update({'from': t, 'to': calendar.timegm(tms)})

    def cavok(self, s):
        self._group['cavok'] = 'true'
        return

    def wind(self, s):

        d = self._group['wind'] = {'str': s, 'index': self.index(), 'uom': 'm/s'}
        uompos = -3

        if s.startswith('VRB'):
            d['dd'] = 'VRB'
        elif s[:3] != '///':
            d['dd'] = s[:3]

        if s.endswith('KT'):
            d['uom'] = '[kn_i]'
            uompos = -2

        tok = s[3:uompos].split('G', 1)
        try:
            d['ff'] = tok[0]
        except ValueError:
            pass

        if len(tok) > 1:
            try:
                d['gg'] = tok[1]
            except ValueError:
                pass

    def vsby(self, s):
        #
        # Parser may confuse elevated layers with visibility
        if not self.tokenOK():
            raise tpg.WrongToken

        d = self._group['vsby'] = {'str': s, 'index': self.index(), 'uom': 'm'}

        if s.endswith('SM'):
            tok = ' '.join(s[:-2].split())
            d['value'] = _Vsby.get(tok,'10000')
        else:
            try:
                d['value'] = s
                if d['value'] == '9999':
                    d['value'] = '10000'

            except ValueError:
                pass

    def pcp(self, s):
        #
        # Volcanic ash layer maybe initially confused with the obstruction type
        if not self.tokenOK():
            raise tpg.WrongToken

        self._group['pcp'] = {'str': s, 'index': self.index()}

    def obv(self, s):
        #
        # Volcanic ash layer maybe initially confused with the obstruction type
        if not self.tokenOK():
            raise tpg.WrongToken

        d = self._group['obv'] = {'str': s, 'index': self.index()}

    def nsw(self, s):
        self._group['nsw'] = {'str': s, 'index': self.index()}

    def vcnty(self, s):
        self._group['vcnty'] = {'str': s, 'index': self.index()}

    def sky(self, s):
        self._group['sky'] = {'str': s, 'index': self.index()}

    def temp(self, s):
        try:
            d = self._group['temp']
        except KeyError:
            d = self._group['temp'] = {'str': s, 'index': self.index(), 'uom':'Cel'}

        temp,tstamp = s.split('/')
        sday,shour = int(tstamp[:2]), int(tstamp[2:4])

        tms = list(time.gmtime(self._taf['vtime']['from']))
        tms[2:4] = sday, shour
        t = calendar.timegm(tms)
        if t < self._taf['vtime']['from']:
            fix_date(tms)

        temp = temp.replace('M','-')

        if s[1] == 'X':
            d.update({'max': {'value': temp[2:], 'at': calendar.timegm(tms)}})
        else:
            d.update({'min': {'value': temp[2:], 'at': calendar.timegm(tms)}})

    def altim(self, s):

        d = self._group['altim'] = {'str': s, 'index': self.index(), 'type': s[:3], 'uom': 'hPa'}
        d['value'] = int(s[3:7])
        if s[7:] == 'INS':
            d['value'] = str(d['value']/100.)
            d['uom'] = "[in_i'Hg]"

    def valyr(self, s):

        try:
            d = self._group['valyr']
            d['index'] = (d['index'][0],self.index()[1])
            d['str'] += ' %s' % s
            d['base'] += ' %s' % str(int(s[2:5])*100)
            d['top'] += ' %s' % str(int(s[5:8])*100)

        except KeyError:

            d = self._group['valyr'] = {'str': s, 'index': self.index(), 'uom':'[ft_i]'}
            d['base'] = str(int(s[2:5])*100)
            d['top']  = str(int(s[5:8])*100)

    def turbicng(self, s, p):

        base = str(int(s[2:5])*100)
        top = str(int(base) + int(s[5])*1000)

        try:
            d = self._group[p]
            d['index'] = (d['index'][0],self.index()[1])
            d['str'] += ' %s' % s
            #
            # If base of this layer coincides with the top of the last layer,
            # combine old base with new top.
            #
            if base == d['top'].split()[-1]:
                d['top'] = top
            else:
                d['base'] += ' %s' % base
                d['top'] += ' %s' % top
                d['type'] += ' %s' % s[1]

        except KeyError:

            d = self._group[p] = {'str': s, 'index': self.index(), 'uom':'[ft_i]'}
            d['type'] = s[1]
            d['base'] = base
            d['top']  = top

    def llws(self, s):

        d = self._group['llws'] = {'str': s, 'index': self.index()}
        h = int(s[2:5])
        dd = s[6:9]
        ff = s[9:-2]
        d.update({ 'hgt': str(h*100), 'dd': dd, 'ff': ff })

    def pkdir(self, s):
        self._group['pkdir'] = {'str': s, 'index': self.index()}

    def amd(self, s):
        if not self.tokenOK():
            raise tpg.WrongToken

        # s does not contain the whole phrase
        phrase = (s+self.lexer.input[self.lexer.pos:]).rstrip()
        m = _AmdPat.match(phrase)
        tms = list(time.gmtime())
        alist = []
        if m:
            s = m.group()
            ix0 = self.index()[0]
            row, col = ix0.split('.')
            col = int(col)+len(s)
            ix = (ix0, '%s.%d' % (row, col))
            self._taf['amd'] = {'str': s, 'index': ix}
            #
            # If reference to time is found in the AMD clause one of these
            # groups will have it.
            #
            timestr = m.group(4) or m.group(5) or m.group(11) or m.group(12)
            if m.group(4) or m.group(11):

                tms[2:6] = int(timestr[:2]),int(timestr[2:4]),int(timestr[-2:]),0
                fix_date(tms)

                if (m.group(3) or m.group(10)) == 'TIL':
                    self._taf['amd']['time'] = {'from':self._taf['itime']['value'],'to':calendar.timegm(tms)}
                elif (m.group(3) or m.group(10)) == 'AFT':
                    self._taf['amd']['time'] = {'from':calendar.timegm(tms),'to':self._taf['vtime']['to']}
            #
            # The D1H1/D2H2 case
            elif m.group(5) or m.group(12):

                for key,timestr in zip(['from','to'],timestr.split('/')):
                    tms[2:6] = int(timestr[0:2]),int(timestr[2:4]),0,0
                    fix_date(tms)
                    alist.append((key,calendar.timegm(tms)))

                self._taf['amd']['time'] = dict(alist)
            #
            # If no reference to time, then its the entire time period of the TAF
            else:
                self._taf['amd']['time'] = self._taf['vtime'].copy()
                self._taf['amd']['time']['from'] = self._taf['itime']['value']

    def amdcor(self, s):
        #
        # Issue time should be close to valid time
        frm = list(time.gmtime(self._taf['vtime']['from']))

        m = _AmdCorTime.search(s)
        tstamp = s[m.start():m.end()]
        if len(tstamp) == 4:
            frm[3:5] = int(tstamp[:2]),int(tstamp[2:])
        elif len(tstamp) == 6:
            frm[2:5] = int(tstamp[:2]),int(tstamp[2:4]),int(tstamp[4:])

        fix_date(frm)
        self._taf['itime'] = {'str': s, 'index': self.index(), 'value':calendar.timegm(frm)}
        self._taf['vtime']['value'] = self._taf['itime']['value']

def Usage():
    """Provide instructions and explanation of TAF decoder/encoder command line arguments"""

    d = argparse.ArgumentParser(description="Run the TAF decoder/encoder unit testing",
                                epilog="""Decoder will wait for input on standard input. Pressing
                                ^D (ETX) will send the raw text to the decoder. Corresponding
                                XML document is written to standard output""",
                                formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    d.add_argument("--version", action="version", version="%(prog)s 1.0")
    #
    d.add_argument('-r','--readable', default=False, action='store_true',
                   help='XML encoder produces a pretty document for human eyes')
    d.add_argument('-n','--namespaces', default=False, action='store_true',
                   help='XML encoder will include namespace declarations in root element')
    return d

if __name__ == '__main__':

    import argparse, sys
    import TAFXMLEncoder as TXE
    #
    # Process command line
    parser = Usage()
    cmdlne = parser.parse_args()

    decoder = Decoder()
    encoder = TXE.XMLEncoder()
    #
    # While intr (^C) sequence not pressed . . .
    while True:
        try:
            #
            # Read input until ^D (ETX)
            taf = ''.join(sys.stdin.readlines())
            #taf = ''.join(fh.readlines())
            result = decoder(taf.replace('\n',''))

        except tpg.SyntacticError, e:
            print taf
            print str(e)

        except KeyboardInterrupt:
            break
        #
        # Pass results to encoder. The first argument, the python dictionary,
        # is required. The other two arguments affect the resulting XML document.
        #
        try:
            encoder(result,nameSpaceDeclarations=cmdlne.namespaces,report=taf)
            encoder.printXML(sys.stdout,readable=cmdlne.readable)

        except KeyError:
            print '%s not found in station directory file' % result['ident']['str']

