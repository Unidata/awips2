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
#       TafDecoder.py
#       GFS1-NHD:A7822.0000-SCRIPT;1.65
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.65 (DELIVERED)
#         Created:  21-OCT-2009 21:53:54      OBERFIEL
#           VC events not handled correctly for VRB winds.
#       
#       Revision 1.64 (DELIVERED)
#         Created:  21-SEP-2009 07:14:57      OBERFIEL
#           Fixed monitoring of TEMPO groups at start of forecast.
#           Monitoring corrected TAFs also fixed.
#           Showers are considered convective and so VRB with speeds >
#           6kts is allowed.
#       
#       Revision 1.63 (DELIVERED)
#         Created:  16-JUN-2009 14:41:14      OBERFIEL
#           Added checks on misuse of LLWS in conditional groups.
#       
#       Revision 1.62 (DELIVERED)
#         Created:  15-MAY-2009 15:09:08      OBERFIEL
#           Fixed crash when bad wind group in prevailing fcst is
#           inherited by the occasional group
#           TEMPO or PROB30.
#       
#       Revision 1.61 (REVIEW)
#         Created:  14-APR-2009 10:36:34      OBERFIEL
#           Added new checks for AMD NOT SKED and AMD LTD TO clauses,
#           proper use of NSW and VRB winds.
#       
#       Revision 1.60 (REVIEW)
#         Created:  30-MAR-2009 23:01:09      OBERFIEL
#           Fixed typo in check_wind() routine.
#       
#       Revision 1.59 (REVIEW)
#         Created:  30-MAR-2009 18:31:54      OBERFIEL
#           Removed obsolete TAF format code.  Fixed VRB winds with
#           convection. Fixed NSW handling.
#           Added checks for times in AMD clauses.
#       
#       Revision 1.58 (DELIVERED)
#         Created:  10-MAR-2009 08:49:20      OBERFIEL
#           Added new check for end of month, e.g. 31st April.  Fixed
#           typos and added overlooked check for wind shear in
#           TEMPO/PROB groups.
#       
#       Revision 1.57 (REVIEW)
#         Created:  10-DEC-2008 12:46:59      OBERFIEL
#           Documented patched OB9 TafDecoder.py
#       
#       Revision 1.56 (DELIVERED)
#         Created:  10-DEC-2008 12:40:20      OBERFIEL
#           Documented patched OB8.3.1 TafDecoder.py
#       
#       Revision 1.55 (DELIVERED)
#         Created:  02-DEC-2008 10:11:20      OBERFIEL
#           Used fix_date routine to handle month-to-month and
#           year-to-year transitions.
#       
#       Revision 1.54 (DELIVERED)
#         Created:  19-NOV-2008 14:08:03      OBERFIEL
#           Fixed swapped dates/hours in TEMPO and PROB30 groups
#       
#       Revision 1.53 (DELIVERED)
#         Created:  31-OCT-2008 11:40:08      OBERFIEL
#           Fixed beginning of month problem with TEMPO and PROB30
#           groups
#       
#       Revision 1.52 (DELIVERED)
#         Created:  29-OCT-2008 12:57:54      OBERFIEL
#           Fixed end-of-month/beginning-of-next-month bug
#       
#       Revision 1.51 (DELIVERED)
#         Created:  28-AUG-2008 15:28:43      OBERFIEL
#           NIL AMD is now flagged.  TAF COR is now allowed.
#       
#       Revision 1.50 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.49 (DELIVERED)
#         Created:  30-JUN-2008 14:42:35      OBERFIEL
#           Initial guess for part-time TAF, pending consensus from
#       
#       Revision 1.48 (REVIEW)
#         Created:  19-JUN-2008 14:19:35      OBERFIEL
#           Allowed different TAF formats without raising fatal error.
#       
#       Revision 1.47 (DELIVERED)
#         Created:  31-MAR-2008 12:48:21      OBERFIEL
#           Corrected valid_day for OB8.3 release
#       
#       Revision 1.46 (DELIVERED)
#         Created:  03-MAR-2008 10:08:20      OBERFIEL
#           Fixed Leap-Day bug.
#       
#       Revision 1.45 (APPROVED)
#         Created:  29-FEB-2008 09:55:46      OBERFIEL
#           Fixed bug with leap-day.  Made the logic a bit more clear.
#       
#       Revision 1.44 (DELIVERED)
#         Created:  26-FEB-2008 14:25:36      OBERFIEL
#           Fixed _talk notification (unimplemented) and DTG for Nov
#           2008
#       
#       Revision 1.43 (DELIVERED)
#         Created:  27-JUN-2007 13:14:16      OBERFIEL
#           MetarMonitorP.py revamped logic and created new rule for
#           Alaska region.
#           TafDecoder.py changed to allow variable AMD LTD element
#           list
#       
#       Revision 1.42 (DELIVERED)
#         Created:  14-MAY-2007 10:04:50      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.41 (DELIVERED)
#         Created:  21-JUN-2006 08:57:36      TROJAN
#           spr 7162: fixed start time of the first TAF group
#       
#       Revision 1.40 (DELIVERED)
#         Created:  20-JUN-2006 08:28:29      TROJAN
#           spr 7163: fixed start time of the first TAF group
#       
#       Revision 1.39 (DELIVERED)
#         Created:  02-JUN-2006 10:28:09      TROJAN
#           spr 7160: changed regular expression for visibility
#       
#       Revision 1.38 (DELIVERED)
#         Created:  02-JUN-2006 09:04:22      TROJAN
#           spr 7159: changed regular expression for visibility
#       
#       Revision 1.37 (DELIVERED)
#         Created:  03-MAY-2006 13:49:15      TROJAN
#           SPR 7139: fixed issue/valid time checks
#       
#       Revision 1.36 (DELIVERED)
#         Created:  03-MAY-2006 13:28:39      TROJAN
#           SPR 7140: fixed issue/valid time checks
#       
#       Revision 1.35 (DELIVERED)
#         Created:  14-APR-2006 13:17:50      TROJAN
#           spr 7118
#       
#       Revision 1.34 (DELIVERED)
#         Created:  14-APR-2006 08:42:21      TROJAN
#           spr 7117
#       
#       Revision 1.33 (APPROVED)
#         Created:  14-APR-2006 08:24:42      TROJAN
#           spr 7117
#       
#       Revision 1.32 (DELIVERED)
#         Created:  27-MAR-2006 08:30:43      TROJAN
#           spr 7112: added check for valid time of FM group for
#           regular issuance TAFs
#       
#       Revision 1.31 (DELIVERED)
#         Created:  19-JAN-2006 11:11:08      OBERFIEL
#           Numerous changes to sync up platforms
#       
#       Revision 1.30 (APPROVED)
#         Created:  03-NOV-2005 13:16:33      TROJAN
#           spr 7051
#       
#       Revision 1.29 (APPROVED)
#         Created:  12-OCT-2005 18:29:27      TROJAN
#           spr 7038
#       
#       Revision 1.28 (DELIVERED)
#         Created:  19-SEP-2005 13:47:39      TROJAN
#           spr 7011
#       
#       Revision 1.27 (APPROVED)
#         Created:  08-SEP-2005 14:40:39      TROJAN
#           spr 7011
#       
#       Revision 1.26 (DELIVERED)
#         Created:  19-AUG-2005 19:50:32      TROJAN
#           spr 6999
#       
#       Revision 1.25 (APPROVED)
#         Created:  17-AUG-2005 19:50:31      TROJAN
#           spr 6992
#       
#       Revision 1.24 (APPROVED)
#         Created:  16-AUG-2005 13:23:17      TROJAN
#           spr 6987
#       
#       Revision 1.23 (APPROVED)
#         Created:  12-AUG-2005 12:39:09      OBERFIEL
#           Volcanic Ash can be forecasted regardless of visibility.
#       
#       Revision 1.22 (APPROVED)
#         Created:  09-AUG-2005 15:02:46      TROJAN
#           spr 6975
#       
#       Revision 1.21 (DELIVERED)
#         Created:  26-JUL-2005 18:27:56      TROJAN
#           spr 6946
#       
#       Revision 1.20 (APPROVED)
#         Created:  06-JUL-2005 18:16:41      TROJAN
#           spr 6548
#       
#       Revision 1.19 (DELIVERED)
#         Created:  13-MAY-2005 18:56:28      TROJAN
#           spr 6834
#       
#       Revision 1.18 (REVIEW)
#         Created:  12-MAY-2005 14:20:56      TROJAN
#           spr 6834
#       
#       Revision 1.17 (REVIEW)
#         Created:  07-MAY-2005 11:38:28      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.16 (DELIVERED)
#         Created:  26-APR-2005 15:07:40      TROJAN
#           spr 6812
#       
#       Revision 1.15 (REVIEW)
#         Created:  26-APR-2005 14:12:30      TROJAN
#           spr 6812
#       
#       Revision 1.14 (APPROVED)
#         Created:  26-APR-2005 13:56:52      TROJAN
#           spr 6812
#       
#       Revision 1.13 (DELIVERED)
#         Created:  06-APR-2005 11:46:47      TROJAN
#           spr 6776
#       
#       Revision 1.12 (APPROVED)
#         Created:  04-APR-2005 15:12:25      TROJAN
#           spr 6778
#       
#       Revision 1.11 (DELIVERED)
#         Created:  01-MAR-2005 21:07:14      TROJAN
#           spr 6689
#       
#       Revision 1.10 (DELIVERED)
#         Created:  14-FEB-2005 20:54:51      TROJAN
#           spr 6649
#       
#       Revision 1.9 (APPROVED)
#         Created:  24-JAN-2005 21:18:47      TROJAN
#           spr 6612
#       
#       Revision 1.8 (APPROVED)
#         Created:  24-JAN-2005 17:49:56      TROJAN
#           spr 6608
#       
#       Revision 1.7 (APPROVED)
#         Created:  19-JAN-2005 15:13:59      TROJAN
#           spr 6565
#       
#       Revision 1.6 (APPROVED)
#         Created:  07-DEC-2004 19:18:35      TROJAN
#           spr 6527
#       
#       Revision 1.5 (UNDER WORK)
#         Created:  21-OCT-2004 19:33:49      TROJAN
#           spr 6398
#       
#       Revision 1.4 (APPROVED)
#         Created:  01-OCT-2004 13:38:14      TROJAN
#           spr 6399
#       
#       Revision 1.3 (APPROVED)
#         Created:  19-AUG-2004 20:49:16      OBERFIEL
#           Code chage
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 18:07:07      OBERFIEL
#           Updated to fix problem with NSW
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:45:02      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7431
#       	Action Date:       22-OCT-2009 20:05:48
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: OB9.2 installation breaks mtrs.cfg file
#       
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02APR2014       17211         zhao (code obtained from the listserver via Virgil that implements a new rule regarding CB, TS etc) 
#
#
#
import exceptions, re, time, types
import tpg
import Avn, AvnLib, AvnParser
###############################################################################
# local exceptions

class Error(exceptions.Exception): pass
class Warning1(exceptions.Exception): pass  # avoiding name clash

###############################################################################
# dictionary of errors and warnings
_Errors = { \
11: """Thunderstorm forecast requires CB in the cloud 
group (NWSI 10-813, Appendix C, 1.2.7.3)""",
12: """Visibility <= 6SM requires forecast of significant
weather (NWSI 10-813, Appendix C, 1.2.5)""",
13: """Volcanic ash requires visibility forecast 
(NWSI 10-813, Appendix C, 1.2.6)""",
14: """FG or FZFG forecast requires visibility < 5/8SM,
MIFG requires visibility >= 5/8SM
(NWSI 10-813, Appendix C, 1.2.6)""",
15: """BR forecast requires visibility between 5/8SM
and 6SM (NWSI 10-813, Appendix C, 1.2.6)""",
16: """PROB group must include forecast of a thunderstorm
or precipitation event (NWSI 10-813, Appendix C, 1.2.9.4)""",
17: """Invalid sky cover sequence 
(NWSI 10-813, Appendix C, 1.2.7.1)""",
18: """Invalid weather with visibility >= 6SM 
(NWSI 10-813, Appendix C, 1.2.5)""",
19: """Missing terminating blank""",
20: """Invalid datetime format""",
21: """Invalid end hour""",
22: """Invalid start hour""",
23: """Invalid day""",
24: """Issue and valid times do not match""",
25: """Group time period not within TAF forecast period""",
26: """Only PROB30 is allowed""",
27: """The PROB group shall not be used in the first 
9 hours of the valid TAF forecast 
(NWSI 10-813, Appendix C, 1.2.9.4)""",
28: """Weather in the vicinity shall not be used in TEMPO
or PROB groups (NWSI 10-813, Appendix C, 1.2.6.1)""",
29: """Wrong day of the month""",
30: """Invalid value of wind direction""", 
31: """Invalid value of wind speed""",
32: """Wind gust <= wind speed""",
33: """Forecast of non-convective low-level wind shear 
shall not be included in TEMPO or PROB groups 
(NWSI 10-813, Appendix C, 1.2.8)""",
34: """Missing weather in this group""",
35: """Invalid value of wind shear direction""",
36: """Invalid value of wind shear height""",
37: """Invalid value of visibility 
(NWSI 10-813, Appendix C, 1.2.5)""",
38: """Invalid value of sig weather 
(NWSI 10-813, Appendix B, 4)""", 
39: """Unnecessary leading zero for wind shear speed""",
40: """Invalid cloud amount
(NWSI 10-813, Appendix C, 1.2.7.1)""",
41: """Invalid cloud base 
(NWSI 10-813, Appendix C, 1.2.7.1)""",
42: """Cannot forecast partial obscuration 
(NWSI 10-813, Appendix C, 1.2.7.2)""",
43: """Invalid VC weather. Allowed entries:
VCFG, VCSH, VCTS. 
(NWSI 10-813, Appendix C, 1.2.6.1)""",
44: """CLR shall not be used in TAF
(NWSI 10-813, Appendix C, 1.2.7.1)""",
45: """Invalid precipitation string
(NWSI 10-813, Appendix C, 1.2.6)""",
46: """P6SM needed with NSW in this group
(NWSI 10-813, Appendix C, 1.2.6)""",
47: """Consecutive NSW groups not permitted
(NWSI 10-813, Appendix C, 1.2.6)""",
48: """When reduction in visibility is forecast
to change in TEMPO group, the significant weather
causing the deterioration shall be included 
(NWSI 10-813, Appendix C, 1.2.9.2)""",
49: """Valid time of FM group must be > valid time for 
the entire TAF or a previous FM group""",
50: """Valid time must be >= valid time of previous
TEMPO or PROB group""",
51: """Bad TEMPO/PROB group duration""",
52: """Valid time of TEMPO/PROB must be >= valid time 
of previous FM group""",
53: """+PL requires visibility < 3SM
PL requires visibility <= 6SM (FMH#1, p 8-3)""",
54: """+SN or +DZ requires visibility <= 1/4SM 
SN or DZ requires visibility <= 1/2SM 
(FMH#1, p 8-3)""",
55: """+SS or +DS requires visibility <= 1/4SM 
SS or DS requires visibility <= 1/2SM 
(NWSI 10-813, B-4)""",
56: """The period covered by a TEMPO group will not 
exceed 4 hours (NWSI 10-813, 4.3.4)""",
57: """Repeated occurence of weather elements""",
59: """Invalid AMD phrase. Valid phrases are:
AMD NOT SKED
AMD NOT SKED AFT ddHHmm
AMD NOT SKED TIL ddHHmm
AMD NOT SKED ddHH/ddHH
AMD LTD TO (element list) (AFT ddHHmm, or TIL ddHHmm, or
ddHH/ddHH)""",
60: """NSW not needed""",
61: """The period covered by a TAF shall not exceed 30
hours""",
81: """CB may only be mentioned when TS or VCTS mentioned
(NWSI 10-813, Appendix B, 1.2.7.3)""",
}

_Warnings = { \
#11: """Forecast weather in the vicinity should be
#the last entry in the weather group 
#(NWSI 10-813, 1.2.6.1)""",
12: """Number of cloud groups should not exceed three
(NWSI 10-813, Appendix C, 1.2.7.1)""",
13: """The period of time covered by a PROB should be 
6 hours or less (NWSI 10-813, Appendix C, 1.2.9.4)""",
14: """Tornadoes or Waterspouts should not be
forecast in terminal forecasts
(NWSI 10-813, Appendix B, 4)""",
15: """Suspicious value of wind speed""",
16: """Suspicious value of wind gust""",
17: """Suspicious value of llws speed""",
19: """AMD restriction not within TAF forecast period""",
31: """Variable wind speed must be between 1 and 6KT
without convective activity.
(NWSI 10-813, Appendix C, 1.2.4)""",
}

_Messages = {'error': _Errors, 'warning': _Warnings}

###############################################################################
# valid wx elements
# NWSI 10-813, 1.2.5
_ValidVsby = { \
    '0': 0.0,
    'M1/4': 0.0,
    '1/4': 0.25,
    '1/2': 0.5,
    '3/4': 0.75,
    '1': 1.0,
    '1 1/2': 1.5,
    '2': 2.0,
    '3': 3.0,
    '4': 4.0,
    '5': 5.0,
    '6': 6.0,
    'P6': 99.0
}

# Appendix B, NWSI 10-813
_ValidObvis = dict.fromkeys(['BR', 'FG', 'FZFG', 'MIFG', 'PRFG', 'BCFG', \
    'FU', 'VA', 'HZ', 'BLPY', \
    'DU', 'DRDU', 'BLDU', \
    'SA', 'DRSA', 'BLSA', \
    'BLSN', 'BLSA', 'BLDU', \
    'PO', 'SQ', 'FC', '+FC', 'SS', '+SS', 'DS', '+DS'])

_ValidPcp = dict.fromkeys(['-DZ', 'DZ', '+DZ', '-FZDZ', 'FZDZ', '+FZDZ', \
    '-RA', 'RA', '+RA', '-SHRA', 'SHRA', '+SHRA', \
        '-TSRA', 'TSRA', '+TSRA', '-FZRA', 'FZRA', '+FZRA', \
    '-SN', 'SN', '+SN', '-SHSN', 'SHSN', '+SHSN', \
        '-TSSN', 'TSSN', '+TSSN', \
    '-PL', 'PL', '+PL', '+SHPL', 'SHPL', '+SHPL', \
        '-TSPL', 'TSPL', '+TSPL', \
    '-SG', 'SG', '+SG', \
    'IC', \
    'GR', 'SHGR', 'TSGR', \
    'GS', 'SHGS', 'TSGS', \
    'TS'])

_ValidVcnty = dict.fromkeys(['TS', 'SH', 'FG'])

# NWSI 10-813, 1.2.7
_ValidCover = {'FEW': 1, 'SCT': 2, 'BKN': 3, 'OVC': 4}

# NWSI 10-813, 1.2.6
_UnltdVsbyWx = dict.fromkeys(['VA', 'DRDU', 'DRSA', 'DRSN', 'MIFG', \
    'PRFG', 'BCFG'])

##############################################################################
# parser stuff
_Cld = '(FEW|SCT|BKN|OVC|VV)\d{3}(CB)?'
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
    ('prefix', r'TAF(\s+(AMD|COR))?'),
    ('ident', r'[A-Z][A-Z0-9]{3}'),
    ('itime', r'\d{6}Z'),
    ('nil', r'NIL'),
    ('vtime', r'\d{4}/\d{4}'),
    ('ftime', r'FM\d{6}'),
    ('ttime', r'TEMPO \d{4}/\d{4}'),
    ('ptime', r'PROB\d{2}\s+\d{4}/\d{4}'),
    ('wind', r'(VRB|\d{3})\d{2,3}(G\d{2,3})?KT'),
    ('vsby', r'(%s|\d|P6)SM' % _Fract),
    ('pcp', r'%s|TS(\s+%s)?' % (_pcptok, _pcptok)),
    ('obv', r'%s(\s+%s)*' % (_obvtok, _obvtok)),
    ('vcnty', r'VC\w+'),
    ('nsw', r'NSW'),
    ('sky', r'SKC|CLR|(%s(\s+%s)*)' % (_Cld, _Cld)),
    ('llws', r'WS\d{3}/\d{5,6}KT'),
    ('amd', r'AMD\s+(NOT\s+|LTD\s+)[^=]+'),
]

_Tokens = '\n'.join([r"token %s: '%s' ;" % tok for tok in _TokList])

_Rules = r"""
START/e -> TAF/e $ e=self.taf() $ ;
TAF -> Prefix? Main (TGroup | PGroup)? (FGroup (TGroup | PGroup)?)* Amd? '(=.*|$)' ;
Main -> Ident ITime VTime/t (Nil | FWeather) $ self.add_group('FM') $ ;
FGroup -> FTime FWeather $ self.add_group('FM') $ ;
TGroup -> TTime/t TWeather $ self.add_group('TEMPO') $ ;
PGroup -> PTime/t PWeather $ self.add_group('PROB') $ ;
FWeather -> Wind Vsby Pcp? Obv? Vcnty? Sky Shear? ;
TWeather -> Wind? Vsby? Pcp? Obv? Vcnty? Nsw? Sky? Shear? ;
PWeather -> Wind? Vsby? Pcp? Obv? Vcnty? Sky? Shear? ;

Prefix -> prefix/x $ self.prefix(x) $ ;
Ident -> ident/x $ self.ident(x) $ ;
ITime -> itime/x $ self.itime(x) $ ;
VTime -> vtime/x $ self.vtime(x) $ ;
FTime -> ftime/x $ self.ftime(x) $ ;
TTime -> ttime/x $ self.ttime(x) $ ;
PTime -> ptime/x $ self.ptime(x) $ ;
Nil -> nil $ self._nil = 1 $ ;
Wind -> wind/x $ self.wind(x) $ ;
Vsby -> vsby/x $ self.vsby(x) $ ;
Pcp -> pcp/x $ self.pcp(x) $ ;
Obv -> obv/x $ self.obv(x) $ ;
Nsw -> nsw/x $ self.nsw(x) $ ;
Vcnty -> vcnty/x $ self.vcnty(x) $ ;
Sky -> sky/x $ self.sky(x) $ ;
Shear -> llws/x $ self.llws(x) $ ;
Amd -> amd/x $ self.amd(x) $ ;
"""

##############################################################################
# to produce a meaningful error messsage
_TokDict = {
    'prefix': 'TAF or TAF AMD or TAF COR',
    'ident': 'site id',
    'itime': 'issue time',
    'nil': 'NIL',
    'vtime': 'valid time',
    'ftime': 'FMddHHMM',
    'ttime': 'TEMPO ddHH/ddhh',
    'ptime': 'PROB30 ddHH/ddhh',
    'wind': 'wind',
    'vsby': 'visibility',
    'pcp': 'precipitation or thunder',
    'obv': 'obstruction to vision',
    'vcnty': 'weather in vicinity',
    'nsw': 'no significant wx',
    'sky': 'sky conditions',
    'llws': 'low level wind shear',
    'amd': 'AMD ... phrase',
}

alist = [r'AMD\s+NOT\s+SKED(\s+(%s))?$' % _TimePhrase, 
         r'AMD\s+LTD\s+TO(\s+(CLD|VIS|WX|AND|WIND)){1,5}(\s+(%s))?$' % _TimePhrase]
_AmdPat = re.compile('|'.join(alist))
_ConvectionPat = re.compile(r'TS|SH')

##############################################################################
# local functions
def valid_base(base):
    """Checks if cloud base is valid"""
    if base <= 30:
        return True
    elif base <= 50:
        return base % 5 == 0
    else:
        return base % 10 == 0

def invalid_pl_vsby(i, v):
    """Checks if visibility is inconsistent with PL"""
    if i == '+' and v >= 3.0:
        return True
    elif i == '' and v > 6.0:
        return True
    else:
        return False

def invalid_sn_vsby(i, v):
    """Checks if visibility is inconsistent with SN or DZ
Returns 0 if consistent, -1 if too high, +1 if too low"""
    if i == '+':
        if v > 0.25:
            return -1
        else:
            return 0
    elif i == '':
        if v > 0.5:
            return -1
        elif v <= 0.25:
            return 1
        else:
            return 0
    elif i == '-':
        if v <= 0.5:
            return 1
        else:
            return 0
    else:
        return 0

def invalid_fg_vsby(s, v):
    """Checks if visibility is inconsistent with FG"""
    # NWSI 10-813, 1.2.6
    if len(s) == 2 or s.startswith('FZ'):
        if v > 0.6:
            return True
    elif s.startswith('MI'):
        if v < 0.6:
            return True
    return False

def invalid_br_vsby(v):
    """Checks if visibility is inconsistent with BR"""
    return not 0.6 < v < 6.1

def invalid_ds_vsby(i, v):
    """Checks if visibility is inconsistent with DS or SS"""
    if i == '+' and v >= 0.3:
        return True
    elif i == '' and not 0.3 < v < 0.6:
        return True
    return False

def check_sky(tok, cover, base, cig):
    """Verifies validity of cloud layer. 
Calculates summary cover, base, cig. Raises exception on error. 
Arguments: (cover, base, cig) evaluated from lower layers."""
    if tok.startswith('VV'):
        if cover != 0:
            raise Error(_Errors[40])
        base = cig = int(tok[2:5])
    elif tok == 'CLR':
        cig = Avn.CLEAR
    elif tok == 'SKC':
        cig = Avn.UNLIMITED
    else:
        tcover, tbase = _ValidCover.get(tok[:3], None), int(tok[3:6])
        if tcover is None or tcover < cover or cover == 4:
            raise Error(_Errors[17])
        if not valid_base(tbase) or tbase <= base:
            raise Error(_Errors[41])
        if tbase == 0 and tcover != 4:
            raise Error(_Errors[42])
        if tcover in [3, 4]:
            cig = min(cig, tbase)
        cover, base = tcover, tbase

    return cover, base, cig

def valid_day(tms):
    """Checks if day of month is valid"""
    year, month, day = tms[:3]
    if day > 31:
        return False
    if month in [4, 6, 9, 11] and day > 30:
        return False
    if month == 2:
        if (year%4 and day > 28) or day > 29:
            return False
    return True

def fix_date(tms):
    """Tries to determine month and year from report timestamp"""
    now = time.time()
    t = time.mktime(tms) - time.timezone
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

def get_prev_pcp(s):
    """Parses precipitation string to extract prevailing precipitation.
Return tuple: (prev, other). Skips stand-alone TS"""
    if s.startswith('TS'):
        s = s.split()[-1]
    if s[0] in '-+':
        n = 1
    else:
        n = 0
    if s[n:n+2] in _PcpQ:
        n += 2
    return s[:n+2], s[n+2:]

def add_msg(d, key, msg):
    """Adds text error message to dictionary d. msg is either a text or
message number from _Errors or _Warnings"""
    if type(msg) == types.IntType:
        msg = _Messages[key].get(msg, 'Unknown %s %d' % (key, msg))
    else:
        msg = str(msg)
    if key in d:
        d[key].append(msg)
    else:
        d[key] = [msg]

##############################################################################
# decoder class
class Decoder(tpg.VerboseParser):
    """TAF decoder class"""
    verbose = 0
    __doc__ = '\n'.join([_Options, _Separator, _Tokens, _Rules])

    def __call__(self, taf, bbb=None, firstline=0, strict=False):
        """Decodes TAF. taf starts with CCCC ddhhmmZ"""
        if type(taf) == types.ListType:
            taf = '\n'.join(taf)
        if not bbb:
            bbb = '   '
        self._taf = {'bbb': bbb, 'group': []}
        self._strict = strict
        self._group = {}
        self._first = firstline
        self._cutoff = 0
        self.expected = []
        self._nil = 0
        self.bad = {}
        # strip trailing '=' and add a terminating blank
        i = taf.find('=')
        if i > 0:
            taf = taf[:i]
        self._text = taf + ' '
        try:
            return super(Decoder, self).__call__(self._text)
        except tpg.SyntacticError:
            if self.expected:
                return {'index': self.bad['index'], \
                    'fatal': ['Invalid word %s. Expecting one of:\n%s' % \
                        (self.bad['text'], '\n'.join(self.expected))]}
            else:
                return {'index': self.bad['index'], \
                    'fatal': ['Unexpected end after %s.' % self.bad['text']]}
        except Exception, e:
            # highlight only site id
            row = self._first+2
            index = ('%d.%d' % (row, 0), '%d.%d' % (row, 4))
            return {'index': index, 'fatal': ['TAF decoder bug', str(e)]}

    def __index(self, pos, token):
        tmp = self.lexer.input[:pos]
        line = tmp.count('\n') + self._first + 1
        row = pos - tmp.rfind('\n') - 1
        return ('%d.%d' % (line, row), '%d.%d' % (line, row+len(token)))

    def index(self):
        token = self.lexer.token()
        return self.__index(token.start, token.text)

    def tokenOK(self, pos=0):
        """Checks whether token ends with a blank"""
        return self._text[self.lexer.token().stop+pos] in ' \t\n'

    def taf(self):
        """Called by the parser at the end of work"""
        if self._strict:
            self.check_issue_time()
        return self._taf

    def eatCSL(self, name):
        """Overrides super definition"""
        try:
            value = super(Decoder, self).eatCSL(name)
            self.bad = {}
            self.expected = []
            return value
        except tpg.WrongToken:
            if self.lexer.input[self.lexer.pos:]:
                bad = self.lexer.input[self.lexer.pos:].split(None, 1)[0]
                if self.bad:
                    if bad == self.bad['text']:
                        self.expected.append(_TokDict.get(name, ''))
                else:
                    self.bad['text'] = bad
                    self.bad['index'] = self.__index(self.lexer.pos, bad)
                    self.expected.append(_TokDict.get(name, ''))
            else:
                bad = self.lexer.input.split()[-1]
                self.bad['text'] = bad
                self.bad['index'] = self.__index(self.lexer.pos-len(bad)-1, bad)
            raise

    def add_group(self, type):
        """Checks for valid syntax between elements in a group and
        and valid times""" 
        if self._nil or not self._group:
            return
        if type == 'PROB':
            self.check_prob_group()
        if type == 'TEMPO':
            self.check_tempo_group()
            
        self._group['type'] = type
        
        # TEMPO and PROB groups modify conditions reported in FM group
        if self._group['type'] in ['PROB', 'TEMPO']:
            tmpgroup = self._taf['group'][-1]['prev'].copy()
            # pcp, obv and vcnty go together, wx in TEMPO overrides FM
            if 'nsw' in self._group or 'pcp' in self._group or \
                   'obv' in self._group:    # no VC in TEMPO
                rmvditem = False
                for item in ['pcp', 'obv', 'vcnty']:
                    if item in tmpgroup:
                        del tmpgroup[item]
                        rmvditem = True
                        
                if not rmvditem and 'nsw' in self._group:
                    add_msg(self._group['nsw'],'error',60)
                        
            tmpgroup.update(self._group)
        else:
            tmpgroup = self._group

        try:
            self.check_wind(tmpgroup)
        except Warning1, e:
            #
            # Wind 'inherited' from prevailing group if not
            # in occasional group
            try:
                add_msg(self._group['wind'], 'warning', e)
            except KeyError:
                pass
            
        try:
            self.check_pcp(tmpgroup)
        except Error, e:
            try:
                add_msg(self._group['pcp'], 'error', e)
            except KeyError:
                pass

        self.check_obv()

        try:
            self.check_vsby_wx(tmpgroup)
        except Error, e:
            for k in ['pcp', 'obv', 'vsby']:
                if k in self._group:
                    add_msg(self._group[k], 'error', e)
                    break
        try:
            self.check_ts_cb(tmpgroup)
        except Error, e:
            for k in ['vcnty', 'pcp', 'sky']:
                if k in self._group:
                    add_msg(self._group[k], 'error', e)
                    break
                
        if self._group['type'] == 'FM':
            self.check_prev_time()
            self._taf['group'].append({'prev': self._group})
        else:
            period = self._taf['group'][-1]
            self.check_ocnl_time()
            period['ocnl'] = self._group
        self._group = {}

    ###################################################################
    # Element checks
    def check_issue_time(self):
        try:
            itime = self._taf['itime']['value']
            vtime = self._taf['vtime']['from']
            bbb = self._taf['bbb'][0]
            if bbb == ' ':
                if -2401.0 <= itime - vtime < 0:
                    return
            elif bbb == 'R':    # to allow incoming TAFs
                if -2401.0 < itime - vtime < 1800.0:
                    return
            elif bbb == 'C':
                if -2401.0 <= itime - vtime < 21600.0:
                    return
            else:
                if -1801.0 < itime - vtime < 1801.0:
                    return
            add_msg(self._taf['itime'], 'error', 24)
        except KeyError:
            pass

    def check_prev_time(self):
        if not self._taf['group']:  # main group
            return
        period = self._taf['group'][-1]
        t = self._group['time']
        try:
            if t['from'] <= max(period['prev']['time']['from'], self._cutoff):
                add_msg(t, 'error', 49)
            if 'ocnl' in period and t['from'] < period['ocnl']['time']['to']:
                add_msg(t, 'error', 50)
            period['prev']['time']['to'] = t['from']
        except KeyError:
            pass

    def check_ocnl_time(self):
        period = self._taf['group'][-1]
        t = self._group['time']
        #
        # adjust start of valid time for the first conditional group
        if len(self._taf['group']) == 1:
            itime = self._taf['itime']['value']
            vtime = self._taf['vtime']['from']
            
            if itime - 1800.0 < t['from'] < itime:
                t['from'] = itime
            elif t['from'] == vtime and vtime % 21600 < 60.0:
                t['from'] = itime
        try:
            if t['from'] < period['prev']['time']['from']:
                add_msg(t, 'error', 52)
        except KeyError:
            pass

    def check_prob_group(self):
        if 'pcp' not in self._group:
            add_msg(self._group['time'], 'error', 16)
        if 'llws' in self._group:
            add_msg(self._group['llws'], 'error', 33)
        if 'vcnty' in self._group:
            add_msg(self._group['vcnty'], 'error', 28)
            
    def check_tempo_group(self):
        if len(self._group) < 2:
            add_msg(self._group['time'], 'error', 34)
        if 'llws' in self._group:
            add_msg(self._group['llws'], 'error', 33)
        if 'vcnty' in self._group:
            add_msg(self._group['vcnty'], 'error', 28)

        if 'nsw' in self._group:
            try:
                if 'nsw' in self._taf['group'][-2]['ocnl']:
                    add_msg(self._group['nsw'], 'error', 47)
                    return
            except (IndexError,KeyError):
                pass
                
            if 'vsby' in self._group:
                if self._group['vsby']['value'] < 7.0:
                    add_msg(self._group['vsby'], 'error', 46)                
            else:
                add_msg(self._group['nsw'], 'error', 46)

            if 'obv' in self._group or 'pcp' in self._group:
                add_msg(self._group['nsw'], 'error', 60)

        else:
#            if 'vsby' in self._group:
            # When invalid 'value' can be missing.
            if 'vsby' in self._group and 'value' in self._group['vsby']:
                #
                # If reduction in visibility is forecasted in TEMPO and there's
                # no mention of obv or wx...
                prev = self._taf['group'][-1]['prev']
                if prev['vsby']['value'] > self._group['vsby']['value'] and \
                       ('pcp' in prev or 'obv' in prev or 'vcnty' in prev) and \
                       not ('pcp' in self._group or 'obv' in self._group):
                    add_msg(self._group['vsby'], 'error', 48)
                        
    def check_wind(self,g):
        # variable wind speed > 6kt needs convective weather
        try:
            wind = g['wind']
        except KeyError:
            return

        dd, ff = wind.get('dd', None), wind.get('ff', None)
        if dd == 'VRB' and ff > 6:
            wx = g.get('pcp', None)
            if wx:
                for tok in wx['str'].split():
                    if _ConvectionPat.search(tok):
                        return

            wx = g.get('vcnty', None)
            if wx:
                if wx['str'] in ['VCTS','VCSH']:
                    return

            sky = g.get('sky', None)
            if sky:
                for lyr in sky['str'].split():
                    if lyr.endswith('CB'):
                        return

            raise Warning1(_Warnings[31])

    def check_pcp(self,g):
        # NWSI 10-813, 1.2.6
        if 'pcp' not in g:
            return
        try:
            plist = g['pcp']['str'].split()
            if plist.index('TS') != 0 or 'FZ' not in plist[1]:
                raise Error(_Errors[45])
        except (KeyError, IndexError, ValueError):
            pass
        
        prev, other = get_prev_pcp(g['pcp']['str'])
        tok = [prev[-2:]] + [other[n:n+2] for n in range(0, len(other)-2, 2)]
        if len(tok) != len(dict.fromkeys(tok)):
            raise Error(_Errors[57])

    def check_ts_cb(self, g):
        # NWSI 10-813, 1.2.6
        if 'pcp' in g and 'TS' in g['pcp']['str'] or 'vcnty' in g and \
            'TS' in g['vcnty']['str']:
            if 'sky' not in g or 'CB' not in g['sky']['str']:
                raise Error(_Errors[11])
        if 'sky' in g and 'CB' in g['sky']['str']:
            if ('pcp' not in g or 'TS' not in g['pcp']['str']) and \
                    ('vcnty' not in g or 'TS' not in g['vcnty']['str']):
                raise Error(_Errors[81])

    def check_obv(self):
        # NWSI 10-813, 1.2.6
        if 'obv' in self._group and 'VA' in self._group['obv']['str'] \
               and not 'vsby' in self._group:
            add_msg(self._group['obv'], 'error', 13)

    def check_vsby_wx(self, g):
        # NWSI 10-813, 1.2.6
        if 'vsby' in g and g['vsby']['str'] == 'P6SM':
            if 'obv' in g and 'nsw' not in g:
                for wx in g['obv']['str'].split():
                    if wx not in _UnltdVsbyWx:
                        raise Error(_Errors[18])
        else:
            try:
                vsby = g['vsby']['value']
            except KeyError:
                return
            if not ('pcp' in g or 'obv' in g or 'nsw' in g):
                add_msg(g['vsby'], 'error', 12)
            # visibility consistent with precipitation
            snow = 0
            if 'pcp' in g:
                prev = get_prev_pcp(g['pcp']['str'])[0]
                try:
                    i, ptype = prev[0], prev[-2:]
                    if i not in '+-':
                        i = ''
                    if ptype == 'PL':
                        if invalid_pl_vsby(i, vsby):
                            raise Error(_Errors[53])
                    elif ptype in ('SN', 'DZ'):
                        snow = invalid_sn_vsby(i, vsby)
                except TypeError:   # TS
                    pass
            if 'obv' in g:
                for tok in g['obv']['str'].split():
                    wx = tok[-2:]
                    if wx == 'FG' and \
                        invalid_fg_vsby(tok, vsby):
                        raise Error(_Errors[14])
                    if wx == 'BR' and invalid_br_vsby(vsby):
                        raise Error(_Errors[15])
                    if wx in ('DS', 'SS'):
                        if tok[0] in '+-':
                            i = tok[0]
                        else:
                            i = ''
                        if invalid_ds_vsby(i, vsby):
                            raise Error(_Errors[55])
                if snow == -1:
                    raise Error(_Errors[54])
            else:
                if snow:
                    raise Error(_Errors[54])
    
    #######################################################################
    # Methods called by the parser
    def prefix(self, s):
        pass

    def ident(self, s):
        self._taf['ident'] = {'str': s, 'index': self.index()}
        if not self.tokenOK():
            add_msg(self._taf['ident'], 'error', 19)

    def itime(self, s):
        self._group = {'type': 'FM'}
        d = self._taf['itime'] = {'str': s, 'index': self.index()}
        mday, hour, minute = int(s[:2]), int(s[2:4]), int(s[4:6])
        try:
            tms = list(time.gmtime())
            tms[2:6] = mday, hour, minute, 0
            fix_date(tms)
            d['value'] = time.mktime(tms) - time.timezone
            if mday > 31 or hour > 23 or minute > 59:
                raise Error(_Errors[20])
            if not valid_day(tms):
                raise Error(_Errors[29])
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def vtime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}
        
        tms = list(time.gmtime())
        tms[2:6] = int(s[0:2]),int(s[2:4]),0,0
        fix_date(tms)
        
        mday, shour, eday, ehour = int(s[:2]), int(s[2:4]), int(s[5:7]), int(s[7:9])
            
        try:
            tms[2:6] = mday, shour, 0, 0
            fix_date(tms)
            d['from'] = time.mktime(tms) - time.timezone
            
            if len(s) == 6:
                period = ehour - shour
                if period <= 0:
                    period += 24
                d['to'] = d['from'] + 3600*period
            else:
                tms[2:6] = eday, ehour, 0, 0
                fix_date(tms)
                d['to'] = time.mktime(tms) - time.timezone

            period = abs((d['to'] - d['from'])/3600)
            
            if period > 30.01:
                add_msg(d, 'error', 61)
                
            if mday > 31:
                raise Error(_Errors[23])
            if shour > 23:
                raise Error(_Errors[22])
            if not valid_day(tms):
                raise Error(_Errors[29])
            if not 0 < ehour <= 24:
                raise Error(_Errors[21])
            
        except Error, e:
            add_msg(d, 'error', e)
            
        self._taf['vtime'] = self._group['time'].copy()
        if not self.tokenOK():
            add_msg(d, 'error', 19)
        # to determine the earliest time of the first FM group
        if self._strict:
            # make start of valid time to be issue time and
            # determine the earliest time of the first FM group
            if self._taf['bbb'][0] == 'C':
                if shour in [0, 6, 12, 18]:
                    d['from'] = self._taf['itime']['value']
                else:
                    self._cutoff = d['from'] = self._taf['vtime']['from']-1800
            else:
                self._cutoff = d['from'] = self._taf['itime']['value']
                
            if shour in [0, 6, 12, 18] and self._cutoff < self._taf['vtime']['from']:
                self._cutoff = self._taf['vtime']['from']
        else:
            # it should not matter, for monitoring
            d['from'] = min(self._taf['vtime']['from'], 
                            self._taf['itime']['value'])

    def ftime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}

        mday, hour, minute = int(s[2:4]), int(s[4:6]), int(s[6:8])    
        if not (0 <= hour <= 23 and 0 <= minute <= 59):
            add_msg(d, 'error', 22)
        try:
            tms = list(time.gmtime(self._taf['vtime']['from']))
            tms[2:5] = mday, hour, minute
            
            t = time.mktime(tms) - time.timezone
            if t <= self._taf['vtime']['from']-1800:
                fix_date(tms)
                t = time.mktime(tms) - time.timezone

            if not (self._taf['vtime']['from']-1800 <= t < self._taf['vtime']['to']):
                add_msg(d, 'error', 25)
                
            if not d.has_key('error') and mday != time.gmtime(t)[2]:
                add_msg(d, 'error', 29)
                        
            d.update({'from': t, 'to': self._taf['vtime']['to']})
                
        except KeyError:
            pass
        
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def ttime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}
        try:
            tmp = s.split()[1]
            sday, shour, eday, ehour = int(tmp[:2]), int(tmp[2:4]),\
                                       int(tmp[5:7]),int(tmp[7:9])

            tms = list(time.gmtime(self._taf['vtime']['from']))
            tms[2:4] = sday,shour
            t = time.mktime(tms) - time.timezone
            if t < self._taf['vtime']['from']:
                fix_date(tms)
                                    
            t = time.mktime(tms) - time.timezone
            if t < self._taf['vtime']['from']:
                add_msg(d, 'error', 25)

            tms[2:4] = eday, ehour
            if eday < sday:
                tms[1] += 1
                    
            d.update({'from': t, 'to': time.mktime(tms) - time.timezone})
            
            if not 0 <= shour < 24:
                add_msg(d, 'error', 22)
            if not 0 < ehour <= 24:
                add_msg(d, 'error', 21)
                
            if t >= self._taf['vtime']['to']:
                add_msg(d, 'error', 25)
                
            if d['to'] > self._taf['vtime']['to']:
                add_msg(d, 'error', 25)
                
            if d['to'] - d['from'] > 14400.0:
                add_msg(d, 'error', 56)
                
            if d['to'] <= d['from']:
                add_msg(d, 'error', 51)
                
            if not d.has_key('error'):
                if sday != time.gmtime(t)[2]:
                    add_msg(d, 'error', 29)
                else:
                    if ehour != 24 and eday != time.gmtime(d['to'])[2]:
                        add_msg(d, 'error', 29)
                
        except Error, e:
            add_msg(d, 'error', e)

        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def ptime(self, s):
        d = self._group['time'] = {'str': s, 'index': self.index()}
        try:
            if not s.startswith('PROB30'):
                raise Error(_Errors[26])

            tmp = s.split()[1]
            sday, shour, eday, ehour = int(tmp[:2]), int(tmp[2:4]),\
                                       int(tmp[5:7]),int(tmp[7:9])
            
            tms = list(time.gmtime(self._taf['vtime']['from']))
            tms[2:4] = sday, shour
            t = time.mktime(tms) - time.timezone
            if t < self._taf['vtime']['from']:
                fix_date(tms)

            t = time.mktime(tms) - time.timezone
            if t <= self._taf['vtime']['from']:
                add_msg(d, 'error', 25)

            tms[2:4] = eday,ehour
            if eday < sday:
                tms[1] += 1

            d.update({'from': t, 'to': time.mktime(tms) - time.timezone})
                
            if not 0 <= shour < 24:
                add_msg(d, 'error', 22)
            if not 0 < ehour <= 24: 
                add_msg(d, 'error', 21)
                
            if t >= self._taf['vtime']['to']:
                add_msg(d, 'error', 25)
                
            if d['to'] > self._taf['vtime']['to']:
                add_msg(d, 'error', 25)
                
            if d['from'] < 9*3600.0+self._taf['vtime']['from']:
                add_msg(d, 'error', 27)
                         
            if d['to'] - d['from'] > 21600.0:
                add_msg(d, 'warning', 13)

            if d['to'] <= d['from']:
                add_msg(d, 'error', 51)
                
            if not d.has_key('error'):
                if sday != time.gmtime(t)[2]:
                    add_msg(d, 'error', 29)
                else:
                    if ehour != 24 and eday != time.gmtime(d['to'])[2]:
                        add_msg(d, 'error', 29)

        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def vsby(self, s):
        d = self._group['vsby'] = {'str': s, 'index': self.index()}
        tok = ' '.join(s[:-2].split())
        v = _ValidVsby.get(tok, None)
        if v is None:
            add_msg(d, 'error', 37)
        else:
            d['value'] = v
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def wind(self, s):
        d = self._group['wind'] = {'str': s, 'index': self.index()}
        try:
            if s.startswith('VRB'):
                dd = d['dd'] = 'VRB'
            else:
                dd = d['dd'] = int(s[:3])
            tok = s[3:-2].split('G', 1)
            ff = d['ff'] = int(tok[0])
            if len(tok[0]) > 2 and tok[0][0] != '1':
                raise Error(_Errors[31])
            if len(tok) > 1:
                gg = d['gg'] = int(tok[1])
                if gg <= ff:
                    raise Error(_Errors[32])
            else:
                gg = None
            if dd == 'VRB': 
                if ff == 0: 
                    raise Error(_Errors[30])
            else:
                if dd % 10 != 0 or dd > 360 or ff == 0 and dd != 0 or \
                    ff > 0 and dd == 0:
                    raise Error(_Errors[30])
            if ff > 99:
                raise Warning1(_Warnings[15])
            if gg and gg - ff > 30:
                raise Warning1(_Warnings[16])
        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def obv(self, s):
        d = self._group['obv'] = {'str': s, 'index': self.index()}
        try:
            tmp = s.split()
            for tok in tmp:
                if tok not in _ValidObvis:
                    raise Error(_Errors[38])
            if len(tmp) != len(dict.fromkeys(tmp)):
                raise Error(_Errors[57])
            if 'FC' in s:
                raise Warning1(_Warnings[14])
        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def pcp(self, s):
        d = self._group['pcp'] = {'str': s, 'index': self.index()}
        prev, other = get_prev_pcp(s)
        if prev is None:    # TS
            return
        try:
            if prev not in _ValidPcp:
                raise Error(_Errors[38])
            tmp = [other[i:i+2] for i in range(0, len(other), 2)]
            for tok in tmp:
                if tok not in _ValidPcp:
                    raise Error(_Errors[38])
            tmp.append(prev[-2:])  # also has to check prevailing precipitation
            if len(tmp) != len(dict.fromkeys(tmp)):
                raise Error(_Errors[57])
        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def nsw(self, s):
        d = self._group['nsw'] = {'str': s, 'index': self.index()}
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def vcnty(self, s):
        d = self._group['vcnty'] = {'str': s, 'index': self.index()}
        if not s[2:] in _ValidVcnty:
            add_msg(d, 'error', 43)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def sky(self, s):
        d = self._group['sky'] = {'str': s, 'index': self.index()}
        cover, base, cig = 0, -1, Avn.UNLIMITED
        try:
            clds = s.split()
            for tok in clds:
                cover, base, cig = check_sky(tok, cover, base, cig)
            if cig == Avn.CLEAR:
                add_msg(d, 'error', 44)
            elif cig < Avn.CLEAR:
                cig *= 100
            if len(clds) > 3:
                raise Warning1(_Warnings[12])
        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)
        d.update({'cover': cover, 'cig': cig})

    def llws(self, s):
        d = self._group['llws'] = {'str': s, 'index': self.index()}
        try:
            h = int(s[2:5])
            dd = int(s[6:9])
            ff = int(s[9:-2])
            d.update({'hgt': h, 'dd': dd, 'ff': ff})
            if ff > 99:
                raise Warning1(_Warnings[17])
            elif len(s) >= 14:
                raise Error(_Errors[39])
            if not 0 < h <= 20:
                raise Error(_Errors[36])
            if dd % 10 != 0 or dd > 360 or ff == 0 and dd != 0:
                raise Error(_Errors[35])
        except Warning1, e:
            add_msg(d, 'warning', e)
        except Error, e:
            add_msg(d, 'error', e)
        if not self.tokenOK():
            add_msg(d, 'error', 19)

    def amd(self, s):
        # s does not contain the whole phrase
        phrase = (s+self.lexer.input[self.lexer.pos:]).rstrip()
        m = _AmdPat.match(phrase)
            
        if m:
            s = m.group()
            ix0 = self.index()[0]
            row, col = ix0.split('.')
            col = int(col)+len(s)
            ix = (ix0, '%s.%d' % (row, col))
            self._taf['amd'] = {'str': s, 'index': ix}
            #
            # If reference to time is found in the clause one of these
            # groups will have it.
            #
            strng = m.group(4) or m.group(5) or m.group(11) or m.group(12)                
            if strng:
                tms = list(time.gmtime(self._taf['vtime']['from']))
                mins = 0
                if len(strng) == 6:
                    mins = int(strng[-2:])
                    
                for ddmm in strng.split('/'):
                    tms[2:6] = int(ddmm[0:2]),int(ddmm[2:4]),mins,0    
                    fix_date(tms)
                    if not (self._taf['vtime']['from'] <= (time.mktime(tms) - time.timezone) <= self._taf['vtime']['to']):
                        self._taf['amd'] = {'str': s, 'index': self.index()}
                        add_msg(self._taf['amd'], 'warning', 19)
                        break                
        else:
            self._taf['amd'] = {'str': s, 'index': self.index()}
            add_msg(self._taf['amd'], 'error', 59)

    def __updateIssueValidTimes(self, bbb, fcst):
        TafIdent = re.compile(r'(?P<ident>[KTPN]\w{3})\s+\d{6}Z\s+\d{4}/(?P<evtime>\d{4})\s+')
        # Updates issuance and valid times in a forecast
        t=time.time()
        itime = AvnLib.getFmtIssueTime('taf', bbb,t)
        #
        if bbb and bbb[0] == 'C':
            # corrected forecast has the same timestamp
            return re.sub(' (DD|\d{2})\d{4}Z ', ' %s ' % itime, fcst, 1)
        else:
            result = TafIdent.search(fcst)
            if result:
                ident = result.group('ident')
                tafDuration=int(AvnParser.getTafSiteCfg(ident)['thresholds']['tafduration'])
            else:
                tafDuration = 24

            if result:
                vtime = AvnLib.getFmtValidTime('taf',bbb,None,tafDuration=tafDuration,
                                               evtime=result.group('evtime'))[4:]
            else:
                vtime = AvnLib.getFmtValidTime('taf',bbb,None,tafDuration)[4:]
            
            return re.sub(' (DD|\d{2})\d{4}Z [/D\d]{6,9} ',
                          ' %s %s ' % (itime, vtime), fcst, 1)

    def splitBulletin(self, text):
        SplitReg = re.compile(r'=+[\s\n]*|\n{2,}|\n$')
        # Splits bulletin into forecasts. Assumes that a forcast is 
        # terminated with '=' or forecasts are separated by a blank 
        # line
        list = [x.strip() for x in \
            SplitReg.split(text)]
        return ['%s=\n' % x for x in list if x]

##############################################################################
# java interface part ... added to support calling python from java
# Like the TafEditDialog this formats then parses the TAF.
    def parseFromJava(self, text, bbb):
        
        import JUtil
#        print 'Pre-text START ----------------------'
#        print text
#        print 'Pre-text END ------------------------'
        
        fcsts = self.splitBulletin(text)
        
        tmpText = []
        for fcst in map(Avn.curry(_format, bbb), fcsts) :
            offset = len(tmpText)            
            tmpText.extend(fcst+[''])
            
        text = '\n'.join(tmpText)

#        print 'Formated-text START ----------------------'
#        print text
#        print 'Formated-text END ------------------------'
       
        result = self(text, bbb)
        text = '\n'.join([self.__updateIssueValidTimes(bbb, f) \
                          for f in self.splitBulletin(text)])
        headerTime = AvnLib.getFmtHeaderTime('taf', bbb)
        
#        print 'Post-text START ----------------------'
#        print text
#        print 'Post-text END ------------------------'
#        
#        print 'result = ', result
        
        return JUtil.pyDictToJavaMap({'result': result, 'text': text, 'headerTime': headerTime})
    
##############################################################################
# java interface part ... added to support calling python from java
# updated java interface method to work with TAF editor QC
    def updateTime(self, text, bbb):
        import JUtil
        text = '\n'.join([self.__updateIssueValidTimes(bbb, f) \
                          for f in self.splitBulletin(text)])
        headerTime = AvnLib.getFmtHeaderTime('taf', bbb)
        
        
        #print 'result = ', result
        
        return JUtil.pyDictToJavaMap({'text': text, 'headerTime': headerTime})

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

###############################################################################
# These methods were grabbed from TafEditDialog, OB9.2.X_source 
# Revision 1.76. Placed here so all the TafEditDialg GUI is not imported.
###############################################################################
import itertools
def _preamble(kind, bbb):
    if bbb[:2] == 'AA':
        return kind + ' AMD'
    elif bbb[:2] == 'CC':
        return kind + ' COR'
    else:
        return kind

_format_pat = re.compile('|'.join([r'(FM\d+)', r'(TEMPO)', r'(AMD\s+[LN])', \
                                   r'(NIL\s+AMD)', r'(TAF\s+AMD)', r'(TAF\s+COR)',
                                   r'(TAF)']))

def _format(bbb, fcst):
    # split each forecast into groups
    tmplist = filter(None, _format_pat.split(' '.join(fcst.split())))
    # fix first line
    kind = tmplist[0][:3]
    if kind == 'TAF':
        preamble = _preamble(kind, bbb)
        tmplist[0] = preamble
    elif re.match('\s*[A-Z]{4}\s', tmplist[0]):    # missing TAF
        preamble = _preamble('TAF', bbb)
        tmplist.insert(0, preamble)
    else:   # unknown string, leave alone
        return tmplist
    newlist = tmplist[:2] + [''.join(x) for x in \
        zip(itertools.islice(tmplist, 2, None, 2), \
        itertools.islice(tmplist, 3, None, 2))]
    return AvnLib.indentTaf(newlist)

####################################################################################################################
# End of TafEditDialog code
####################################################################################################################
# test
def main(report):
    _startTaf = re.compile(r'[A-Z][A-Z0-9]{3}\s+\d{6}Z|TAF(\s+(AMD|COR))?')
    def splitTaf(report):
        # Splits report into header and TAF
        if type(report) == types.StringType:
            report = report.rstrip().split('\n')
        # skip header
        for n, line in enumerate(report):
            if _startTaf.match(line):
                return report[:n], '\n'.join(report[n:])+'\n'
        else:
            raise Avn.AvnError('Cannot find start of forecast')
    header, taf = splitTaf(report)
    tmp = header[0].split()
    try:
        bbb = tmp[3]
    except IndexError:
        bbb = '   '
    decoder = Decoder()
    decoded = decoder(''.join(taf), bbb, strict=True)
    if 'fatal' in decoded:
        print 'Fatal error at', decoded['index'], decoded['fatal']
        return
    for key in decoded:
        if key == 'group':
            for g in decoded['group']:
                print
                for key2 in g:
                    print time.ctime(g[key2]['time']['from']), \
                        time.ctime(g[key2]['time']['to'])
                    print '\t', key2, g[key2]
        elif key == 'bbb':
            print '%s \'%s\'' % (key, decoded[key])
        else:
            print key, decoded[key]
            
    errlist = errors(decoded)
    print '====== Errors ======='
    for k, d in errlist['error']:
        print k, d['index'], d['error']
    print '====== Warnings ====='
    for k, d in errlist['warning']:
        print k, d['index'], d['warning']


###############################################################################
# commented-out the standalone program part to support calling python from java
#if __name__ == '__main__':
#    import sys
#    main(sys.stdin.read())

