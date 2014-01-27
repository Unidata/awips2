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
#       MetarViewer.py
#       GFS1-NHD:A6637.0000-SCRIPT;1.21
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.21 (DELIVERED)
#         Created:  30-OCT-2007 14:19:26      OBERFIEL
#           Removed CR characters
#       
#       Revision 1.20 (REVIEW)
#         Created:  26-OCT-2007 09:33:59      GILMOREDM
#           Related to GFS1-NHD_SPR_7324
#       
#       Revision 1.19 (INITIALIZE)
#         Created:  26-OCT-2007 09:32:03      GILMOREDM
#           Added functionality that allows forecasters to determine
#           the number of observations shown by default when "all" TAF
#           sites are shown in the metar viewer window.
#       
#       Revision 1.18 (DELIVERED)
#         Created:  20-JUN-2007 08:56:55      OBERFIEL
#           Updates to take care of PIT DRs
#       
#       Revision 1.17 (DELIVERED)
#         Created:  19-SEP-2006 06:46:39      OBERFIEL
#           Added VC types to display
#       
#       Revision 1.16 (DELIVERED)
#         Created:  23-FEB-2006 14:40:59      TROJAN
#           multiple METAR sites were not processed
#       
#       Revision 1.15 (DELIVERED)
#         Created:  06-FEB-2006 09:53:24      TROJAN
#           missing check for undefined alternate sites
#       
#       Revision 1.14 (APPROVED)
#         Created:  31-JAN-2006 18:14:19      TROJAN
#           spr 7081
#       
#       Revision 1.13 (APPROVED)
#         Created:  23-JAN-2006 08:23:15      TROJAN
#           stdr 956
#       
#       Revision 1.12 (APPROVED)
#         Created:  12-OCT-2005 18:26:36      TROJAN
#           spr 7040
#       
#       Revision 1.11 (DELIVERED)
#         Created:  06-JUL-2005 18:16:40      TROJAN
#           spr 6548
#       
#       Revision 1.10 (DELIVERED)
#         Created:  07-MAY-2005 11:36:07      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.9 (DELIVERED)
#         Created:  04-APR-2005 14:47:32      TROJAN
#           spr 6781
#       
#       Revision 1.8 (APPROVED)
#         Created:  02-APR-2005 17:47:24      TROJAN
#           spr 6772
#       
#       Revision 1.7 (DELIVERED)
#         Created:  28-FEB-2005 21:37:47      TROJAN
#           spr 6686
#       
#       Revision 1.6 (DELIVERED)
#         Created:  19-AUG-2004 20:04:11      OBERFIEL
#           Bug fixes
#       
#       Revision 1.5 (APPROVED)
#         Created:  09-JUL-2004 19:11:35      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.4 (APPROVED)
#         Created:  01-JUL-2004 14:59:43      OBERFIEL
#           Update
#       
#       Revision 1.3 (DELIVERED)
#         Created:  06-FEB-2004 15:53:43      OBERFIEL
#           Fixed MOFF error
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:11      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:58      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7324
#       	Action Date:       19-MAR-2008 07:58:32
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Number of obs shown in TAF Editor/Viewer dependent on 'All' button 
#       
#
import logging, re, time
import AvnLib, AvnParser, Globals, MetarMonitor

TAG = 'warning'
sfcvis = re.compile(r'SFC\s+VIS\s+((\d{1,2}(?!/)\s*)?([1357]/1?[2468])?)')

_Logger = logging.getLogger(__name__)

##############################################################################
# Local functions used to create strings for elements in Items below
def _makeType(dcd):
    return dcd['type']['str'][0]

def _makeTime(dcd):
    return dcd['itime']['str'][:-1]

def _makeSky(dcd):
    if 'sky' in dcd:
        return ' '.join(dcd['sky']['str'].split())
    else:
        return ''

def _makeVsby(dcd):
    if 'vsby' in dcd:
        vsbystr = dcd['vsby']['str']
        #
        if vsbystr.endswith('SM'):
            return vsbystr[:-2]
        #
        # If SFC VIS is found in remarks strip that off and
        # any extraneous new line and whitespaces
        #
        else:
            re_result = sfcvis.match(vsbystr)
            if re_result:
                return ' '.join(re_result.group(1).split())
            else:
                return vsbystr
    else:
        return ''

def _makeWx(dcd):
    tmp = []
    if 'pcp' in dcd:
        if dcd['pcp']['str'][0] not in ('+', '-'):
            tmp.append(' ' + dcd['pcp']['str'])
        else:
            tmp.append(dcd['pcp']['str'])
    if 'obv' in dcd:
        tmp.append(dcd['obv']['str'])

    if 'vcnty' in dcd:
        tmp.append(dcd['vcnty']['str'])
    
    if len(tmp) == 0 :
        return ''
    return ' '.join(tmp)

def _makeSlp(dcd):
    if 'mslp' in dcd:
        return dcd['mslp']['str'][3:]
    else:
        return ''

def _makeTemp(dcd):
    if 'tempdec' in dcd and 'tt' in dcd['tempdec']:
        return '%3.0f' % (dcd['tempdec']['tt']*9.0/5.0+32.0)
    elif 'temp' in dcd and 'tt' in dcd['temp']:
        return '%3.0f' % (dcd['temp']['tt']*9.0/5.0+32.0)
    else:
        return ''

def _makeDewp(dcd):
    if 'tempdec' in dcd and 'td' in dcd['tempdec']:
        return '%3.0f' % (dcd['tempdec']['td']*9.0/5.0+32.0)
    elif 'temp' in dcd and 'td' in dcd['temp']:
        return '%3.0f' % (dcd['temp']['td']*9.0/5.0+32.0)
    else:
        return ''

def _makeWind(dcd):
    if 'wind' in dcd:
        return dcd['wind']['str'][:-2]
    else:
        return ''

def _makeAltim(dcd):
    if 'alt' in dcd:
        return dcd['alt']['str'][2:]
    else:
        return ''

def _makePcp1h(dcd):
    if 'pcp1h' in dcd:
        return dcd['pcp1h']['str'][1:]
    else:
        return ''

##############################################################################
# Items is a list of element dictionaries, each consists of
# item, length, left-alignment-flag: either '' or '-', header 
#   (always left-aligned), output-function (defined above)
# sky, vsby, wx and wind are mandatory items
##############################################################################
Items = [ \
    {'item': 'type',  'length': 1,  'left': '',  \
        'header': 'T', 'fun': _makeType, \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
    {'item': 'time',  'length': 6,  'left': '', \
        'header': 'TIME', 'fun': _makeTime, \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
    {'item': 'wind',  'length': 8,  'left': '-', \
        'header': 'WIND', 'fun': _makeWind, \
        'startTag' : '<wind>', 'endTag' : '</wind>', \
        'adjustLength' : False}, \
    {'item': 'vsby',  'length': 5,  'left': '-', \
        'header': 'VSBY', 'fun': _makeVsby, \
        'startTag' : '<vsby>', 'endTag' : '</vsby>', \
        'adjustLength' : False}, \
    {'item': 'wx',    'length': 11, 'left': '-', \
        'header': 'WX', 'fun': _makeWx, \
        'startTag' : '<wx>', 'endTag' : '</wx>', \
        'adjustLength' : True}, \
    {'item': 'sky',   'length': 13, 'left': '-', \
        'header': 'SKY CONDITION', 'fun': _makeSky, \
        'startTag' : '<sky>', 'endTag' : '</sky>', \
        'adjustLength' : True}, \
    {'item': 'slp',   'length': 3,  'left': '',  \
        'header': 'SLP', 'fun': _makeSlp,  \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
    {'item': 'temp',  'length': 3,  'left': '',  \
        'header': ' TT', 'fun': _makeTemp, \
        'startTag' : '', 'endTag' : '', 'adjustLength' : False}, \
    {'item': 'dewp',  'length': 3,  'left': '',  \
        'header': ' DP', 'fun': _makeDewp,  \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
    {'item': 'altim', 'length': 3,  'left': '',  \
        'header': 'ALT', 'fun': _makeAltim, \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
    {'item': 'pcp1h', 'length': 4,  'left': '',  \
        'header': 'PCP', 'fun': _makePcp1h, \
        'startTag' : '', 'endTag' : '', \
        'adjustLength' : False}, \
]

##############################################################################
def _makeIndices():
    sky, vsby, wx, wind = 0, 0, 0, 0
    ix = {}
    for it in Items:
        if it['item'] == 'sky':
            ix['sky'] = ['1.%d' % sky, '1.%d' % (sky+it['length'])]
        else:
            sky += it['length'] + 1
        if it['item'] == 'vsby':
            ix['vsby'] = ['1.%d' % vsby, '1.%d' % (vsby+it['length'])]
        else:
            vsby += it['length'] + 1
        if it['item'] == 'wx':
            ix['wx'] = ['1.%d' % wx, '1.%d' % (wx+it['length'])]
        else:
            wx += it['length'] + 1
        if it['item'] == 'wind':
            ix['wind'] = ['1.%d' % wind, '1.%d' % (wind+it['length'])]
        else:
            wind += it['length'] + 1
    return ix

def _makeHeaderLine():
    return  ' '.join(['%%-%ds' % it['length'] % it['header'] for it in Items])
    
def _adjustLengths(data):
    for item in Items:
        if item['adjustLength']:
            maxLen = len(item['header'])
            fun = item['fun']
            for d in data:
                length = len(fun(d['dcd']))
                if maxLen < length :
                    maxLen = length
            item['length'] = maxLen

##############################################################################
class Viewer(object):
    def _format(dcd, addTages = False):
        if addTages :
            return '%s' % ' '.join(['%%s%%%s%ds%%s' % (it['left'], \
                    it['length']) % \
                    (it['startTag'], it['fun'](dcd), it['endTag']) for it in Items])
        else:
            return '%s' % ' '.join(['%%%s%ds' % (it['left'], it['length']) % \
                    it['fun'](dcd) for it in Items])
    _format = staticmethod(_format)
    
    def __init__(self, master, tafviewer, **kw):
        # master: parent widget (page in a notebook)
        # getcmd: data access method, returnig dictionary 
        # d = {'text': text, 'dcd': decoded, 'wind': (), 'vsby': (),
        #      'wx': (), 'cig': ()}
        # tafviewer: TafViewer instance
        self._master = master
        self._id = None
        self._mtrid = None
        self._ids = []
        self._data = None
        self._getcmd = kw['command']
        self.messagebar = kw['messagebar']
        self._tafviewer = tafviewer

        self._header = _makeHeaderLine()    # decoded METARs
        self._index = _makeIndices()        # for highlightning

        self._tkNumHours = IntVar()
        self._tkNumHours.set(int(master.option_get('numHours', '')))
	self._tkNumHoursHold = IntVar()
	self._tkNumHoursAll = IntVar()
	self._tkNumHoursAll.set(int(master.option_get('numHoursAll', '')))
        self._tkShowDecoded = IntVar()
        self._tkShowDecoded.set(int(master.option_get('showDecoded', '')))
        self._tkShowHeaders = IntVar()
        self._tkShowHeaders.set(int(master.option_get('showHeaders', '')))
        self._tkHighlightFlightCat = IntVar()
        self._tkHighlightFlightCat.set(int(master.option_get( \
            'highlightFlightCat', '')))
        self._tkAll = IntVar()

        frame = Frame(master)
        menu = Pmw.OptionMenu(frame,
            labelpos='w',
            label_text='Num Hours',
            menubutton_width=3,
            menubutton_textvariable=self._tkNumHours,
            items=('1', '3', '6', '12', '24', '99'),
            command=self.load,
            )
        menu.pack(side='right', expand='no', fill='x', padx=2)
        Balloon().bind(menu, 'Number of hours to display')
        cb = Checkbutton(frame,
            text='Decoded',
            variable=self._tkShowDecoded,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Display decoded obs ARONET style')
        cb = Checkbutton(frame,
            text='Headers',
            variable=self._tkShowHeaders,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Display WMO header for raw reports')
        cb = Checkbutton(frame,
            text='Flight Categories',
            variable=self._tkHighlightFlightCat,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Background color depicts flight categories')
        cb = Checkbutton(frame,
            text='All',
            variable=self._tkAll,
            command=self.setAll,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Displays data for all sites')
        frame.pack(side='top', expand='no', fill='x')
            
        self.text = Pmw.ScrolledText(master,
            columnheader=1,
            columnheader_name='textViewer',
            borderframe=1,
            vscrollmode='static',
            hscrollmode='dynamic',
            text_state='disabled',
            text_wrap='none',
            text_setgrid='true',
            text_name='textViewer',
            )
        self.text.pack(side='top', expand='yes', fill='both')
        self.text.tag_configure(TAG, background='red')
        self.text.tag_raise(TAG)
        self.text.component('text').bind('<Button-3>', self.__popupMenu)
        self.popmenu = Menu(master,
            tearoff=0,
            type='normal',
            )
        self.popmenu.add_command(label='Copy', command=self.__copy)

        AvnLib.configureFlightCatColors(self.text)

    def __copy(self):
        try:
            t = self.text.component('text')
            t.selection_own()
            selection = t.selection_get()
            t.clipboard_clear()
            t.clipboard_append(selection)
        except:
            pass

    def __popupMenu(self, e):
        self.popmenu.tk_popup(e.widget.winfo_rootx() + e.x,
            e.widget.winfo_rooty() + e.y)

    def __highlightDecoded(self):
        for e in self._data['status']:
            if self._data['status'][e].severity < 2:
                continue
            if e == 'tempo':
                pass
            elif e == 'cat':
                for e1 in ['vsby', 'sky']:
                    self.text.tag_add(TAG, *self._index[e1])
            else:
                self.text.tag_add(TAG, *self._index[e])

    def __highlightRaw(self):
        if self._tkShowHeaders.get():
            hlen = self._data['header'].count('\n')
        else:
            hlen = 0
        dcd = self._data['dcd']
        for e in self._data['status']:
            if self._data['status'][e].severity < 2:
                continue
            for ix in AvnLib.findIndex(e, dcd, hlen):
                self.text.tag_add(*(TAG,)+ix)

    def __showDecoded(self):
        htw = self.text.component('columnheader')
        htw.configure(state='normal')
        htw.delete('1.0', 'end')
        htw.insert('1.0', self._header)
        htw.configure(state='disabled')
        showfc = self._tkHighlightFlightCat.get()
        if 'dcd' in self._data:
            formatted = self._format(self._data['dcd'])
            self.text.insert('end', formatted)
            self.__highlightDecoded()
            if showfc:
                fcat = AvnLib.flightCategory(self._data['dcd'])
                self.text.tag_add(fcat, 1.0, 2.0)
        ftime = time.time() - 3600.0*self._tkNumHours.get()
        for n, metar in enumerate([x for x in Globals.DRC.getMetars( \
            self._mtrid, True, ftime, 99)[1:] if 'fatal' not in x.dcd]):
            self.text.insert('end', self._format(metar.dcd))
            if showfc:
                fcat = AvnLib.flightCategory(metar.dcd)
                self.text.tag_add(fcat, float(n+2), float(n+3))
        # First line of most recent METAR
        return 1

    def __showRaw(self):
        htw = self.text.component('columnheader')
        htw.configure(state='normal')
        htw.delete('1.0', 'end')
        htw.configure(state='disabled')
        if self._tkShowHeaders.get():
            self.text.insert('end', self._data['header'])
        self.text.insert('end', self._data['text'])
        if 'dcd' in self._data:
            self.__highlightRaw()
        ftime = time.time() - 3600.0 * self._tkNumHours.get()
        for metar in Globals.DRC.getMetars(self._mtrid, False, ftime, 99)[1:]:
            if self._tkShowHeaders.get():
                self.text.insert('end', metar.header)
            self.text.insert('end', metar.text)

    def __showAll(self):
        htw = self.text.component('columnheader')
        htw.configure(state='normal')
        htw.delete('1.0', 'end')
        htw.configure(state='disabled')
        ftime = time.time() - 3600.0*self._tkNumHours.get()
        last = None
        for metar in Globals.DRC.getMetars(self._ids, False, ftime):
            try:
                id_ = metar.text.split(None, 3)[1]
                if last and last != id_:
                    self.text.insert('end', '\n')
                last = id_
            except IndexError:
                pass
            if self._tkShowHeaders.get():
                self.text.insert('end', metar.header)
            self.text.insert('end', metar.text)
        self.text.configure(text_state='disabled')

###############################################################################
# public methods
###############################################################################
    def load(self, arg=None):
        self._tafviewer.load(self._id)
        try:
            self.text.configure(text_state='normal')
            self.text.clear()
            if self._tkAll.get():
                self.__showAll()
            else:
                self._data = self._getcmd(self._id)
                if self._tkShowDecoded.get():
                    self.__showDecoded()
                else:
                    self.__showRaw()
                self._tafviewer.highlight(self._data)
        except Exception:
            msg = 'Cannot load data for %s' % self._mtrid
            _Logger.exception(msg)
            if self._master.winfo_ismapped():
                self.messagebar().message('usererror', msg)
        self.text.configure(text_state='disabled')

    def setAll(self, arg=None):
	if self._tkAll.get():
	    self._tkNumHoursHold.set(self._tkNumHours.get())
	    self._tkNumHours.set(self._tkNumHoursAll.get())
	else:
	    self._tkNumHoursAll.set(self._tkNumHours.get())
	    self._tkNumHours.set(self._tkNumHoursHold.get())
	self.load(arg)

    def setSite(self, id_, ids):
        if self._id == id_:
            return
        self._id = id_
        self._mtrid = None
        self._ids = []
        for i in ids:
            siteinfo = AvnParser.getTafSiteCfg(i)
            if siteinfo and siteinfo['sites'].get('metar'):
                self._ids.extend(siteinfo['sites']['metar'])
                if i == self._id:
                    self._mtrid = siteinfo['sites']['metar']
