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
#       EtaViewer.py
#       GFS1-NHD:A7796.0000-SCRIPT;1.20
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.20 (DELIVERED)
#         Created:  01-MAR-2007 11:43:52      OBERFIEL
#           Replace occurrences of ETA to NAM-WRF
#       
#       Revision 1.19 (DELIVERED)
#         Created:  26-SEP-2006 09:31:24      BLI
#           Modified to work with the new TafGen
#       
#       Revision 1.18 (DELIVERED)
#         Created:  06-FEB-2006 10:12:43      TROJAN
#           lost change document history block
#       
#       Revision 1.17 (REVIEW)
#         Created:  06-FEB-2006 09:53:23      TROJAN
#           missing check for undefined alternate sites
#       
#       Revision 1.16 (REVIEW)
#         Created:  02-FEB-2006 15:58:01      TROJAN
#           wrong method prevented display when "All" toggle  set
#       
#       Revision 1.15 (APPROVED)
#         Created:  01-FEB-2006 15:15:33      TROJAN
#           Incorrect handling of missing data
#       
#       Revision 1.14 (APPROVED)
#         Created:  29-JAN-2006 12:16:16      TROJAN
#           stdr 958
#       
#       Revision 1.13 (APPROVED)
#         Created:  03-NOV-2005 13:39:36      TROJAN
#           spr 7052
#       
#       Revision 1.12 (REVIEW)
#         Created:  21-OCT-2005 18:12:20      TROJAN
#           spr 7045
#       
#       Revision 1.11 (DELIVERED)
#         Created:  10-JUL-2005 20:32:57      TROJAN
#           spr 6912
#       
#       Revision 1.10 (APPROVED)
#         Created:  07-JUL-2005 12:41:32      TROJAN
#           spr 6909, 6912
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:33:09      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.8 (DELIVERED)
#         Created:  02-APR-2005 17:47:23      TROJAN
#           spr 6772
#       
#       Revision 1.7 (APPROVED)
#         Created:  21-MAR-2005 15:47:11      TROJAN
#           spr 6736
#       
#       Revision 1.6 (DELIVERED)
#         Created:  15-FEB-2005 18:12:20      TROJAN
#           spr 6561
#       
#       Revision 1.5 (DELIVERED)
#         Created:  27-JAN-2005 19:09:44      BLI
#           Modified a module name
#       
#       Revision 1.4 (APPROVED)
#         Created:  27-JAN-2005 18:35:21      BLI
#           Modified to use new taf formatter
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:53:42      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 19:11:09      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:39:50      OBERFIEL
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
import logging, time
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnLib, AvnParser, Globals, TafDecoder, TafGen

_Logger = logging.getLogger(__name__)

###############################################################################
class Viewer(object):
    Format = ['table', 'long', 'short']
    def __init__(self, master, tafviewer, **kw):
        # master: parent widget (page in a notebook)
        # tafviewer: TafViewer instance
        self._master = master
        self._id = None
        self._ids = []
        self._etaid = None
        self._tafviewer = tafviewer
        self._decoder = TafDecoder.Decoder()
        self.messagebar = kw['messagebar']

        self._tkHighlightFlightCat = IntVar()
        self._tkHighlightFlightCat.set(int(master.option_get( \
            'highlightFlightCat', '')))
        self._format = master.option_get('showFormatted', '')
        if self._format not in self.Format:
            self._format = self.Format[0]
        self._tkBBB = StringVar()
        self._tkBBB.set('   ')
        self._tkAll = IntVar()

        frame = Frame(master)
        cb = Checkbutton(frame,
            text='Flight Categories',
            variable=self._tkHighlightFlightCat,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Background color depicts flight categories')
        rbtns = Pmw.RadioSelect(frame,
            buttontype = 'radiobutton',
            labelpos='w',
            label_text='Format',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge',
            command=self.__setformat,
            )
        rbtns.pack(side='right', padx=5)
        for text in self.Format:
            rbtns.add(text)
        Balloon().bind(rbtns, 'Display format')
        rbtns.setvalue(self._format)
        cb = Checkbutton(frame,
            text='Routine',
            variable=self._tkBBB,
            onvalue='   ',
            offvalue='AAX',
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Determines TAF issuance time')
        cb = Checkbutton(frame,
            text='All',
            variable=self._tkAll,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Displays data for all sites')
        frame.pack(side='top', expand='no', fill='x')

        self.text = Pmw.ScrolledText(master,
            columnheader=1,
            columnheader_height=2,
            columnheader_name='textViewer',
            borderframe = 1,
            vscrollmode='static',
            hscrollmode='dynamic',
            text_state='disabled',
            text_wrap='none',
            text_setgrid='true',
            text_name='textViewer',
            )
        self.text.pack(side='top', expand='yes', fill='both')
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

    def __makeHeader(self, data):
        return 'NAM-WRF Guidance  %s' % time.strftime('%m/%d/%y %H%M UTC',
            time.gmtime(data['itime']['value']))

    def __showFormatted(self):
        data = Globals.DRC.getEtaData(self._etaid)
        if data is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._etaid)
        self.text.component('columnheader').insert('end', 
            self.__makeHeader(data.data))
        tc=TafGen.TafGen('Eta',data.data,self._tkBBB.get())
        donot_group = self._format == 'long'
        taf=tc.createTaf(donot_group)
        self.text.insert('end', '\n'.join(taf))
        if self._tkHighlightFlightCat.get():
            dcd = self._decoder(taf)
            if 'fatal' in dcd:
                return
            for ix, g in AvnLib.getTafPeriods(dcd):
                fc = AvnLib.flightCategory(g)
                self.text.tag_add(fc, *ix)

    def __showTable(self):
        data = Globals.DRC.getEtaTable(self._etaid)
        if data is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._etaid)
        rpt = data.table.split('\n')
        htw = self.text.component('columnheader')
        htw.insert('end', '\n'.join(rpt[:2]))
        self.text.insert('end', '\n'.join(rpt[2:]))
        if self._tkHighlightFlightCat.get():
            data = Globals.DRC.getEtaData(self._id)
            if not data:
                return
            nrows = len(rpt) - 2
            c = 6   # length of row label
            for g in data.data['group']: 
                fc = AvnLib.flightCategory(g)
                for n in range(1, nrows+1):
                    self.text.tag_add(fc, '%d.%d' % (n,c), '%d.%d' % (n,c+3))
                c += 4  # column width + space

    def __showAll(self):
        donot_group = self._format == 'long'
        bbb = self._tkBBB.get()
        data = None
        for data in Globals.DRC.getEtaData(self._ids):
            tc=TafGen.TafGen('Eta',data.data,bbb)
            taf=tc.createTaf(donot_group)
            offset = int(self.text.index('end').split('.')[0])-2
            self.text.insert('end', '\n'.join(taf))
            self.text.insert('end', '\n')
            if self._tkHighlightFlightCat.get():
                dcd = self._decoder(taf, bbb, offset)
                if 'fatal' in dcd:
                    continue
                for ix, g in AvnLib.getTafPeriods(dcd):
                    fc = AvnLib.flightCategory(g)
                    self.text.tag_add(fc, *ix)
        if data:
            self.text.component('columnheader').insert('end', 
                self.__makeHeader(data.data))

    def __setformat(self, tag):
        self._format = tag
        self.load()

###############################################################################
# public methods
###############################################################################
    def load(self, arg=None):
        self._tafviewer.load(self._id)
        self.text.configure(text_state='normal')
        self.text.clear()
        htw = self.text.component('columnheader')
        htw.configure(state='normal')
        htw.delete('1.0', 'end')
        msg = None
        if self._tkAll.get():
            if self._format == 'table':
                msg = 'Table display for all sites is not supported'
            else:
                self.__showAll()
        else:
            if self._etaid:
                try:
                    if self._format == 'table':
                        self.__showTable()
                    else:
                        self.__showFormatted()
                except Avn.AvnError, e:
                    msg = str(e)
                    _Logger.warning(msg)
                except Exception:
                    msg = 'ERROR: cannot load data for %s' % self._id
                    _Logger.exception(msg)
            else:
                msg = 'There is no NAM-WRF data for %s' % self._id
        self.text.configure(text_state='disabled')
        htw.configure(state='disabled')
        if self._master.winfo_ismapped():
            if msg:
                self.messagebar().message('usererror', msg)
            else:
                self.messagebar().resetmessages('usererror')

    def setSite(self, id_, ids):
        if self._id == id_:
            return
        self._id = id_
        self._etaid = None
        self._ids = []
        for i in ids:
            siteinfo = AvnParser.getTafSiteCfg(i)
            if siteinfo and siteinfo['sites'].get('eta'):
                self._ids.extend(siteinfo['sites']['eta'])
                if i == self._id:
                    self._etaid = self._ids[-1]
