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
#       GridViewer.py
#       GFS1-NHD:A7802.0000-SCRIPT;1.17
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.17 (DELIVERED)
#         Created:  22-APR-2008 09:35:30      OBERFIEL
#           Fixed bug when displaying NDFD data in TAF format. (Forgot
#           to import time module)
#       
#       Revision 1.16 (DELIVERED)
#         Created:  18-APR-2008 14:48:53      OBERFIEL
#           Tried to update header.
#       
#       Revision 1.15 (DELIVERED)
#         Created:  26-SEP-2006 09:35:45      BLI
#           Modified to work with new TafGen
#       
#       Revision 1.14 (DELIVERED)
#         Created:  21-FEB-2006 12:34:21      TROJAN
#           spr 7093
#       
#       Revision 1.13 (APPROVED)
#         Created:  01-FEB-2006 15:15:37      TROJAN
#           Incorrect handling of missing data
#       
#       Revision 1.12 (APPROVED)
#         Created:  29-JAN-2006 12:16:17      TROJAN
#           stdr 958
#       
#       Revision 1.11 (APPROVED)
#         Created:  03-NOV-2005 13:39:37      TROJAN
#           spr 7052
#       
#       Revision 1.10 (DELIVERED)
#         Created:  06-JUL-2005 18:16:38      TROJAN
#           spr 6548
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:34:13      OBERFIEL
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
#         Created:  15-FEB-2005 18:12:21      TROJAN
#           spr 6561
#       
#       Revision 1.5 (DELIVERED)
#         Created:  01-FEB-2005 18:45:18      BLI
#           Make GridViewer call a new taf formater
#       
#       Revision 1.4 (APPROVED)
#         Created:  10-NOV-2004 19:27:48      OBERFIEL
#           Fixed so that it will work for both formats
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:53:42      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 19:11:16      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:41:03      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7384
#       	Action Date:       02-JUN-2008 20:45:47
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Fix formatting errors in TUG
#       
#
import logging, time
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnLib, Globals, GridData, TafDecoder, TafGen

_Logger = logging.getLogger(__name__)

##############################################################################
class Viewer:
    Format = ['table', 'long', 'short']
    def __init__(self, master, tafviewer, **kw):
        # master: parent widget (page in a notebook)
        # tafviewer: TafViewer instance
        self._master = master
        self._id = None
        self._ids = []
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
        rbtns.setvalue(self._format)
        frame.pack(side='top', expand='no', fill='x')

        self.text = Pmw.ScrolledText(master,
            columnheader=1,
            columnheader_height=2,
            borderframe = 1,
            vscrollmode='static',
            hscrollmode='dynamic',
            text_state='disabled',
            text_wrap='no',
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
        return '%s NDFD Guidance  %s' % (self._id, time.strftime('%m/%d/%y  %H%M UTC', 
                                                                      time.gmtime(data['itime']['value'])))

    def __showFormatted(self):
        content = Globals.DRC.getGrids(self._id)
        if content is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._id)
        data = GridData.makeData(self._id, content)
        self.text.component('columnheader').insert('end', 
            self.__makeHeader(data))
        tc=TafGen.TafGen('grid',data,self._tkBBB.get())
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
        content = Globals.DRC.getGrids(self._id)
        if content is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._id)
        rpt = GridData.makeTable(self._id, content)
        htw = self.text.component('columnheader')
        htw.insert('end', '\n'.join(rpt[:2]))
        self.text.insert('end', '\n'.join(rpt[2:]))
        if self._tkHighlightFlightCat.get():
            data = GridData.makeData(self._id, content)
            if not data:
                return
            nrows = len(rpt) - 2
            c = 6   # length of row label
            for g in data['group']: 
                fc = AvnLib.flightCategory(g)
                for n in range(1, nrows+1):
                    self.text.tag_add(fc, '%d.%d' % (n,c), '%d.%d' % (n,c+4))
                c += 5  # column width + space

    def __showAll(self):
        donot_group = self._format == 'long'
        bbb = self._tkBBB.get()
        data = None
        for id_, content in Globals.DRC.getGrids(self._ids):
            data = GridData.makeData(id_, content)
            tc = TafGen.TafGen('grid',data,bbb)
            taf = tc.createTaf(donot_group)
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
                self.__makeHeader(data))

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
            try:
                if self._format == 'table':
                    self.__showTable()
                else:
                    self.__showFormatted()
            except Avn.AvnError, e:
                msg = str(e)
                _Logger.warning(msg)
            except Exception:
                msg = 'Cannot load data for %s' % self._id
                _Logger.exception(msg)
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
        self._ids = ids
