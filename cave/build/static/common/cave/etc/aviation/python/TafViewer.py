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
#       TafViewer.py
#       GFS1-NHD:A6644.0000-SCRIPT;1.10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.10 (DELIVERED)
#         Created:  14-MAY-2007 10:04:47      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.9 (DELIVERED)
#         Created:  23-JAN-2006 08:23:18      TROJAN
#           stdr 956
#       
#       Revision 1.8 (DELIVERED)
#         Created:  19-SEP-2005 13:47:39      TROJAN
#           spr 7011
#       
#       Revision 1.7 (DELIVERED)
#         Created:  06-JUL-2005 18:16:42      TROJAN
#           spr 6548
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-MAY-2005 11:39:14      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.5 (DELIVERED)
#         Created:  24-JAN-2005 21:18:48      TROJAN
#           spr 6612
#       
#       Revision 1.4 (APPROVED)
#         Created:  09-JUL-2004 19:11:05      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:55      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:30      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:46:22      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7277
#       	Action Date:       19-MAR-2008 07:59:13
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: (OB8.2) AvnFPS decoders need to conform to new ICAO format for TAFs
#       
#
import logging, time
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnLib, Busy, Globals

TAG = 'warning'

_Logger = logging.getLogger(__name__)

class Viewer(object):
    def __init__(self, master, getcmd, editcmd):
        # master: parent widget (page in a notebook)
        # getdata: data access method, returnig dictionary 
        # d = { 'raw': raw, 'dcd': decoded}  
        self._master = master
        self._id = None
        self._taf = None
        self._getcmd = getcmd
        self._editcmd = editcmd

        self._tkShowHeaders = IntVar()
        self._tkShowHeaders.set(int(master.option_get('showHeaders', '')))
        self._tkNumTaf = IntVar()
        self._tkNumTaf.set(int(master.option_get('numTafs', '')))

        frame = Frame(master)
        btnbox = Pmw.ButtonBox(frame)
        btn = btnbox.add('Text Editor', command=self._editcmd)
        Balloon().bind(btn, 'Initializes editor page with current forecast')
        btnbox.alignbuttons()
        btnbox.pack(side='left', expand='no', fill='x')
        menu = Pmw.OptionMenu(frame,
            labelpos='w',
            label_text='Num TAFs',
            menubutton_width=3,
            menubutton_textvariable=self._tkNumTaf,
            items=('1', '3', '99'),
            command=self.load,
            )
        menu.pack(side='right', expand='no', fill='x', padx=2)
        Balloon().bind(menu, 'Number of TAFs to display')
        cb = Checkbutton(frame,
            text='Show Headers',
            variable=self._tkShowHeaders,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Display WMO header')
        frame.pack(side='top', expand='no', fill='x')

        self.text = Pmw.ScrolledText(master,
            borderframe = 1,
            vscrollmode='static',
            text_state='disabled',
            text_wrap='word',
            )
        self.text.pack(side='top', expand='yes', fill='both')
        self.text.tag_configure(TAG, background='red')
        self.text.component('text').bind('<Button-3>', self.__popupMenu)
    
        self.popmenu = Menu(master,
            tearoff=0,
            type='normal',
            )
        self.popmenu.add_command(label='Copy', command=self.__copy)

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

##############################################################################
# public methods
##############################################################################
    def highlight(self, mtrdata):
        # called by MetarViewer
        # needs to change logic if other viewers use this method
        if not self._taf or not 'group' in self._taf.dcd \
            or not self._taf.dcd['group']:
            return
        p = self._taf.dcd['group'][0]
        t = max(time.time(), p['prev']['time']['from'])
        for p in self._taf.dcd['group']:
            if t < p['prev']['time']['to']:
                if 'ocnl' in p and \
                    p['ocnl']['time']['from'] <= t < p['ocnl']['time']['to']:
                    tempo = p['ocnl']
                else:
                    tempo = None
                prev = p['prev']
                break
        else:
            return
        if self._taf.header and self._tkShowHeaders.get():
            hlen = self._taf.header.count('\n')
        else:
            hlen = 0
        for e in [e for e in mtrdata['status'] if \
            mtrdata['status'][e].severity >= 2 and e != 'tempo']:
            for ix in AvnLib.findIndex(e, prev, hlen):
                self.text.tag_add(TAG, *ix)
            if tempo:
                for ix in AvnLib.findIndex(e, tempo, hlen):
                    self.text.tag_add(TAG, *ix)

    def load(self, arg=None):
        self.text.configure(text_state='normal')
        self.text.clear()
        try:
            self._taf = self._getcmd(self._id)
            if self._taf is None:
                raise Avn.AvnError('')
            if self._tkShowHeaders.get():
                self.text.insert('end', self._taf.header)
            # first line of most recent TAF
            self.text.insert('end', self._taf.text+'\n')
            for taf in Globals.DRC.getTafs(self._id, False, 0, 
                self._tkNumTaf.get())[1:]:
                if self._tkShowHeaders.get():
                    self.text.insert('end', taf.header)
                self.text.insert('end', taf.text+'\n')
        except Exception:
            msg = 'Cannot load data for %s' % self._id
            _Logger.exception(msg)
            if self._master.winfo_ismapped():
                Busy.showwarning(msg, self._master)
        self.text.configure(text_state='disabled')

    def setSite(self, id):
        self._id = id
