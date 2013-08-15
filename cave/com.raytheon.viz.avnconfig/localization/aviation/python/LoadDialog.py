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
#       LoadDialog.py
#       GFS1-NHD:A6634.0000-SCRIPT;1.12
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.13 (DELIVERED)
#         Created:  23-MAR-2006 15:14:14      TROJAN
#           spr 7109 - modified method to determine work PIL
#       
#       Revision 1.12 (DELIVERED)
#         Created:  22-MAR-2006 13:05:16      TROJAN
#           spr 7110. Revised TAF handling for consistency
#       
#       Revision 1.11 (DELIVERED)
#         Created:  29-JAN-2006 12:25:32      TROJAN
#           spr 7083
#       
#       Revision 1.10 (DELIVERED)
#         Created:  08-AUG-2005 13:00:35      TROJAN
#           spr 6969
#       
#       Revision 1.9 (DELIVERED)
#         Created:  06-JUL-2005 20:50:38      TROJAN
#           spr 6910
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:34:57      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  18-APR-2005 18:12:19      TROJAN
#           stdr 917
#       
#       Revision 1.6 (DELIVERED)
#         Created:  18-APR-2005 17:31:58      OBERFIEL
#           Changes to support gamin
#       
#       Revision 1.5 (DELIVERED)
#         Created:  19-AUG-2004 20:46:13      OBERFIEL
#           Change code
#       
#       Revision 1.4 (APPROVED)
#         Created:  09-JUL-2004 19:11:01      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:32      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:08      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:48      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7110
#       	Action Date:       26-FEB-2007 09:50:31
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: DUP SPR 7109 Uable to save TWEBs under work PILs
#       
#
# LoadDialog.py
# Aviation Workstation setup editor 
# Author: George Trojan, SAIC/MDL, July 2003
# last update: 03/21/06

import logging
from Tkinter import *
import Pmw
import Avn, AvnParser, Busy, Globals
        
_Logger = logging.getLogger(__name__)

def _position(parent):
    return 'first+%d+%d' % (parent.winfo_rootx()+30, parent.winfo_rooty()+30)

#############################################################################
class _Loader(Pmw.Dialog):
    # Base class
    def __init__(self, parent, **kw):
        Pmw.Dialog.__init__(self, parent)
        self.withdraw()
        self.title(Avn.Name + ' Loader')

        self.displayBulletin = kw['display']

        self.configure(buttons=('OK', 'Apply', 'Close'),
            defaultbutton='OK',
            command=self.__execute,
            )

        self.parent = parent
        self._tkProduct = StringVar()
        self._tkOrder = StringVar()
        self._tkOrder.set(self.option_get('loadOrder', '').lower())

        frame = Frame(self.interior())
        self.productlist = Pmw.ScrolledListBox(frame,
            labelpos='n',
            label_text='Products',
            vscrollmode='static',
            listbox_width=20,
            listbox_height=4,
            listbox_selectmode='extended',
            listbox_exportselection=0,
            )
        self.productlist.pack(side='top', expand='yes', fill='both')
        menu = Pmw.OptionMenu(frame,
            menubutton_textvariable=self._tkOrder,
            items=('latest', 'template', 'merge'),
            labelpos='w',
            label_text='Initialize From',
            )
        menu.pack(side='top', expand='no')
        self.radiobuttons = Pmw.RadioSelect(frame,
            buttontype='radiobutton',
            orient='vertical',
            labelpos='n',
            label_text='Forecast Type',
            hull_borderwidth=2,
            padx=5,
            pady=1,
            hull_relief='ridge',
            )
        self.radiobuttons.pack(side='top', expand='no', padx=5)
        for type, text in [('Rtn', 'Routine'), ('Amd', 'Amendment'),
            ('Rtd', 'Delayed'), ('Cor', 'Correction')]:
            self.radiobuttons.add(type, text=text)
        frame.pack(side='left', expand='yes', fill='y')

        self.sitelist = Pmw.ScrolledListBox(self.interior(),
            labelpos='n',
            label_text='Sites',
            vscrollmode='static',
            listbox_width=4,
            listbox_selectmode='extended',
            listbox_exportselection=0,
            )
        self.sitelist.pack(side='left', expand='yes', fill='y')

        self.radiobuttons.invoke('Rtn')
        self.resizable(width='false', height='true')
        self._position = _position(parent)

    def __execute(self, result):
        if result in [None, 'Close']:
            self.deactivate()
            return
        try:
            tag = self.radiobuttons.getcurselection() 
            if not tag:
                raise Avn.AvnError('Select forecast type')
            sites = self.sitelist.getcurselection()
            if not sites:
                raise Avn.AvnError('Select at least one site')
            self.displayBulletin(Avn.tagToBBB(tag), self._tkOrder.get(), *sites)
        except Avn.AvnError, e:
            Busy.showwarning(str(e), self.interior())
            return
        except:
            msg = 'Cannot load bulletin'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
        if result == 'OK':
            self.deactivate()

    def activate(self):
        Busy.Manager.busy(None, self.component('hull'))
        Pmw.Dialog.activate(self, geometry=self._position)
        Busy.Manager.notbusy()

#############################################################################
class TafLoader(_Loader):
    def __init__(self, parent, **kw):
        _Loader.__init__(self, parent, **kw)
        self.productlist.component('listbox').bind('<<ListboxSelect>>', 
            self.__loadSites)

    def __loadSites(self, event=None):
        lbox = self.sitelist.component('listbox')
        lbox.delete(0, 'end')
        try:
            for p in self.productlist.getcurselection():
                lbox.insert('end', *AvnParser.getTafProductCfg(p)['sites'])
            lbox.selection_set(0, 'end')
        except Exception, e:
            Busy.showerror(e, self.interior())

    def setProductList(self):
        if self.radiobuttons.getcurselection() == 'Delayed': 
            self.radiobuttons.invoke('Routine')
        self.productlist.setlist(AvnParser.getTafProducts())
        try:
            self.productlist.setvalue(Globals.Products)
        except IndexError:
            return
        self.__loadSites()

    def getWorkPIL(self):
        try:
            return AvnParser.getTafProductCfg( \
                self.productlist.getvalue()[0])['workpil']
        except (IndexError, KeyError):
            return Avn.TAFWorkPIL

#############################################################################
class TwbLoader(_Loader):
    def __init__(self, parent, **kw):
        _Loader.__init__(self, parent, **kw)
        self.productlist.component('listbox').bind('<<ListboxSelect>>', 
            self.__loadSites)

    def __loadSites(self, event=None):
        lbox = self.sitelist.component('listbox')
        lbox.delete(0, 'end')
        try:
            for p in self.productlist.getcurselection():
                lbox.insert('end', *AvnParser.getTwbProductCfg(p)['sites'])
            lbox.selection_set(0, 'end')
        except Exception, e:
            Busy.showerror(e, self.interior())

    def setProductList(self):
        if self.radiobuttons.getcurselection() == 'Delayed': 
            self.radiobuttons.invoke('Routine')
        items = AvnParser.getTwbProducts()
        self.productlist.setlist(items)
        try:
            self.productlist.setvalue(items[0])
        except IndexError:
            return
        self.__loadSites()

    def getWorkPIL(self):
        try:
            return AvnParser.getTwbProductCfg( \
                self.productlist.getvalue()[0])['workpil']
        except (IndexError, KeyError):
            return Avn.TWBWorkPIL
