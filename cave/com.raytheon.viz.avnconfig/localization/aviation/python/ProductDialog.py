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
#       ProductDialog.py
#       GFS1-NHD:A8056.0000-SCRIPT;13
#
#    Status:
#       $TO_BE_DEFINED
#    
#    History:
#       Revision 12 (DELIVERED)
#         Created:  01-JUN-2006 08:37:50      TROJAN
#           spr 7157 - errors when adding new product
#       
#       Revision 11 (REVIEW)
#         Created:  01-JUN-2006 08:34:30      TROJAN
#           spr 7156 - errors when adding new product
#       
#       Revision 10 (DELIVERED)
#         Created:  23-MAR-2006 15:14:14      TROJAN
#           spr 7109 - modified method to determine work PIL
#       
#       Revision 9 (DELIVERED)
#         Created:  22-MAR-2006 13:05:17      TROJAN
#           spr 7110. Revised TAF handling for consistency
#       
#       Revision 8 (DELIVERED)
#         Created:  29-JAN-2006 12:25:33      TROJAN
#           spr 7083
#       
#       Revision 7 (DELIVERED)
#         Created:  06-JUL-2005 20:50:38      TROJAN
#           spr 6910
#       
#       Revision 6 (DELIVERED)
#         Created:  01-JUN-2005 17:41:18      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 5 (DELIVERED)
#         Created:  07-MAY-2005 11:37:00      OBERFIEL
#           Added Item Header Block
#       
#       Revision 4 (DELIVERED)
#         Created:  18-APR-2005 17:32:17      OBERFIEL
#           Changes to support gamin
#       
#       Revision 3 (DELIVERED)
#         Created:  28-FEB-2005 19:42:40      TROJAN
#           spr 6284
#       
#       Revision 2 (APPROVED)
#         Created:  28-FEB-2005 19:04:55      TROJAN
#           spr 6685
#       
#       Revision 1 (BUILD_RELEASE)
#         Created:  30-SEP-2004 18:17:38      TROJAN
#           rewrite of avnsetup
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7407
#       	Action Date:       29-SEP-2008 15:38:12
#       	Relationship Type: In Response to
#       	Status:           UNDER WORK
#       	Title:             AvnFPS: Allow WFOs to update HDF5 climate files
#       
#
# ProductDialog.py
# Aviation setup editor 
# Author: George Trojan, SAIC/MDL, September 2004
# last update: 06/01/06

import logging, os
import ConfigParser
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, Busy

_Help = {
    'title': 'Route Setup Help',
    'content': """
This dialog is used to define TAF/TWEB product (list of forecasts).

Thee products should be defined AFTER relevant site/route configuration
files have been created.

To add a new product, enter product label in the "Products" entry
field and press <Enter>. Then enter all TAF ids or TWEB routes in
the "Idents" entry field, press <Enter> after typing one item.
Press "Save" button to save configuration file.

To remove a product, press "Delede" below "Products" list.

To remove an ident from the product definition, use "Delete"
button in the "Idents" column. You must then save the product.
This will NOT delete TAF/TWEB configuration files, this can only be
done from the command line.

The "Verify" button can be used to check for existence and proper
syntax of all relevant files.
"""
}

_Logger = logging.getLogger(__name__)

##############################################################################
class _Editor(AvnDialog.Dialog):
    Key = ''
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)

        # from AvnDialog
        self.createMessageBar(self.interior(), True)

        frame = Frame(self.interior())
        self.products = Pmw.ComboBox(frame,
            label_text='Products',
            labelpos='n',
            dropdown=0,
            scrolledlist_vscrollmode='static',
            listbox_width=16,
            entryfield_entry_width=16,
            listbox_exportselection=0,
            selectioncommand=self.listIdents,
            )
        self.products.grid(row=0, column=0, sticky='news')
        btn = Button(frame, text='Delete', command=self.__deleteProduct)
        btn.grid(row=1, column=0, sticky='news')
        frame.grid_rowconfigure(0, weight=1)
        frame.grid_rowconfigure(1, weight=0)
        frame.pack(side='left', expand='yes', fill='both')
        frame = Frame(self.interior())
        self.idents = Pmw.ComboBox(frame,
            label_text='Idents',
            labelpos='n',
            dropdown=0,
            scrolledlist_vscrollmode='static',
            listbox_width=4,
            entryfield_entry_width=4,
            listbox_exportselection=0,
            )
        self.idents.grid(row=0, column=0, sticky='news')
        btn = Button(frame, text='Delete', command=self.__deleteIdent)
        btn.grid(row=1, column=0, sticky='news')
        Balloon().bind(btn, 'Delete selected id from list')
        frame.grid_rowconfigure(0, weight=1)
        frame.grid_rowconfigure(1, weight=0)
        frame.pack(side='left', expand='yes', fill='both', padx=5)
        bbox = Pmw.ButtonBox(self.interior(),
            orient='vertical',
            hull_borderwidth=2,
            hull_relief='groove',
            pady=1,
            )
        btn = bbox.add('Save', command=self.__save)
        Balloon().bind(btn, 'Saves route info to a file')
        btn = bbox.add('Verify', command=self.verify)
        Balloon().bind(btn, 'Verifies existence of all relevant files')
        btn = bbox.add('Default', command=self.__setDefault)
        Balloon().bind(btn, 'Makes selected product default one')
        btn = bbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = bbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        bbox.alignbuttons()
        bbox.pack(side='top', expand='no')
        self.pil = Pmw.EntryField(self.interior(),
            labelpos='n',
            label_text='Work PIL',
            entry_width=9,
            validate={'validator': 'alphabetic', 'minstrict': 0, \
                'min': 8, 'max': 9}
            )
        self.pil.pack(side='top', expand='no')

        self.initialiseoptions(_Editor)
        self.setGeometry()

    def __deleteIdent(self):
        self.messagebar().resetmessages('systemerror')
        try:
            self.idents.delete('active')
        except IndexError:  
            self.messagebar().message('usererror', 'Select ident to delete')
            return

    def __deleteProduct(self):
        self.messagebar().resetmessages('systemerror')
        try:
            item = self.products.getcurselection()[0]
            path = os.path.join('etc', self.Key, item+'.cfg')
            if not Busy.askokcancel('This will remove file %s' % \
                path + '\nAre you sure?', self.interior()):
                return
            os.unlink(path)
            self.products.delete('active')
            msg = 'Deleted %s' % os.path.basename(path)
            _Logger.info(msg)
            self.messagebar().message('userevent', msg) 
        except IndexError:  
            self.messagebar().message('usererror', 'Select product to delete')
        except Exception:   
            msg = 'Cannot delete %s' % os.path.basename(path)
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg) 

    def __setDefault(self):
        self.messagebar().resetmessages('systemerror')
        try:
            item = self.products.getcurselection()[0]
            file(os.path.join('etc', self.Key, 'DEFAULT'), 'w').write(item+'\n')
            products = list(self.products.get(0, 'end'))
            products.remove(item)
            products.sort()
            products.insert(0, item)
            self.products.setlist(products)
        except IndexError:  
            self.messagebar().message('usererror', 
                'Select product to mark as default')
        except IOError:
            msg = 'Cannot write DEFAULT file'
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg) 

    def __save(self):
        self.messagebar().resetmessages('systemerror')
        try:
            product = self.products.getcurselection()[0]
            idents = [i.upper() for i in self.idents.get(0, 'end')]
            if self.pil.valid():
                pil = self.pil.get().strip().upper()
            else:
                Busy.showerror('Invalid Work PIL', self.interior())
                return
            path = os.path.join('etc', self.Key, product+'.cfg')
            cp = ConfigParser.ConfigParser()
            cp.add_section('sites')
            cp.set('sites', 'idents', ','.join(idents))
            cp.set('sites', 'workpil', pil)
            try:
                if not os.path.exists(os.path.dirname(path)):
                    os.mkdir(os.path.dirname(path))
                fp = file(path, 'w')
                fp.write('# %s\n\n' % path) 
                cp.write(fp)
            finally:
                fp.close()
            msg = 'Updated %s' % path
            self.messagebar().message('systemevent', msg)
        except IndexError:  
            self.messagebar().message('usererror', 'Select product to save')
        except Exception:
            msg = 'Failed to update %s' % path
            _Logger.exception(msg)

##############################################################################
class TafEditor(_Editor):
    Key = 'tafs'
    def __init__(self, parent, **kw):
        _Editor.__init__(self, parent, **kw)
        self.title(Avn.Name + ' TAF Product Configuration')

    def verify(self):
        self.messagebar().resetmessages('systemerror')
        try:
            product = self.products.getcurselection()[0]
            cfg = AvnParser.getTafProductCfg(product)
            if not cfg:
                raise Avn.AvnError('Cannot parse product definition file')
            idents = cfg.get('sites', [])
            for i in idents:
                if not AvnParser.getTafSiteCfg(i):
                    raise Avn.AvnError('Cannot parse %s info file' % i)
                for h in [0, 6, 12, 18]:
                    try:
                        AvnParser.getTafTemplate(i, h)
                    except IOError:
                        raise Avn.AvnError('Cannot read %s %02dZ template' % \
                            (i, h))
            self.messagebar().message('userevent', 'Verify succeeded')
        except IndexError:  
            self.messagebar().message('usererror', 'Select product to verify')
        except Avn.AvnError, e: 
            self.messagebar().message('usererror', str(e))

    def listIdents(self, product):
        self.messagebar().resetmessages('systemerror')
        cfg = AvnParser.getTafProductCfg(product)
        if cfg:
            idents = cfg.get('sites', [])
            pil = cfg.get('workpil')
            self.idents.setlist(idents)
            if pil is None:
                pil = Avn.TAFWorkPIL
            self.pil.setvalue(pil)
        else:
            self.idents.clear()
            self.pil.clear()
            msg = 'Cannot read product %s' % product
            self.messagebar().message('usererror', msg)

    def listProducts(self):
        self.messagebar().resetmessages('systemerror')
        products = AvnParser.getTafProducts()
        if products:
            self.products.setlist(products)
            self.products.selectitem(0)
            self.products.invoke()
        else:
            self.products.clear()
            self.messagebar().message('usererror', 'Empty product list')

##############################################################################
class TwbEditor(_Editor):
    Key = 'twbs'
    def __init__(self, parent, **kw):
        _Editor.__init__(self, parent, **kw)
        self.title(Avn.Name + ' TWEB Product Configuration')

    def verify(self):
        self.messagebar().resetmessages('systemerror')
        try:
            product = self.products.getcurselection()[0]
            cfg = AvnParser.getTwbProductCfg(product)
            if not cfg:
                raise Avn.AvnError('Cannot parse product definition')
            idents = cfg.get('sites', [])
            for i in idents:
                if not AvnParser.getTwbSiteCfg(i):
                    raise Avn.AvnError('Cannot parse %s info file' % i)
                try:
                    AvnParser.getTwbTemplate(i)
                except IOError:
                    raise Avn.AvnError('Cannot read %s template' % i)
            self.messagebar().message('userevent', 'Verify succeeded')
        except IndexError:  
            self.messagebar().message('usererror', 'Select product to verify')
        except Avn.AvnError, e: 
            self.messagebar().message('usererror', str(e))

    def listIdents(self, product):
        self.messagebar().resetmessages('systemerror')
        cfg = AvnParser.getTwbProductCfg(product)
        if cfg:
            idents = cfg.get('sites', [])
            pil = cfg.get('workpil')
            self.idents.setlist(idents)
            if pil is None:
                pil = Avn.TWBWorkPIL
            self.pil.setvalue(pil)
        else:
            self.idents.clear()
            self.pil.clear()
            msg = 'Cannot read product %s' % product
            self.messagebar().message('usererror', msg)

    def listProducts(self):
        self.messagebar().resetmessages('systemerror')
        products = AvnParser.getTwbProducts()
        if products:
            self.products.setlist(products)
            self.products.selectitem(0)
            self.products.invoke()
        else:
            self.products.clear()
            self.messagebar().message('usererror', 'Empty product list')
