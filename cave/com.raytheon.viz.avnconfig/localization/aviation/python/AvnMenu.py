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
#       AvnMenu.py
#       GFS1-NHD:A7850.0000-SCRIPT;1.9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.9 (DELIVERED)
#         Created:  24-MAR-2006 17:45:42      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 1.8 (DELIVERED)
#         Created:  24-MAR-2006 09:48:22      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 1.7 (DELIVERED)
#         Created:  06-JUL-2005 20:11:32      TROJAN
#           spr 6885
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-MAY-2005 11:29:50      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.5 (DELIVERED)
#         Created:  04-APR-2005 15:51:02      TROJAN
#           spr 6775
#       
#       Revision 1.4 (DELIVERED)
#         Created:  30-SEP-2004 19:59:19      TROJAN
#           stdr 867
#       
#       Revision 1.3 (APPROVED)
#         Created:  12-JUL-2004 12:12:55      OBERFIEL
#           Modified startup interface
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 19:10:57      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 16:43:45      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7106
#       	Action Date:       26-FEB-2007 09:50:31
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: DUP 7103 Pmw code does not forward python exceptions to AvnFPS _Logger
#       
#
# AvnMenu.py
# Forecaster selection GUI for AvnFPS
# Author: George Trojan, SAIC/MDL, March 2004
# last update: 03/09/06

import logging, os, signal
from Tkinter import *
import Pmw
import Avn, AvnParser, Busy, ErrorRedirect
        
Python = os.environ['PYTHON']
TopDir = os.environ['TOP_DIR']

Image1 = os.path.join('etc', 'avn.gif')
Image2 = os.path.join('etc', 'pyro.gif')

_Logger = logging.getLogger(__name__)

Items = {'TAFs': ('Starting AvnFPS Monitor', 'avnwatch.py'), \
    'Climate': ('Starting AvnFPS Climate Tool', 'avnclimate.py'), \
}

class AvnMenu(Pmw.MegaWidget):
    AppName = Avn.Name + ' Menu'

    def handler(signum, frame):
        if signum == signal.SIGCHLD:
             _Logger.error('Cannot start AvnFPS. Aborting')
        raise SystemExit
    handler = staticmethod(handler)

    def __init__(self, **kw):
        self.root = Tk()
        self.root.withdraw()
        self.get_option_db()
        Pmw.initialise(self.root, useTkOptionDb=1)
        self.root.title(self.AppName)
        Pmw.MegaWidget.__init__(self, parent=self.root)
        self.root.resizable(0, 0)
        ErrorRedirect.fixlogging(_Logger, self.interior())

        interior = self.interior()
        Busy.instantiate(interior)

        self._img1 = PhotoImage(file=Image1)
        self.slist = Pmw.ScrolledListBox(interior,
            labelpos='n',
            label_image=self._img1,
            listbox_exportselection=0,
            )
        self.slist.pack(side='top', expand='yes', fill='x')
        f = Frame(interior)
        self._img2 = PhotoImage(file=Image2)
        cfg = AvnParser.getGuiCfg()
        self.servers = Pmw.ScrolledListBox(f,
            labelpos='n',
            label_image=self._img2,
            items=cfg['ns'],
            listbox_height=max(4, len(cfg['ns'])),
            listbox_exportselection=0,
            )
        self.servers.pack(side='left', expand='yes', fill='x')
        self.servers.selection_set(0)

        self.bbox = Pmw.ButtonBox(f,
            orient='vertical',
            labelpos='w',
            frame_borderwidth=2,
            frame_relief='groove',
        )
        self.bbox.pack(side='left', expand='yes', fill='x')
        self.bbox.add('TAFs', command=Avn.curry(self.ok, 'TAFs'))
        self.bbox.add('Climate', command=Avn.curry(self.ok, 'Climate'))
        self.bbox.add('Cancel', command=self.cancel)
        self.bbox.setdefault('TAFs')
        self.bbox.alignbuttons()
        f.pack(side='top')

        self.loadForecasters()
        self.prods = 0
        self.pid = 0
        signal.signal(signal.SIGUSR1, self.handler)
        signal.signal(signal.SIGCHLD, self.handler)

    def get_option_db(self):
        try:
            self.root.option_readfile(os.path.join('etc', 'app-resources', 'X'))
            Pmw.Color.setscheme(self.root,
                background=self.root.option_get('background', ''))
        except Exception:
            msg = 'Cannot access default resources file, using build-in values'
            _Logger.info(msg)

    def loadForecasters(self):
        try:
            fcstnames = AvnParser.getForecasters().keys()
            if not fcstnames:
                raise ValueError, 'Empty file'
            fcstnames.sort()
            self.slist.setlist(fcstnames)
        except IOError:
            msg = 'Cannot open forecaster file'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            raise SystemExit
        except (ValueError, IndexError):
            msg = 'Invalid entry in forecaster file'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            raise SystemExit
    
    def ok(self, choice):
        self.pid = os.fork()
        if self.pid:
            self.bbox.button('TAFs').configure(state='disabled')
            msg = Pmw.MessageDialog(self.interior(),
                message_text='\n'.join([Items[choice][0], 'Please wait']),
                iconpos='n',
                icon_bitmap='info',
                buttons=(),
                )
            x = self.root.winfo_rootx() + 30
            y = self.root.winfo_rooty() + 30
            msg.wm_geometry('+%d+%d' % (x, y))
            msg.show()
            # up to 60 s wait
            self.root.after(60000, self.cancel)
            return
        try:
            opts = ('-f', self.slist.getcurselection()[0])
        except IndexError:
            opts = ()
        logging.shutdown()
        # close all open files
        for n in xrange(3, 255):
            try:
                os.close(n)
            except OSError:
                pass
        os.execl(Python, 'avn'+os.path.basename(Python), 
            os.path.join(TopDir, 'py', Items[choice][1]), 
            *(opts + tuple(self.prods)))

    def cancel(self):
        if self.pid:
            os.kill(self.pid, signal.SIGTERM)
        raise SystemExit

    def run(self):
        self.pack()
        self.root.deiconify()
        self.mainloop()

    def setArgs(self, forecaster, prods):
        self.prods = prods
        try:
            ix = list(self.slist.get()).index(forecaster)
        except ValueError:
            ix = 0
        self.slist.selection_set(ix)
        self.slist.see(ix)
