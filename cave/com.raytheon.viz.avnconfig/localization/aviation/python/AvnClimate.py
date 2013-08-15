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
#       AvnClimate.py
#       GFS1-NHD:A7890.0000-SCRIPT;1.20
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.20 (DELIVERED)
#         Created:  19-NOV-2008 10:29:03      OBERFIEL
#           Fixed spelling error.
#       
#       Revision 1.19 (DELIVERED)
#         Created:  28-NOV-2007 13:01:54      OBERFIEL
#           Remove references to obsolete climate GUI and the import of
#           the climmodule.so shared library.
#       
#       Revision 1.18 (DELIVERED)
#         Created:  21-MAR-2007 09:54:48      OBERFIEL
#           For AvnClimate.py: Updated the Help text; MetarMonitorP.py:
#           fix potential permission problem;
#           WindRose.py: Added Auto Update feature, by default always
#           on.
#       
#       Revision 1.17 (REVIEW)
#         Created:  20-MAR-2007 13:36:55      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 1.16 (DELIVERED)
#         Created:  07-DEC-2006 14:25:55      BLI
#           Commented out unused code
#       
#       Revision 1.15 (APPROVED)
#         Created:  06-DEC-2006 14:10:39      BLI
#           Modified to make xmit configurable for each user
#       
#       Revision 1.14 (APPROVED)
#         Created:  17-NOV-2006 11:02:12      BLI
#           Removed the 'Old' climate gui
#       
#       Revision 1.13 (DELIVERED)
#         Created:  29-JUN-2006 06:36:01      OBERFIEL
#           Updated to remove references to HTML and other obsolete
#           stuff
#       
#       Revision 1.12 (DELIVERED)
#         Created:  08-JUN-2006 11:37:40      OBERFIEL
#           Remove HTML option from Help pulldowns
#       
#       Revision 1.11 (DELIVERED)
#         Created:  18-MAY-2006 10:32:32      TROJAN
#           SPR7150: port to HDF5 data format
#       
#       Revision 1.10 (DELIVERED)
#         Created:  24-MAR-2006 17:45:42      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 1.9 (DELIVERED)
#         Created:  24-MAR-2006 09:56:15      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 1.8 (DELIVERED)
#         Created:  16-FEB-2006 14:17:30      TROJAN
#           fixed help, removed unnecessary imports
#       
#       Revision 1.7 (APPROVED)
#         Created:  15-FEB-2006 14:34:40      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 1.6 (APPROVED)
#         Created:  13-FEB-2006 10:35:21      TROJAN
#           added buttons for the 3 new climate programs
#       
#       Revision 1.5 (DELIVERED)
#         Created:  06-JUL-2005 20:50:37      TROJAN
#           spr 6910
#       
#       Revision 1.4 (DELIVERED)
#         Created:  07-MAY-2005 11:29:35      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  30-SEP-2004 20:38:46      TROJAN
#           spr 6370
#       
#       Revision 1.2 (APPROVED)
#         Created:  12-JUL-2004 12:12:59      OBERFIEL
#           Modified startup interface
#       
#       Revision 1.1 (APPROVED)
#         Created:  09-JUL-2004 19:52:49      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7407
#       	Action Date:       03-JAN-2009 09:26:51
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Allow WFOs to update HDF5 climate files
#       
#
import atexit, logging, os, signal
from Tkinter import *
import Pmw
from Balloon import Balloon
import AppShell, Avn, AvnParser, Busy, DataRequestServ, ErrorRedirect, Globals
from HelpDialog import HelpDialog
from Pyro.errors import *

_Title = 'Aviation Forecast Preparation System Climate Display'

_Help = {
    'title': _Title,
    'content': """
This master menu GUI is used to launch applications that display NCDC
climatological data for observation sites in a variety of formats.

Button description:

METARs:       use to display reconstructed METARs for a user-defined
              span of days
Wind Rose:    displays Wind Rose for selected dates, times and flight
              category conditions
CigVis Dist:  displays ceiling, visibility and flight category
              distributions by month, hour and wind direction
CigVis Trend: displays 3-12 hour ceiling, visibility and flight
              category forecast based on initial conditions
"""
}

AvnStart = os.path.join(os.environ['TOP_DIR'], 'bin', 'avnstart.sh') 
_Logger = logging.getLogger(__name__)

###############################################################################
def _exitfun():
    pass

##############################################################################
class AvnClimate(AppShell.AppShell):
    appname = AppShell.AppShell.appname + ' Climate Menu'
    resizewidth = 'false'
    resizeheight = 'false'

    def __getXResources(self):
        # Reads application resources file
        #The following code was commented out because Globals.Forecaster
        #was never passed into this class
        #id_ = AvnParser.getForecasters().get(Globals.Forecaster, '')['id']
        #if id_:
            #fname = os.path.join('etc', 'app-resources', 'X.'+id_)
            #if os.path.isfile(fname):
                #self.option_readfile(fname)
                #return
        fname = os.path.join('etc', 'app-resources', 'X')
        if os.path.isfile(fname):
            self.option_readfile(fname)
        else:
            msg = 'Cannot access default resources file,\n' + \
                'using build-in values'
            _Logger.info(msg)
            Busy.showwarning(msg, self.root) 

    def initializeTk(self):
        pass

    def appInit(self, **kw):
        self.__getXResources()
        Pmw.Color.setscheme(self.root,
            background=self.option_get('background', ''))
        self.root.wm_iconbitmap(Avn.WatchBitmap)
        ErrorRedirect.fixlogging(_Logger, self.root)

    def appFinish(self):
        try:
            Globals.DRC = DataRequestServ.Client()
        except PyroError:
            _Logger.exception('Cannot access data server')
            raise SystemExit
        atexit.register(_exitfun)

    def createMenuBar(self):
        self.menubar.addmenuitem('Help', 'command',
            label='About...', command=self.showAbout)
        self.menubar.addmenuitem('Help', 'command',
            label='Usage ...', command=self.showHelp)
        
# HTML documentation no longer shipped with software -- mgo
#        self.menubar.addmenuitem('Help', 'command',
#            label='HTML...', command=self.showHTML)

        self.menubar.addmenuitem('File', 'command',
            label = 'Quit', command=self.exit)

    def createInterface(self):
        NewItems = [ \
            ('METARs', 'METAR display', \
                Avn.curry(self.runProg, 'metars')), \
            ('Wind Rose', 'Wind Rose', \
                Avn.curry(self.runProg, 'wrose')), \
            ('CigVis Dist', 'Ceiling/Visibility Distribution', \
                Avn.curry(self.runProg, 'cvdist')), \
            ('CigVis Trend', 'Ceiling/Visibility Forecast', \
                Avn.curry(self.runProg, 'cvtrend')),
            ]
        self.titlelabel = Label(self.interior(), text=_Title)
        grp = Pmw.Group(self.interior(),tag_pyclass=None)
        bbox = Pmw.ButtonBox(grp.interior(), orient='vertical')
        for item in NewItems:
            btn = bbox.add(item[0], command=item[2])
            Balloon().bind(btn, item[1])
        bbox.pack(side='top', expand='yes', fill='x', padx=5, pady=2)
        grp.pack(side='top', expand='yes', fill='x', padx=5, pady=2)

    def runProg(self, prog):
        os.system('%s %s &' % (AvnStart, prog))

    def showHelp(self):
        HelpDialog().display(_Help)

    def run(self):
        # overrides Tkinter method
        # shut down forecaster selection menu
        try:
            os.kill(os.getppid(), signal.SIGUSR1)
        except OSError:
            pass
        AppShell.AppShell.run(self)
