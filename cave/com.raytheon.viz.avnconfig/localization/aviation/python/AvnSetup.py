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
#       AvnSetup.py
#       GFS1-NHD:A3643.0000-SCRIPT;23
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 23 (DELIVERED)
#         Created:  06-NOV-2008 10:07:36      GILMOREDM
#           Made small changes
#       
#       Revision 22 (REVIEW)
#         Created:  16-OCT-2008 15:26:30      GILMOREDM
#           Updates to the new Climate Data File GUI
#       
#       Revision 21 (DELIVERED)
#         Created:  29-SEP-2008 16:00:59      OBERFIEL
#           Updates to reflect addition of new functionality
#       
#       Revision 20 (REVIEW)
#         Created:  02-SEP-2008 12:53:03      GILMOREDM
#           Added functionality to open Climate Data GUI
#       
#       Revision 19 (DELIVERED)
#         Created:  09-JUL-2007 14:46:48      OBERFIEL
#           Commented out instructions to create the TWB route and
#           product buttons.
#       
#       Revision 18 (DELIVERED)
#         Created:  29-JUN-2006 06:36:08      OBERFIEL
#           Updated to remove references to HTML and other obsolete
#           stuff
#       
#       Revision 17 (DELIVERED)
#         Created:  08-JUN-2006 11:47:21      OBERFIEL
#           Remove reference to Informix
#       
#       Revision 16 (APPROVED)
#         Created:  08-JUN-2006 11:37:50      OBERFIEL
#           Remove HTML option from Help pulldowns
#       
#       Revision 15 (DELIVERED)
#         Created:  24-MAR-2006 17:45:43      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 14 (DELIVERED)
#         Created:  24-MAR-2006 09:48:22      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 13 (DELIVERED)
#         Created:  15-FEB-2006 14:34:41      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 12 (DELIVERED)
#         Created:  27-JAN-2006 11:28:18      PCMS
#           test
#       
#       Revision 11 (DELIVERED)
#         Created:  06-JUL-2005 20:50:37      TROJAN
#           spr 6910
#       
#       Revision 10 (DELIVERED)
#         Created:  07-MAY-2005 11:30:27      OBERFIEL
#           Added Item Header Block
#       
#       Revision 9 (DELIVERED)
#         Created:  04-APR-2005 14:16:29      TROJAN
#           spr 6777
#       
#       Revision 8 (DELIVERED)
#         Created:  24-JAN-2005 15:51:12      TROJAN
#           spr 6259
#       
#       Revision 7 (APPROVED)
#         Created:  16-NOV-2004 20:13:20      PCMS
#           Restoring history
#       
#       Revision 6 (DELIVERED)
#         Created:  05-NOV-2003 19:05:22      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 5 (DELIVERED)
#         Created:  11-JUN-2002 18:14:37      PCMS
#           Fixed Forecaster Editor in Setup gui
#       
#       Revision 4 (DELIVERED)
#         Created:  21-MAY-2002 19:31:34      PCMS
#           Fixing launch problem related to appresources variable.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 18:18:41      PCMS
#           Updated HTML help display
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:56      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:29:19      MOELLER
#           Initial version
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
import logging, os
from Tkinter import *
import Pmw
from Balloon import Balloon
from HelpDialog import HelpDialog
import AppShell, Avn, AvnParser, ErrorRedirect, Globals
import EditDialog, RuleEditor, SiteInfoEditor, RouteInfoEditor
import ProductDialog, TriggerEditor, ClimateDataDialog

Python = os.environ['PYTHON']
TopDir = os.environ['TOP_DIR']

_Help = {
    'title': 'AvnFPS Setup Help',
    'content': """
This application is used to configure AvnFPS.

Button description:

Text Editor:    use to modify forecaster list and default resource
        file.

Monitoring rules: use to edit watch rules for TAF monitoring.   

TAF Site Info:  use to create TAF definition files

TAF Products:   use to create lists of TAFs to load into forecast
    editor

Triggers:       use to create and install Postgres trigger file.

Climate Data:   use to create and update HDF5 climate files.
"""
}

_Logger = logging.getLogger(__name__)

##############################################################################
class AvnSetup(AppShell.AppShell):
    appname = Avn.Name + ' Setup'
    resizewidth = False
    resizeheight = False

    def __init__(self, **kw):
        guicfg = AvnParser.getGuiCfg()
        if not guicfg:
            raise SystemExit
        Globals.Colors = guicfg['colors']
        AppShell.AppShell.__init__(self, **kw)
        ErrorRedirect.fixlogging(_Logger, self.interior())

    def __createRuleEditor(self):
        self.ruleeditor = RuleEditor.Editor(self._hull)
        self.ruleeditor.setGeometry()

    def __createSiteInfoEditor(self):
        self.siteeditor = SiteInfoEditor.Editor(self._hull)
        self.siteeditor.setGeometry()

    def __createRouteInfoEditor(self):
        self.routeeditor = RouteInfoEditor.Editor(self._hull)
        self.routeeditor.setGeometry()

    def __createTAFProductEditor(self):
        self.tafeditor = ProductDialog.TafEditor(self._hull)
        self.tafeditor.setGeometry()

    def __createTWBProductEditor(self):
        self.twbeditor = ProductDialog.TwbEditor(self._hull)
        self.twbeditor.setGeometry()

    def __createTriggerEditor(self):
        self.trigeditor = TriggerEditor.Editor(self._hull)
        self.trigeditor.setGeometry()

    def __createClimateDataEditor(self):
	os.spawnlp(os.P_NOWAIT,Python, Python,
		os.path.join(TopDir, 'py', 'avnclimdata.py'), '')

    def initializeTk(self):
        import XDefaults
        for item in XDefaults.Defaults:
            self.root.option_add(item[0], item[1], 30)

    def appInit(self, **kw):
        try:
            self.option_readfile(os.path.join('etc', 'app-resources', 'X'))
            Pmw.Color.setscheme(self.root,
                background=self.option_get('background', ''))
        except Exception:
            msg = 'Cannot access default resources file, ' + \
                'using build-in values'
            _Logger.info(msg)

    def createMenuBar(self):
        self.menubar.addmenuitem('Help', 'command',
            label='About...', command=self.showAbout)
        self.menubar.addmenuitem('Help', 'command',
            label='Usage ...', command=self.showHelp)

        self.menubar.addmenuitem('File', 'command',
            label = 'Quit', command=self.exit)

    def createInterface(self):
        bbox = Pmw.ButtonBox(self.interior(),
            labelpos='n',
            label_text='AvnFPS Setup',
            frame_borderwidth=2,
            frame_relief='ridge',
            orient='vertical',
        )
        btn = bbox.add('Text Editor', command=self.editText)
        Balloon().bind(btn, 'Text editor')
        btn = bbox.add('Monitoring Rules', command=self.editRules)
        Balloon().bind(btn, 'Monitor criteria editor')
        btn = bbox.add('TAF Site Info', command=self.editSiteInfo)
        Balloon().bind(btn, 'TAF site info editor')
        btn = bbox.add('TAF Products', command=self.editTafProduct)
        Balloon().bind(btn, 'TAF product editor')
        btn = bbox.add('Triggers', command=self.makeTriggers)
        Balloon().bind(btn, 'Creates trigger template file')
	btn = bbox.add('Climate Data', command=self.editClimateData)
	Balloon().bind(btn, 'Update or create new HDF5 climate files')
        bbox.pack(expand='yes', fill='both')

    def editText(self):
        if not hasattr(self, 'texteditor'):
            self.texteditor = EditDialog.Editor(self.interior())
        self.texteditor.setworkdir('etc')
        self.texteditor.display()

    def editRules(self):
        if not hasattr(self, 'ruleeditor'):
            self.__createRuleEditor()
        self.ruleeditor.load()
        self.ruleeditor.display()

    def editSiteInfo(self):
        if not hasattr(self, 'siteeditor'):
            self.__createSiteInfoEditor()
        self.siteeditor.display()

    def editRouteInfo(self):
        if not hasattr(self, 'routeeditor'):
            self.__createRouteInfoEditor()
        self.routeeditor.display()

    def editTafProduct(self):
        if not hasattr(self, 'tafeditor'):
            self.__createTAFProductEditor()
        self.tafeditor.listProducts()
        self.tafeditor.display()

    def editTwbProduct(self):
        if not hasattr(self, 'twbeditor'):
            self.__createTWBProductEditor()
        self.twbeditor.listProducts()
        self.twbeditor.display()

    def makeTriggers(self):
        if not hasattr(self, 'trigeditor'):
            self.__createTriggerEditor()
        self.trigeditor.listTriggers()
        self.trigeditor.display()

    def editClimateData(self):
	self.__createClimateDataEditor()

    def showHelp(self):
        HelpDialog().display(_Help)
