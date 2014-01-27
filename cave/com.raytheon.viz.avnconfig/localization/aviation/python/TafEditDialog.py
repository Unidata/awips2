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
#       TafEditDialog.py
#       GFS1-NHD:A6643.0000-SCRIPT;1.76
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.76 (DELIVERED)
#         Created:  11-AUG-2009 10:11:41      OBERFIEL
#           Restore 'AMD LTD TO...' and 'AMD NOT SKED...' statements
#           when TAF Loader GUI 
#           is used for amending TAFs.
#       
#       Revision 1.75 (DELIVERED)
#         Created:  31-JUL-2009 15:39:03      OBERFIEL
#           Added new forecaster resource, checkPendingQueue.
#           By default, forecasters will be notified when attempting to
#           edit TAF(s) that come
#           from the transmission server 'pending' queue.  This is the
#           cause of many unnecessary
#           trouble tickets.
#       
#       Revision 1.74 (DELIVERED)
#         Created:  16-JUL-2009 14:30:43      OBERFIEL
#           Allow collective product issuance to be decided by TAF
#           product in use.
#       
#       Revision 1.73 (DELIVERED)
#         Created:  16-JUN-2009 14:28:13      OBERFIEL
#           If tafduration cannot be determined from the info.cfg file,
#           default to 24 hours
#       
#       Revision 1.72 (REVIEW)
#         Created:  29-APR-2009 14:03:53      GILMOREDM
#           Code no longer looks at forecaster's X resource file for
#           'collective' setting
#       
#       Revision 1.71 (REVIEW)
#         Created:  08-APR-2009 09:59:26      GILMOREDM
#           Included fix for multiple AMD statements
#       
#       Revision 1.70 (DELIVERED)
#         Created:  01-APR-2009 14:44:51      OBERFIEL
#           Fixed QC before Syntax check problem.
#       
#       Revision 1.69 (REVIEW)
#         Created:  27-MAR-2009 11:13:59      OBERFIEL
#           Simplified call to toolpy functions.  Added new forecaster
#           resource for Save button behavior.
#       
#       Revision 1.68 (DELIVERED)
#         Created:  31-DEC-2008 10:14:27      OBERFIEL
#           Changes to support amending TAFs prior to valid period.
#       
#       Revision 1.67 (DELIVERED)
#         Created:  29-OCT-2008 13:01:13      OBERFIEL
#           Updated help document.  Handle exception when TclError are
#           thrown.
#       
#       Revision 1.66 (DELIVERED)
#         Created:  01-OCT-2008 22:08:25      OBERFIEL
#           Simplified construction of items
#       
#       Revision 1.65 (DELIVERED)
#         Created:  25-SEP-2008 09:40:49      OBERFIEL
#           Updated Help Dialog contents to be consistent with main
#           GUI.
#       
#       Revision 1.64 (DELIVERED)
#         Created:  28-AUG-2008 15:29:13      OBERFIEL
#           Update to put TAF COR on first line.
#       
#       Revision 1.63 (DELIVERED)
#         Created:  18-AUG-2008 12:52:37      OBERFIEL
#           Successfully merged with 1.58
#       
#       Revision 1.62 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.61 (DELIVERED)
#         Created:  25-JUN-2008 15:48:25      OBERFIEL
#           No limits on subtitution of D1 and D2 macros in TAF
#           templates
#       
#       Revision 1.60 (REVIEW)
#         Created:  24-JUN-2008 15:03:56      OBERFIEL
#           Updated to handle new time format in TAF templates
#       
#       Revision 1.59 (REVIEW)
#         Created:  19-JUN-2008 14:25:48      OBERFIEL
#           Fixed merge function
#       
#       Revision 1.58 (REVIEW)
#         Created:  22-MAY-2008 13:26:59      GILMOREDM
#           Fixed error introduced in previous update
#       
#       Revision 1.57 (REVIEW)
#         Created:  15-MAY-2008 15:12:14      GILMOREDM
#           Applies QC to TAFs on an individual basis rather than as a
#           group when all are loaded into one window
#       
#       Revision 1.56 (DELIVERED)
#         Created:  26-FEB-2008 14:25:36      OBERFIEL
#           Fixed _talk notification (unimplemented) and DTG for Nov
#           2008
#       
#       Revision 1.55 (DELIVERED)
#         Created:  28-JAN-2008 06:39:02      OBERFIEL
#           TAF day and timestamp is updated before its written to
#           xmit/pending directory
#       
#       Revision 1.54 (REVIEW)
#         Created:  18-JAN-2008 08:17:36      GILMOREDM
#           changed xmit time change to include current day
#       
#       Revision 1.53 (DELIVERED)
#         Created:  26-SEP-2007 13:40:26      OBERFIEL
#           Fixed rounding problem when display MOS/LAMP guidance.
#           Updated Issuance and Valid time update code to call
#           time.time() just once.
#       
#       Revision 1.52 (DELIVERED)
#         Created:  14-MAY-2007 10:04:48      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.51 (INITIALIZE)
#         Created:  18-APR-2007 12:45:19      SOLSON
#           Removed CR characters from the previous rev of this item.
#       
#       Revision 1.50 (DELIVERED)
#         Created:  06-DEC-2006 14:15:40      BLI
#           Modified for making xmit configurable
#       
#       Revision 1.49 (BUILD_RELEASE)
#         Created:  15-NOV-2006 13:50:28      BLI
#           Created balloons for flight categories
#       
#       Revision 1.48 (DELIVERED)
#         Created:  07-JUL-2006 10:37:33      OBERFIEL
#           Minor change to documentation
#       
#       Revision 1.47 (DELIVERED)
#         Created:  06-JUL-2006 09:13:24      TROJAN
#           spr 7193: disallowed saving backup files without WMO header
#       
#       Revision 1.46 (DELIVERED)
#         Created:  06-JUN-2006 11:32:19      OBERFIEL
#           Change pulldown option name
#       
#       Revision 1.45 (DELIVERED)
#         Created:  06-JUN-2006 09:43:38      OBERFIEL
#           Change pulldown menu option name
#       
#       Revision 1.44 (DELIVERED)
#         Created:  03-MAY-2006 13:49:16      TROJAN
#           SPR 7139: fixed issue/valid time checks
#       
#       Revision 1.43 (DELIVERED)
#         Created:  03-MAY-2006 13:28:39      TROJAN
#           SPR 7140: fixed issue/valid time checks
#       
#       Revision 1.42 (DELIVERED)
#         Created:  23-MAR-2006 15:14:16      TROJAN
#           spr 7109 - modified method to determine work PIL
#       
#       Revision 1.41 (DELIVERED)
#         Created:  22-MAR-2006 13:05:18      TROJAN
#           spr 7110. Revised TAF handling for consistency
#       
#       Revision 1.40 (DELIVERED)
#         Created:  15-FEB-2006 14:34:46      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 1.39 (APPROVED)
#         Created:  31-JAN-2006 13:10:23      TROJAN
#           messagebar has to be passed to all viewers
#       
#       Revision 1.38 (INITIALIZE)
#         Created:  31-JAN-2006 07:53:52      TROJAN
#           A spuroius empty file was written to xmit/pending when
#           sending collective forecast
#       
#       Revision 1.37 (APPROVED)
#         Created:  29-JAN-2006 14:30:54      TROJAN
#           menugar passed as an argument to all viewers
#       
#       Revision 1.36 (APPROVED)
#         Created:  19-JAN-2006 11:07:13      OBERFIEL
#           Numerous changes since snapshot.
#       
#       Revision 1.35 (APPROVED)
#         Created:  21-OCT-2005 19:34:48      TROJAN
#           spr 7046
#       
#       Revision 1.34 (DELIVERED)
#         Created:  19-SEP-2005 20:50:36      TROJAN
#           tag_delete() failed
#       
#       Revision 1.33 (REVIEW)
#         Created:  19-SEP-2005 13:46:05      TROJAN
#           spr 7012
#       
#       Revision 1.32 (APPROVED)
#         Created:  15-SEP-2005 18:46:08      TROJAN
#           spr 7024
#       
#       Revision 1.31 (APPROVED)
#         Created:  07-SEP-2005 12:47:28      TROJAN
#           spr 7011
#       
#       Revision 1.30 (DELIVERED)
#         Created:  24-AUG-2005 14:18:06      OBERFIEL
#           Rearranged button order in TAF Editor
#       
#       Revision 1.29 (APPROVED)
#         Created:  18-AUG-2005 14:08:00      TROJAN
#           spr 6995
#       
#       Revision 1.28 (DELIVERED)
#         Updated:  08-AUG-2005 15:18:27      TROJAN
#           spr  6972
#         Created:  08-AUG-2005 15:09:42      TROJAN
#           spr 6972
#       
#       Revision 1.27 (APPROVED)
#         Created:  08-AUG-2005 12:51:02      TROJAN
#           spr 6970
#       
#       Revision 1.26 (APPROVED)
#         Created:  04-AUG-2005 18:01:17      TROJAN
#           spr 6966, 6963
#       
#       Revision 1.25 (DELIVERED)
#         Created:  26-JUL-2005 18:28:19      TROJAN
#           spr 6947
#       
#       Revision 1.24 (APPROVED)
#         Created:  10-JUL-2005 19:44:21      TROJAN
#           spr 6884
#       
#       Revision 1.23 (UNDER WORK)
#         Created:  06-JUL-2005 18:16:42      TROJAN
#           spr 6548
#       
#       Revision 1.22 (DELIVERED)
#         Created:  07-MAY-2005 11:38:37      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.21 (DELIVERED)
#         Created:  21-APR-2005 18:45:25      TROJAN
#           failed spr 6776
#       
#       Revision 1.20 (DELIVERED)
#         Created:  18-APR-2005 17:57:39      TROJAN
#           spr 6800
#       
#       Revision 1.19 (DELIVERED)
#         Created:  06-APR-2005 11:46:47      TROJAN
#           spr 6776
#       
#       Revision 1.18 (APPROVED)
#         Created:  04-APR-2005 14:59:38      TROJAN
#           spr 6776
#       
#       Revision 1.17 (APPROVED)
#         Created:  25-MAR-2005 12:04:33      TROJAN
#           spr 6748
#       
#       Revision 1.16 (APPROVED)
#         Created:  21-MAR-2005 16:46:26      TROJAN
#           spr 6740
#       
#       Revision 1.15 (DELIVERED)
#         Created:  15-MAR-2005 18:39:25      OBERFIEL
#           Fixed spelling errors
#       
#       Revision 1.14 (DELIVERED)
#         Created:  14-FEB-2005 20:54:51      TROJAN
#           spr 6649
#       
#       Revision 1.13 (APPROVED)
#         Created:  23-JAN-2005 19:05:30      TROJAN
#           spr 6586
#       
#       Revision 1.12 (APPROVED)
#         Created:  19-JAN-2005 15:05:13      TROJAN
#           spr 6564
#       
#       Revision 1.11 (APPROVED)
#         Created:  07-DEC-2004 14:24:52      TROJAN
#           spr 6509
#       
#       Revision 1.10 (APPROVED)
#         Created:  30-SEP-2004 18:56:03      TROJAN
#           stdr 874
#       
#       Revision 1.9 (APPROVED)
#         Created:  19-AUG-2004 20:56:09      OBERFIEL
#           Code change
#       
#       Revision 1.8 (APPROVED)
#         Created:  15-JUL-2004 18:17:35      OBERFIEL
#           Fixed statistical output
#       
#       Revision 1.7 (REVIEW)
#         Created:  09-JUL-2004 19:28:30      OBERFIEL
#           Added climate QC
#       
#       Revision 1.6 (APPROVED)
#         Created:  01-JUL-2004 14:59:52      OBERFIEL
#           Update
#       
#       Revision 1.5 (DELIVERED)
#         Created:  17-MAR-2004 19:40:11      TROJAN
#           sprs for 2.1
#       
#       Revision 1.4 (DELIVERED)
#         Created:  15-JAN-2004 22:31:21      PCMS
#           Fixed problem with "Transfer" button raising exception in
#           Graphical Editor
#       
#       Revision 1.3 (DELIVERED)
#         Created:  08-JAN-2004 21:40:27      PCMS
#           Updating for code cleanup
#       
#       Revision 1.2 (APPROVED)
#         Created:  03-DEC-2003 18:42:57      TROJAN
#           spr 5681
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:46:18      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
import itertools, logging, os, re, socket, sys, threading, time
import Queue
from Tkinter import *
import Pmw
from Balloon import Balloon
from LoadDialog import TafLoader
import Avn, AvnDialog, AvnLib, AvnParser, AvnThread
import Busy, EditorTools, Globals
import MetarMonitor, SearchDialog, TafDecoder, TafQC

_NumPages = 4
# text tags used to mark forecast errors
_TagList = ['always', 'warning', 'error', 'fatal']
_Host = socket.gethostname()

_Help= {
    'title': 'TAF Editor Help',
    'content': """
This is a text editor specialized for composing and checking TAFs.

The dialog consists of two areas. The top part is for viewing and
editing TAFs, the bottom part displays guidance data.

The text editor consists of 4 independent pages. Each one displays WMO
header. The 'Rtn', ..., 'Cor' toggles change forecast type. Use
'Clear' button to clear the text window.  The 'Tools' combo box
displays list of site-specific utilities to modify the forecast.

Menus:
    Most of the items are relevant when the 'Editor' tab is selected. 
    File:
    Print         - call print dialog
    Clear Errors  - clears error tags set by the formatting 
                    (quality check) action. Can be used to force 
                    transmission of forecasts that did not pass
                    the Syntax check.  It also reverses colors in
                    the editor window to normal after forecast is
                    sent
    Update Times  - updates issue and valid times
    Save As       - allows to save edited forecast to a file. 
    Restore From  - use to restore forecast from a backup file
    Store in DB   - use to store forecast in AWIPS text database
    Close         - closes the editor window

    Options: 
    Auto Save     - toggles auto-save feature
    Auto Print    - toggles automatic printout of sent forecasts
    Update Times on Format - if selected, the issue and valid times in 
                    the forecast are updated before quality control
                    checks
    Send in Collective - Toggles collective versus split bulletin
                    transmission. Intended for OCONUS sites only.

    Edit:
    Provides the usual editing functions (i.e. Cut, Copy, Paste, 
    and Find/Replace)

TAF editor area:

Buttons:
    Load: invokes forecast selection dialog. Bulletin (or product) 
        is selected from 'Bulletins' menu. To load bulletin from 
        previously saved file, set the 'From file' toggle.
        Otherwise the forecasts will be loaded depending on 
        the 'Load Order' selecton:
        Latest: first an attempt is made to access the most 
            recent previous forecast. If one cannot be found,
            a template file is loaded.
        Merge:  loads previous forecast, then appends template.
            The intent is to allow phrases such as
            AMD NOT SKED AFT D1HHZ.
        Template: loads forecasts from template file.
        'Forecast Type' selection is used to initialize WMO
        header (DDHHMM and BBB) fields. These fields will be 
        updated when forecast is sent. 

    Syntax: Performs syntax check and assures proper indentation 
        and maximum line length. If Syntax Check fails the forecast, 
        the problem areas will be highlighted. The color 
        corresponds to the severity of the problem. Red means 
        the forecast could not be parsed sucessfully. Orange 
        means error according to NWSI 10-813. Green is a warning.

    QC: Performs selected quality control checks

    Send:   Splits the bulletin into separate files, one per site, 
        which are written to directory 'xmit/pending'. 
        The transmission program running on the data server is 
        responsible for actual transmission.
        The program will check whether a regular forecast is 
        sent within the transmission time window. If not, an 
        error dialog is displayed.

    Save:   Stores bulletin as a work TAF in a file

    Restore: Restores bulletin from the work file

Toggles:
    Insert        - toggles insert/overwrite mode
    Wrap          - if selected, the line is folded when its length
                    exceedes window width. Has no effect on 
                    the final format.

Viewer area:
    Use 'Site ID' combo box to view site data from the list of 
    currently monitored sites. 
    Select page in the notebook for a specific data source. The list 
    of data sources is configurable. A set of display options is 
    available, depending on the data source.
"""
}

_Logger = logging.getLogger(__name__)

###############################################################################
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

def _getIds(taf):
    n = taf.find('\n')+1
    return taf[n:n+4]

def _make_collective(fcsts):
    f_taf = []
    for f in fcsts:
        pre, taf = f.split('\n', 1)
        if pre.startswith('TAF'):
            if not f_taf:
                f_taf.append(pre)
            f_taf.extend([taf.rstrip(), ''])
    return ['\n'.join(f_taf)]

#############################################################################
class TafEditor(AvnDialog.Dialog):
    """TAF editor"""
    Key = 'taf'
    # pattern to match in template files. Requires flag re.DOTALL
    TemplPat =  r'.+?(?=((\n(\s*\n)*((\s*\n)|$)))|=\n)'
    SplitReg = re.compile(r'=+[\s\n]*|\n{2,}|\n$')
    TafIdent = re.compile(r'(?P<ident>[KTPN]\w{3})\s+\d{6}Z\s+\d{4}/(?P<evtime>\d{4})\s+')
    
#############################################################################
# class methods
    def __updateIssueValidTimes(cls, bbb, fcst):
        # Updates issuance and valid times in a forecast
        t=time.time()
        itime = AvnLib.getFmtIssueTime(cls.Key, bbb,t)
        #
        if bbb and bbb[0] == 'C':
            # corrected forecast has the same timestamp
            return re.sub(' (DD|\d{2})\d{4}Z ', ' %s ' % itime, fcst, 1)
        else:
            result = cls.TafIdent.search(fcst)
            if result:
                ident = result.group('ident')
                try:
                    tafDuration=int(AvnParser.getTafSiteCfg(ident)['thresholds']['tafduration'])
                except:
                    tafDuration = 24
            else:
                tafDuration = 24

            if result:
                vtime = AvnLib.getFmtValidTime(cls.Key,bbb,None,tafDuration=tafDuration,
                                               evtime=result.group('evtime'))[4:]
            else:
                vtime = AvnLib.getFmtValidTime(cls.Key,bbb,None,tafDuration)[4:]
                
            return re.sub(' (DD|\d{2})\d{4}Z [/D\d]{6,9} ',
                          ' %s %s ' % (itime, vtime), fcst, 1)
        
    __updateIssueValidTimes = classmethod(__updateIssueValidTimes)

#############################################################################
# constructors
    def __init__(self, parent=None, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)

        self.__appInit()
        self.__getOptions()
        self.__createMenuBar()
        # from AvnDialog
        self.createMessageBar(self.interior(), True)
        self.__createMainWindow()
        self.__createLoadDialog()
        self.__appFinish()

        self.initialiseoptions(TafEditor)
        self.setGeometry()

    def __appInit(self):
        self.title(Avn.Name + ' TAF Editor: ' + Globals.Forecaster)
        self._tkInsert = IntVar()
        self._tkWrap = StringVar()
        self._tkAutosave = IntVar()
        self._tkUpdateTimes = IntVar()
        self._tkCollective = IntVar()
        self._tkAutoPrint = IntVar()
        self._page = []
        self._tafids = {}
        self._metarids = {}
        self._viewer = dict.fromkeys([v.tag for v in Globals.Viewers])
        self._fcatColor = {}
        self._xmit = 0
        self._sitemonitors = []
        self._qcqueue = Queue.Queue()
        self._qc = AvnThread.Worker(Queue.Queue(), self._qcqueue)
        for tag in [Avn.LIFR, Avn.IFR, Avn.MVFR, Avn.VFR]:
            self._fcatColor[tag] = self.option_get('%sColor' % tag, '')
        try:
            nameDict = AvnParser.getForecasters()[Globals.Forecaster]
            self._fcstid = nameDict['id']
            self._xmit = nameDict['xmit']
        except (IOError, KeyError):
            self._fcstid = 0

    def __appFinish(self):
        self.sp = self._page[0]
        t = self._page[0].text.component('text')
        self._bg, self._fg = t.cget('background'), t.cget('foreground')
        self._headers = AvnParser.getTafHeaders()
        self._taf_decoder = TafDecoder.Decoder()
        self.__autosave()
        self.loaddialog.setProductList()
        self._tkCollective.set(Globals.Collective)

    def __createMenuBar(self):
        self.menubar = Pmw.MenuBar(self.interior(),
            hull_relief='raised',
            hull_borderwidth=1,
            )
        self.menubar.pack(side='top', fill='x')

        self.menubar.addmenu('Help', 'Help actions', side='right')
        self.menubar.addmenuitem('Help', 'command',
            'Editor Help',
            label='Key bindings',
            command=Avn.curry(self.showHelp, EditorTools.Help),
            )
        self.menubar.addmenuitem('Help', 'command',
            'Editor Help',
            label='Usage',
            command=Avn.curry(self.showHelp, _Help),
            )
        self.menubar.addmenu('File', 'Main actions', side='left')
        self.menubar.addmenuitem('File', 'command',
            'Invoke print dialog',
            label = 'Print',
            command=self.__showPrintDialog,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Clear Errors',
            command=self.__clearErrors,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Update Times',
            command=self.__updateTimes,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Save As',
            command=self.__saveAs,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Restore From',
            command=self.__restore,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Store in DB',
            command=Avn.curry(self.__saveWorkFile, 1),
            )
        self.menubar.addmenuitem('File', 'separator')
        self.menubar.addmenuitem('File', 'command',
            label = 'Close',
            command=self.close,
            )

        self.menubar.addmenu('Options', 'Configuration options')
        self.menubar.addmenuitem('Options',
            'checkbutton',
            label='Auto Save',
            variable=self._tkAutosave,
            )
        self.menubar.addmenuitem('Options', 'checkbutton',
            label='Auto Print',
            variable=self._tkAutoPrint,
            )
        self.menubar.addmenuitem('Options', 'checkbutton',
            label='Update Times on Format',
            variable=self._tkUpdateTimes,
            )
        self.menubar.addmenuitem('Options', 'checkbutton',
            label='Send in Collective',
            variable=self._tkCollective,
            )

        self.menubar.addmenu('Edit', 'Editing actions', side='left')
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Cut',
            command=self.__cut,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Copy',
            command=self.__copy,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Paste',
            command=self.__paste,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Find',
            command=self.__find,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Undo',
            command=self.__undo,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Redo',
            command=self.__redo,
            )

    def __createMainWindow(self):
        layout = self.option_get('orientation', '')
        if not layout in ['horizontal', 'vertical']:
            layout = 'vertical'
        self.pane = Pmw.PanedWidget(self.interior(), orient=layout)
        self.pane.pack(side='top', expand='yes', fill='both')
        pane = self.pane.add('editor')
        self.__createEditorWindow(pane)
        pane = self.pane.add('viewer')
        self.__createViewerWindow(pane)
        self.pane.setnaturalsize()

    def __createPage(self, name):
        page = self.editpager.add(name, tab_width=10)
        tab = self.editpager.tab(name)
        tab.configure(text='')
        
        frame = Frame(page, relief='ridge', bd=2)
        header = Label(frame, text='', width=12)
        header.pack(side='left', expand='no', fill='x', padx=2)
        Balloon().bind(header, 'WMO Header')
        htime = Pmw.EntryField(frame,
            entry_width=6,
            validate=EditorTools.validateHeaderTime,
            )
        htime.pack(side='left', expand='no', fill='x')
        bbb = Pmw.EntryField(frame,
            entry_width=3,
            validate=EditorTools.validateBBB,
            )
        bbb.pack(side='left', expand='no', fill='x')
        typebox = Pmw.RadioSelect(frame,
            buttontype='radiobutton',
            command=self.__settype,
            )
        typebox.pack(side='left', padx=5)
        for type in ('Rtn', 'Amd', 'Rtd', 'Cor'):
            typebox.add(type)
        typebox.setvalue('Rtn')
        btn = Button(frame,
            text='Clear',
            command=Avn.curry(self.__clearPage, name),
            )
        btn.pack(side='right', expand='no', fill='x', padx=2)
        Balloon().bind(btn, 'Clear current page')
        frame.pack(side='top', expand='no', fill='x')

        stext = Pmw.ScrolledText(page,
            borderframe=1,
            hscrollmode='dynamic',
            vscrollmode='static',
            text_setgrid='true',
            text_name='textEditor',
            text_undo='true',
#           text_maxundo=16,
            )
        stext.pack(side='bottom', expand='yes', fill='both')
        stext.configure(text_wrap=self._tkWrap.get())
        t = stext.component('text')
        t.bind('<Insert>', self.__toggleInsert)
        t.bind('<KeyPress>', self.__keyPress)
        t.bind('<Button-3>', self.__popupMenu)
        t.bind('<Control-u>', self.__undo)
        t.bind('<Control-r>', self.__redo)
        self._edit_tags = {'info': t.cget('background')}
        self._edit_tags.update(Globals.EditTag)
        for tag, color in self._edit_tags.items():
            t.tag_configure(tag, background=color)
        t.tag_lower('error', 'fatal')
        t.tag_lower('warning', 'error')
        t.tag_lower('info', 'warning')
        return Avn.Bunch(name=name, header=header, htime=htime,
            bbb=bbb, text=stext, typebox=typebox, numtags=0,
            sent=0, errorlevel=0, lastix=t.index('insert'))

    def __createEditorWindow(self, master):
        self.pager = Pmw.NoteBook(master)
        self.pager.pack(side='top', fill='both', expand='yes')
        page = self.pager.add('Viewer', tab_width=10)
        self._viewer['taf'] = Globals.Viewers[0].module.Viewer(page, 
            self.__getTafs, self.__editViewedTaf)
        page = self.pager.add('Editor', tab_width=10)
        self.__createEditor(page)
        self.pager.setnaturalsize()

    def __createEditor(self, master):
        # command buttons
        frame = Frame(master)
        btnbox = Pmw.ButtonBox(frame)
        btn = btnbox.add('Load', command=self.__showLoadDialog)
        Balloon().bind(btn, 'Invokes forecast selection dialog')
        btn = btnbox.add('Syntax', command=self.__checkSyntax)
        Balloon().bind(btn, 'Formats forecast and check for valid syntax')
        btn = btnbox.add('QC', command=self.__checkQC)
        btn.bind('<Button-3>', self.__checkQC)
        Balloon().bind(btn, 'Performs selected QC checks')
        if self._xmit:
            btn = btnbox.add('Send', command=self.__sendForecast)
        Balloon().bind(btn, 'Put forecast into transmission queue')
        btn = btnbox.add('Save', command=Avn.curry(self.__saveWorkFile, self._storeindb))
        Balloon().bind(btn, 'Store bulletin in a work file')
        btn = btnbox.add('Restore', command=self.__restoreWorkFile)
        Balloon().bind(btn, 'Restore bulletin from a work file')
        btnbox.alignbuttons()
        btnbox.pack(side='left', expand='yes', fill='x')
        frame.pack(side='top', expand='no', fill='x')

        frame = Frame(master)
        # smart tools area
        modules = [f[:-3] for f in os.listdir('toolpy') if f.endswith('.py')]
        modules.sort()
        self.toolbox = Pmw.ComboBox(frame,
            label_text='Tools: ',
            labelpos='w',
            entry_width=32,
            selectioncommand=self.__userMethod,
            scrolledlist_items=modules,
            entry_state='readonly',
            )
        self.toolbox.pack(side='left', expand='no', fill='x', padx=10)
        if len(modules) > 0:
            self.toolbox.selectitem(modules[0], 1)
        btn = Button(frame, text='Apply', command=self.__userMethod)
        btn.pack(side='left', expand='no')
        Balloon().bind(btn, 'Invokes method from "Tools" box')
        # Insert and Wrap toggles
        checkbutton = Checkbutton(frame,
            text='Insert',
            variable=self._tkInsert,
            )
        checkbutton.pack(side='left', padx=2)
        checkbutton = Checkbutton(frame,
            text='Wrap',
            onvalue='word',
            offvalue='none',
            variable=self._tkWrap,
            command=self.__toggleWrap,
            )
        checkbutton.pack(side='left', padx=2)
        frame.pack(side='bottom', expand='no', fill='x')

        self.editpager = Pmw.NoteBook(master)
        self.editpager.pack(side='top', fill='both', expand='yes')
        for n in range(_NumPages):
            self._page.append(self.__createPage(str(n)))
        self.editpager.configure(raisecommand=self.__selectEditPage)
        self.editpager.setnaturalsize()

    def __createViewerWindow(self, master):
        frame = Frame(master)
        self._topdrop = Pmw.ComboBox(frame,
            label_text='Site ID:',
            labelpos='w',
            entry_width=5,
            selectioncommand=self.updateViewer,
            scrolledlist_items=(),
            )
        self._topdrop.pack(side='left', expand='no', padx=10)
        tafviewer = self._viewer['taf']
        fg = tafviewer.text.component('text').cget('foreground')
        vis_range=['1/2 =< vis < 1 SM','1 =< vis < 3 SM','3 =< vis <= 5 SM','vis > 5 SM']
        cig_range=['200 =< cig < 500 ft','500 =< cig < 1000 ft','1000 =< cig <= 3000 ft','cig > 3000 ft']

        for tag,vis,cig in zip([Avn.LIFR, Avn.IFR, Avn.MVFR, Avn.VFR],vis_range,cig_range):
            label = Label(frame,
                text=tag.upper(),
                foreground=fg,
                background=self._fcatColor[tag],
                )
            label.pack(side='right', padx=5)
            Balloon().bind(label,'%s or \n%s'%(vis,cig))
        label = Label(frame, text='Flight Categories:')
        label.pack(side='right', padx=5)
        frame.pack(side='top', expand='no', fill='x')

        self.viewpager = Pmw.NoteBook(master)
        self.viewpager.pack(fill='both', expand='yes', padx=3, pady=3)
        for v in Globals.Viewers[1:]:
            page = self.viewpager.add(v.tag, tab_text=v.args['label'])
            del v.args['label']
            v.args['command'] = self.__getMonitoredMetar
            v.args['messagebar'] = self.messagebar
            self._viewer[v.tag] = v.module.Viewer(page, tafviewer, **v.args)
        self.viewpager.configure(raisecommand=self.updateViewer)
        self.viewpager.setnaturalsize()

    def __createSendDialog(self):
        # Creates transmission time selection dialog
        self.senddialog = EditorTools.SendDialog(self)
        self.senddialog.transient(master=self._hull)

    def __createLoadDialog(self):
        # Creates load dialog
        self.loaddialog = TafLoader(self._hull, display=self.displayBulletin)
        self.loaddialog.transient(master=self._hull)

    def __createQCDialog(self):
        # Creates QC dialog
        self.qcdialog = EditorTools.QCDialog(self._hull)
        self.qcdialog.transient(master=self._hull)

    def __createPopupMenu(self):
        self.popmenu = Menu(self._hull,
            tearoff=0,
            type='normal',
            )
        self.popmenu.add_command(label='Cut', command=self.__cut)
        self.popmenu.add_command(label='Copy', command=self.__copy)
        self.popmenu.add_command(label='Paste', command=self.__paste)
        self.popmenu.add_command(label='Undo', command=self.__undo)
        self.popmenu.add_command(label='Redo', command=self.__redo)

    def __createSearchDialog(self):
        self.searchdialog = SearchDialog.Dialog(self._hull, self.sp.text)
        self.searchdialog.transient(master=self._hull)
        AvnDialog.setGeometry(self, self.searchdialog)

#############################################################################
# editing methods
    def __copy(self):
        t = self.sp.text.component('text')
        t.selection_own()
        try:
            selection = t.selection_get()
            t.clipboard_clear()
            t.clipboard_append(selection)
            return 1
        except:
            return 0

    def __cut(self):
        if not self.__copy():
            return
        t = self.sp.text.component('text')
        start, end = t.tag_ranges(SEL)
        t.delete(start, end)

    def __find(self, e=None):
        if not hasattr(self, 'searchdialog'):
            self.__createSearchDialog()
        self.searchdialog.setText(self.sp.text)
        self.searchdialog.show()
        self.searchdialog.focus_set()

    def __keyPress(self, e):
        if not self._tkInsert.get() and e.widget.get('insert') != '\n' and \
            e.char and e.keysym != 'Delete':
            e.widget.delete('insert')

    def __paste(self):
        t = self.sp.text.component('text')
        try:
            t.insert('insert', t.selection_get(selection='CLIPBOARD'))
        except:
            pass

    def __popupMenu(self, e):
        if not hasattr(self, 'popmenu'):
            self.__createPopupMenu()
        self.popmenu.tk_popup(e.widget.winfo_rootx() + e.x,
            e.widget.winfo_rooty() + e.y)

    def __toggleInsert(self, e=None):
        if self._tkInsert.get():
            self._tkInsert.set(0)
        else:
            self._tkInsert.set(1)
        return 'break'

    def __toggleWrap(self):
        wrap=self._tkWrap.get()
        for p in self._page:
            p.text.configure(text_wrap=wrap)

    def __undo(self, e=None):
        try:
            self.sp.text.component('text').edit_undo()
        except TclError:
            self.messagebar().message('userevent', 'Nothing to undo')

    def __redo(self, e=None):
        try:
            self.sp.text.component('text').edit_redo()
        except TclError:
            self.messagebar().message('userevent', 'Nothing to redo')

##############################################################################
# menu methods
    def __clearErrors(self):
        self.sp.sent = 0
        self.sp.text.component('text').edit_modified(False)
        self.__reverseVideo()

    def __restore(self, event=None):
        # Loads forecast from a backup file
        if self.sp.text.edit_modified() and not Busy.askokcancel( \
            'Forecast not saved. Do you want to continue?', self.interior()):
            return
        while True:
            filename = Busy.askopenfilename(self.interior(),
                initialdir='tmp', filetypes=[('all', '*')])
            if filename:
                # launch dialog to preview content of the file
                data = file(filename).readlines()
                if Busy.askokcancel(''.join(data[:min(30, len(data))]),
                    self.interior()):
                    # parse first line for time and bbb
                    words = data[0].split()
                    wmo = ' '.join(words[:2])
                    htime = words[2]
                    if len(words) >= 4:
                        bbb = words[3]
                    else:
                        bbb = ''
                    body = ''.join(data[1:])
                    self.__updateEditorWindow(wmo, htime, bbb, body)
                    self.updateViewer()
                    self.sp.text.edit_reset()
                    break
            else:
                break

    def __restoreWorkFile(self):
        # Loads forecast from a work file
        if self.sp.text.edit_modified() and not Busy.askokcancel( \
            'Forecast not saved. Do you want to continue?',
            self.interior()):
            return
        afos = self.__getCurrentDummyAFOS()
        try:
            data = file(os.path.join('tmp', afos)).readlines()
        except IOError:
            msg = 'Cannot restore workfile %s' % afos
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            return
        # parse first line for header
        words = data[0].split()
        if len(words) >= 4:
            htime, bbb = words[2:4]
        else:
            htime, bbb = words[2], ''
        wmo = ' '.join(words[:2])
        body = ''.join(data[1:])
        self.__updateEditorWindow(wmo, htime, bbb, body)
        self.updateViewer()

    def __save(self, filename, new=0):
        # Writes content of the text widget to a file 'filename'
        try:
            body = self.sp.text.get().strip().expandtabs()
            if not body:
                return 0
            wmo = self.sp.header.cget('text')
            if not wmo:
                raise Avn.AvnError('No WMO header')
            if not self.sp.htime.valid():
                raise Avn.AvnError('Invalid header time')
            if not self.sp.bbb.valid():
                raise Avn.AvnError('Invalid BBB value')
            htime = self.sp.htime.get().strip()
            bbb = self.sp.bbb.get().strip().upper()
            header = '%s %s %s' % (wmo, htime, bbb)
            if new and os.path.isfile(filename):
                prevname = filename + '.prev'
                try:
                    os.rename(filename, prevname)
                except:
                    _Logger.exception('Cannot rename %s', filename)
            file(filename, 'w').write('\n'.join([header, body, '']))
            msg = 'Backup saved in %s' % filename
            _Logger.debug(msg)
            self.messagebar().message('userevent', msg)
            return 1
        except Avn.AvnError, e:
            msg = '%s, cannot save file %s' % (e, filename)
            _Logger.error(msg)
            Busy.showerror(msg, self.interior())
            return 0
        except IOError:
            msg = 'Cannot save file %s' % filename
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            return 0

    def __saveAs(self, event=None):
        # Invokes promt dialog asking for file name.
        # Called via 'File/Save' menu
        filename = Busy.asksaveasfilename(self.interior(), initialdir='tmp')
        if not filename:
            return
        self.__save(filename)

    def __saveWorkFile(self, storeindb=0):
        # Writes bulletin to a work file and optionally in
        # the fxatext database
        try:
            # use dummy headers for filename and the WMO header
            path = os.path.join('tmp', self.__getCurrentDummyAFOS())
            if not self.__save(path):
                return
            if not storeindb:
                return
            if not Busy.askokcancel('Are you sure?', self.interior()):
                return
            msg = Avn.storeInTextDB(path)
            if msg:
                Busy.showinfo(msg, self.interior())
            self.messagebar().message('userevent',
                'Forecast stored in text database')
        except Exception:
            msg = 'Save failed'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())

    def __settype(self, tag):
        self.sp.bbb.setentry(Avn.tagToBBB(tag))

    def __showPrintDialog(self):
        if not hasattr(self, 'printdialog'):
            self.printdialog = AvnDialog.PrintDialog(self.interior(), 
                self.__print)
        self.printdialog.transient(self.interior())
        self.printdialog.show()
        self.printdialog.focus_set()

    def __userMethod(self, arg=None):
        name = self.toolbox.get()
        if not name:
            return
        try:
            timepy = os.path.getmtime(os.path.join('toolpy', name+'.py'))
            timepyc = os.path.getmtime(os.path.join('toolpy', name+'.pyc'))
            need_reload = timepy > timepyc
        except OSError:
            need_reload = 1
        if name in sys.modules and need_reload:
            module = sys.modules[name]
            reload(module)
        else:
            module = __import__(name, globals(), locals(), [name])
        self._fcsts = self.splitBulletin()
        if not self._fcsts:
            return
        try:
            ids = [_getIds(f) for f in self._fcsts]
            bbb = self.sp.bbb.get().strip().upper()
            fcsts = module.updateTafs(self.interior(), bbb,
                dict(zip(ids, self._fcsts)))
            if not fcsts:
                return
            self.sp.text.settext('\n'.join([fcsts[i] for i in ids]))
        except Exception:
            msg = 'Update failed'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())

##############################################################################
# other methods
    def __autosave(self, new=0):
        # Every 60 seconds save content of the text widget to 
        # a backup file
        ix = self.sp.text.index('insert')
        if self._tkAutosave.get() and self.winfo_ismapped() and \
            self.sp.text.edit_modified() and ix != self.sp.lastix:
            self.__save(os.path.join('tmp', 
                '.'.join([self.Key, self.sp.name, _Host])), new)
        self.sp.lastix = ix
        self._afterid = self.after(60000, self.__autosave)

    def __clearTags(self):
        # Removes tags associated with QC errors
        for tag in self._edit_tags:
            self.sp.text.tag_remove(tag, 1.0, 'end')
        for n in range(self.sp.numtags):
            self.sp.text.tag_delete(str(n))
        self.sp.numtags = 0
        self.sp.errorlevel = 0

    def __addSyntaxTags(self, errors, type):
        for e in errors:
            # this one to color text
            self.sp.text.tag_add(type, *e['index'])
            # this one to set message
            tag = str(self.sp.numtags)
            self.sp.text.tag_add(tag, *e['index'])
            Balloon().tagbind(self.sp.text, tag, '\n'.join(e[type]))
            self.sp.numtags += 1

    def __checkSyntax(self):
        # Invokes syntax check and formatting methods
        self.messagebar().resetmessages('systemerror')
        if self._tkUpdateTimes.get():
            self.__updateTimes()
        fcsts = self.splitBulletin()
        if not fcsts:
            self.messagebar().message('userevent', 'Load bulletin(s) first')
            return
        self.__clearTags()
        bbb = self.sp.bbb.get().strip().upper()
        text = []
        errors = {'fatal': [], 'error': [], 'warning': []}
        for fcst in map(Avn.curry(_format, bbb), fcsts):
            offset = len(text)
            taf = self._taf_decoder(fcst, bbb, offset, strict=True)
            if 'fatal' in taf:
                errors['fatal'].append(taf)
            else:
                d = TafDecoder.errors(taf)
                for k in ['error', 'warning']:
                    errors[k].extend([x[1] for x in d[k]])
            text.extend(fcst+[''])
        self.sp.text.settext('\n'.join(text))
        for k in errors:
            self.__addSyntaxTags(errors[k], k)
        self.sp.errorlevel = max([_TagList.index(k) for k in errors \
	    if errors[k]] or [0])
        self.sp.text.edit_modified(False)
        if self.sp.numtags > 0:
            Balloon().tkraise()
            self.messagebar().message('usererror', 'Errors found')
        else:
            self.messagebar().message('userevent', 'Check successful')

    def __addQCTags(self, result):
        for ix in result:
            item = result[ix]
            t = {1: 'warning', 2: 'error', 3: 'fatal'}.get(item['level'], 
                'info')
            try:
                self.sp.text.tag_add(t, *ix)
                # this one to set message
                tag = str(self.sp.numtags)
                self.sp.text.tag_add(tag, *ix)
                Balloon().tagbind(self.sp.text, tag, item['text'])
                self.sp.numtags += 1
            except TclError:
                pass

    def __showQCResult(self):
        try:
            i, result = self._qcqueue.get_nowait()
        except Queue.Empty:
            self.after(100, self.__showQCResult)
            return
        Busy.Manager.notbusy()
        self.messagebar().resetmessages('systemevent')
        self.__clearTags()

        if 'error' in result:
            self.messagebar().message('usererror', 'Incomplete checks: %s' % \
                                      result['error']['text'])
            
        if result and max([result[x]['level'] for x in result]) > 0:
            self.__addQCTags(result)
            self.messagebar().message('usererror', 'Inconsistencies found')
        else:
            self.__addQCTags(result)
            if 'error' not in result:
                self.messagebar().message('userevent', 'Checks successful')

    def __checkQC(self, event=None):
        self.messagebar().resetmessages('systemerror')
        fcsts = self.splitBulletin()
        if not fcsts:
            self.messagebar().message('usererror', 'Nothing to check')
            return
        if self.sp.text.edit_modified():
            Busy.showerror("Press Syntax before QC'ing", self.interior())
            return
        if self.sp.errorlevel >= self._disallowSendTag:
            if not Busy.askokcancel('Bulletin has errors\nContinue?', 
                self.interior()):
                return
        bbb = self.sp.bbb.get().strip().upper()
        tafs = []
        offset = 0
        for fcst in fcsts:
            taf = self._taf_decoder(fcst, bbb, offset)
            offset += fcst.count('\n')+1
            if 'fatal' in taf:
                Busy.showerror('Cannot decode TAF', self.interior())
                continue
            tafs.append(taf)
            
        if not hasattr(self, 'qcdialog'):
            self.__createQCDialog()

        tafsInEditor = [taf['ident']['str'] for taf in tafs]
        if event:   # right button
            qcitems = self.qcdialog.activate()
            items = dict([(s.info['ident'], qcitems) for s in self._sitemonitors \
                         if s.info['ident'] in tafsInEditor])
        else:       # left button
            items = dict([(s.info['ident'], s.info['qc']) for s in self._sitemonitors \
                         if s.info['ident'] in tafsInEditor])

        if not items or not tafs:
            return
        
        siteinfo = dict([(s.info['ident'], s.info) \
            for s in self._sitemonitors])
        self.messagebar().resetmessages('systemerror')
        self.messagebar().message('userevent', 'Doing quality control...')
        Busy.Manager.busy()
        
        _Logger.debug('Invoking TAFQC.py run method')
        self._qc.performWork(TafQC.run, tafs, siteinfo, items=items)
        self.__showQCResult()

    def __clearPage(self, name):
        if int(self.option_get('confirmClose', '')):
            if not Busy.askokcancel('Really?', self.interior()):
                return
        self.__clearTags()
        self.editpager.tab(Pmw.SELECT).configure(text='')
        self.sp.header.configure(text='')
        self.sp.htime.clear()
        self.sp.bbb.clear()
        self.sp.text.clear()
        self.sp.text.edit_reset()
        self.sp.text.edit_modified(False)

    def __editViewedTaf(self):
        # loads viewed TAF into editor
        tafs = self._viewer['taf'].text.get().split('\n\n', 1)
        if len(tafs) == 0:
            return
        self.displayBulletin('AAX', None, self._topdrop.get())
        self.pager.selectpage('Editor')

    def __findBlankPage(self):
        for n in range(len(self._page)):
            name = str(n)
            tab, p = self.editpager.tab(name), self._page[n]
            if not tab.cget('text') or p.sent:
                self.editpager.selectpage(name)
                return 1
        return 0

    def __getCurrentDummyAFOS(self):
        return '%s.%s' % (self.loaddialog.getWorkPIL(), \
            self.sp.typebox.getvalue())

    def __getOptions(self):
        self._disallowSendTag = 0
        for key, var in (('autosave', self._tkAutosave), \
            ('updatetimes', self._tkUpdateTimes), \
            ('autoprint', self._tkAutoPrint), \
            ('insert', self._tkInsert)):
            try:
                var.set(int(self.option_get(key, '')))  
            except: 
                pass
        value = self.option_get('wrap', '') 
        if not value:
            value = 'none'
        self._tkWrap.set(value)
        try:
            value = self.option_get('disallowSend', '')
            if value:
                self._disallowSendTag = _TagList.index(value.lower())
        except: 
            pass
        try:
            self._confirmSend = int(self.option_get('confirmSend', ''))
        except: 
            self._confirmSend = 1
        try:
            self._storeindb = int(self.option_get('alwaySaveToDB','0'))
        except:
            self._storeindb = 0
        try:
            self._checkPendingQueue = int(self.option_get('checkPendingQueue','1'))
        except:
            self._checkPendingQueue = 1
            

    def __getMonitoredMetar(self, ident):
        return [s.getMetar() for s in self._sitemonitors \
            if s.info['ident']==ident][0]

    def __getTafs(self, ident):
        return [s.getTaf() for s in self._sitemonitors \
            if s.info['ident']==ident][0]

    def __print(self, result):
        if result is None or result == 'Close':
            self.printdialog.withdraw()
            return
        cmd = self.printdialog.get().strip()
        if not cmd:
            return
        try:
            content = self.sp.text.get().strip()
            (chldin, chldout) = os.popen4(cmd, -1)
            chldin.write(content)
            chldin.close()
            msg = chldout.read()
        except Exception:
            msg = 'Print failed'
            _Logger.exception(msg)
        if msg:
            Busy.showinfo(msg, self.interior())
        self.printdialog.withdraw()

    def __reverseVideo(self):
        self.__clearTags()
        t = self.sp.text.component('text')
        if self.sp.sent:
            t.configure(foreground=self._bg, background=self._fg)
        else:
            t.configure(foreground=self._fg, background=self._bg)

    def __selectEditPage(self, arg):
        self.sp = self._page[int(self.editpager.getcurselection())]

    def __sendForecast(self):
        # Writes transmission file(s) to 'xmit/pending' directory
        _Logger.info('Attempting to send forecast...')
        self.messagebar().resetmessages('systemerror')
        try:
            if self._disallowSendTag == 0:
                raise Avn.AvnError('Send is disabled')
            if self.sp.sent:
                raise Avn.AvnError('Forecast already sent')
            if self.sp.text.edit_modified():
                raise Avn.AvnError('Press Syntax before transmission')
            if self.sp.errorlevel >= self._disallowSendTag:
                raise Avn.AvnError('Bulletin has errors\n' + \
                    'Use Clear Errors to send it without changes')
            fcsts = self.splitBulletin()
            ids = [_getIds(x) for x in fcsts]
            badids = [x for x in ids if x not in self._headers]
            if badids:
                raise Avn.AvnError('Ids %s are not defined' % ' '.join(badids))
            if not self.sp.htime.valid():
                raise Avn.AvnError('Invalid header time')
            if not self.sp.bbb.valid():
                raise Avn.AvnError('Invalid BBB value')
            bbb = self.sp.bbb.get().strip().upper()
            if not bbb: # regular or delayed forecast
                bbb = AvnLib.getBBB(self.Key)
                self.sp.bbb.setentry(bbb)
            headertime = self.sp.htime.get().strip()
            if not self._confirmSend and bbb:
                xmittime, fcstid = time.time(), self._fcstid
            else:
                if not hasattr(self, 'senddialog'):
                    self.__createSendDialog()
                xmittime, fcstid = self.senddialog.activate(self.Key, 
                    headertime, bbb)
            if not xmittime:    # "Cancel" pressed
                msg = 'Transmission cancelled'
                self.messagebar().message('userevent', msg)
                _Logger.info(msg)
                return
            # update issuetime
            ts = time.strftime('%d%H%MZ ', time.gmtime(xmittime))
            fcsts = [re.sub('\d{6}Z ', ts, f, 1) for f in fcsts]
            if self._tkCollective.get():
                # split into TAF files
                fcsts = _make_collective(fcsts)
            filelist = []
            for id_, fcst in zip(ids, fcsts):
                if not fcst.strip():
                    continue
                
                filelist.append(Globals.DRC.putForecast(fcst,
                                                        self._headers[id_]['awips'],
                                                        self._headers[id_]['wmo'],
                                                        headertime, bbb, xmittime,
                                                        fcstid))
            if filelist:
                msg = 'Forecast put in queue'
                self.messagebar().message('userevent', msg)
                _Logger.info(msg)
                self.sp.sent = 1
                if not EditorTools.xmitServerOK():
                    msg = 'AvnFPS Transmission Server on px2f machine may be down.  Restart.'
                    Busy.showwarning(msg, self.interior())
            else:
                msg = 'Nothing to send'
                self.messagebar().message('userevent', msg)
                _Logger.info(msg)
            self.__reverseVideo()
            self.update_idletasks()
            if self._tkAutoPrint.get():
                msg = AvnLib.printForecast(filelist)
                if msg:
                    Busy.showinfo(msg, self.interior())
        except Avn.AvnError, e:
            msg = 'Cannot send forecast: %s' % e
            _Logger.error(msg)
            Busy.showerror(msg, self.interior())
        except Exception:
            msg = 'Cannot send bulletin. Check /data/logs/adapt/avnfps/avnwatch_%s on this machine.' % \
                  time.strftime('%a')
            
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())

    def __showLoadDialog(self):
        self.loaddialog.setProductList()
        self.loaddialog.activate()
        self._tkCollective.set(Globals.Collective)

    def __updateTimes(self):
        bbb = self.sp.bbb.get().strip().upper()
        self.sp.text.settext('\n'.join([self.__updateIssueValidTimes(bbb, f) \
            for f in self.splitBulletin()]))
        self.sp.htime.setentry(AvnLib.getFmtHeaderTime(self.Key, bbb))

    def __updateEditorWindow(self, wmo, htime, bbb, body, ids=None):
        self.sp.header.configure(text=wmo)
        self.sp.htime.setentry(htime)
        self.sp.bbb.setentry(bbb)
        btn = {'A': 'Amd', 'R': 'Rtd', 'C': 'Cor'}.get(bbb[:1], 'Rtn')
        self.sp.typebox.invoke(btn)
        self.sp.sent = 0
        self.after_cancel(self._afterid)
        self.__reverseVideo()
        self.sp.text.settext(body)
        self.__autosave(1)
        if ids is None:     # loading backup file
            ids = [x[:4].rstrip() for x in self.splitBulletin()]
        self.editpager.tab(Pmw.SELECT).configure(text='%s %s' % (ids[0], bbb))

#############################################################################
# forecast retrieval methods
    def __getForecast(self, bbb, order, ident):
        # Retrieves recent TAF from database or template
        # Returns pair (taf, msg)
        taf, tmpl = [''] * 2
        errmsg = []
        if order != 'template':
            try:
                taf = self.__getTafLast(order, ident)
            except Avn.AvnError, e:
                errmsg.append(str(e))
        if re.match('\s*[A-Z]{4}\s', taf):    # missing TAF
            preamble = _preamble('TAF', bbb)
            taf = '%s\n%s' % (preamble, taf)
            
        if taf and order != 'merge':
            return (taf, ''.join(errmsg))
        
        tmpl = self.__getTafTemplate(ident, bbb)
        if taf and order == 'merge':
            tmp = tmpl.split('\n')
            tmp.pop(0)
            s = tmp.pop(0)
            rexp_tmpl = re.compile('^%s \d{6}Z (\d{4}/\d{4}|\d{6})\s?' % ident)
            string = rexp_tmpl.sub('', s, 1)
            tmp.insert(0,string)
	    rexp_ramd = re.compile('\s+AMD\s(NOT\sSKED|LTD\sTO)([\s\w/]+)?')
            taf = rexp_ramd.sub('', taf, 1)
            return ('%s %s' % (taf.split('=')[0].rstrip(), \
                              '\n'.join(tmp)), ''.join(errmsg))
        else:
            return (tmpl, ''.join(errmsg))

    def __getTafPending(self, ident, bbb):
        # Gets forecast from 'xmit/pending' directory
        if not bbb or bbb[0] in ' R':
            bpat = r'[_R]..'
        else:
            bpat = r'[^_R]..'
        pattern = r'^\d{3}-' + self._headers[ident]['awips'] + \
                  r'-[A-Z]{4}\d{1,2}-[A-Z]{4}-\d{10}-' + bpat + r'-\d{10}'
        
        return Globals.DRC.getForecast(pattern)
    
    def __getTafLast(self, order, ident):
        # Retrieves most recent TAF given 'ident'
        if order == 'template':
            return None
        try:
            taf = Globals.DRC.getTafs(ident, False, time.time()-86400, 1)[0]
            return taf.text
        except Exception:
            msg = 'Cannot access previous TAF for %s' % ident
            _Logger.exception(msg)
            raise Avn.AvnError(msg)

    def __getTafTemplate(self, ident, bbb):
        # Retrieves TAF template given 'ident'
        # uses end of valid period as the start hour
        try:
            tafDuration=int(AvnParser.getTafSiteCfg(ident)['thresholds']['tafduration'])
        except:
            tafDuration=24
            
        itime = AvnLib.getFmtIssueTime('taf', bbb)
        vtime = AvnLib.getFmtValidTime('taf', bbb, tafDuration=tafDuration)[4:]
        try:
            D1=vtime[:2]
            D2=vtime[5:7]
            d1_re = re.compile(r'D1',re.IGNORECASE)
            d2_re = re.compile(r'D2',re.IGNORECASE)
            
            tmpl = AvnParser.getTafTemplate(ident,AvnLib._startHour(bbb,time.time()))
            rtmpl = re.compile(r'^%s [D\d]{6}Z [/D\d]{6,9}\s?' % ident)
            taf = d2_re.sub(D2,d1_re.sub(D1,rtmpl.sub('', tmpl, 1)))

        except Exception:
            taf = '='
            
        return 'TAF\n%s %s %s %s' % (ident, itime, vtime, taf)

    def __checkPendingQueue(self, idents, bbb):
        # Checks for forecast in 'xmit/pending' directory
        if not bbb or bbb[0] in ' R':
            bpat = r'[_R]..'
        else:
            bpat = r'[^_R]..'
            
        for ident in idents:
            pattern = r'^\d{3}-' + self._headers[ident]['awips'] + \
                      r'-[A-Z]{4}\d{1,2}-[A-Z]{4}-\d{10}-' + bpat + r'-\d{10}'
            if Globals.DRC.checkPendingQueue(pattern):
                return True
        return False

###############################################################################
    def displayBulletin(self, bbb, order, *idents):
        # Loads forecasts for selected 'bbb', 'order' and 'idents'
        if not self.__findBlankPage():
            Busy.showerror('Clear page first', self.interior())
            return
        wmo = self._headers[idents[0]]['wmo']
        htime = AvnLib.getFmtHeaderTime(self.Key, bbb)
        fcstlist, errmsg = [], []
        if self._checkPendingQueue and self.__checkPendingQueue(idents, bbb):
            try:
                tafType = {'A':'Amended','C':'Corrected','R':'Delayed'}.get(bbb[0],'Routine')
            except IndexError:
                tafType = 'Routine'
                
            msg = '%s TAF(s) in pending queue will be removed!\n' \
                  'Is this what you want to do? If not, press \'No\' and re-check\n' \
                  'TAF forecast type for editing.'  % tafType
            if not Busy.askyesno(msg,self.interior()):
                return
            
        for ident in idents:
            # always retrieve pending TAF from the queue
            taf = self.__getTafPending(ident, bbb)
            if taf:        
                fcstlist.append(taf.rstrip())
            else:
                taf, msg = self.__getForecast(bbb, order, ident)
                if taf:
                    fcstlist.append(taf.rstrip())
                if msg:
                    errmsg.append(msg)
        if errmsg:
            _Logger.error('\n'.join(errmsg))
            Busy.showerror('\n'.join(errmsg), self.interior())
        self.__updateEditorWindow(wmo, htime, bbb, '\n\n'.join(fcstlist), 
            idents)
        self.updateViewer() 

    def getSite(self):
        return self._topdrop.get()

    def setSite(self, sms, ident=None):
        self._sitemonitors = sms
        self._topdrop.setlist([sm.info['ident'] for sm in self._sitemonitors])
        if ident is None:
            ident = self._sitemonitors[0].info['ident']
        self._topdrop.selectitem(ident, 1)

    def splitBulletin(self):
        # Splits bulletin into forecasts. Assumes that a forcast is 
        # terminated with '=' or forecasts are separated by a blank 
        # line
        list = [x.strip() for x in \
            self.SplitReg.split(self.sp.text.get().upper())]
        return ['%s=\n' % x for x in list if x] 

    def updateViewer(self, arg=None):
        siteid = self._topdrop.get()
        if not siteid:
            return
        for page in self.viewpager.pagenames():
            ids = self._topdrop.get(0, 'end')
            self._viewer[page].setSite(siteid, ids)
        self._viewer['taf'].setSite(siteid)        
        page = self.viewpager.getcurselection()
        self._viewer[page].load()
        
    def setCollectiveOption(self):
        self._tkCollective.set(Globals.Collective)
