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
#       AvnWatch.py
#       GFS1-NHD:A3644.0000-SCRIPT;68
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 68 (DELIVERED)
#         Created:  25-SEP-2008 09:40:49      OBERFIEL
#           Updated Help Dialog contents to be consistent with main
#           GUI.
#       
#       Revision 67 (DELIVERED)
#         Created:  18-APR-2008 14:18:14      OBERFIEL
#           Fixed Typos in the Help Dialog, update CCFP section
#       
#       Revision 66 (DELIVERED)
#         Created:  26-FEB-2008 14:25:36      OBERFIEL
#           Fixed _talk notification (unimplemented) and DTG for Nov
#           2008
#       
#       Revision 65 (REVIEW)
#         Created:  19-FEB-2008 13:26:07      OBERFIEL
#           Updated Globals.Colors list to reflect user's changes.
#       
#       Revision 64 (REVIEW)
#         Created:  18-JAN-2008 08:04:46      GILMOREDM
#           Changed to use forecaster defined colors for status window
#       
#       Revision 63 (DELIVERED)
#         Created:  21-SEP-2007 17:26:08      OBERFIEL
#           Reorganized alert messages for flight category alerts in
#           balloon popup.  Changed wording and added additional
#           dynamic info flt cat msgs.
#       
#       Revision 62 (DELIVERED)
#         Created:  29-MAY-2007 12:19:41      OBERFIEL
#           Updated colophon.  Fixed logic within MetarMonitorP.py to
#           produce same behavior as 3.4 for
#           for vis and cig monitoring.  Fixed AvnWatch to better
#           detect flight category type messages.
#       
#       Revision 61 (DELIVERED)
#         Created:  25-MAY-2007 14:27:10      OBERFIEL
#           Update to support additional information in remarks
#       
#       Revision 60 (DELIVERED)
#         Created:  22-MAY-2007 10:11:42      GILMOREDM
#           Added impactPlacement which is configurable through the
#           options interface and code changes that allow the
#           configuration of placement of impact statements within
#       
#       Revision 59 (REVIEW)
#         Created:  18-MAY-2007 09:56:32      GILMOREDM
#           changed order of taf, metar, and messages; 
#           stripped out trailing newline on metar text
#       
#       Revision 58 (UNDER WORK)
#         Created:  18-APR-2007 12:32:44      SOLSON
#           Removed the CR characters that were present in previous rev
#           of this item.
#       
#       Revision 57 (DELIVERED)
#         Created:  06-DEC-2006 14:10:41      BLI
#           Modified to make xmit configurable for each user
#       
#       Revision 56 (BUILD_RELEASE)
#         Created:  30-NOV-2006 09:34:01      OBERFIEL
#           Updated Help documentation.  Thanks Amanda.
#       
#       Revision 55 (BUILD_RELEASE)
#         Created:  17-NOV-2006 11:02:59      BLI
#           Changed to call the new climate GUI
#       
#       Revision 54 (DELIVERED)
#         Created:  29-JUN-2006 06:36:14      OBERFIEL
#           Updated to remove references to HTML and other obsolete
#           stuff
#       
#       Revision 53 (DELIVERED)
#         Created:  20-MAY-2006 09:26:27      OBERFIEL
#           Updated documentation and removed references to HTML
#           documentation
#       
#       Revision 52 (DELIVERED)
#         Created:  24-MAR-2006 17:45:43      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 51 (DELIVERED)
#         Created:  24-MAR-2006 09:48:23      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 50 (DELIVERED)
#         Created:  15-FEB-2006 14:34:42      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 49 (APPROVED)
#         Created:  27-JAN-2006 16:00:37      TROJAN
#           stdr 956
#       
#       Revision 48 (DELIVERED)
#         Created:  16-AUG-2005 13:03:19      TROJAN
#           spr 6989
#       
#       Revision 47 (DELIVERED)
#         Created:  08-AUG-2005 13:13:55      TROJAN
#           spr 6971
#       
#       Revision 46 (DELIVERED)
#         Created:  07-JUL-2005 12:14:47      TROJAN
#           spr 6548, 6887
#       
#       Revision 45 (DELIVERED)
#         Created:  07-MAY-2005 11:31:03      OBERFIEL
#           Added Item Header Block
#       
#       Revision 44 (DELIVERED)
#         Created:  02-APR-2005 17:02:16      TROJAN
#           spr 6763
#       
#       Revision 43 (APPROVED)
#         Created:  21-MAR-2005 14:39:06      TROJAN
#           spr 6735
#       
#       Revision 42 (DELIVERED)
#         Created:  11-MAR-2005 15:55:30      TROJAN
#           spr 6717
#       
#       Revision 41 (DELIVERED)
#         Created:  14-FEB-2005 20:54:48      TROJAN
#           spr 6649
#       
#       Revision 40 (APPROVED)
#         Created:  24-JAN-2005 21:18:47      TROJAN
#           spr 6612
#       
#       Revision 39 (APPROVED)
#         Created:  24-JAN-2005 15:42:44      TROJAN
#           spr 6604
#       
#       Revision 38 (APPROVED)
#         Created:  30-SEP-2004 20:53:42      TROJAN
#           stdr 873
#       
#       Revision 37 (APPROVED)
#         Created:  19-AUG-2004 20:32:56      OBERFIEL
#           Code change
#       
#       Revision 36 (APPROVED)
#         Created:  12-JUL-2004 12:13:02      OBERFIEL
#           Modified startup interface
#       
#       Revision 35 (APPROVED)
#         Created:  09-JUL-2004 19:10:41      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 34 (APPROVED)
#         Created:  01-JUL-2004 14:59:13      OBERFIEL
#           Update
#       
#       Revision 33 (DELIVERED)
#         Created:  17-MAR-2004 19:40:09      TROJAN
#           sprs for 2.1
#       
#       Revision 32 (DELIVERED)
#         Created:  09-JAN-2004 15:27:55      PCMS
#           Updating for code cleanup
#       
#       Revision 31 (REVIEW)
#         Created:  08-JAN-2004 21:39:52      PCMS
#           Updating for code cleanup
#       
#       Revision 30 (APPROVED)
#         Created:  03-DEC-2003 18:42:56      TROJAN
#           spr 5681
#       
#       Revision 29 (APPROVED)
#         Created:  05-NOV-2003 19:05:45      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 28 (DELIVERED)
#         Created:  24-APR-2003 14:54:46      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 27 (DELIVERED)
#         Created:  10-APR-2003 15:26:55      TROJAN
#           spr 4997
#       
#       Revision 26 (BUILD_RELEASE)
#         Created:  10-MAR-2003 13:39:02      TROJAN
#           sprs 4904 - 4908
#       
#       Revision 25 (BUILD_RELEASE)
#         Created:  28-FEB-2003 12:34:34      TROJAN
#           spr 4749 4753 4815 4823
#       
#       Revision 24 (DELIVERED)
#         Created:  24-OCT-2002 16:37:53      PCMS
#           Fixing more changes related to new NWS rules (Tempo group)
#       
#       Revision 23 (DELIVERED)
#         Created:  21-OCT-2002 21:52:57      PCMS
#           Updating of rnew NWSI 10-813 migration
#       
#       Revision 22 (DELIVERED)
#         Created:  24-SEP-2002 22:20:58      PCMS
#           Separate editor windows are created when invoking the
#           forecast editor from TAFGen, TWBGen and Amd/Cor buttons in
#           the main GUI.
#       
#       Revision 21 (DELIVERED)
#         Created:  02-AUG-2002 14:11:27      PCMS
#           Implementing autoupdate of text windows with incoming
#           products
#       
#       Revision 20 (REVIEW)
#         Created:  24-JUL-2002 16:07:28      PCMS
#           Fixed problem using incorrect forecast group.
#       
#       Revision 19 (DELIVERED)
#         Created:  24-JUL-2002 15:17:25      PCMS
#           Fixed problem when incorrect forecast group is used.
#       
#       Revision 18 (DELIVERED)
#         Created:  24-JUL-2002 14:41:56      PCMS
#           Additional fixes for 5.2.1 patch
#       
#       Revision 17 (DELIVERED)
#         Created:  23-JUL-2002 14:24:25      PCMS
#           Modified to compare TAFs & METARs every 10 mins rather than
#           upon arrival.
#       
#       Revision 16 (DELIVERED)
#         Created:  17-JUL-2002 13:20:36      PCMS
#           Implemented functionality to compare METARs and TAFs every
#           10 minutes rather than upon arrival.
#       
#       Revision 15 (DELIVERED)
#         Created:  16-JUL-2002 20:35:26      PCMS
#           Set comparison of TAF and METARS to be done every 10
#           minutes
#       
#       Revision 14 (DELIVERED)
#         Created:  09-JUL-2002 21:07:05      PCMS
#           Fixed missing line break in error message and invalid path
#           when workstation uses automounter.
#       
#       Revision 13 (DELIVERED)
#         Created:  25-JUN-2002 19:48:34      PCMS
#           Updating documentation for AVN Watch
#       
#       Revision 12 (DELIVERED)
#         Created:  18-JUN-2002 19:26:49      PCMS
#           Fixed no display of METAR data on startup when last hour
#           obs missing.
#       
#       Revision 11 (BUILD_RELEASE)
#         Created:  14-JUN-2002 15:11:06      PCMS
#           Fixed time problems which affected which TAF is monitored.
#       
#       Revision 10 (DELIVERED)
#         Created:  29-MAY-2002 22:17:58      PCMS
#           Adding dialog to edit resource configuration file
#       
#       Revision 9 (DELIVERED)
#         Created:  13-MAY-2002 21:45:17      PCMS
#           Fixed viewing problem when TAFs with improper time on TEMPO
#           group are decoded.
#       
#       Revision 8 (DELIVERED)
#         Created:  03-DEC-2001 19:13:15      PCMS
#           SPR 2981
#       
#       Revision 7 (DELIVERED)
#         Created:  14-NOV-2001 21:19:51      PCMS
#           Fixing when rules take affect
#       
#       Revision 6 (DELIVERED)
#         Created:  06-NOV-2001 23:13:08      PCMS
#           updating
#       
#       Revision 5 (DELIVERED)
#         Created:  06-NOV-2001 20:58:39      PCMS
#           New rules take effect immediately.
#       
#       Revision 4 (DELIVERED)
#         Created:  30-OCT-2001 18:17:05      PCMS
#           Made the ordering of the bulleting list configurable.
#       
#       Revision 3 (DELIVERED)
#         Created:  10-OCT-2001 20:06:28      PCMS
#           Allow to select multiple products in the editor window
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:36      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:29:43      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7383
#       	Action Date:       06-NOV-2008 15:25:22
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Lack of customization in QC check
#       
#
import Queue, atexit, itertools, logging, os, signal, time
from Tkinter import *
import Pmw
import Pyro.core, Pyro.naming
from Pyro.errors import *
import AppShell, Avn, AvnLib, AvnDialog, AvnParser, AvnThread
import Busy, DataRequestServ, ErrorRedirect, Globals, TafDecoder
from HelpDialog import HelpDialog
from Balloon import Balloon

Python = os.environ['PYTHON']
TopDir = os.environ['TOP_DIR']

_Help = {
    'title': 'AvnFPS Help',
    'content': """
This is the main interface to AvnFPS.

All menus and buttons have associated help.  When the mouse cursor is
placed over context sensitive help's 'hot spot', a 'balloon' message
will appear.

Successful completion of a task is usually shown in a message window
at the bottom of the main GUI or dialogs. Important system messages
will be shown there also.  When an error occurs, a warning/error
dialog will pop up which requires acknowledgment before one can
interact further with the application.

Menu options:
    File:
        Check Now:  Forces check of all TAFs and transmission
                    status.
        Restart:    Restarts the program, using current 
                    configuration
        Quit:       Terminates the application.

    Options:
        Setup:      Calls setup configuration dialog which 
                    allows for setting configuration 
                    resources: fonts, colors and values that 
                    affect program behavior.
        Alert:      Used to select alert criteria 'on-the-fly' 
                    when the program detects a condition 
                    requiring forecaster's action.
        Blink:      If selected, station id button will blink
                    when new notification arrives
    Help:
        Used to provide version number and location of AvnFPS
        documentation web sites and this help window.

TAF Editor: Starts TAF editor
Climate:    Displays Climate GUI
Plot:       Displays Weather Plot GUI
Backup:     Invokes list dialog allowing selection of products to 
            monitor.

Server status indicators. green means server is running, red indicates
serious misconfiguration of a server(s) or a large (> 1 minute) clock
difference (skew) between px2f and workstations.

DATA-xxx:   Provides data to the GUI. The monitor will not function 
            without this server running.
            
INGEST-xxx: Data ingest server. You may still issue forecasts when 
            this server is not running, although program will not update
            with new information as it arrives.
            
XMIT-xxx:   Forecast transmission server

Queue:  Background color indicates whether last issued forecast was 
        successfully transmitted. The button invokes transmission queue
        control dialog.

Product monitoring window consists of the following units:

Site Id button: used to invoke TAF editor. Its background color is used 
    to indicate problem with data. A new alert will cause the button 
    to blink. Press right mouse button to stop blinking.
    
Last TAF and METAR time labels: those display issue time. When either
    one is late, the corresponding label is highlighted. If there 
    is no TAF, or TAF is older than 24 hours, time is set to None 
    for both TAF and MTR. 

For each monitored data source there is a set of labels indicating 
whether a particular weather element is in agreement with the forecast.

The following data sources are currently available:

Current Observation: Most recent observation
Nhr Persistence: Most recent observation compared to forecast N hours 
                 ahead
ltg:             Real-time CG lightning strikes
rltg:            Radar-based 3 hour lightning probability forecast
NDFD Grids:      GFE generated grids
llws:            Low Level Wind Shear, based on METAR and radars', profilers'
                 or aircrafts' vertical wind profile data
ccfp:            Collaborative Convective Forecast Product from AWC

Depending on configuration, some of the above can be accessed through 
popup menus associated with the data source heading labels. Use right 
mouse button to display the menu. Not all labels have an associated 
menu.

By pointing mouse cursor at a particular data source you will get 
the forecast, that data values and list of violated rules, if any,
displayed in a balloon message.

Optional shortcut buttons to the TAF Editor.
Amd:    call TAF editor initialized for amended TAF for selected site.
Rtd:    call TAF editor initialized for delayed TAF for selected site.
Cor:    call TAF editor initialized for corrected TAF for selected site.
"""
}

_Logger = logging.getLogger(__name__)

###############################################################################
def _exitfun():
    pass

###############################################################################
class BButton(Button):
    # blinking button
    def __init__(self, master, *args, **kw):
        if 'blinkvar' in kw:
            self._tkBlink = kw['blinkvar']
            del kw['blinkvar']
        else:
            self._tkBlink = IntVar()
            self._tkBlink.set(1)
        Button.__init__(self, master, *args, **kw)
        self.bg = self.cget('background')
        self.abg = self.cget('activebackground')
        self._timer = None
        self._num = 0
        # binding to cancel blinking
        self.bind('<Button-3>', Avn.curry(self.stopblink, self.bg))

    def setbackground(self, bg):
        self.bg = bg
        self.configure(background=self.bg)

    def blink(self, num=0):
        if self._num > 0 and self._tkBlink.get() == 0:
            self.stopblink()
        if num > 0:
            self._num = num
            self._on = 0
            if self._timer is not None:
                self.after_cancel(self._timer)
        self._on = not self._on
        if self._on:
            self.configure(background=self.bg)
        else:
            self.configure(background=self.abg)
        if self._num > 0:
            self._num -= 1
            self._timer = self.after(1000, self.blink)
        else:   # finished with blinks
            self._timer = None
            self.configure(background=self.bg)
            self._on = 0

    def stopblink(self, bg=None, event=None):
        if bg:
            self.setbackground(bg)
        self._num = 0

###############################################################################
class TrafficLight(Pmw.MegaWidget):
    def __init__(self, parent = None, **kw):
        optiondefs = (
            ('ok', 'green', Pmw.INITOPT),
            ('warning', 'yellow', Pmw.INITOPT),
            ('error', 'red', Pmw.INITOPT),
            ('size', 24, Pmw.INITOPT),
            ('labelmargin', 0, Pmw.INITOPT),
            ('labelpos', 'w', Pmw.INITOPT),
            ('sticky', 'news', Pmw.INITOPT),
        )
        self.defineoptions(kw, optiondefs)
        # Initialise the base class (after defining the options).
        Pmw.MegaWidget.__init__(self, parent)
        # Create the components.
        interior = self.interior()
        self._canvas = self.createcomponent('canvas',
            (), None,
            Canvas, (interior,),
            relief='raised',
            width=self['size'], 
            height=self['size']
        )
        self._canvas.grid(column=2, row=2, sticky=self['sticky'])
        interior.grid_columnconfigure(2, weight=1)
        interior.grid_rowconfigure(2, weight=1)
        orig0, orig1 = 2, self['size']-2
        self._light = self._canvas.create_oval(orig0, orig0, orig1, orig1)
        self.createlabel(interior)
        # Check keywords and initialise options.
        self.initialiseoptions()

    def get(self):
        color = self._canvas.itemcget(self._light, 'fill')
        for k in ['ok', 'warning', 'error']: 
            if color == self[k]:
                return k
        return None

    def set(self, status):
        if status in ['ok', 'warning', 'error']:
            self._canvas.itemconfigure(self._light, fill=self[status])
        else:
            self._canvas.itemconfigure(self._light, fill='grey')

###############################################################################
class AlertDialog(AvnDialog.Dialog):
    # A pop-up dialog to configure alert levels
    AlertTypes = ['deiconify', 'raise', 'play']
    Help = { 'deiconify': 'Deiconifies the applications', \
        'raise': 'Raises the application to the top', \
        'play': 'Plays sound file'}

    def __init__(self, parent):
        AvnDialog.Dialog.__init__(self, parent)
        Label(self.interior(), text='Alert Options').pack(side='top',
            pady=5, expand='yes', fill='x')
        itemlist = ['disabled'] + ['alertLevel%d' % (i+2) for i in xrange(len(Globals.Colors[2:]))]
        menulist = []
        for alert in self.AlertTypes:
            menu = self.createcomponent(alert,
                (), None,
                Pmw.OptionMenu,
                (self.interior(),),
                labelpos='w',
                label_text=alert,
                menubutton_width=12,
                items=itemlist,
                command=self._setcolor
                )
            menu.pack(side='top', padx=5)
            menulist.append(menu)
            Balloon().bind(menu.component('label'), self.Help[alert])
            try:
                menu.invoke(self.option_get('notify%s' % alert.title(), ''))
            except ValueError:
                pass
        
        Pmw.alignlabels(menulist)

    def _setcolor(self,*args):
        pass
        
    def getitems(self):
        # Returns dictionary dict[alerttype] = color index
        d = {}
        for k in self.AlertTypes:
            value = self.component(k).getcurselection()
            if value == 'disabled':
                d[k] = 99
            else:
                d[k] = int(value[-1])
        return d

###############################################################################
class SiteMonitor(object):
    def __init__(self, top, master, row, info):
        self.top = top          # AvnWatch
        self.info = info
        self.errorColor = 'red'
        self.warningColor = 'orange'
        self.tempoColor = 'yellow'
        self.status = {}
        self.maxSeverity = 0
        self.maxNewSeverity = 0
        self._tkActive = IntVar()
        self._tkActive.set(1)
        self._taf = None
        self._bg = master.option_get('background', '')
	self.impactPlacement = master.option_get('impactPlacement','')

        self._monitors = [m['module'].Monitor(self.info, m) \
            for m in top._activeMonitors]

        f = Frame(master, relief='ridge', borderwidth=2)
        f.grid(row=row, column=1, columnspan=3+len(self._monitors),
            padx=1, pady=1, sticky='news')
        kw = {'row': row, 'column': 1, 'padx': 2, 'pady': 4, 'sticky': 'news'}
        self.idbutton = BButton(master,
            width=5,
            text=self.info['ident'],
            background=self._bg,
            command=self.__showEditor,
            blinkvar=top._tkBlink,
            )
        self.idbutton.grid(**kw); kw['column'] += 1
        Balloon().bind(self.idbutton, 'Displays recent TAFs and METARs')
        self.checkbutton = Checkbutton(master,
            variable=self._tkActive,
            )
        self.checkbutton.grid(**kw); kw['column'] += 1
        Balloon().bind(self.checkbutton, 'If selected, this site is monitored') 
        f = Frame(master)
        self.timelabel = {}
        for tag in ['TAF', 'MTR']:
            self.timelabel[tag] = Label(f, text='None', width=10, bd=0)
            self.timelabel[tag].pack(side='top', expand='no', pady=0)
        f.grid(**kw); kw['column'] += 1
        self.label = {}
        for n, m in enumerate(self._monitors):
            f = Frame(master, relief='ridge', borderwidth=2)
            self.label[n] = {}
            for t in m.args['items']:
                l = Label(f,
                    text=m.args['labels'][t][:3],
                    background=Globals.Colors[1],
                    width=3,
                    )
                l.pack(side='left')
                self.label[n][t] = l
            f.grid(**kw); kw['column'] += 1
            self.status[n] = {}

        if int(master.option_get('amdbuttons', '0')):
            bbox = Pmw.ButtonBox(master, padx=0, pady=0)
            for tag in ['Amd', 'Rtd', 'Cor']:
                btn = bbox.add(tag, command=Avn.curry(self.__showEditor, 
                    Avn.tagToBBB(tag)))
                btn.configure(pady=1)
                Balloon().bind(btn, 'Calls TAF Editor')
            bbox.alignbuttons()
            kw['padx'], kw['pady'] = 0, 2
            bbox.grid(**kw)

    def __bindTafReport(self, taf, newtaf):
        err = TafDecoder.errors(self._taf.dcd)['error']
        msg = []
        for e in err:
            msg.extend(e[1]['error'])
        try:
            # to deal with TAFs resent as delayed, depends on wording
            # in TafDecoder
            msg.remove('Issue and valid times do not match')
        except ValueError:
            pass
        if msg:
            if newtaf:
                self.idbutton.setbackground(self.warningColor)
            msg.append(taf)
            Balloon().bind(self.idbutton, '\n'.join(msg))
            return
        self.idbutton.setbackground(self._bg)
        Balloon().bind(self.idbutton, 'Displays recent TAFs and METARs')

    def __configureWxLabels(self, tag, module, taf=''):
        """Make balloon messages for the indicators on main GUI"""
        def _cleanup_msg(text):
            """Flight category messages need to be treated a little differently"""
            fltcat = False
            numMETARs = text.count('METAR:')
            #
            # If its a flight category message: consolidate and simplify
            if numMETARs > 0:
                fltcat = True
                
                if numMETARs > 1:
                    mtrtafinfo = ''
                    fltcatmsgs = []
                    othermsgs = []
                    for lne in text.split('\n'):
                        METARpos = lne.find('METAR:')
                        if METARpos > 0:
                            msg,mtrtafinfo = lne.split(':',1)
                            fltcatmsgs.append(msg)
                        elif METARpos == 0:
                            mtrtafinfo = lne
                        else:
                            fltcatmsgs.append(lne.split(':',1)[0])
                            
                    new_msg = '%s: %s' % (',\n'.join(fltcatmsgs),
                                           mtrtafinfo.strip())
                    return fltcat, new_msg
                
            return fltcat, text
        
        if not self.status[tag]:
            return
        
        text = self.status[tag].get('text', '')
        for item in module.args['items']:
            d = self.status[tag]['status'][item]
            fltcat, msg = _cleanup_msg(d.msg)
            
            label = self.label[tag][item]
            label.configure(background=Globals.Colors[d.severity])
            
	    if self.impactPlacement == 'top':
		Balloon().bind(label, '\n'.join([msg, taf, text]).rstrip())
	    elif self.impactPlacement == 'split':
                if fltcat:
                    Balloon().bind(label, '\n'.join([taf, text.rstrip('\n'), msg]).rstrip())
                else:
		    Balloon().bind(label, '\n'.join([msg, taf, text]).rstrip())
	    elif self.impactPlacement == 'bottom':
		Balloon().bind(label, '\n'.join([taf, text.rstrip('\n'), msg]).rstrip())
	    else:
		Balloon().bind(label, '\n'.join([msg, taf, text]).rstrip())

    def __configureTimeLabel(self, kind, t):
        if t == 0.0:    # no data
            self.timelabel[kind].configure(text='%s None' % kind,
                background='grey')
            return
        elif time.time() > t + {'MTR': 3900, 'TAF': 24000}[kind]:
            color = self.warningColor
        else:
            color = self._bg
        self.timelabel[kind].configure(text='%s %s' % \
            (kind, time.strftime('%H:%M', time.gmtime(t))),
            background=color)

    def __showEditor(self, type=None):
        self.idbutton.stopblink(self._bg)
        self.top.showTafEditor(self.info['ident'], type) 

    def __getWarnLevels(self):
        # FIXME: currently alerts only for METARs 
        tag = 0
        m = self._monitors[tag]
        if not self.status[tag]:
            return [1]*len(m.args['items']) # missing data
        s = self.status[tag]['status']
        return [s[i].severity for i in m.args['items']]

    def __setMissing(self, msg):
        for n, m in enumerate(self._monitors):
            self.status[n]['status'] = m.setMissing(msg)
            self.__configureWxLabels(n, m)
        self.__configureTimeLabel('TAF', 0.0)
        self.__configureTimeLabel('MTR', 0.0)
        self.idbutton.setbackground(self.errorColor)
        if self._taf:
            Balloon().bind(self.idbutton, '%s\n%s' % (msg, self._taf.text))
        else:
            Balloon().bind(self.idbutton, msg)

    def checkStatus(self, event): 
        if not self._tkActive.get():
            return None
        result = {'ident': self.info['ident'], 'taf': self._taf, \
            'newtaf': False, 'status': {}}
        if event.src in ['ALL', 'tafs']:
            # new TAF or force check
            result['newtaf'] = True
            try:
                tafs = Globals.DRC.getTafs(self.info['ident'], True, 
                    time.time()-43200.0,1)[:1]
                if tafs:
                    result['taf'] = tafs[0]
                    if 'fatal' in result['taf'].dcd:
                        raise Avn.AvnError('Cannot decode TAF')
                    result['taf'].hourly = AvnLib.TafData( \
                        result['taf'].dcd['group'])
                else:
                    raise Avn.AvnError('Cannot load TAF')
                for n, m in enumerate(self._monitors):
                    result['status'][n] = m.compare(result['taf'])
            except Avn.AvnError, e:
                result['msg'] = str(e)
        else:
            for n, m in enumerate(self._monitors):
                if m.Source != event.src or result['taf'] is None \
                    or 'fatal' in result['taf'].dcd:
                    continue
                result['status'][n] = m.compare(result['taf'])
        return result

    def updateGUI(self, result):
        self._taf = result.get('taf')
        if result['newtaf']:
            if 'msg' in result:
                self.__setMissing(result['msg'])
                # don't bother with the rest
                return
            self.idbutton.configure(background=self._bg)
            self.__configureTimeLabel('TAF', self._taf.dcd['itime']['value'])
        if not self._taf or 'fatal' in self._taf.dcd:
            return
        # save warning state
        previous = self.__getWarnLevels()
        self.status.update(result['status'])
        for n, m in enumerate(self._monitors):
            if not self.status[n]:
                continue
            s = self.status[n]
            taf = self._taf.text.rstrip()
            if n == 0:  # METAR
                self.__bindTafReport(taf, result['newtaf'])
                if 'dcd' in s:
                    t = s['dcd']['itime']['value']
                else:
                    t = 0.0
                self.__configureTimeLabel('MTR', t)
            self.__configureWxLabels(n, m, taf)
        current = self.__getWarnLevels()
        self.maxSeverity = max(current)
        if result['newtaf'] or not previous:
            self.maxNewSeverity = self.maxSeverity
        else:
            # compare previous and current warning levels
            tmp = [x for (x, y) in zip(current, previous) if x > y]
            if tmp:
                self.maxNewSeverity = max(tmp)
            else:
                self.maxNewSeverity = 0
        if self.maxSeverity <= 1:
            self.idbutton.stopblink()
        else:
            self.idbutton.blink(300)    # blink for 5 min
        # update TAF Editor window
        if hasattr(self.top, 'tafEditor') and \
            self.top.tafEditor.winfo_ismapped() and \
            self.top.tafEditor.getSite() == self.info['ident']:
            self.top.tafEditor.updateViewer()

    def reset(self, monitors):
        # Sets internal data for a site to monitor
        self._monitors = [m['module'].Monitor(self.info, m) for m in monitors]
        color = Globals.Colors[1]
        for n, m in enumerate(self._monitors):
            self.status[n] = {}
            for tag in m.args['items']:
                self.label[n][tag].configure(background=color)
                Balloon().bind(self.label[n][tag], '')

    def getTaf(self):
        return self._taf

    def getMetar(self):
        return self.status[0]

###############################################################################
class ServerLights(Frame):
    def __init__(self, master):
        Frame.__init__(self, master, relief=GROOVE, bd=2)
        self.light = {}

    def configure(self):
        for tag in Globals.ServerStatus:
            if tag not in self.light:
                self.light[tag] = TrafficLight(self, label_text=tag)
                self.light[tag].pack(side='left', padx=5)
            status = self.light[tag].get()
            newstatus = status
            if time.time() - Globals.ServerStatus[tag] < 60.0:
                newstatus = 'ok'
            else:
                newstatus = 'error'
            if status != newstatus:
                self.light[tag].set(newstatus)

###############################################################################
class AvnWatch(AppShell.AppShell):
    appname = AppShell.AppShell.appname + ' Monitor'

    def __init__(self, **kw):
        self._guicfg = AvnParser.getGuiCfg()
        if not self._guicfg:
            raise SystemExit
        #
        # Up to seven alert colors are customizable in AvnFPS.
        Globals.Colors = [0,1,2,3,4,5,6]
        Globals.Viewers = self._guicfg['viewers']
        Globals.EditTag = self._guicfg['edittags']
        # import modules: replace module name by the module itself
        d = self._guicfg['monitors']
        for k in d:
            d[k]['module'] = __import__(d[k]['module']) 
        atexit.register(_exitfun)
        AppShell.AppShell.__init__(self, **kw)
        server = os.environ.get('PYRO_NS_HOSTNAME', 'local host')
        self.root.wm_title('%s connected to %s' % \
            (self.root.wm_title(), server))
        _Logger.info('Connected to %s, forecaster: %s', server,
            Globals.Forecaster)
        # redirect Pmw error messages
        ErrorRedirect.fixlogging(_Logger, self.interior())

# methods that override base class
    def appInit(self, **kw):
        self._tag = {'src': 'gui'}
        if not Globals.Products:
            Globals.Products = AvnParser.getTafProducts()[:1]
            if not Globals.Products:
                msg = 'TAFs not configured. Cannot continue'
                _Logger.exception(msg)
                Busy.showerror(msg, self.root)
                raise SystemExit
        # initialize variables
        self._status_timer = None
        self._sitemons = []     # list of current SiteMonitor widgets
        self._playFile = None
        self._checkNow = 1
        self._lastPeriod = 0    # a half hour period of a forced check
        self._lastplay = 0.0    # time of sound alert
        self._tkBlink = IntVar()
        Globals.Forecaster = kw.get('forecaster', '')

        self.__getOptionDB()
        self.root.wm_iconbitmap(Avn.WatchBitmap)
        
        Globals.Colors = [str(self.option_get('alertLevel%d' % x, 'grey')) for x in xrange(7)]
        
    def createInterface(self):
        bbox = Pmw.ButtonBox(self.interior(),
            hull_relief='groove',
            hull_bd=2,
            padx=0,
            pady=0,
            )
        if self._guicfg['features']['tafeditor']:
            btn = bbox.add('TAF Editor', command=self.showTafEditor)
            Balloon().bind(btn, 'TAF editor')
        if self._guicfg['features']['twbeditor']:
            btn = bbox.add('TWB Editor', command=self.__showTwbEditor)
            Balloon().bind(btn, 'TWB editor')
        btn = bbox.add('Climate', command=self.__showCondClimate)
        Balloon().bind(btn, 'Climatology Tools')
        btn = bbox.add('Plot', command=self.__showPlotDialog)
        Balloon().bind(btn, 'Graph')
        btn = bbox.add('Backup', command=self.__showProductSelectionDialog)
        Balloon().bind(btn, 'Product selection')
        bbox.alignbuttons()
        bbox.pack(side='top', expand='no', fill='x')
        # status lights
        f = Frame(self.interior())
        self._lights = ServerLights(f)
        self._lights.pack(side='left', expand='no')
        self.statusbtn = BButton(f,
            background='green',
            text='Queue',
            command=self.__showQueue,
        )
        Balloon().bind(self.statusbtn, 'Transmission log viewer')
        self.statusbtn.pack(side='right')
        f.pack(side='top', fill='x')

        self.frame = Pmw.ScrolledFrame(self.interior(),
            vscrollmode='static',
            )
        interior = self.frame.interior()
        f = Frame(interior, relief='ridge', borderwidth=2)
        ncols = len(self._guicfg['menus'])
        wanteditbtns = int(self.option_get('amdbuttons', '0'))
        if wanteditbtns:
            ncols += 1
        f.grid(row=0, column=1, columnspan=3+ncols, sticky='news')
        self.label = {}
        self._activeMonitors = []
        for n, items in enumerate(self._guicfg['menus']):
            m = self._guicfg['monitors'][items[0]]
            self._activeMonitors.append(m)
            self.label[n] = Label(interior, text=m['menu'])
            self.label[n].grid(row=0, column=4+n, pady=3)
            if len(items) > 1:
                popup = Menu(f, tearoff=0, type='normal')
                for item in items:
                    m = self._guicfg['monitors'][item]
                    popup.add_command(label=m['menu'],
                        command=Avn.curry(self.__reload, m, n))
                self.label[n].bind('<Button-3>',
                    Avn.curry(self.__popupMenu, popup))
        if wanteditbtns:
            label = Label(interior, text='Editor Shortcuts')
            label.grid(row=0, column=5+n, pady=3)
        self.frame.pack(side='bottom', fill='both', expand='yes')

        # monitor method uses AlertDialog values
        self.__createAlertDialog()

    def __reload(self, monitor, n):
        self._activeMonitors[n] = monitor
        self.label[n].configure(text=monitor['menu'])
        for sm in self._sitemons:
            sm.reset(self._activeMonitors)
        self.__checkStatusNow()

    def __popupMenu(self, menu, e):
        menu.tk_popup(e.widget.winfo_rootx()+e.x, e.widget.winfo_rooty()+e.y)

    def createMenuBar(self):
        self.menubar.addmenuitem('Help', 'command',
            label='About...',
            command=self.showAbout,
            )
        self.menubar.addmenuitem('Help', 'command',
            label='Usage ...',
            command=self.__showHelp,
            )
        self.menubar.addmenuitem('File', 'command',
            label='Check Now',
            command=self.__checkStatusNow,
            )
        self.menubar.addmenuitem('File', 'separator')
        self.menubar.addmenuitem('File', 'command',
            label='Restart',
            command=self.__restart,
            )
        self.menubar.addmenuitem('File', 'separator')
        self.menubar.addmenuitem('File', 'command',
            label='Quit',
            command=self.exit,
            )                       

        self.menubar.addmenu('Options', 'Configuration options')
        if Globals.Forecaster:
            self.menubar.addmenuitem('Options', 'command',
                label='Setup',
                command=self.__showResourceDialog
                )
        self.menubar.addmenuitem('Options', 'command',
            label='Alert',
            command=self.__showAlertDialog,
            )
        self.menubar.addmenuitem('Options', 'checkbutton',
            label='Blink',
            variable=self._tkBlink,
            )

    def initializeTk(self):
        import XDefaults
        for item in XDefaults.Defaults:
            self.root.option_add(item[0], item[1], 30)

# private methods
    # alert methods
    def __deiconify(self, ids, ix):
        self.root.deiconify()

    def __raise(self, ids, ix):
        self.root.tkraise()

    def __play(self, idis, ix):
        # avoid frequent bells
        now = time.time()
        if now < self._lastplay + 5.0:
            return
        self._lastplay = now
        cmd = Avn.playCommand(self._playFile)
        if cmd:
            os.system(cmd)

    def __talk(self, ids, ix):
        pass

    def __checkStatusNow(self):
        self._checkNow = 1

    def __createAlertDialog(self):
        # Creates dialog for selection of alert levels
        self.alertdialog = AlertDialog(self.interior())

    def __createQueueDialog(self):
        # Creates data ingest/transmission monitor dialog
        import QueueViewer
        self.queuedialog = QueueViewer.Viewer(self.interior())
        self.queuedialog.setGeometry()

    def __createProductSelectionDialog(self):
        # Creates dialog for selection of products to monitor
        self.productdialog = Pmw.SelectionDialog(self.interior(),
            buttons=('OK', 'Close'),
            defaultbutton='OK',
            command=self.__setProductsToMonitor,
            scrolledlist_labelpos='n',
            label_text='Select product(s)',
            scrolledlist_listbox_selectmode='extended',
            scrolledlist_listbox_width=20,
            scrolledlist_listbox_height=4,
            scrolledlist_vscrollmode='static',
            )
        self.productdialog.withdraw()

    def __getXResources(self):
        # Reads application resources file
        id = AvnParser.getForecasters().get(Globals.Forecaster, '')['id']
        if id:
            path = os.path.join('etc', 'app-resources', 'X.'+str(id))
            if os.path.isfile(path):
                self.option_readfile(path)
                return
        path = os.path.join('etc', 'app-resources', 'X')
        if os.path.isfile(path):
            self.option_readfile(path)
        else:
            msg = 'Cannot access default resources file,\n' + \
                'using build-in values'
            _Logger.info(msg)
            Busy.showwarning(msg, self.root) 

    def __getOptionDB(self):
        # Sets application resources
        try:
            self.__getXResources()
            self._playFile = self.option_get('playFile', '')
            self._tkBlink.set(int(self.option_get('blink', '')))
            Pmw.Color.setscheme(self.root,
                background=self.option_get('background', ''))
        except Exception:
            msg = 'Error processing X resources'
            _Logger.exception(msg)
            Busy.showerror(msg, self.root)
	
    def __listProducts(self):
        # Displays list of defined products
        sl = self.productdialog.component('scrolledlist')
        sl.setlist(AvnParser.getTafProducts())
        # highlight currently selected products
        for index in xrange(sl.size()):
            if sl.get(index) in Globals.Products:
                sl.selection_set(index)

    def __restart(self):
        msg = 'Restarting'
        _Logger.info(msg)
        self.messagebar().message('userevent', msg)
        if self._status_timer:
            self.after_cancel(self._status_timer)
        self._eventClient.abort()
        if os.fork():
            Globals.DRC.release()
            self.root.after(3000, self.quit)
            return
        logging.shutdown()
        os.execl(Python, 'avn'+os.path.basename(Python), 
            os.path.join(TopDir, 'py', 'avnmenu.py'),
            '-f', Globals.Forecaster, *tuple(Globals.Products))

    def __setProductsToMonitor(self, result):
        # Callback for product selection dialog
        if result != 'OK':
            self.productdialog.deactivate()
            return
        Globals.Products = self.productdialog.getcurselection()
        self.__setMainWindow()
        self.productdialog.deactivate()
        self.__checkStatusNow()

    def __showAlertDialog(self):
        self.alertdialog.setGeometry()
        self.alertdialog.display()

    def __showCondClimate(self):
        opts = '-f %s' %Globals.Forecaster
        os.spawnlp(os.P_NOWAIT,Python, Python, 
                os.path.join(TopDir, 'py','avnclimate.py'),
                opts)

    def __showPlotDialog(self):
        if not hasattr(self, 'plotDialog'):
            import WxPlot
            self.plotDialog = WxPlot.WxPlot(self.interior())
            self.plotDialog.setGeometry()
        self.plotDialog.setSite(self._sitemons)
        self.plotDialog.display()

    def __showTwbEditor(self):
        if not hasattr(self, 'twbEditor'):
            from TwbEditDialog import TwbEditor
            self.twbEditor = TwbEditor(self.interior())
        self.twbEditor.display()

    def __showHelp(self):
        HelpDialog().display(_Help)

    def __showQueue(self):
        self.statusbtn.setbackground('green')   # reset
        self.statusbtn.stopblink()
        if not hasattr(self, 'queuedialog'):
            self.__createQueueDialog()
        self.queuedialog.display()

    def __showProductSelectionDialog(self):
        if not hasattr(self, 'productdialog'):
            self.__createProductSelectionDialog()
        self.__listProducts()
        position = 'first+%d+%d' % (self.winfo_rootx()+30, \
            self.winfo_rooty()+30)
        Busy.Manager.busy(None, self.productdialog.component('hull'))
        self.productdialog.activate(geometry=position)
        Busy.Manager.notbusy()
#       Globals.DRC.release()
        if hasattr(self, 'tafEditor'):
            self.tafEditor.setSite(self._sitemons)
        if hasattr(self, 'plotDialog'):
            self.plotDialog.setSite(self._sitemons)

    def __showResourceDialog(self):
        if not hasattr(self, 'resourceEditor'):
            from ResourceDialog import ResourceEditor
            self.resourceEditor = ResourceEditor(self.interior())
            self.resourceEditor.setGeometry()
        self.resourceEditor.display()
        
    def __setMainWindow(self):
        # Initializes main display window for the selected sites
        # Unmap site widgets. row=0 are column headings
        for sm in self._sitemons:
            sm.idbutton.stopblink()
        interior = self.frame.interior()
        for w in interior.grid_slaves():
            if w.grid_info()['row'] != '0':
                w.grid_forget()
        # create/configure site widgets
        tmp = [AvnParser.getTafProductCfg(p)['sites'] for p in Globals.Products]
        tafinfo = filter(None, [AvnParser.getTafSiteCfg(ident) \
            for ident in itertools.chain(*tmp)])
        self._sitemons = [SiteMonitor(self, interior, n+1, info) \
            for (n, info) in enumerate(tafinfo)]
        self.update_idletasks()
        # resize window
        w = interior.winfo_reqwidth() + 5
	h = min(interior.winfo_reqheight(), 512)
	self.frame.component('clipper').configure(width=w, height=h)

    def __setXmitStatus(self, msg):
        if not msg.value.startswith('FAIL') and \
            not msg.value.startswith('SUCCESS'):
            return
        if msg.value.startswith('FAIL'):
            self.statusbtn.setbackground('red')
            self.messagebar().message('systemerror', msg.value)
        else:
            self.messagebar().message('systemevent', msg.value)
        self.statusbtn.blink(30)

    def __phonyMessage(self):
        # creates phony message that will make __checkData() work
        self._checkNow = 0
        msg = Avn.Bunch(src='ALL', ident='ALL', file=None)
        return Avn.Bunch(time=time.time(), 
            subject=Pyro.config.PYRO_NS_DEFAULTGROUP, msg=msg)

    def __alert(self, severity):
        if severity <= 0:
            return
        alertdict = self.alertdialog.getitems()
        for key, alertfun in [('deiconify', self.__deiconify),
            ('raise', self.__raise), ('play', self.__play)]:
            if severity >= alertdict[key]:
                alertfun(None, severity)

    def __startThreads(self):
        self._requestQueue = Queue.Queue()
        self._resultsQueue = Queue.Queue()
        self.__startEventClient()
        self._refresher = AvnThread.Worker(self._requestQueue, 
            self._resultsQueue)

    def __startEventClient(self):
        self._eventClient = AvnThread.Subscriber(self._resultsQueue)
        self._eventCount = 0

    def __listen(self):
        # Main monitoring method.
        currentPeriod = time.time()//1800.0
        if self._lastPeriod != currentPeriod:
            self._lastPeriod = currentPeriod
            self._checkNow = 1
        while True:
            if self._checkNow:
                # forced check
                i = AvnThread.Subscriber.requestId
                event = self.__phonyMessage()
            else:
                try:
                    i, event = self._resultsQueue.get_nowait()
                except Queue.Empty:
                    # re-subscribe every 30 seconds, in case Event Server 
                    # is restarted
                    self._eventCount += 1
                    if self._eventCount >= 30:
                        self._eventCount = 0
                        self._eventClient.subscribe()
                    break
            try:
                if i == AvnThread.Subscriber.requestId:
                    self.__processNotifications(event)
                else:
                    self.__processMonitors(event)
            except Exception:
                _Logger.exception('Cannot process event')
        self._lights.configure()
        self._status_timer = self.after(1000, self.__listen)

    def __selectSiteMonitors(self, event):
        def _metars(sm):
            return event.msg.ident in sm.info['sites']['metar']
        def _ident(sm):
            return event.msg.ident == sm.info['ident']
        if event.msg.ident == 'ALL':
            return self._sitemons
        if event.msg.src == 'mtrs':
            return filter(_metars, self._sitemons)
        else:
            return filter(_ident, self._sitemons)

    def __processNotifications(self, event):
        if event.msg.src.startswith('INGEST') or \
            event.msg.src.startswith('DATA'):
            Globals.ServerStatus[event.msg.src] = event.time
        elif event.msg.src.startswith('XMIT'):
            Globals.ServerStatus[event.msg.src] = event.time
            self.__setXmitStatus(event.msg)
        else:
            sms = self.__selectSiteMonitors(event)
            if not sms:
                return
            message = 'Checking %s for %s' % (event.msg.src, \
                ' '.join([sm.info['ident'] for sm in sms]))
            _Logger.info(message)
            if event.msg.src == 'ALL':
                self.messagebar().message('userevent', 'Checking all data ...')
            else:
                self.messagebar().message('userevent', message)
            self.update_idletasks()
            for sm in sms:
                self._refresher.performWork(sm.checkStatus, event.msg)

    def __processMonitors(self, event):
        ident = event.get('ident', None)
        try:
            sm = [s for s in self._sitemons if s.info['ident']==ident][0]
        except IndexError:
            _Logger.exception('Invalid ident %s', ident)
            return
        sm.updateGUI(event)
        self.__alert(sm.maxNewSeverity)

# public methods
    def run(self):
        # overrides Tkinter method
        self.__setMainWindow()
        # connect to data request server
        try:
            Globals.DRC = DataRequestServ.Client()
            self.__startThreads()
            self.__listen()
        except PyroError:
            msg = 'Cannot access servers'
            _Logger.error(msg)
            self.messagebar().message('systemerror', msg)
        # shut down forecaster selection menu
        os.kill(os.getppid(), signal.SIGUSR1)
        signal.signal(signal.SIGUSR1,signal.SIG_IGN)
        AppShell.AppShell.run(self)

    def showTafEditor(self, ident=None, bbb=''):
        if not hasattr(self, 'tafEditor'):
            from TafEditDialog import TafEditor
            self.tafEditor = TafEditor(self.interior())
        self.tafEditor.setSite(self._sitemons, ident)
        if ident:
            if bbb:
                self.tafEditor.displayBulletin(bbb, 'Latest', ident)
                self.tafEditor.pager.selectpage('Editor')
            else:
                self.tafEditor.updateViewer()
                self.tafEditor.pager.selectpage('Viewer')
        else:
            self.tafEditor.pager.selectpage('Editor')
        self.tafEditor.display()

    def exit(self):
        if int(self.option_get('confirmClose', '')):
            if not Busy.askokcancel('Do you really want to quit?', self.root):
                return
        try:
            os.kill(os.getppid(), signal.SIGUSR1)
        except OSError:
            pass
        try:
            Globals.DRC.release()
        except:
            pass
        self.quit()
