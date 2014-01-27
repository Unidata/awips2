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
#       WxPlot.py
#       GFS1-NHD:A8544.0000-SCRIPT;19
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 19 (DELIVERED)
#         Created:  16-JUN-2009 14:28:12      OBERFIEL
#           If tafduration cannot be determined from the info.cfg file,
#           default to 24 hours
#       
#       Revision 18 (REVIEW)
#         Created:  20-MAR-2009 18:29:26      OBERFIEL
#           Removed code cruft. ETA changed to NAM. NGMMOS removed.
#       
#       Revision 17 (DELIVERED)
#         Created:  01-AUG-2008 15:44:46      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 16 (DELIVERED)
#         Created:  25-JUN-2008 15:47:42      OBERFIEL
#           Display now accounts for either 24- or 30-h TAFs
#       
#       Revision 15 (DELIVERED)
#         Created:  29-JUN-2006 06:36:20      OBERFIEL
#           Updated to remove references to HTML and other obsolete
#           stuff
#       
#       Revision 14 (DELIVERED)
#         Created:  02-JUN-2006 11:32:12      TROJAN
#           spr 7163: fixed Display button action - was resetting
#           guidance time before plotting
#       
#       Revision 13 (DELIVERED)
#         Created:  02-JUN-2006 11:23:24      TROJAN
#           spr 7162: fixed Display button action - was resetting
#           guidance time before plotting
#       
#       Revision 12 (DELIVERED)
#         Created:  20-MAY-2006 09:32:21      OBERFIEL
#           Updated documentation
#       
#       Revision 11 (DELIVERED)
#         Created:  23-FEB-2006 13:59:53      TROJAN
#           MOS/LAMP data not plotted
#       
#       Revision 10 (DELIVERED)
#         Created:  15-FEB-2006 14:34:49      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 9 (APPROVED)
#         Created:  01-FEB-2006 15:15:40      TROJAN
#           Incorrect handling of missing data
#       
#       Revision 8 (APPROVED)
#         Created:  31-JAN-2006 18:18:50      TROJAN
#           added check for nonexistent item in sites dictionary
#       
#       Revision 7 (APPROVED)
#         Created:  31-JAN-2006 08:46:35      TROJAN
#           added LAMP item
#       
#       Revision 6 (DELIVERED)
#         Created:  24-AUG-2005 19:21:46      TROJAN
#           spr 7002
#       
#       Revision 5 (DELIVERED)
#         Created:  10-JUL-2005 20:08:05      TROJAN
#           spr 6915
#       
#       Revision 4 (DELIVERED)
#         Created:  07-MAY-2005 11:40:17      OBERFIEL
#           Added Item Header Block
#       
#       Revision 3 (DELIVERED)
#         Created:  18-APR-2005 17:32:34      OBERFIEL
#           Changes to support gamin
#       
#       Revision 2 (DELIVERED)
#         Created:  06-APR-2005 11:41:07      TROJAN
#           spr 6763
#       
#       Revision 1 (APPROVED)
#         Created:  02-APR-2005 17:15:17      TROJAN
#           spr 6763
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
import logging, math, os, time
from Tkinter import *
import Pmw
import Avn, AvnDialog, AvnLib, AvnParser, Busy, Globals, GraphWidget
from Balloon import Balloon

_Help = {
    'title': 'Weather Plot Help',
    'content': """
This dialog is used to display TAFs, METARs and guidance forecasts.

Menus:
    Site ID     - pulldown menu displaying list of all TAF sites.
                  Selection of a site from the list redraws the window.
    zoom        - zoom factor (time scale). 

Buttons:
    Display     - Redraws the window.
    Times       - Displays forecast time selection window
    Print       - Dumps an image of the window to a command specified in
                  the configration file etc/wxplot.cfg.
    Close       - Closes this dialog.
    Help        - Displays this help.

Data Sources    - selection of available data sources. 
"""
}

_Logger = logging.getLogger(__name__)

def _formatCig(v):
    return ' %03d' % (v/100)

def _formatVsby(v):
    if v < 0.2:
        return ' 1/8'
    elif v < 0.3:
        return ' 1/4'
    elif v < 0.6:
        return ' 1/2'
    elif v < 0.8:
        return ' 3/4'
    elif v < 1.1:
        return '   1'
    elif v < 1.6:
        return '11/2'
    else:
        return '%.0f' % ((v+0.49)//1.0)

###############################################################################
class TimeSelector(Pmw.Dialog):
    def __init__(self, parent, viewers, **kw):
        Pmw.Dialog.__init__(self, parent, **kw)
        self.withdraw()
        self.title(Avn.Name + ' Time Selector')

        self._ident = None
        self._sites = None

        self.configure(buttons=('Reset', 'Close'),
            defaultbutton='Close',
            command=self.__execute,
            )

        self.viewers = [v for v in viewers if v.tag != 'metar']

        f = Frame(self.interior())
        self.combo = {}
        for v in self.viewers:
            self.combo[v.tag] = Pmw.ComboBox(f,
                label_text=v.label,
                labelpos='w',
                entry_width=22,
            )
            self.combo[v.tag].pack(side='top', padx=5)
        Pmw.alignlabels(self.combo.values())
        f.pack(side='top')

    def __execute(self, result):
        if result == 'Reset':
            for v in self.viewers:
                items = self.combo[v.tag].get()
                if items:
                    self.combo[v.tag].selectitem(0, 1)
        else:
            self.withdraw()
            self.deactivate()

    def display(self, above=None):
        if self.winfo_ismapped():
            return
        self.transient(self['master'])
        self.show()

    def setTimes(self, ident=None, sites=None):
        if ident:
            self._ident = ident
        if sites:
            self._sites = sites
        for v in self.viewers:
            if v.tag == 'taf':
                tafs = Globals.DRC.getTafs(self._ident, False)
                headers = [taf.header.split('\n')[0] for taf in tafs]
            elif ('mos' in v.tag or 'lamp' in v.tag) and v.tag in self._sites:
                files = Globals.DRC.getMosFiles(self._sites[v.tag], v.args['model'])
                headers = [f.split('.')[0] for f in files]
            elif (v.tag == 'nam' or v.tag == 'ruc' or v.tag == 'gfs') and v.tag in self._sites:
                files = Globals.DRC.getEtaFiles(self._sites[v.tag], v.args['model'])
                headers = [f.split('.')[0] for f in files]
            else:
                headers = []
            if headers:
                self.combo[v.tag].setlist(headers)
                self.combo[v.tag].selectitem(0, 1)
            else:
                self.combo[v.tag].clear()

    def getTimes(self):
        d = dict([(v.tag, self.combo[v.tag].get()) for v in self.viewers])
        d['metar'] = 'dummy'
        return d

###############################################################################
class WxPlot(AvnDialog.Dialog):
    Height = {'cig': 150, 'vsby': 150, 'wind': 100}
    Width = 1000

    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' Weather Plot')

        self.cfg = AvnParser.getPlotCfg()
        if not self.cfg:
            raise SystemExit
        self._viewer = {}
        for v in self.cfg['viewers']:
            self._viewer[v.tag] = v.module.Plot(**v.args)
        self.ident = None

        # from AvnDialog
        self.createMessageBar(self.interior(), True)
        
        # time selection dialog
        self.timeselector = TimeSelector(self.interior(), self.cfg['viewers'],
            master=self.interior())

        # GUI
        f = Frame(self.interior())

        self.siteids = Pmw.ComboBox(f,
            label_text='Site ID:',
            labelpos='w',
            entry_width=5,
            selectioncommand=self.update,
            )
        self.siteids.pack(side='left', padx=5)

        btnbox = Pmw.ButtonBox(f)
        btn = btnbox.add('Display', command=self.draw)
        Balloon().bind(btn, 'Draws weather')
        btn = btnbox.add('Times', command=self.timeselector.display)
        Balloon().bind(btn, 'Displays time selection dialog')
        btn = btnbox.add('Print', command=self.print_)
        Balloon().bind(btn, 'Saves window image to a file')
        btn = btnbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = btnbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        btnbox.pack(side='right')
        self.counter = Pmw.Counter(f,
            labelpos='w',
            label_text='zoom',
            entry_width=1,
            entryfield_value=1,
            entryfield_validate={'validator': 'integer', 'min': 1, 'max': 3},
            )
        self.counter.pack(side='right', padx=5)
        f.pack(side='top', expand='yes', fill='x')      
        f = Frame(self.interior())
        self.chkbtns = Pmw.RadioSelect(f,
            buttontype='checkbutton',
            orient='horizontal',
            labelpos='w',
            label_text='Data Sources',
            hull_borderwidth=2,
            hull_relief='ridge',
            )
        self.chkbtns.pack(side='left', expand='yes', fill='x', padx=5)
        for v in self.cfg['viewers']:
            self.chkbtns.add(v.tag, text=v.label, background=v.args['color'])
        self.chkbtns.setvalue(self.cfg['selected'])
        f.pack(side='top', expand='yes', fill='x')      
        self.date = Label(self.interior(), text='')
        self.date.pack(side='top', expand='no', padx=5)

        self.sf = Pmw.ScrolledFrame(self.interior(),
            usehullsize=1,
            hscrollmode='static',
            vscrollmode='static',
            )
        kwds = {'relief': 'sunken', 'background': 'gray90', 
            'font': 'fixed', 'border': 2}
        f = self.sf.interior()
        self.graph = {}
        h = 0
        for item in ['cig', 'vsby', 'wind']:
            label = Label(f, text=item.upper())
            label.pack(side='top')
            h += label.winfo_reqheight()+4
            self.graph[item] = GraphWidget.Graph(f, self.Width, 
                self.Height[item], **kwds)
            self.graph[item].pack(side='top', fill='both', expand='yes')
            h += self.Height[item]+4
        self.sf.pack(side='left', expand='yes', fill='both')
        h += self.sf.component('horizscrollbar').winfo_reqheight()
        w = self.Width+self.sf.component('vertscrollbar').winfo_reqwidth()
        self.sf.configure(hull_height=h, hull_width=w)

        self.initialiseoptions(WxPlot)
        self.setGeometry()

    def getSite(self):
        return self.siteids.get()

    def setSite(self, sitemonitors):
        self.siteids.setlist([sm.info['ident'] for sm in sitemonitors])
        self.siteids.selectitem(0, 1)
        self.update()

    def print_(self):
        if self.ident is None:
            return
        window = self.interior().winfo_id()
        if '%s' in self.cfg['printcmd']:
            tmp = self.cfg['printcmd'] % self.ident
            command = 'xwd -id %d | %s' % (window, tmp)
        else:
            command = 'xwd -id %d | %s' % (window, self.cfg['printcmd'])
        self.messagebar().message('userevent', command)
        os.system(command)

    def _getSiteConfig(self, ident):
        self.ident = ident
        siteinfo = AvnParser.getTafSiteCfg(ident)
        if not siteinfo:
            msg = 'Missing configuration file for %s' % ident
            _Logger.error(msg)
            Busy.showerror(msg, self.interior())
            return False
        self.sites = siteinfo['sites']
        thresholds = siteinfo['thresholds']
        self.tafduration = int(thresholds.get('tafduration','24'))
        self.vsby = thresholds['vsby']
        self.cig = thresholds['cig']
        if self.cfg['vsby']['bot'] < thresholds['vsby'][0]:
            self.vsby.insert(0, self.cfg['vsby']['bot'])
        if self.cfg['vsby']['top'] > thresholds['vsby'][-1]:
            self.vsby.append(self.cfg['vsby']['top'])
        if self.cfg['cig']['bot'] < thresholds['cig'][0]:
            self.cig.insert(0, self.cfg['cig']['bot'])
        if self.cfg['cig']['top'] > thresholds['cig'][-1]:
            self.cig.append(self.cfg['cig']['top'])

        self.yaxis = { \
            'cig': tuple([(math.log(h), _formatCig(h)) for h in self.cig]),\
            'vsby': tuple([(math.log(v), _formatVsby(v)) for v in self.vsby]), \
            'wind': ((-1, ' '*4), (1, ' '*4)), \
            }
        return True

    def draw(self, arg=None):
        self.messagebar().resetmessages('systemerror')

        width = self.Width * int(self.counter.get())
        graphics = {'cig': [], 'vsby': [], 'wind': []}
        for key in graphics:
            self.graph[key].clear()
            event = Avn.Bunch(width=width, height=self.Height[key])
            self.graph[key]._reconfigure(event)
        items = self.chkbtns.getvalue()
        if not items:
            return

        ident = self.siteids.get()
        if ident != self.ident:
            if not self._getSiteConfig(ident):
                return
            
        now = time.time()
        ftime = now - 3600.0*self.cfg['hours']['back']
        ttime = now + (3600.0*max(self.cfg['hours']['forward'],self.tafduration))
        
        tticks = [t for t in Avn.frange((ftime//3600.0)*3600.0, ttime, 3600.0)]
        self.xaxis = ((tticks[0]-1800.0, ''),) + \
            tuple([(t, time.strftime('%H', time.gmtime(t))) for t in tticks]) +\
                ((tticks[-1]+1800.0, ''),)

        times = self.timeselector.getTimes()
        for item in items:
            header = times.get(item)
            if not header:
                continue
            try:
                g = self._viewer[item].plot(ident, self.sites, now, tticks, 
                    self.vsby, self.cig, header)
                for key in graphics:
                    graphics[key].extend(g[key])
            except Avn.AvnError, e:
                msg = str(e)
                _Logger.error(msg)
                self.messagebar().message('usererror', msg)
            except Exception:
                msg = 'Unexpected error, check log file'
                _Logger.exception(msg)
                self.messagebar().message('systemerror', msg)

        if graphics['cig']:
            graphObject = GraphWidget.Objects(graphics['cig'])
            self.graph['cig'].draw(graphObject, self.xaxis, self.yaxis['cig'], 
                grid=True, numYLabels=4)
        if graphics['vsby']:
            graphObject = GraphWidget.Objects(graphics['vsby'])
            self.graph['vsby'].draw(graphObject, self.xaxis, 
                self.yaxis['vsby'], grid=True, numYLabels=4)
        if graphics['wind']:
            graphObject = GraphWidget.Objects(graphics['wind'])
            self.graph['wind'].draw(graphObject, self.xaxis, 
                self.yaxis['wind'], grid=True)

        self.date.configure(text='%s %s' % \
            (self.ident, time.strftime('%x %X UTC')))

    def update(self, ident=None):
        if ident is None:
            ident = self.siteids.get()
        if not self._getSiteConfig(ident):
            return
        self.timeselector.setTimes(ident, self.sites)
        self.draw()
