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
#       CigVisTrend.py
#       GFS1-NHD:A9059.0000-SCRIPT;16
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 17
#         Created:  10-AUG-2012 15:00:00      GZHANG
#           DR 14702: Added fix for PyTables in Gui().trend()
#
#       Revision 16 (DELIVERED)
#         Created:  06-MAR-2008 17:01:20      OBERFIEL
#           Added fix for leap-years.
#       
#       Revision 15 (DELIVERED)
#         Created:  20-MAR-2007 13:36:53      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 14 (DELIVERED)
#         Created:  04-JAN-2007 12:52:14      OBERFIEL
#           Corrected misspelling.
#       
#       Revision 13 (REVIEW)
#         Created:  28-DEC-2006 11:33:09      OBERFIEL
#           Made validate function less restrictive.
#       
#       Revision 12 (DELIVERED)
#         Created:  30-JUN-2006 10:58:42      TROJAN
#           spr 7190: added plot area window parameters to
#           app-recources file
#       
#       Revision 11 (DELIVERED)
#         Created:  30-MAY-2006 15:10:39      TROJAN
#           spr 7144: added auto-update feature, number of years in
#           database
#       
#       Revision 10 (DELIVERED)
#         Created:  23-MAY-2006 08:47:13      TROJAN
#           spr 7152: fixed select days matching criteria - round time, 
#           added history button in TWEB Editor's statusbar, 
#           fixed spelling
#       
#       Revision 9 (DELIVERED)
#         Created:  19-MAY-2006 08:32:07      TROJAN
#           SPR 7146: fixed select days matching criteria - round
#           observation time up
#       
#       Revision 8 (DELIVERED)
#         Created:  02-MAY-2006 09:34:10      TROJAN
#           SPR 7133: fixed "where" selection expression for dates
#           wrapping around end of the year
#       
#       Revision 7 (DELIVERED)
#         Created:  02-MAY-2006 08:52:49      TROJAN
#           SPR 7138: fixed "where" selection expression for dates
#           wrapping around end of the year
#       
#       Revision 6 (DELIVERED)
#         Created:  27-MAR-2006 08:11:49      TROJAN
#           spr 7103: added ErrorRedirect method
#       
#       Revision 5 (DELIVERED)
#         Created:  24-MAR-2006 17:45:44      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#       
#       Revision 4 (DELIVERED)
#         Created:  23-FEB-2006 08:24:45      TROJAN
#           moved paths to text database commands to avnenv.sh
#       
#       Revision 3 (DELIVERED)
#         Created:  16-FEB-2006 14:20:58      TROJAN
#           removed unnecessary imports, renamed shadowed variables
#       
#       Revision 2 (APPROVED)
#         Created:  15-FEB-2006 14:34:43      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 1 (APPROVED)
#         Created:  13-FEB-2006 10:25:14      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7373
#       	Action Date:       02-JUN-2008 20:44:52
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: AvnWatch monitoring not reliable.
#       
#
import logging, os, sys, time
import numpy
import Avn, ClimLib, MetarDecoder, ClimateProcessLogger
# for profiling
#import hotshot, hotshot.stats

_Help = {
    'title': 'AvnFPS - Ceiling/Visibility Trend Help', \
    'content': """
This application displays ceiling/visibility trend based
on selected initial conditions.

Use "Get" button to retrieve METAR for a selected site.
The METAR can be modified.
Use "Decode" button to initialize selection widgets.
The initial conditions can be adjusted either by typing
in the "value" and "range" windows, or by mouse actions.
Left button moves value or an edge of range (red area on
the element widget). Middle button is used to move both
value and range. In the "Wind Direction" widget use right
button to toggle between wind arrow and a circle representing
calm and variable wind.
Use "Element" radiobuttons to select forecasted element.
Press "Draw" to display the forecast.

The displayed image can be printed or stored in a graphic file.
Use "File" menu for that purpose.

"""
}

import sys
sys.argv = [__name__]

_tmp = range(len(ClimLib.FlightCats))
CigCat = list(enumerate([ClimLib.FlightCats[_x]['cig'] for _x in _tmp]))
VisCat = list(enumerate([ClimLib.FlightCats[_x]['vis'] for _x in _tmp]))
NumCat = len(ClimLib.FlightCats)
Fill = 9

## used by Selection class
# some documentation
CatDict = { \
    'dd': [30, 90, 150, 210, 270, 330], \
    'ff': [5, 12, 32], \
    'vsby': [0.4, 1.1, 3.1, 6.1], \
    'cig': [210, 610, 1020, 3130, 5050], \
}

fields = {}
_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)
# does not seem to work
#na.Error.setMode(dividebyzero='ignore', invalid='ignore')

##############################################################################
def ff_range(value):
    seq = CatDict['ff']
    seq.insert(0, 0)
    seq.append(sys.maxint)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad ff_range')
    return lo, hi

def vsby_range(value):
    seq = CatDict['vsby']
    seq.insert(0, 0)
    seq.append(sys.maxint)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad vsby_range')
    return lo, hi

def cig_range(value):
    seq = CatDict['cig']
    seq.insert(0, 0)
    seq.append(sys.maxint)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad cig_range')
        return
    return lo, hi

def _cigCat(v):
    for n, cig in CigCat:
        if v < cig:
            return n
    else:
        return NumCat

def _visCat(v):
    for n, vis in VisCat:
        if v < vis:
            return n
    else:
        return NumCat

def squeeze(matches, tref):
    # eliminate multiple events within the same time window
    def _num(t):
        return (t-tref)//86400.0
    d = dict([(_num(t), (n, t)) for n, t in matches])
    tmp = d.values()
    tmp.sort()
    return tmp

def process_data_all(count, data, ref_t, num_hours, table):
    for obs in data:
        dh = int((obs[0]-ref_t)//3600.0)
        if not (0 <= dh < num_hours):
            raise Avn.AvnError('Bug in process_data_all()')
        if obs[1] != table.attrs.cig['fill'] and \
            obs[2] != table.attrs.vis['fill']:
            c = _cigCat(obs[1])
            v = _visCat(obs[2])
            j = min(c, v)
            if c < NumCat:
                count['cig'][dh,c] += 1
            if v < NumCat:
                count['vis'][dh,c] += 1
            if j < NumCat:
                count['joint'][dh,c] += 1
            count['total'][dh] += 1

def process_data_onhour(count, data, ref_t, num_hours, table):
    tmp_cig = numpy.ones((num_hours,))*Fill
    tmp_vis = numpy.ones((num_hours,))*Fill
    tmp_joint = numpy.ones((num_hours,))*Fill
    for obs in data:
        dh = int((obs[0]-ref_t)//3600.0)
        if not (0 <= dh < num_hours):
            raise Avn.AvnError('Bug in process_data_onhour()')
        if obs[1] != table.attrs.cig['fill'] and \
            obs[2] != table.attrs.vis['fill']:
            if tmp_joint[dh] == Fill:
                tmp_cig[dh] = _cigCat(obs[1])
                tmp_vis[dh] = _visCat(obs[2])
                tmp_joint[dh] = min(tmp_cig[dh], tmp_vis[dh])
    for h in range(num_hours):
        if tmp_cig[h] < NumCat:
            count['cig'][h,tmp_cig[h]] += 1
        if tmp_vis[h] < NumCat:
            count['vis'][h,tmp_vis[h]] += 1
        if tmp_joint[h] < NumCat:
            count['joint'][h,tmp_joint[h]] += 1
        if tmp_joint[h] < Fill:
            count['total'][h] += 1

def process_data_worst(count, data, ref_t, num_hours, table):
    tmp_cig = numpy.ones((num_hours,))*Fill
    tmp_vis = numpy.ones((num_hours,))*Fill
    tmp_joint = numpy.ones((num_hours,))*Fill
    for obs in data:
        dh = int((obs[0]-ref_t)//3600.0)
        if not (0 <= dh < num_hours):
            raise Avn.AvnError('Bug in process_data_worst()')
        if obs[1] != table.attrs.cig['fill'] and \
            obs[2] != table.attrs.vis['fill']:
            tmp_cig[dh] = min(tmp_cig[dh],_cigCat(obs[1]))
            tmp_vis[dh] = min(tmp_vis[dh],_visCat(obs[2]))
            tmp_joint[dh] = min(tmp_cig[dh], tmp_vis[dh])
    for h in range(num_hours):
        if tmp_cig[h] < NumCat:
            count['cig'][h,tmp_cig[h]] += 1
        if tmp_vis[h] < NumCat:
            count['vis'][h,tmp_vis[h]] += 1
        if tmp_joint[h] < NumCat:
            count['joint'][h,tmp_joint[h]] += 1
        if tmp_joint[h] < Fill:
            count['total'][h] += 1

##############################################################################
class Gui():
    AppName = 'AvnFPS - Ceiling/Visibility Trend'
    Xresfile = os.path.join('etc', 'app-resources', 'XCigVisTrend')
    IdsFile = os.path.join('etc', 'ids.cfg')
    RowLabels = ['MVFR', 'IFR', 'LIFR', 'VLIFR', 'COUNT']
    MaxHours = 13   # hour_menu must not contain value >= MaxHours
    def __init__(self):
        pass

    def get_option_db(self):
        # set default X resources. Read resource file, if available
        self.root.option_add('*font', 'fixed')
        self.root.option_add('*background', 'grey')
        self.root.option_add('*foreground', 'black')
        self.root.option_add('*wrapLength', '0')
        self.root.option_add('*MessageBar.Entry.background', 'grey85')
        self.root.option_add('*wrapLength', '0')
        self.root.option_add('*EntryField.Entry.background', 'white')
        self.root.option_add('*Listbox*background', 'white')
        self.root.option_add('*plotWidth', '6.0')
        self.root.option_add('*plotHeight', '3.5')
        self.root.option_add('*dpi', '90')
        self.root.option_add('*vlifr', 'purple')
        self.root.option_add('*lifr', 'red')
        self.root.option_add('*ifr', 'yellow')
        self.root.option_add('*mvfr', 'blue')
        self.root.option_add('*plotX0', '0.12')
        self.root.option_add('*plotY0', '0.30')
        self.root.option_add('*plotX1', '0.98')
        self.root.option_add('*plotY1', '0.90')
        if os.path.isfile(self.Xresfile):
            self.root.option_readfile(self.Xresfile)
        vlifr_color = self.root.option_get('vlifr', '')
        lifr_color = self.root.option_get('lifr', '')
        ifr_color = self.root.option_get('ifr', '')
        mvfr_color = self.root.option_get('mvfr', '')
        # white for COUNT
        self.colors = [mvfr_color, ifr_color, lifr_color, vlifr_color, 'white'] 
        Pmw.Color.setscheme(self.root, 
            background=self.root.option_get('background', ''))

    def get_site_config(self):
        # get list of data files
        cp = ConfigParser.SafeConfigParser()
        cp.read(self.IdsFile)
        self.ids = dict([(x, dict(cp.items(x))) for x in cp.sections()])
        if self.ids:
            tmp = self.ids.keys()
            tmp.sort()
            self.station_w.setlist(tmp)
            self.station_w.selection_set(0)

    def set_geometry(self, widget):
        """forces dialog 30 pixels off parent's left top position"""
        x, y = self.interior().winfo_rootx(), self.interior().winfo_rooty()
        s_w, s_h = self.interior().winfo_screenwidth(), \
            self.interior().winfo_screenheight()
        w_w, w_h = widget.winfo_reqwidth(), widget.winfo_reqheight()
        x, y = min(x+30, s_w-w_w), min(y+30, s_h-w_h)
        widget.wm_geometry('+%d+%d' % (x, y))

    def save_image(self):
        # save image to a file
        opts = {'initialdir': 'tmp', 'filetypes': [('PostScript', '.ps'), \
            ('JPEG', '.jpg'), ('PNG', '.png'), ('all', '*')]}
        filename = Busy.asksaveasfilename(self.interior(), **opts)
        if not filename:
            return
        if '.' not in filename:
            msg = '%s does not have an extension, defaulting to Postscript' % \
                filename
            if Busy.askokcancel(msg, self.interior()):
                self.canvas.get_tk_widget().postscript(file=filename, 
                   colormode='color')
        elif filename.endswith('.ps'):
            self.canvas.get_tk_widget().postscript(file=filename, 
                colormode='color')
        else:
            tmpfile = os.tmpnam()+'.ps'
            self.canvas.get_tk_widget().postscript(file=tmpfile, 
                colormode='color')
            command = 'convert %s %s' % (tmpfile, filename)
            chldin, chldout = os.popen4(command, -1)
            chldin.close()
            msg = chldout.read()
            if msg:
                Busy.showinfo(msg, self.interior())
            os.system('convert %s %s' % (tmpfile, filename))
            os.unlink(tmpfile)

    def create_print_dialog(self):
        # create print dialog
        self.print_dialog = Pmw.Dialog(self.interior(),
            buttons=('OK', 'Cancel'),
            defaultbutton='OK',
            title='Print',
            command=self.print_,
            )
        self.print_dialog.withdraw()

        interior = self.print_dialog.interior()

        self.print_dialog.mode = Pmw.RadioSelect(interior,
            buttontype = 'radiobutton',
            labelpos='w',
            label_text='Palette',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge',
            )
        self.print_dialog.mode.pack(side='top', fill='x', expand='yes', padx=5)
        for btn in ['gray', 'color']:
            self.print_dialog.mode.add(btn)
        self.print_dialog.mode.setvalue('gray')
        self.print_dialog.command = Pmw.EntryField(interior,
            labelpos='w',
            label_text='command',
            entry_width=12,
            validate={'min': 1,'max': 50, 'minstrict':0},
            value='lpr',
            )
        self.print_dialog.command.pack(side='top', fill='x', expand='yes', 
            padx=5)

    def show_help_dialog(self):
        dialog = HelpDialog()
        if dialog.winfo_ismapped():
            return
        self.set_geometry(dialog)
        dialog.display(_Help)

    def show_print_dialog(self):
        if self.print_dialog.winfo_ismapped():
            return
        self.set_geometry(self.print_dialog)
        self.print_dialog.transient(self.interior())
        self.print_dialog.show()

    def print_(self, action):
        # print image 
        if action == 'OK' and self.print_dialog.command.valid():
            command = self.print_dialog.command.getvalue()
            mode = self.print_dialog.mode.getvalue()
            #
            # If color palette selected, then make sure the -P flag is present
            if mode == 'color':
                lpdest = re.compile('-P\s+\w+')
                if not lpdest.search(command):
                    cmd = command.split(' ')
                    cmd.insert(1,'lp2')
                    cmd.insert(1,'-P')
                    command = ' '.join(cmd)
                    
            chldin, chldout = os.popen4(command, -1)
            chldin.write(self.canvas.get_tk_widget().postscript(
                colormode=mode))
            chldin.close()
            msg = chldout.read()
            if msg:
                Busy.showinfo(msg, self.interior())
        self.print_dialog.withdraw()
        self.print_dialog.deactivate()

    def get_metar(self, siteID):
        import JUtil
        import MetarData
        data = MetarData.retrieve([siteID], 1)
        if data is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % siteID)
        data = [{'header' : d.header, 'text' : d.text, 'dcd' : d.dcd} for d in data]
        data.sort(lambda x, y: cmp(y['dcd']['itime']['str'], x['dcd']['itime']['str']))
        tms = time.gmtime(data[0]['dcd']['itime']['value'])
        return JUtil.pyValToJavaObj(data[0])

    def decode_metar(self):
        metar = self.metar_w.getvalue().upper().split('\n')
        if not metar:
            return
        for n, line in enumerate(metar):
            if line.startswith('METAR') or line.startswith('SPECI'):
                metar = ''.join(metar[n:])
                break
        else:
            msg = 'Cannot find keywords METAR or SPECI'
            Busy.showerror(msg, self.interior())
            return
        dcd = self.decoder(metar)
        if 'fatal' in dcd:
            msg = 'Cannot decode report:\n', metar
            Busy.showerror(msg, self.interior())
            return
        tms = time.gmtime(dcd['itime']['value'])
        hour = tms.tm_hour+tms.tm_min/60.0
        self.hour_w.draw_range(hour-1.0, hour+1.0)
        self.hour_w.draw_value(hour)
        self.day_w.draw_range(tms.tm_yday-30, tms.tm_yday+30)
        self.day_w.draw_value(tms.tm_yday-1)
        try:
            vsby = dcd['vsby']['value']
            lo, hi = self.vsby_w.get_range()
            if not lo <= vsby < hi:
                lo, hi = vsby_range(vsby)
                self.vsby_w.draw_range(lo, hi)
            self.vsby_w.draw_value(vsby)
        except KeyError:
            self.messagebar.message('usererror', 'Skipping visibility...')
        try:
            cig = dcd['sky']['cig']
            lo, hi = self.cig_w.get_range()
            if not lo <= cig < hi:
                lo, hi = cig_range(cig)
                self.cig_w.draw_range(lo, hi)
            self.cig_w.draw_value(cig)
        except KeyError:
             self.messagebar.message('usererror', 'Skipping ceiling...')
        try:
            dd = dcd['wind']['dd']
            if dd == 0 or dd == 'VRB':
                dd = 'CALM'
            lo, hi = self.wind_dir_w.get_range()
            if dd == 'CALM':
                self.wind_dir_w.draw_range(1, 360)
            else:
                self.wind_dir_w.draw_range(dd-30, dd+30)
            self.wind_dir_w.draw_value(dd)
        except KeyError:
            self.messagebar.message('usererror', 'Skipping wind direction...')
        try:
            ff = dcd['wind']['ff']
            lo, hi = self.wind_speed_w.get_range()
            if not lo <= ff < hi:
                lo, hi = ff_range(ff)
                self.wind_speed_w.draw_range(lo, hi)
            self.wind_speed_w.draw_value(ff)
        except KeyError:
            self.messagebar.message('usererror', 'Skipping wind speed...')
        try:
            dcd['pcp']['str']
            self.pcp_w.setvalue('yes')
        except KeyError:
            self.pcp_w.setvalue('no')

    def trend(self, table, selection, unltd_cig):
        def _in(value, from_, to):
            if from_ <= to:
                return from_ <= value <= to
            else:
                return not (to < value < from_)
    
        def time_selector11(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('(yday0<=yday) & (yday<=yday1)'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o
    
        def time_selector12(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('(yday0<=yday) & (yday<=yday1)'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o
    
        def time_selector21(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('yday0<=yday'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o
            for o in table.where('yday<=yday1'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o
    
        def time_selector22(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('yday0<=yday'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o
            for o in table.where('yday<=yday1'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o
    
        def wx_selector(row, sel):
            ff = row['wind_spd']
            if ff == table.attrs.wind_spd['fill']:
                return False
            if not _in(ff, *sel.wind_speed):
                return False
            dd = row['wind_dir']
            if dd == table.attrs.wind_dir['fill']:
                if ff == 0:
                    dd = 0
                else: 
                    typ = row['wdir_type']
                    if typ == 'V':
                        dd = 0
                    else:
                        return False
            else:
                dd = float(dd)
            if dd > 0:
                if not _in(dd, *sel.wind_dir):
                    return False
            cig = row['cig']
            if cig == table.attrs.cig['fill']:
                return False
            if not _in(cig, *sel.cig):
                return False
            vsby = row['vis']
            if vsby == table.attrs.vis['fill']:
                return False
            if not _in(vsby, *sel.vsby):
                return False
            pcp = [x for x in row['pres_wxm_code'][:] if 50 <= x < 100] != []
            if pcp and sel.pcp == 'no' or not pcp and sel.pcp == 'yes':
                return False
            return True

        # convert units
        sel = Avn.Bunch(wind_dir=selection.wind_dir,
            wind_speed=[ClimLib.us2hd_wind_speed(x) \
                        for x in selection.wind_speed],
            cig=[ClimLib.us2hd_cig(x, unltd_cig) for x in selection.cig],
            vsby=[ClimLib.us2hd_vsby(x) for x in selection.vsby],
            hour=selection.hour,
            yday=selection.yday,
            pcp=selection.pcp,
            cur_hour=selection.cur_hour,
            )
        # fix hour selection - round to full hour
        def _round(h):
            return (h+1.0/6.0)//1.0     # 10 min before hour
        sel.cur_hour = _round(sel.cur_hour)
        sel.hour = [_round(x) for x in sel.hour]
    
        # create search selector
        if sel.yday[0] <= sel.yday[1]:
            if sel.hour[0] <= sel.hour[1]:
                time_selector = time_selector11
            else:
                time_selector = time_selector12
        else:
            if sel.hour[0] <= sel.hour[1]:
                time_selector = time_selector21
            else:
                time_selector = time_selector22

        # select days matching criteria
        matches = [(row.nrow, row['date_time']+600) for row in \
            time_selector(sel) if wx_selector(row, sel)]
        tref = 3600.0*sel.hour[0]
        count = {'cig': numpy.zeros((self.MaxHours, NumCat), numpy.float32), \
                'vis': numpy.zeros((self.MaxHours, NumCat), numpy.float32), \
                'joint': numpy.zeros((self.MaxHours, NumCat), numpy.float32), \
                'total': numpy.zeros((self.MaxHours,))
                }
        delta = (sel.cur_hour-sel.hour[0])%24.0*3600.0 + tref
        # stats method
        process_data = process_data_worst
        for n, t in squeeze(matches, tref):
            t0 = (t-tref)//86400.0*86400.0 + delta  # time corrresponding to 
                                                    # the selected hour
            t1 = t0-600.0                           # start search time
            t2 = t1 + self.MaxHours*3600.0          # end search time
#           print '==========', time.ctime(t), time.ctime(t1), time.ctime(t2)
            n1 = max(0, n-2*int((t-t1)//3600.0)-1)  # small enough
            n2 = n1 + 5*self.MaxHours               # big enough
            data = [(row['date_time'], row['cig'], row['vis']) for row in \
                table.where('(t1<=date_time) & (date_time<t2)', start=n1, stop=n2)]
            process_data(count, data, t1, self.MaxHours, table)
        # calculate frequencies
        args = count.keys()
        args.remove('total')
        tmp = 0.01*count['total']
        for arg in args:
            for n in range(NumCat):
                count[arg][:,n] /= tmp
        return count

    def draw(self, event=None):
        if self.data is None:
            return
        self.ax.cla() 
        elem = self.element_w.getvalue()
        item = {'vis': 'visibility', 'cig': 'ceiling', \
            'joint': 'flight category'}.get(elem, '')
        title = '%s %02dZ %s forecast' % (self.id_, self.data.hour, item)
        self.ax.set_title(title)
        num_hours = int(self.hour_menu.getvalue())+1
        col_labels = ['%02dZ' % ((x+self.data.hour)%24) \
            for x in range(num_hours)]
        xvals = na.arange(num_hours) + 0.2
        yoff = na.zeros(len(col_labels), na.Float32)
        bar = [None]*NumCat
        widths = [0.6]*num_hours
        self.ax.xaxis.set_major_locator(multipleLocator)
        cell_text = []
        for row in range(NumCat):
            yvals = self.data.count[elem][:,row][:num_hours]
            bar[row] = self.ax.bar(xvals, yvals, widths, bottom=yoff, 
                color=self.colors[NumCat-row-1])
            yoff += yvals
            cell_row = ['']*num_hours
            for n in range(num_hours):
                if self.data.count['total'][n] > 0:
                    cell_row[n] = '%.0f' % yvals[n]
            cell_text.append(cell_row)
        cell_text.reverse()
        cell_row = [str(self.data.count['total'][n]) for n in range(num_hours)]
        cell_text.append(cell_row)
        self.ax.table(cellText=cell_text, rowLabels=self.RowLabels,
            rowColours=self.colors, colLabels=col_labels, loc='bottom')
        # axes
        self.ax.set_ylabel('Percent occurrence')
        self.ax.set_xticks([])
        ymax, delta = 105, 10
        self.ax.set_yticks(na.arange(0, ymax, delta))
        self.ax.grid(True)
        self.canvas.draw()

    def wait_for_data(self):
        try:
            i, (id_, data) = self.resultqueue.get_nowait()
        except Queue.Empty:
            self.after(100, self.wait_for_data)
            return
        if type(data) == type(''):
            self.messagebar.message('systemerror', data)
            self.data = None
        else:
            self.data = data
            self.id_ = id_
        Busy.Manager.notbusy()
        self.draw()

    def display(self):
        try:
            id_ = self.station_w.getcurselection()[0]
            selection = Avn.Bunch(cig=self.cig_w.get_range(),
                vsby=self.vsby_w.get_range(),
                wind_speed=self.wind_speed_w.get_range(),
                wind_dir=self.wind_dir_w.get_range(),
                hour=self.hour_w.get_range(),
                yday=self.day_w.get_range(),
                pcp=self.pcp_w.getvalue(),
                cur_hour=self.hour_w.get_value(),
                )
        except IndexError:
            Busy.showerror('Select site from list', self.interior())
            return
        except ValueError, e:
            Busy.showerror(str(e), self.interior())
            return
        msg = 'Retrieving data for %s, this will take a while' % id_
        self.messagebar.message('userevent', msg)
        Busy.Manager.busy()
        self.update_idletasks()
        self.worker.performWork(self.get_data, selection, id_)
        self.wait_for_data()

    def get_data(self, selectionDict, id_, hours, fname, queue):
        self.MaxHours = hours
        selection = Avn.Bunch(cig=selectionDict['cig'],
                vsby=selectionDict['vsby'],
                wind_speed=selectionDict['wind_speed'],
                wind_dir=selectionDict['wind_dir'],
                hour=selectionDict['hour'],
                yday=selectionDict['yday'],
                pcp=selectionDict['pcp'],
                cur_hour=selectionDict['cur_hour'],
                )
        
        try:
            if not os.path.isfile(fname):
                raise Avn.AvnError('File %s does not exist' % fname)
            import tables
            fh = tables.openFile(fname, 'r')
            try:
                import warnings
                # following function - table = fh.getNode('/obs') throws extraneous warnings
                # so going to filter warnings on this function
                warnings.simplefilter("ignore")
                table = fh.getNode('/obs')
                warnings.simplefilter("default")
                unltd = ClimLib.Unlimited
                data = self.trend(table, selection, unltd)
                cig_count = data['cig']
                vis_count = data['vis']
                joint_count = data['joint']
                total_count = data['total']
                dataDict = {}
                cigList = []
                visList = []
                jntList = []
                totalList = []
                for h in range(self.MaxHours):
                    totalList.append(float(total_count[h]))
                    tmpCig = []
                    tmpVis = []
                    tmpJnt = []
                    for c in range(NumCat):
                        tmpCig.append(float(cig_count[h][c]))
                        tmpVis.append(float(vis_count[h][c]))
                        tmpJnt.append(float(joint_count[h][c]))
                    cigList.append(tmpCig)
                    visList.append(tmpVis)
                    jntList.append(tmpJnt)
                dataDict['cig'] = cigList
                dataDict['vis'] = visList
                dataDict['joint'] = jntList
                dataDict['total'] = totalList
                queue.put(dataDict)
                queue.put("done")
            finally:
                fh.close()
            return data
        except Exception, e:
            msg = 'Cannot retrieve data for %s: %s' % (id_, e)
            _Logger.error(msg)
            print msg

##############################################################################

