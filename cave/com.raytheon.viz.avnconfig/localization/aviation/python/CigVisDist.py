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
#       CigVisDist.py
#       GFS1-NHD:A9171.0000-SCRIPT;17
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 17 (DELIVERED)
#         Created:  14-MAR-2008 09:58:05      OBERFIEL
#           Fixed misspelling of occurrence.
#       
#       Revision 16 (DELIVERED)
#         Created:  26-FEB-2008 14:20:54      OBERFIEL
#           imported re module in CigVisDist; Fix output and help
#           documentation in WindRose
#       
#       Revision 15 (DELIVERED)
#         Created:  12-FEB-2008 17:11:55      OBERFIEL
#           Change print dialog to be consistent with other climatology
#           tools.
#       
#       Revision 14 (DELIVERED)
#         Created:  06-FEB-2008 09:52:11      OBERFIEL
#           Fixed Print Stats dialog to set dialog to what forecaster
#           is seeing.
#       
#       Revision 13 (REVIEW)
#         Created:  31-JAN-2008 09:56:25      GILMOREDM
#           changed code so that save stats only saves for either 24
#           hours or the current hour.
#       
#       Revision 12 (DELIVERED)
#         Created:  16-NOV-2007 10:18:56      OBERFIEL
#           Added exception handling to get_month_range() and
#           pop-down 'Print Stats' GUI and status when successful.
#       
#       Revision 11 (REVIEW)
#         Created:  09-NOV-2007 09:19:41      OBERFIEL
#           Remove dependency on another, temporary module.  Functions
#           in that module have been incorporated into this one.
#       
#       Revision 10 (INITIALIZE)
#         Created:  02-NOV-2007 07:43:28      OBERFIEL
#           Removed CR characters and added doc block
#       
#       Revision 9 (REVIEW)
#         Created:  31-OCT-2007 12:49:19      GILMOREDM
#           Added functionality to save climo statistics
#       
#       Revision 8 (REVIEW)
#         Created:  23-JUL-2007 08:45:36      GILMOREDM
#           Added code to allow saving of tables of stats
#       
#       Revision 7 (DELIVERED)
#         Created:  20-MAR-2007 13:36:54      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 6 (DELIVERED)
#         Created:  05-FEB-2007 07:51:00      OBERFIEL
#           Corrected misspellings in help text and graphic.
#       
#       Revision 5 (DELIVERED)
#         Created:  04-JAN-2007 12:52:14      OBERFIEL
#           Corrected misspelling.
#       
#       Revision 4 (REVIEW)
#         Created:  28-DEC-2006 11:33:09      OBERFIEL
#           Made validate function less restrictive.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-JUN-2006 10:58:41      TROJAN
#           spr 7190: added plot area window parameters to
#           app-recources file
#       
#       Revision 2 (DELIVERED)
#         Created:  30-MAY-2006 15:10:38      TROJAN
#           spr 7144: added auto-update feature, number of years in
#           database
#       
#       Revision 1 (DELIVERED)
#         Created:  16-MAY-2006 16:00:30      TROJAN
#           spr 7144
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
import logging, os, re, time
import ConfigParser, cPickle, Queue
import numarray as na
import matplotlib.backends.backend_tkagg as mpltk
from matplotlib.ticker import MultipleLocator
import Tkinter as tk
import Pmw
import Avn, AvnThread, Busy, ClimLib, ErrorRedirect, MessageBar, ClimateProcessLogger
from Balloon import Balloon
from HelpDialog import HelpDialog

_Help = {
    'title': 'AvnFPS - Ceiling and Visibility Display Help', \
    'content': """
This application displays ceiling and visiblity distribution for
selected time range.

The distribution is plotted in a form of stacked histogram. 
Plot Type 
    By Month    - the X axis is month. The category frequency is 
                  calculated for a selected range of hours.
    By Hour     - the X axis is hour of day. The category frequency 
                  is calculated for a selected range of months.
    By Wind Dir - the X axis is 16 point wind direction. The category 
                  frequency is calculated for a selected range of 
                  months and hours.

Auto Redraw 
    Forces screen redraw every time a new sites selected or date/time 
    widgets are modified.

Element
    Visibility - only visibility thresholds are checked to determine 
                 flight category
    Ceiling -    only ceiling thresholds are checked to determine 
                 flight category
    Joint -      both ceiling and visibility thresholds are checked 
                 to determine flight category

y-axis scale
    Sets maximum value for the vertical axis. If 'auto' toggle is set,
    the value is computed. Behaves sensibly if selected value is too 
    low.
"""
}

_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)

multipleLocator = MultipleLocator(1)
na.Error.pushMode(invalid='ignore')

###############################################################################
def _int_validator(widget, min_, max_, text):
    '''Validates month and hour. Allows for cycles.'''
    try:
        n = int(text)
        if n < min_ or n >= max_:
            widget.setvalue(str((n-min_)%(max_-min_)+min_))
        return Pmw.OK
    except ValueError:
        return Pmw.ERROR

###############################################################################
class Gui(Pmw.MegaWidget):
    AppName = 'AvnFPS - Ceiling/Visibility Distribution'
    IdsFile = os.path.join('etc', 'ids.cfg')
    GetData = 'cvdata'
    Key = 'cvdata'
    Month = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', \
        'Oct', 'Nov', 'Dec']
    RowLabels = ['MVFR', 'IFR', 'LIFR', 'VLIFR']
    Element = {'cig': 'Ceiling', 'vis': 'Visibility', \
               'joint': 'Flight Category'}

    def __init__(self, **kw):
        optiondefs = ( \
        )
        self.defineoptions(kw, optiondefs)
        self.root = tk.Tk()
        self.root.withdraw()

        self._tkUpdate = tk.IntVar()

        self.get_option_db()
        Pmw.initialise(self.root, useTkOptionDb=1)
        self.root.title(self.AppName)
        Pmw.MegaWidget.__init__(self, parent=self.root)
        self.root.resizable(0, 0)

        self.initialiseoptions(Gui)

        self.graphics = {}
        self.id_ = None
        self.cache = ClimLib.Cache()
        self.resultqueue = Queue.Queue()
        self.worker = AvnThread.Worker(Queue.Queue(), self.resultqueue)

        self.create_print_dialog()
        self.create_stats_dialog()

        interior = self.interior()
        ErrorRedirect.fixlogging(_Logger, self.interior())
        Busy.instantiate(interior)

        self.menubar = Pmw.MenuBar(interior,
            hull_relief='raised',
            hull_borderwidth=2,
            )
        self.menubar.grid(row=0, column=0, columnspan=2, sticky='ew')

        self.menubar.addmenu('Help', '', side='right')
        self.menubar.addmenuitem('Help', 'command',
            label='Usage',
            command=self.show_help_dialog,
            )
        self.menubar.addmenu('File', '', side='left')
        self.menubar.addmenuitem('File', 'command',
            label = 'Save Image',
            command=self.save_image,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Print Image',
            command=self.show_print_dialog,
            )
        self.menubar.addmenuitem('File', 'command',
            label = 'Save Stats',
            command=self.show_stats_dialog,
            )
        self.menubar.addmenuitem('File', 'separator')
        self.menubar.addmenuitem('File', 'command',
            label = 'Quit',
            command=self.quit,
            )

        frame = tk.Frame(interior,
            bd=2,
            relief='ridge',
            )
        self.station_w = Pmw.ScrolledListBox(frame,
            labelpos='n',
            label_text='Sites',
            listbox_width=5,
            listbox_exportselection=0,
            vscrollmode='static',
            selectioncommand=self.update,
            )
        self.station_w.pack(side='top', expand='yes', fill='y', padx=5, pady=5)
        self.element_w = Pmw.RadioSelect(frame,
            buttontype='radiobutton',
            orient='vertical',
            labelpos='n',
            label_text='Element',
            hull_borderwidth=2,
            padx=1,
            pady=1,
            hull_relief='ridge',
            )
        self.element_w.pack(side='top', expand='no', padx=2, pady=3)
        for ele, text in [('vis', 'Visibility'), ('cig', 'Ceiling'),
            ('joint', 'Joint')]:
            self.element_w.add(ele, text=text)
        self.element_w.setvalue('vis')

        f = tk.Frame(frame, relief='ridge', bd=2)
        label = tk.Label(f, text='y-axis scale')
        label.pack(side='top', pady=2)
        self.tkScale = tk.DoubleVar()
        self.tkScale.set(100.0)
        scale = tk.Scale(f,
            tickinterval=0,
            resolution=5.0,
            showvalue=1,
            from_=100.0,
            to=5.0,
            variable=self.tkScale,
        )
        scale.pack(side='top', pady=2)
        self.tkAutoScale = tk.BooleanVar()
        self.tkAutoScale.set(True)
        btn = tk.Checkbutton(f, 
            text='auto',
            variable=self.tkAutoScale,
        )
        btn.pack(side='top', pady=2)
        f.pack(side='top')

        bbox = Pmw.ButtonBox(frame,
            labelpos='w',
            frame_borderwidth=2,
            frame_relief='ridge',
        )
        btn = bbox.add('Draw', command=self.display)
        Balloon().bind(btn, 'Draws histogram')
        bbox.pack(side='top', padx=2, pady=5)

        frame.grid(row=1, column=0, sticky='ns')

        # notebook
        notebook = Pmw.NoteBook(interior,
            raisecommand=self.setpage,
        )
        notebook.grid(row=1, column=1, sticky='news')

        for item, label in [('month', 'By Month'), ('hour', 'By Hour'), \
            ('wdir', 'By Wind Dir')]:
            page = notebook.add(item, tab_text=label)
            f = tk.Frame(page, relief='ridge', bd=2)
            g = self.graphics[item] = {'selection': {}, 'ax': None, \
                'canvas': None}
            col = 0
            if item == 'month':
                g['selection']['hour'] = self.add_hour_counter(f)
                g['selection']['hour'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
                g['selection']['hrange'] = self.add_hour_range_counter(f)
                g['selection']['hrange'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
            elif item == 'hour':
                g['selection']['month'] = self.add_month_counter(f)
                g['selection']['month'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
                g['selection']['mrange'] = self.add_month_range_counter(f)
                g['selection']['mrange'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
            elif item == 'wdir':
                g['selection']['month'] = self.add_month_counter(f)
                g['selection']['month'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
                g['selection']['mrange'] = self.add_month_range_counter(f)
                g['selection']['mrange'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
                g['selection']['hour'] = self.add_hour_counter(f)
                g['selection']['hour'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
                g['selection']['hrange'] = self.add_hour_range_counter(f)
                g['selection']['hrange'].grid(row=0, column=col, padx=5, pady=1)
                col += 1
            toggle = tk.Checkbutton(f, 
                text='Auto Redraw',
                variable=self._tkUpdate,
                )
            toggle.grid(row=0, column=col, padx=20, pady=1, sticky='news')
            f.grid(row=0, column=0, sticky='ew')

            try:
                figsize = (float(self.root.option_get('plotWidth', '')), \
                    float(self.root.option_get('plotHeight', '')))
                dpi = int(self.root.option_get('dpi', ''))
                x0 = float(self.root.option_get('plotX0', ''))
                y0 = float(self.root.option_get('plotY0', ''))
                x1 = float(self.root.option_get('plotX1', ''))
                y1 = float(self.root.option_get('plotY1', ''))
            except Exception, e:
                _Logger.error('Invalid plot area parameters')
                figsize = (8.5, 5.0)
                dpi = 90
                x0 = 0.1
                y0 = 0.22
                x1 = 0.98
                y1 = 0.9
            figure = mpltk.Figure(figsize=figsize, dpi=dpi)
            g['ax'] = figure.add_axes([x0, y0, x1-x0, y1-y0])
            g['ax'].set_xticks([])
            g['ax'].set_yticks([])
        
            g['canvas'] = mpltk.FigureCanvasTkAgg(figure, master=page)
            g['canvas'].get_tk_widget().grid(row=1, column=0, sticky='news')

        notebook.setnaturalsize()

        # message bar
        self.messagebar = MessageBar.MessageBar(interior,
            entry_relief='sunken',
            entry_bd=1,
            history=True,
            )
        self.messagebar.grid(row=2, column=0, columnspan=2, sticky='ew')
        
        self.get_site_config()

        interior.pack(side='top', fill='both', expand='yes')
        self.focus_set()

    def add_month_counter(self, master):
        w = Pmw.Counter(master,
            labelpos='w',
            label_text='Month:',
            entry_width=2,
            entryfield_value=time.gmtime().tm_mon,
            entryfield_modifiedcommand=self.update,
            )
        w.configure(entryfield_validate=Avn.curry(_int_validator, w, 1, 13))
        return w

    def add_hour_counter(self, master):
        w = Pmw.Counter(master,
            labelpos='w',
            label_text='Hour:',
            entry_width=2,
            entryfield_value=time.gmtime().tm_hour,
            entryfield_modifiedcommand=self.update,
            )
        w.configure(entryfield_validate=Avn.curry(_int_validator, w, 0, 24))
        return w

    def add_hour_range_counter(self, master):
        w = Pmw.Counter(master,
            labelpos='w',
            label_text='Num Hours:',
            entry_width=2,
            entryfield_value=1,
            entryfield_modifiedcommand=self.update,
            )
        w.configure(entryfield_validate=Avn.curry(_int_validator, w, 1, 25))
        return w

    def add_month_range_counter(self, master):
        w = Pmw.Counter(master,
            labelpos='w',
            label_text='Num Months:',
            entry_width=2,
            entryfield_value=1,
            entryfield_modifiedcommand=self.update,
            )
        w.configure(entryfield_validate=Avn.curry(_int_validator, w, 1, 13))
        return w

    def setpage(self, name):
        self.current_plot = name 

    def set_geometry(self, widget):
        """forces dialog 30 pixels off parent's left top position"""
        x, y = self.interior().winfo_rootx(), self.interior().winfo_rooty()
        s_w, s_h = self.interior().winfo_screenwidth(), \
            self.interior().winfo_screenheight()
        w_w, w_h = widget.winfo_reqwidth(), widget.winfo_reqheight()
        x, y = min(x+30, s_w-w_w), min(y+30, s_h-w_h)
        widget.wm_geometry('+%d+%d' % (x, y))
        widget.resizable(1, 0)

    def get_option_db(self):
        # set default X resources. Read resource file, if available
        self.root.option_add('*font', 'fixed')
        self.root.option_add('*background', 'grey')
        self.root.option_add('*foreground', 'black')
        self.root.option_add('*wrapLength', '0')
        self.root.option_add('*MessageBar.Entry.background', 'grey85')
        self.root.option_add('*updateOnSelection', '0')
        self.root.option_add('*plotWidth', '8.5')
        self.root.option_add('*plotHeight', '5.0')
        self.root.option_add('*dpi', '90')
        self.root.option_add('*vlifr', 'purple')
        self.root.option_add('*lifr', 'red')
        self.root.option_add('*ifr', 'yellow')
        self.root.option_add('*mvfr', 'blue')
        self.root.option_add('*plotX0', '0.10')
        self.root.option_add('*plotY0', '0.22')
        self.root.option_add('*plotX1', '0.98')
        self.root.option_add('*plotY1', '0.90')

        path = os.path.join('etc', 'app-resources', 'X'+self.__module__)
        if os.path.isfile(path):
            self.root.option_readfile(path)
        self._tkUpdate.set(int(self.root.option_get('updateOnSelection', '')))
        vlifr_color = self.root.option_get('vlifr', '')
        lifr_color = self.root.option_get('lifr', '')
        ifr_color = self.root.option_get('ifr', '')
        mvfr_color = self.root.option_get('mvfr', '')
        self.colors = [mvfr_color, ifr_color, lifr_color, vlifr_color]
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

    def save_image(self):
        # save image to a file
        opts = {'initialdir': 'tmp', 'filetypes': [('PostScript', '.ps'), \
            ('JPEG', '.jpg'), ('PNG', '.png'), ('all', '*')]}
        filename = Busy.asksaveasfilename(self.interior(), **opts)
        if not filename:
            return
        canvas = self.graphics[self.current_plot]['canvas'].get_tk_widget()
        if '.' not in filename:
            msg = '%s does not have an extension, defaulting to Postscript' % \
                filename
            if Busy.askokcancel(msg, self.interior()):
                canvas.postscript(file=filename, colormode='color')
        elif filename.endswith('.ps'):
            canvas.postscript(file=filename, colormode='color')
        else:
            tmpfile = os.tmpnam()+'.ps'
            canvas.postscript(file=tmpfile, colormode='color')
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
            validate={'min': 1, 'max': 50, 'minstrict':0},
            value='lpr',
            )
        self.print_dialog.command.pack(side='top', fill='x', expand='yes', 
            padx=5)

    def create_stats_dialog(self):
        # create print dialog
        self.stats_dialog = Pmw.Dialog(self.interior(),
            buttons=('OK', 'Close'),
            defaultbutton='OK',
            title='Save Stats',
            command=self.save_stats,
            )
        self.stats_dialog.withdraw()

        interior = self.stats_dialog.interior()

        self.stats_dialog.element = Pmw.RadioSelect(interior,
            buttontype = 'radiobutton',
            labelpos='w',
            label_text='Element',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge',
            )
        self.stats_dialog.element.pack(side='top', fill='x', 
            expand='yes', padx=5)
        for btn in ['vis', 'cig', 'joint']:
            self.stats_dialog.element.add(btn)
        self.stats_dialog.element.setvalue('vis')

        self.stats_dialog.columns = Pmw.RadioSelect(interior,
            buttontype = 'checkbutton',
            labelpos='w',
            label_text='Columns',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge',
            selectmode='multiple',
            )
        self.stats_dialog.columns.pack(side='top', fill='x', 
            expand='yes', padx=5)
        for btn in ['month', 'hour', 'wdir']:
            self.stats_dialog.columns.add(btn)
        self.stats_dialog.columns.setvalue(['month'])
        self.stats_dialog.file = Pmw.EntryField(interior,
            labelpos='w',
            label_text='File',
            entry_width=12,
            value='tmp/stats.txt',
            )
        self.stats_dialog.file.pack(side='top', fill='x', 
            expand='yes', padx=5)

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

    def make_list(self,count):
        """Flattens count to a list of tuples (ixlist, category-count)"""
        result = []
        n = len(count.shape)
        if n == 1:
            result = [((), count.tolist())]
        elif n == 2:
            for i in range(count.shape[0]):
                result.append(((i,), count[i,:].tolist()))
        elif n == 3:
            for i in range(count.shape[0]):
                for j in range(count.shape[1]):
                    result.append(((i, j), count[i,j,:].tolist()))
        elif n == 4:
            for i in range(count.shape[0]):
                for j in range(count.shape[1]):
                    for k in range(count.shape[2]):
                        result.append(((i, j, k), count[i,j,k,:].tolist()))
        else:
            raise ValueError('Invalid shape for count')
        
        return result

    def combine_count(self):
        """Combines 4-dimensional dictionary along items"""
        month_ix, hour_ix, wind_ix = range(3)
        
        tc = self.data[self.stats_dialog.element.getvalue()]
        items=self.stats_dialog.columns.getvalue()
        
        if 'month' not in items:
            tc = na.sum(tc, month_ix)
            hour_ix -= 1
            wind_ix -= 1
        if 'hour' not in items:
            tc = na.sum(tc, hour_ix)
            wind_ix -= 1
        if 'wdir' not in items:
            tc = na.sum(tc, wind_ix)
        return tc

    def show_stats_dialog(self):
        if self.stats_dialog.winfo_ismapped():
            return
        #
        # Set 'Save Stats' element & columns to the parent selection
        self.stats_dialog.element.setvalue(self.element_w.getvalue())
        self.stats_dialog.columns.setvalue([self.current_plot])
        
        try:
            self.stats_dialog.file.setvalue('tmp/%s-%s-%s.txt' % \
                                        (self.id_.strip(),
                                         self.element_w.getvalue(),
                                         self.current_plot))
        except AttributeError:
            msg = 'Press Draw first'
            Busy.showerror(msg, self.interior())
            return
            
        self.set_geometry(self.stats_dialog)
        self.stats_dialog.transient(self.interior())
        self.stats_dialog.show()

    def print_(self, action):
        # print image 
        if action == 'OK' and self.data is not None and \
               self.print_dialog.command.valid():
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
            canvas = self.graphics[self.current_plot]['canvas'].get_tk_widget()
            chldin.write(canvas.postscript(colormode=mode))
            chldin.close()
            msg = chldout.read()
            if msg:
                Busy.showinfo(msg, self.interior())
        self.print_dialog.withdraw()
        self.print_dialog.deactivate()

    def save_stats(self, action):
        # save stats to a file
        if action == 'Close':
            self.stats_dialog.withdraw()
            self.stats_dialog.deactivate()
        else:
            if self.data is None:
                return
            
            element = self.stats_dialog.element.getvalue()
            columns = self.stats_dialog.columns.getvalue()
            path = self.stats_dialog.file.getvalue()
            
	    fh = open(path,'a')
            if not columns:
                msg = 'Select at least one column'
                Busy.showerror(msg, self.interior())
                return
            
            output = self.make_list(self.combine_count())
	    col_labels = self.RowLabels[:]
	    col_labels.reverse()

	    elements = {'vis':'Visibility', 'cig':'Ceiling','joint':'Joint'}
	    hours = self.get_climate_hour_range()
            
	    if 'wdir' in columns:
                months = self.get_month_range()
		num_cat = len(ClimLib.FlightCats)
		dir_labels = ['N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', \
		    'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'VRB', 'C']
		fmtw0 = '%4s ' + '(%4d-%4d)' 
		fmtw4 = 'MONTH: %-5s '
                fmtw1 = 'HOUR: %02dZ' + ' '*9 + '%10s'
		fmtw2 = '%-5s ' + '%-7s '*5 
		fmtw3 = '%-5s ' + '%-7.2f '*5
		for month in months:
		    for hour in hours:
			print >> fh, fmtw0 % (self.id_.strip(),self.data['years'][0],self.data['years'][1])
			print >> fh, fmtw4 % (self.Month[month])
			print >> fh, fmtw1 % (hour, elements[element])
			print >> fh, fmtw2 % tuple(['DIR'] + col_labels[:] + ['VFR'])
			tmp = na.sum(na.sum(na.take(na.take(self.data[element], [month], 0), [hour], 1), 0), 0)
			total = na.sum(tmp, -1)
			wind_hours = []
			wind_hours.append(dir_labels)
			for row in range(num_cat):
                            yvals = tmp[:,row]
                            yvals[na.ieeespecial.isnan(yvals)] = 0.0
			    wind_hours.append(yvals)
			wind_hours.append(total)
			for row in zip(*wind_hours):
			    print >> fh, fmtw3 % row
                            
                    print >> fh, ' '
                    
		self.messagebar.message('userevent','Stats saved to ' + path)
		self.stats_dialog.withdraw()
		self.stats_dialog.deactivate()
		return


	    fmt0 = '%4s ' + '(%4d-%4d)'
	    fmt1 = 'HOUR: %02dZ' + ' '*9 + '%10s'
            fmt2 = '%-5s ' + '%-7s '*5
	    fmt3 = '%-5s ' + '%-7.2f '*5

	    print >> fh, fmt0 % (self.id_,self.data['years'][0],self.data['years'][1])

	    for hour in hours:
		print >> fh, fmt1 % (hour, elements[element])
	  	print >> fh, fmt2 % tuple(['MONTH'] + col_labels[:] + ['VFR'])
	        mth_ctr = 0
		for row in na.sum(na.sum(na.take(self.data[element], [hour], 1), 1), 1):
		    try:
			print >> fh, fmt3 % tuple([self.Month[mth_ctr]] + [x for x in row])
			mth_ctr = mth_ctr + 1
		    except Exception, e:
			print self.Month[mth_ctr]
			print row[:]
			print e
                print >> fh, ' '

	    fh.close()
	    self.messagebar.message('userevent','Stats saved to ' + path)
            self.stats_dialog.withdraw()
            self.stats_dialog.deactivate()

    def get_climate_hour_range(self):
	if 'hour' in self.stats_dialog.columns.getvalue():
            return range(24)
        
        sel = self.graphics[self.current_plot]['selection']
	return [int(sel['hour'].get())] 

    def get_hour_range(self):
        # get hour selection range from widget
        sel = self.graphics[self.current_plot]['selection']
	try:
	    start_hour = int(sel['hour'].get())
	except Exception, e:
	    return range(24)
        end_hour = (start_hour+int(sel['hrange'].get()))%24
        if start_hour < end_hour:
            return range(start_hour, end_hour)
        else:
            return range(start_hour, 24) + range(end_hour)

    def get_month_range(self):
        # get hour selection range from widget
        sel = self.graphics[self.current_plot]['selection']
	try:
	    start_month = int(sel['month'].get())-1
	except Exception, e:
	    return range(12)
        end_month = (start_month+int(sel['mrange'].get()))%12
        if start_month < end_month:
            return range(start_month, end_month)
        else:
            return range(start_month, 12) + range(end_month)

    def draw(self, id_):
        if self.data is None:
            return
        self.id_ = id_
        if self.current_plot == 'month':
            self.draw_month()
        elif self.current_plot == 'hour':
            self.draw_hour()
        elif self.current_plot == 'wdir':
            self.draw_wdir()

    def wait_for_data(self):
        try:
            i, (id_, data) = self.resultqueue.get_nowait()
        except Queue.Empty:
            self.after(100, self.wait_for_data)
            return
        if 'error' in data:
            self.messagebar.message('systemerror', data['error'])
            self.data = None
        else:
            self.messagebar.resetmessages('systemerror')
            self.data = data
            self.cache.put(id_, self.Key, data) 
            self.messagebar.message('userevent', 'Done')
        Busy.Manager.notbusy()
        self.draw(id_)

    def get_data(self, id_):
        prog = os.path.join('bin', 'avnstart.sh')
        command = '%s %s %s' % (prog, self.GetData, self.ids[id_]['file'])
        try:
            chldin, chldout = os.popen2(command, -1)
            chldin.close()
            return id_, cPickle.load(chldout)
        except Exception:
            return id_, {'error': 'Failed to retrieve data'}

    def display(self, cb=None):
        # get and display data
        try:
            id_ = self.station_w.getcurselection()[0]
        except IndexError:
            Busy.showerror('Select site id', self.interior())
            return
        if self.id_ != id_:
            # get mtime for climate file, redo cache if needed
            self.data = self.cache.get(id_, self.Key, self.ids[id_]['file'])
            if self.data is None:
                msg = 'Retrieving data for %s, this will take a while' % id_
                self.messagebar.message('userevent', msg)
                Busy.Manager.busy()
                self.update_idletasks()
                self.worker.performWork(self.get_data, id_)
                self.wait_for_data()
                return
        self.draw(id_)

    def update(self):
        if self._tkUpdate.get():
            self.display()

    def set_yticks(self, elem, item):
        if self.tkAutoScale.get():
            if item in ['month', 'hour']:
                # sum over wind directions and categories
                total = na.sum(na.sum(self.data[elem], 3), 2)
                # fudge columns with no events
                total[na.ieeespecial.index(total, na.ieeespecial.ZERO)] = 1.0
                ymax = 100.0*(na.sum(na.sum(self.data[elem][:,:,:,:-1], 3), 
                    2)/total).max()
            elif item in ['wdir']:
                # sum categories
                total = na.sum(self.data[elem], 3)
                # fudge columns with no events
                total[na.ieeespecial.index(total, na.ieeespecial.ZERO)] = 1.0
                ymax = 100.0*(na.sum(self.data[elem][:,:,:,:-1], 3)/total).max()
            else:
                raise Avn.AvnError('Bug in set_yticks(): unexpected item: %s' % \
                    item)
            self.tkScale.set(ymax)
        else:
            ymax = int(self.tkScale.get())
        if ymax > 30:
            delta = 10
        elif ymax > 10:
            delta = 5
        else:
            delta = 2
        return ymax+delta, delta

    def draw_hour(self):
        months = self.get_month_range()
        elem = self.element_w.getvalue()
        Busy.Manager.busy()
        self.update_idletasks()
        g = self.graphics['hour']
        # clear display
        g['ax'].cla()
        # title
        if len(months) == 1:
            title = '%s %s %s (%d-%d)' % ((self.id_, \
                self.Element.get(elem, ''), self.Month[months[0]]) + \
                tuple(self.data['years']))
        else:
            title = '%s %s %s-%s (%d-%d)' % ((self.id_, \
                self.Element.get(elem, ''), self.Month[months[0]], \
                self.Month[months[-1]]) + tuple(self.data['years']))
        g['ax'].set_title(title)
        cell_text = []
        col_labels = ['%02d' % x for x in range(24)]
        xvals = na.arange(24) + 0.2
        # the bottom values for stacked bar chart
        yoff = na.zeros(len(col_labels), na.Float32) 
        num_cat = len(ClimLib.FlightCats)
        bar = [None]*num_cat
        widths = [0.6]*24
        # stacked bars
        g['ax'].xaxis.set_major_locator(multipleLocator)
        # sum over selected range of months and wind directions
        tmp = na.sum(na.sum(na.take(self.data[elem], months, 0), 0), 1)
        # sum over all categories
        total = na.sum(tmp, -1)
        for row in range(num_cat):
            yvals = 100.0*tmp[:,row]/total
            bar[row] = g['ax'].bar(xvals, yvals, widths, bottom=yoff, 
                color=self.colors[num_cat-row-1])
            yoff += yvals
            cell_row = ['%.0f' % yvals[n] for n in range(24)]
            cell_text.append(cell_row)
        cell_text.reverse()
        g['ax'].table(cellText=cell_text, rowLabels=self.RowLabels,
            rowColours=self.colors, colLabels=col_labels, 
            loc='bottom')
        # legend
        legend_bars = [x[0] for x in bar]
        legend_bars.reverse()
        g['ax'].legend(legend_bars, self.RowLabels)
        # axes
        g['ax'].set_ylabel('Percent occurrence')
        g['ax'].set_xticks([])
        ymax, delta = self.set_yticks(elem, 'hour')
        g['ax'].set_yticks(na.arange(0, ymax, delta))
        g['ax'].grid(True)
        g['canvas'].draw()
        Busy.Manager.notbusy()

    def draw_month(self):
        hours = self.get_hour_range()
        elem = self.element_w.getvalue()
        Busy.Manager.busy()
        self.update_idletasks()
        g = self.graphics['month']
        # clear display
        g['ax'].cla()
        g['ax'].xaxis.set_major_locator(multipleLocator)
        # title
        if len(hours) == 1:
            title = '%s %s %02dZ (%d-%d)' % ((self.id_, \
                self.Element.get(elem, ''), hours[0]) + \
                tuple(self.data['years']))
        else:
            title = '%s %s %02d-%02dZ (%d-%d)' % ((self.id_, \
                self.Element.get(elem, ''), hours[0], hours[-1]) + \
                tuple(self.data['years']))
        g['ax'].set_title(title)
        cell_text = []
        col_labels = self.Month[:]
        col_labels.append('Annual')
        xvals = list(na.arange(len(self.Month)) + 0.2)
        xvals.append(xvals[-1]+1.2)
        # the bottom values for stacked bar chart
        yoff = na.array([0.0] * len(col_labels)) 
        num_cat = len(ClimLib.FlightCats)
        bar = [None]*num_cat
        widths = [0.6]*12 + [1.2]   # 0.6 for monthly, 1.2 for annual
        # stacked bars
        # sum over selected range of hours and wind directions
        tmp = na.sum(na.sum(na.take(self.data[elem], hours, 1), 1), 1)
        # sum over all categories
        total = na.sum(tmp, -1)
        total_sum = total.sum()
        for row in range(num_cat):
            yvals = 100.0*tmp[:,row]/total
            yvals.resize(13)
            yvals[12] = 100.0*tmp[:,row].sum()/total_sum
            bar[row] = g['ax'].bar(xvals, yvals, widths, bottom=yoff, 
                color=self.colors[num_cat-row-1])
            yoff += yvals
            cell_row = ['%.0f' % yvals[n] for n in range(13)]
            cell_text.append(cell_row)
        cell_text.reverse()
        widths = [1.0/14]*12+[1.0/7.0]
        g['ax'].table(cellText=cell_text, rowLabels=self.RowLabels,
            rowColours=self.colors, colLabels=col_labels, colWidths=widths, 
            loc='bottom')
        # legend
        legend_bars = [x[0] for x in bar]
        legend_bars.reverse()
        g['ax'].legend(legend_bars, self.RowLabels)
        # axes
        g['ax'].set_ylabel('Percent occurrence')
        g['ax'].set_xticks([])
        ymax, delta = self.set_yticks(elem, 'month')
        g['ax'].set_yticks(na.arange(0, ymax, delta))
        g['ax'].grid(True)
        g['canvas'].draw()
        Busy.Manager.notbusy()

    def draw_wdir(self):
        months = self.get_month_range()
        hours = self.get_hour_range()
        elem = self.element_w.getvalue()
        Busy.Manager.busy()
        self.update_idletasks()
        g = self.graphics['wdir']
        # clear display
        g['ax'].cla()
        # title
        if len(months) == 1:
            if len(hours) == 1:
                title = '%s %s %02dZ %s (%d-%d)' % ((self.id_, \
                    self.Element.get(elem, ''), hours[0], \
                    self.Month[months[0]]) + tuple(self.data['years']))
            else:
                title = '%s %s  %02d-%02dZ %s (%d-%d)' % ((self.id_, \
                    self.Element.get(elem, ''), hours[0], hours[-1], \
                    self.Month[months[0]]) + tuple(self.data['years']))
        else:
            if len(hours) == 1:
                title = '%s %s %02dZ %s-%s (%d-%d)' % ((self.id_, \
                    self.Element.get(elem, ''), hours[0], self.Month[months[0]], \
                    self.Month[months[-1]]) + tuple(self.data['years']))
            else:
                title = '%s %s %02d-%02dZ %s-%s (%d-%d)' % ((self.id_, \
                    self.Element.get(elem, ''), hours[0], hours[-1], \
                    self.Month[months[0]], self.Month[months[-1]]) + \
                    tuple(self.data['years']))
        g['ax'].set_title(title)
        cell_text = []
        col_labels = ['N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', \
            'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'VRB', 'C']
        num_dir = len(col_labels)
        xvals = na.arange(num_dir) + 0.2
        # the bottom values for stacked bar chart
        yoff = na.zeros(num_dir, na.Float32) 
        num_cat = len(ClimLib.FlightCats)
        bar = [None]*num_cat
        widths = [0.6]*num_dir
        # stacked bars
        g['ax'].xaxis.set_major_locator(multipleLocator)
        # sum over selected range of months, hours
        tmp = na.sum(na.sum(na.take(na.take(self.data[elem], months, 0), 
            hours, 1), 0), 0)
        # sum over all categories
        total = na.sum(tmp, -1)
        for row in range(num_cat):
            yvals = 100.0*tmp[:,row]/total
            yvals[na.ieeespecial.isnan(yvals)] = 0.0
            bar[row] = g['ax'].bar(xvals, yvals, widths, bottom=yoff, 
                color=self.colors[num_cat-row-1])
            yoff += yvals
            cell_row = ['%.0f' % yvals[n] for n in range(num_dir)]
            cell_text.append(cell_row)
        cell_text.reverse()
        cell_row = ['%.0f' % total[n] for n in range(num_dir)]
        cell_text.append(cell_row)
        row_labels = self.RowLabels[:]
        row_labels.append('HOURS')
        row_colors = self.colors[:]
        row_colors.append('white')
        g['ax'].table(cellText=cell_text, rowLabels=row_labels,
            rowColours=row_colors, colLabels=col_labels, loc='bottom')
        # legend
        legend_bars = [x[0] for x in bar]
        legend_bars.reverse()
        g['ax'].legend(legend_bars, self.RowLabels)
        # axes
        g['ax'].set_ylabel('Percent occurrence')
        g['ax'].set_xticks([])
        ymax, delta = self.set_yticks(elem, 'wdir')
        g['ax'].set_yticks(na.arange(0, ymax, delta))
        g['ax'].grid(True)
        g['canvas'].draw()
        Busy.Manager.notbusy()

    def run(self):
        self.pack()
        self.root.deiconify()
        self.mainloop()
