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
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#
import logging, numarray, os, pylab, tables
from Tkinter import *
import matplotlib.backends.backend_tkagg as mpltk
from matplotlib.ticker import FormatStrFormatter

import Avn, AvnDialog, Busy, ConfigParser, Pmw
import ClimateDataUtils as cdutils
from Balloon import Balloon

TopDir = os.environ.get('TOP_DIR', '/awips/adapt/avnfps/')
ish_inv = 'etc/ish-inventory.txt'

_FIG_HEIGHT = 4.0
_FIG_WIDTH = 6.5
_FIG_DPI = 90

_Help = {
    'title': 'Climate History Help',
    'content': """
Some Future Stuff
"""
}

_Logger = logging.getLogger(__name__)

##########################################################################################
#
#
class myScrolledListBox(Pmw.ScrolledListBox):
    def invoke(self):
	command = self['selectioncommand']
	if callable(command):
	    command()

##########################################################################################
#
#
class Details(AvnDialog.Dialog):
    def __init__(self, parent=None, top=None, **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	self.top = top

	# from AvnDialog
	self.title('Climate History')
        self.__createMenuBar()
	self.createWidgets()

    def __createMenuBar(self):
        self.menubar = Pmw.MenuBar(self.interior(),
                                   hull_relief='raised',
                                   hull_borderwidth=1,
                                   )
        self.menubar.pack(side='top', fill='x')
        self.menubar.addmenu('File', 'Main actions', side='left')
        self.menubar.addmenuitem('File', 'command',
                                 label = 'Close',
                                 command=self.close,
                                 )

    def createWidgets(self):
	frame = Frame(self.interior())

	self.idents = myScrolledListBox(frame,
            label_text='Idents',
            labelpos='n',
            listbox_width=8,
            listbox_exportselection=0,
	    listbox_selectmode=SINGLE,
            selectioncommand=self.yearCount,
            )
	self.idents.grid(row=0, column=0, sticky='news')

	#since sites can have multiple station IDs and WBAN IDs, we'll
	#create a notebook, with each page dedicated to a different station/WBAN combo
	self.notebook = Pmw.NoteBook(frame)
	self.notebook.grid(row=0, column=1, sticky='news')

	frame.pack(side='left', expand='yes')

    def setIdents(self):
	idents = [ident for ident in self.top.idents.getvalue()]
	self.idents.setlist(idents)
	if len(idents) == 1: 
	    self.idents.selection_set(0)
	    self.idents.invoke()

    def yearCount(self):
	Busy.Manager.busy()
	def get_year_count(stn=''):
	    #returns a list of TAF site USAF ID #s given a TAF site ID
	    if not stn: return []

	    ids = cdutils.getHistory(stn)

	    fh = file(ish_inv,'r')
	    lines = {}


	    for line in fh.readlines():
		if line[:12] in ids:
		    line_arr = [el.strip() for el in line.split(' ') if el != '']
		    if not lines.has_key((line_arr[0],line_arr[1])):
		        lines[(line_arr[0], line_arr[1])] = []
		    lines[(line_arr[0], line_arr[1])].append([line_arr[2], \
		        numarray.sum([int(x) for x in line_arr[3:14]])])
	    for key in lines:
		lines[key] = continuity_check(lines[key])
	    fh.close()
	    return lines

	def continuity_check(lines):
	    d = [lines.pop(0)] #pop off the first line into our holding list
	    while lines:
		e = lines.pop(0)
		#if the next year in lines is not consecutive,
		# we go through and add years with 0 obs until 
		# we reach the next year with more obs
		if int(e[0]) > int(d[-1][0])+1:
		    for x in range(int(d[-1][0])+1,int(e[0])):
			d.append([str(x),0])
		d.append(e)
	    return d
	    

	def get_min_data(d):
	    mind = d[0]
	    for x in d:
		if min(mind,x) == x: mind = x
	    return mind

	def get_max_data(d):
	    maxd = d[0]
	    for x in d:
		if max(maxd,x) == x: maxd = x
	    return maxd

	def config_labels(l):
	    l.reverse()
	    factor = len(l)/10
	    if factor < 1: factor = 1
	    for x in range(len(l)):
		if x % factor != 0: l[x] = ''
	    l.reverse()
	    return l

	pylab.cla()

	stn = self.idents.getcurselection()[0]
	year_count = get_year_count(stn)

	if not year_count: return

	#let's delete all pages from the notebook:
	for page in self.notebook.pagenames():
	    self.notebook.delete(page)

	for key in year_count:
	    #let's get the info we need from the data
	    labels = config_labels([x[0][2:] for x in year_count[key]])
	    data = [x[1] for x in year_count[key]]
	    mindata = get_min_data(data)
	    maxdata = get_max_data(data)

	    #now we create a page for the data graph
	    pagename = key[0] + '-' + key[1]
	    page = self.notebook.add(pagename, tab_text=pagename)

	    #now we need a figure and canvas for the graph
	    figure = pylab.figure(figsize=(_FIG_WIDTH, _FIG_HEIGHT), dpi=_FIG_DPI)
	    canvas = mpltk.FigureCanvasTkAgg(figure, master=page)
	    canvas.get_tk_widget().grid(row=0, column=0, sticky='news')

	    #finally we create the graph
	    xlocations = numarray.array(range(len(data)))+0.5
	    width = 0.5
	    pylab.bar(xlocations, data, width=width)
	    ystep = 1000
	    if maxdata < 1000: 
		ystep = 100
	    elif maxdata < 100:
		ystep = 10
	    pylab.gca().get_yaxis().set_major_formatter(FormatStrFormatter('%6d'))
	    pylab.yticks(range(mindata, maxdata, ystep))
	    pylab.xticks(xlocations+ width/2, labels)
	    pylab.xlim(0, xlocations[-1]+width*2)
	    pylab.title(year_count[key][0][0] + '-' + year_count[key][-1][0] + ' data for ' + stn)
	    pylab.xlabel('Year')
	    pylab.ylabel('Number of Observations')
	    pylab.gca().get_xaxis().tick_bottom()
	    pylab.gca().get_yaxis().tick_left()

	    canvas.draw()
	self.notebook.setnaturalsize()
	Busy.Manager.notbusy()
