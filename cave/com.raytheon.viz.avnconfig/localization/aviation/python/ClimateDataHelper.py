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
import logging, os, time
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, Busy

TopDir = os.environ['TOP_DIR']
ish_inv = 'etc/ish-inventory.txt'
ish_his = 'etc/ish-history.txt'

_Help = {
    'title': 'Climate Data Site Creation Help',
    'content': """
Some Future Stuff
"""
}

_Logger = logging.getLogger(__name__)

##########################################################################################
#
#
class Helper(AvnDialog.Dialog):
    def __init__(self, parent=None, top=None, **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	self.top = top

	# from AvnDialog
	self.title('NCDC Inventory/History')

	self.createWidgets()

    def createWidgets(self):
	frame = Frame(self.interior())

	inv = Pmw.Group(frame,
	    tag_pyclass = None)
	l = Label(inv.interior(), text='Name: ')
	l.grid(row=0, column=0, sticky='ne')
	self.inv_name = Label(inv.interior(), text='')
	self.inv_name.grid(row=0, column=1, sticky='nw')
	l = Label(inv.interior(), text='Size: ')
	l.grid(row=1, column=0, sticky='ne')
	self.inv_size = Label(inv.interior(), text='')
	self.inv_size.grid(row=1, column=1, sticky='nw')
	l = Label(inv.interior(), text='Last Mod: ')
	l.grid(row=2, column=0, sticky='ne')
	self.inv_last= Label(inv.interior(), text='')
	self.inv_last.grid(row=2, column=1, sticky='nw')
	inv.grid_rowconfigure(0, weight=1)
	inv.grid_rowconfigure(1, weight=1)
	inv.grid_rowconfigure(2, weight=1)
	inv.grid_rowconfigure(3, weight=1)
	btn = Button(inv.interior(), text='Generate Script', command=lambda: self.__scripts('inv'))
	btn.grid(row=3, column=0, columnspan=2, sticky='news')
	
	his = Pmw.Group(frame,
	    tag_pyclass = None)
	l = Label(his.interior(), text='Name: ')
	l.grid(row=0, column=0, sticky='ne')
	self.his_name = Label(his.interior(), text='')
	self.his_name.grid(row=0, column=1, sticky='nw')
	l = Label(his.interior(), text='Size: ')
	l.grid(row=1, column=0, sticky='ne')
	self.his_size = Label(his.interior(), text='')
	self.his_size.grid(row=1, column=1, sticky='nw')
	l = Label(his.interior(), text='Last Mod: ')
	l.grid(row=2, column=0, sticky='ne')
	self.his_last= Label(his.interior(), text='')
	self.his_last.grid(row=2, column=1, sticky='nw')
	his.grid_rowconfigure(0, weight=1)
	his.grid_rowconfigure(1, weight=1)
	his.grid_rowconfigure(2, weight=1)
	his.grid_rowconfigure(3, weight=1)
	self.__updateIsh()
	btn = Button(his.interior(), text='Generate Script', command=lambda: self.__scripts('his'))
	btn.grid(row=3, column=0, columnspan=2, sticky='news')
	
        inv.pack(fill = 'both', expand = 1, padx = 6, pady = 6)
        his.pack(fill = 'both', expand = 1, padx = 6, pady = 6)
	
	bbox = Pmw.ButtonBox(frame)
	bbox.add('Close', command=self.__close)
	bbox.pack(fill='both', expand=1)
	frame.pack(side='left', expand='yes')

	self.setGeometry()

	

    def __updateIsh(self):
	inv = {'name':ish_inv}
	his = {'name':ish_his}
	inv['mtime'] = os.path.getmtime(ish_inv)
	his['mtime']= os.path.getmtime(ish_his)
	inv['size'] = str(int(os.path.getsize(ish_inv))/1024/1000) + ' MB'
	his['size'] = str(int(os.path.getsize(ish_his))/1024/1000) + ' MB'

	self.inv_name.configure(text=inv['name'])
	self.inv_size.configure(text=inv['size'])
	self.inv_last.configure(text=time.ctime(inv['mtime']))
	self.his_name.configure(text=his['name'])
	self.his_size.configure(text=his['size'])
	self.his_last.configure(text=time.ctime(his['mtime']))

    def __scripts(self, style=None):
	if not hasattr(self, 'cdscripts'):
	    import ClimateDataScripts
	    self.cdscripts = ClimateDataScripts.Scripts(self.interior(),update=None,script_style=style)
	    self.cdscripts.setGeometry()
	self.cdscripts.script.clear()
	self.cdscripts.script_style = style #need to have it here and when the object is instantiated
	self.cdscripts.display()
	
    def __close(self):
	self.close()
