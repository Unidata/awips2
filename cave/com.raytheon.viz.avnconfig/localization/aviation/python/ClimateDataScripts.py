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
import logging, os, time, stat, types
import ConfigParser
from Tkinter import *
import tkFileDialog
import Pmw
from Balloon import Balloon
import Avn, AvnDialog

ftp_site = 'ftp://ftp.ncdc.noaa.gov'
ftp_dir = '/pub/data/noaa/'

WIN = 0 #windows script type
LIN = 1 #linux script type

_Help = {
    'title': 'Climate Data Update Dialog Help',
    'content': """
Some Future Stuff
"""
}

_DataInstructions = """
==================================================
Please move these just-downloaded NCDC files to an
AWIPS machine into the %s/tmp
directory. When finished, press the 'Process Data'
button on the Climate Data Update GUI to resume
further processing of the data into a HDF5 climate
file.
==================================================
""" % os.environ.get('TOP_DIR', '/awips/adapt/avnfps/')

_EtcInstructions = """
==================================================
Please move this just-downloaded NCDC file to an
AWIPS machine into the %s/etc
directory.
==================================================
""" % os.environ.get('TOP_DIR', '/awips/adapt/avnfps/')

_Logger = logging.getLogger(__name__)


##########################################################################################
#
#
class Scripts(AvnDialog.Dialog):
    def __init__(self, parent=None, update=None, script_style='data', **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	# from AvnDialog
	self.title('Generate Scripts')

	self.script_style = script_style
	self.update = update

	self.createWidgets()


    def createWidgets(self):
	frame = Frame(self.interior())

	self.script = Pmw.ScrolledText(
			    frame,
			    vscrollmode='static',
			    hscrollmode='dynamic',
			    text_height=20, 
			    text_width=70, 
			    text_state='disabled',
			    text_wrap='none',
			    text_name='monitor')
	self.script.grid(row=2, column=0, columnspan=2, sticky='news')

	frame.grid_rowconfigure(0, weight=1)
	frame.pack(side='top', expand='yes')

	frame = Frame(self.interior())

	bbox = Pmw.ButtonBox(frame,
	    orient='horizontal',
	    hull_borderwidth=2,
	    hull_relief='groove',
	    pady=1,
	    )
	btn = bbox.add('Linux FTP Script', command=self.__linux_script)
	btn = bbox.add('Windows FTP Script', command=self.__windows_script)
	bbox.alignbuttons()

	bbox.pack(side='left', expand='yes', padx=5, pady=5)

	bbox = Pmw.ButtonBox(frame,
	    orient='horizontal',
	    hull_borderwidth=2,
	    hull_relief='groove',
	    pady=1,
	    )
	self.save_btn = bbox.add('Save', command=self.__save, state=DISABLED)
	btn = bbox.add('Bypass', command=self.__continue)
	if self.script_style in ['inv','his']: btn.configure(state=DISABLED)
	btn = bbox.add('Cancel', command=self.__cancel)
	bbox.pack(side='right', expand='no', padx=5, pady=5)

	frame.pack(side='left', expand='yes')
	
	self.initialiseoptions(Scripts)

	self.setGeometry()

    def __linux_script(self):
	self.script_type = LIN
	self.script.clear()
	self.script_msg = '#!/bin/bash\n\n'
	if self.script_style in ['inv','his']:
	    fname = 'ish-history.txt'
	    if self.script_style == 'inv':
		fname = 'ish-inventory.txt'
	    self.script_msg = self.script_msg + 'wget ' + ftp_site + ftp_dir + fname
	else:
	    stns = self.update.stns
	    if not stns: return
	    for stn in stns:
		for f in stns[stn].get_f_list(): 
		    self.script_msg = self.script_msg + 'wget ' + ftp_site + ftp_dir + self.__file_year(f) + '/' + f + '\n'
	self.__updateMonitor(self.script_msg)
	self.save_btn.configure(state=ACTIVE)

    def __windows_script(self):
	self.script_type = WIN
	self.script.clear()
	self.script_msg = """@ECHO OFF\r
>  script.ftp ECHO open ftp.ncdc.noaa.gov\r
>> script.ftp ECHO USER anonymous\r
>> script.ftp ECHO PASS daniel.gilmore@noaa.gov\r
>> script.ftp ECHO cd pub/data/noaa\r
>> script.ftp ECHO bin\r
"""
	if self.script_style in ['inv','his']:
	    fname = 'ish-history.txt'
	    if self.script_style == 'inv':
		fname = 'ish-inventory.txt'
	    self.script_msg = self.script_msg + '>> script.ftp ECHO get ' + fname + '\r\n'
	else:
	    stns = self.update.stns
	    if not stns: return
	    for stn in stns:
		for f in stns[stn].get_f_list():
		    self.script_msg = self.script_msg + '>> script.ftp ECHO get ' + self.__file_year(f) + '/' + f + '\r\n'
	self.script_msg = self.script_msg + '>> script.ftp ECHO BYE\r\n'
	self.script_msg = self.script_msg + 'FTP -v -n -s:script.ftp\r\n'
	self.script_msg = self.script_msg + 'DEL script.ftp\r\n'

	self.__updateMonitor(self.script_msg)
	self.save_btn.configure(state=ACTIVE)

    def __continue(self):
	msg = """
************************************
A script to download data was not
created. Please press 'Process Data'
button to continue.
************************************
"""
	#self.update.bypass = True
	self.__updateMonitor(msg)
	self.update.win.scripts_btn.configure(state=DISABLED)
	del self.update.win.cdscripts
	self.close()

    def __cancel(self):
	if self.script_style == 'data':
	    self.update.kill()
	    del self.update.win.cdscripts
	    self.update.win.start_btn.configure(state=ACTIVE)
	self.close()

    def __save(self):
        instructions = _EtcInstructions
        if self.script_style == 'data':
            instructions = _DataInstructions
        
        if self.script_type:
            ext = '.sh'
            self.script_msg += '\necho '.join(instructions.split('\n'))
        else:
            ext = '.bat'
            self.script_msg += '\r\nECHO '.join(instructions.split('\n'))
            self.script_msg += '\r\nPAUSE'

        if self.script_style == 'data':
            fname = 'getNCDCData-%s%s' % ('-'.join(self.update.stns),ext)
        else:
            fname = 'getNCDC-ISHFile%s' % ext
            
	fh = tkFileDialog.asksaveasfile(initialfile=fname, defaultextension=ext, initialdir='tmp')
	if fh:
	    fh.write(self.script_msg)
	    os.chmod(fh.name, stat.S_IRWXU|stat.S_IRWXG)
	    fh.close()
            
	if self.script_style == 'data':
	    del self.update.win.cdscripts
	    self.update.win.continue_btn.configure(state=ACTIVE)
            
	self.close()

    def __file_year(self,f):
	return f.split('-')[2][:4]

    def __updateMonitor(self, msg):
	self.script.configure(text_state='normal')
	time.sleep(0.1)
	self.script.appendtext(msg)
	self.script.configure(text_state='disabled')

