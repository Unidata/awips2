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
import logging, os, re, time
import ConfigParser
import tables
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, Busy, ClimateProcessLogger
import ClimateDataUtils as cdutils

_Help = {
    'title': 'Climate Data Site Creation Help',
    'content': """
Some Future Stuff
"""
}

tmp_dir = 'tmp/'

_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)


##########################################################################################
#
#
class Resume(AvnDialog.Dialog):
    def __init__(self, parent=None, top=None, **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	self.top = top

	# from AvnDialog
	self.title('Climate Resume')

	self.createWidgets()

    def createWidgets(self):
	frame = Frame(self.interior())

	self.idents = Pmw.ScrolledListBox(frame,
            label_text='Idents',
            labelpos='n',
            listbox_width=8,
            listbox_exportselection=0,
	    listbox_selectmode=SINGLE,
            )
	self.idents.grid(row=0, column=0, sticky='news')
	self.__setIdents()

	bbox = Pmw.ButtonBox(frame,
	    orient='vertical'
	)
	btn = bbox.add('Resume',command=self.__checkStates)
	btn = bbox.add('Cancel',command=self.close)
	bbox.grid(row=0, column=1, sticky='news')

	frame.pack(side='left', expand='yes')

    def __setIdents(self):
	idents = [f[:4] for f in os.listdir(tmp_dir) if re.match('[PKT][A-Z0-9]{3}.hd5',f)]
	idents.sort()
	self.idents.setlist(idents)

    def __checkStates(self):
	# have we selected a station?
	if not self.idents.getvalue(): return
	for x in range(4,0,-1):
	    if self.__checkState(x): 
		self.__updateMain(x)
		break
	else:
	    #send message to message bar saying couldn't determine state and then close
	    self.close()
    
    def __updateMain(self,state):
	stn = self.idents.getvalue()[0]
	self.__updateMonitor(self.__messages(state))
	self.top.listIdents(stn)
	if state in [1,2]: 
	    self.top.start_btn.invoke()
	elif state == 3:
	    self.top.start_btn.configure(state=DISABLED)
	    self.top.validate(stn)
	elif state == 4:
	    self.top.start_btn.configure(state=DISABLED)
	    self.top.commit_btn.configure(state=ACTIVE)
	    self.top.reject_btn.configure(state=ACTIVE)
	self.close()

    def __messages(self, state):
	if state == 1:
	    msg = """
###################################################################
Processing left off just after the HDF file was initialized but
before any NCDC files had been downloaded. Processing will continue
at this point.
###################################################################
"""
	else: 
	    msg = ''
	return msg

    def __gethdfrows(self, fname):
        if os.path.exists(os.path.join('data/climate',fname)):
            import warnings
            warnings.simplefilter("ignore")
            fh = tables.openFile(os.path.join('data/climate',fname))
            warnings.simplefilter("default")
            arcrows = fh.root.obs.nrows
            fh.close()
        else:
            arcrows = None
        if os.path.exists(os.path.join('tmp',fname)):
            fh = tables.openFile(os.path.join('tmp',fname))
            tmprows = fh.root.obs.nrows
            fh.close()
        else:
            tmprows = None
        return arcrows, tmprows
	

    def __checkState(self, check):
	stn = self.idents.getvalue()[0]
	fname = stn + '.hd5'
	bak = '_' + stn + '.bak'
	if check == 4:
	    #original data file backed up and new file copied to data/climate for testing
	    if os.path.exists(os.path.join('data/climate',bak)):
		#file in data/climate is same size with same data as HDF file in tmp/
		arcrows, tmprows = self.__gethdfrows(fname)
		if arcrows == tmprows: return True
	    return False
	elif check == 3:
	    # HDF file has been created/updated but not yet moved to be tested
	    if not os.path.exists(os.path.join('data/climate',bak)):
		# now we'll check the number of obs in both files and if tmp is greater
		#  we'll assume tmp has been updated/created
		arcrows, tmprows = self.__gethdfrows(fname)
		if tmprows > arcrows: return True # if tmprows > arcrows, then tmp/ file has more obs
	    return False
	elif check == 2:
	    # HDF file is prepared (no HDF file in data/climate, or nrows for both files are the same
	    # NCDC Data files are downloaded into tmp/

	    # ok, let's see if we've already downloaded any data files
	    # if we don't have any files, then doing any more checking is moot, just return false
	    station_id = str(tables.openFile(os.path.join('data/climate',fname)).root.info[0]['station_id'])
	    files = [f for f in os.listdir(tmp_dir) if re.match(station_id,f)]
	    if not files: return False

	    # does the file exist in the archive? if yes, then does it have the same number of obs
	    #  as the file in tmp?
	    arcrows, tmprows = self.__gethdfrows(fname)
	    if os.path.exists(os.path.join('data/climate',fname)):
		if tmprows == arcrows: return True # if tmprows == arcrows, then assum tmp/ file is a copy of archive
	    else:
		# if the archive file doesn't exist and the tmp/ file doesn't have obs in it, 
		#  then assume tmp/ file is being created new
		if tmprows == 0: return True
	    return False
	elif check == 1:
	    # HDF file is prepared (no HDF file in data/climate, or nrows for both files are the same
	    # NCDC Data files are not downloaded into tmp/

	    # ok, let's see if we've already downloaded any data files
	    station_id = str(tables.openFile(os.path.join('data/climate',fname)).root.info[0]['station_id'])
	    files = [f for f in os.listdir(tmp_dir) if re.match(station_id,f)]

	    # does the file exist in the archive? if yes, then does it have the same number of obs
	    #  as the file in tmp?
	    arcrows, tmprows = self.__gethdfrows(fname)
	    if os.path.exists(os.path.join('data/climate',fname)):
		if tmprows == arcrows: # if tmprows == arcrows, then assum tmp/ file is a copy of archive
		    # if ncdc files haven't been downloaded, return True
		    if not files: return True
	    else:
		# if the archive file doesn't exist and the tmp/ file doesn't have obs in it, 
		#  then assume tmp/ file is being created new
		if tmprows == 0: 
		    # if ncdc files haven't been downloaded, return True
		    if not files: return True
	    return False

    def __updateMonitor(self, msg):
	self.top.monitor.configure(text_state='normal')
	time.sleep(0.1)
	self.top.monitor.appendtext(msg)
	self.top.monitor.configure(text_state='disabled')
