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
import logging, os, time, stat, types, sys, trace, atexit, signal, re
import ConfigParser
from Tkinter import *
import Pmw
import tkFileDialog
from Balloon import Balloon
import Avn, AppShell, AvnDialog, Busy, ErrorRedirect, ConfigParser
import ftplib, gzip
import ClimateDataUtils as cdutils
import ClimateDataUpdate as cdupdate
import threading
from HelpDialog import HelpDialog

Python = os.environ.get('PYTHON','/awips/adapt/avnfps/Python-2.4.1/bin/python')
TopDir = os.environ.get('TOP_DIR','/awips/adapt/avnfps/')

_Help = {
    'title': 'Climate Data Update Dialog Help',
    'content': """This dialog is used to create and append climatology data files.

Menu Bar
File:
    Quit:   Close the dialog immediately

Commands:
    Show observations history:	Opens a graphical display showing the current
				inventory of climate data available.
    Update NCDC "ish" files:	Opens a dialog allowing the user to generate
                                automated scripts for downloading the Integrated
                                Surface Hourly (ISH) Database.

Tools:
    Pulldown menu consist of the AvnFPS Climate Tools that read the HDF5 climate
    files.
    
Options:
    Append: Add new data to existing climate data file
    Create: Generate new climate data files, regardless of whether a file for
            that site already exists

Fields:
    SITE ID:	Site ID of the site currently selected, or 
		a user entered site ID if creating a new data file
    METAR AFOS ID:  The AFOS ID used to retrieve location's METAR product for
                    use in AvnFPS's climate Cig/Vis Trend tool.

Idents:
    List of current site IDs.

Site info list:
    List of IDs and years of data available for each selected site

Monitor area:
    Area where all informative messages are displayed.

Buttons:
    Assess Data:
	After sites are selected, click this to start the creation
        or append process
    Generate Scripts:
	Generate download scripts to retrieve data files from NCDC
    Process Data:
	Incorporate NCDC data into HDF5 file(s).
    Validate Data:
	Temporarily move newly changed/created files to a location
        so that AvnFPS climate tools can examine the new climate
        file.
    Commit:
	Move newly changed/created files to its permanent location. 
	Clicking this will also generate new station climate qc 
	files (files that end in .nc in	the data/climate directory)
    Reject:
	Reject the newly created files in favor of the original file(s),
        if available. This action deletes newly created files.
    Save Log:
	Save all output in the Monitor area to a file
"""
}

# this script will create the qc climate files for 
# each station whenever the changed file is 'committed'
# note that this file locates in $TOP_DIR/tmp and 
# deletes itself after running
make_qc_script="""#!/bin/csh
foreach site ( %s )
    foreach month ( 01 02 03 04 05 06 07 08 09 10 11 12 )
       $TOP_DIR/bin/avnstart.sh avnqcstats $site $month
    end
end
rm $0
"""

_Logger = logging.getLogger(__name__)

###############################################################################
def _exitfun():
    pass

##########################################################################################
#
# class extensions and overrides
#
class myScrolledListBox(Pmw.ScrolledListBox):
    def invoke(self):
	command = self['selectioncommand']
	if callable(command):
	    command()

class myRadioSelect(Pmw.RadioSelect):
    def disabled(self, disabled=ACTIVE):
	for x in range(self.numbuttons()):
	    self.button(x).configure(state=disabled)

##########################################################################################
#
#
class Editor(AppShell.AppShell):
    appname = AppShell.AppShell.appname + ' Climate Data Menu'
    resizewidth = 'true'
    resizeheight = 'true'
    IdsFile = os.path.join('etc', 'ids.cfg')

    append = True
    stns   = {}
    idnums = {}
    oldIdList = []

    def __getXResources(self):
        fname = os.path.join('etc', 'app-resources', 'X')
        if os.path.isfile(fname):
            self.option_readfile(fname)
        else:
            msg = 'Cannot access default resources file,\n' + \
                'using build-in values'
            _Logger.info(msg)
            Busy.showwarning(msg, self.root)	

    def initializeTk(self):
	pass

    def appInit(self, **kw):
	self.__getXResources()
	Pmw.Color.setscheme(self.root,
            background=self.option_get('background', ''))
        self.root.wm_iconbitmap(Avn.WatchBitmap)
        ErrorRedirect.fixlogging(_Logger, self.root)

    def appFinish(self):
        atexit.register(_exitfun)

    def __del__(self):
	if hasattr(self, "mythread"):
	    self.mythread.kill()
	    time.sleep(.1)
	    del self.mythread
	    time.sleep(.1)

    def destroy(self):
	self.__del__()

    def createInterface(self):
	frame = Frame(self.interior())
	topper = Frame(frame)
	action = Frame(frame)
	create = Frame(action,
		    borderwidth=2,
		    relief='ridge')

	self.rbsel = Pmw.RadioSelect(action,
	    buttontype='radiobutton',
	    labelpos='e',
	    hull_borderwidth=2,
	    hull_relief='ridge',
	    orient='vertical',
	    command=self.__setAppend,
	    )
	appendbtn=self.rbsel.add("Append")
	Balloon().bind(appendbtn,"Append data to existing HDF5 climate file")
	createbtn=self.rbsel.add("Create")
	Balloon().bind(createbtn,"Create new HDF5 climate file")
	self.rbsel.setvalue("Append")
	self.rbsel.grid(row=0, column=0, sticky='news', padx=0, pady=5)

	self.site_id = Pmw.EntryField(create,
	    labelpos='nw',
	    label_text='SITE ID:',
	    validate={'validator': 'alphanumeric',
                      'min': 4, 'max': 4, 'minstrict': 0},
	    entry_width=10,
	    )
	self.site_id.grid(row=0, column=0, sticky='news', padx=5, pady=0)
	self.site_id.component('entry').configure(state=DISABLED)

	self.pil = Pmw.EntryField(create,
	    labelpos='nw',
	    label_text='METAR AFOS ID:',
	    validate=self.__validateAFOS,
	    entry_width=10,
	    )
	self.pil.grid(row=1, column=0, sticky='news', padx=5, pady=0)
	self.pil.component('entry').configure(state=DISABLED)

	self.create_btn = Button(create, text='Write Site Data', command=self.__create)
	Balloon().bind(self.create_btn, 'Add new site to etc/ids.cfg file')
	self.create_btn.configure(state=DISABLED)
	self.create_btn.grid(row=2, column=0, padx=5, pady=5)

	create.grid_rowconfigure(0, weight=1)
	create.grid_rowconfigure(1, weight=1)
	create.grid_rowconfigure(2, weight=1)
	create.grid(row=1, column=0, sticky='news')

	action.grid_rowconfigure(0, weight=0)
	action.grid_rowconfigure(1, weight=0)
	action.grid_rowconfigure(2, weight=0)
	action.grid_rowconfigure(3, weight=1)
	action.grid(row=0, column=0, rowspan=2, sticky='news', padx=5, pady=5)

	self.idents = myScrolledListBox(topper,
            label_text='Idents',
            labelpos='n',
            listbox_width=6,
	    listbox_height=8,
            listbox_exportselection=0,
	    listbox_selectmode=MULTIPLE,
            selectioncommand=self.listIdnums,
            )
	self.idents.grid(row=0, column=0, rowspan=2)

        fixedFont = Pmw.logicalfont('Fixed')
	self.st = Pmw.ScrolledText(topper,
	    columnheader=1,
	    columnheader_height=36,
	    usehullsize=1,
	    hull_height=36,
	    hull_width=414,
            Header_font = fixedFont,
            Header_foreground = 'white',
	    )
	self.st.grid(row=0, column=1, sticky='news')

	self.sites = Pmw.ScrolledListBox(topper,
	    listbox_width=41,
	    listbox_height=6,
	    hscrollmode='none',
	    listbox_font=fixedFont,
            listbox_exportselection=0,
	    listbox_selectmode=MULTIPLE,
            )
	self.sites.grid(row=1, column=1, sticky='news')

	header = '                        Years in Archive:\n'
	header = header + 'Site   USAF-WBAN      NCDC	  Local '
	self.st.component('columnheader').insert(0.0, header)

	self.monitor = Pmw.ScrolledText(
			    topper,
			    vscrollmode='static',
			    hscrollmode='dynamic',
			    text_height=14, 
			    text_width=50, 
			    text_state='disabled',
			    text_wrap=WORD,
			    text_name='monitor')
	self.monitor.grid(row=2, column=0, columnspan=2, sticky='news')

	self.pct_complete = Pmw.ScrolledText(topper,
	    text_height = 1,
	    text_width = 50,
	    text_background = 'white',
	    text_foreground = 'blue',
	    )
	self.pct_complete.grid(row=3, column=0, columnspan=2, sticky='news')

	topper.grid_rowconfigure(0, weight=0)
	topper.grid_rowconfigure(1, weight=1)
	topper.grid_rowconfigure(2, weight=1)
	topper.grid_rowconfigure(3, weight=1)
	topper.grid(row=0, column=1)

	frame.grid_rowconfigure(0, weight=0)
	frame.grid_rowconfigure(1, weight=1)
	frame.pack(side='left', expand='yes', fill='both')

	bframe = Frame(frame,
	    relief=RIDGE,
	    borderwidth=2,
	    )
	bwidth=14
	self.start_btn = Button(bframe, text='Assess Data', command=self.__start, state=DISABLED, width=bwidth)
	self.start_btn.pack(padx=5, pady=6)
	Balloon().bind(self.start_btn, 'Start or re-start updating data for selected station(s)')

	self.scripts_btn = Button(bframe, text='Generate Scripts', command=self.__scripts, state=DISABLED, width=bwidth)
	self.scripts_btn.pack(padx=5, pady=6)
	Balloon().bind(self.scripts_btn, 'Create scripts to download NCDC data files')

	self.continue_btn = Button(bframe, text='Process Data', command=self.__continue, state=DISABLED, width=bwidth)
	self.continue_btn.pack(padx=5, pady=6)
	Balloon().bind(self.continue_btn, 'Process NCDC data for station(s)')

	self.validate_btn = Button(bframe, text='Validate', command=self.__validate, state=DISABLED, width=bwidth)
	self.validate_btn.pack(padx=5, pady=6)
	Balloon().bind(self.validate_btn, 'Check new HDF5 file(s)')

	self.commit_btn = Button(bframe, text='Commit', command=self.__commit, state=DISABLED, width=bwidth)
	self.commit_btn.pack(padx=5, pady=6)
	Balloon().bind(self.commit_btn, 'Keep new HDF5 climate file(s)')

	self.reject_btn = Button(bframe, text='Reject', command=self.__reject, state=DISABLED, width=bwidth)
	self.reject_btn.pack(padx=5, pady=6)
	Balloon().bind(self.reject_btn, 'Reject new HDF5 file(s) and restore previous')

	btn = Button(bframe, text='Save Log', command=self.__saveLog, width=bwidth)
	btn.pack(padx=5, pady=6)
	Balloon().bind(btn, 'Save a log for this update')

	bframe.grid(column=3, row=0, padx=5, pady=5, sticky='nwe')

	self.listIdents()
	self.initialiseoptions(Editor)

	#self.setGeometry()

    def createMenuBar(self):
	self.menubar.addmenu('Commands','Additional Commands')
	#self.menubar.addmenuitem('Commands', 'command',
	#    label='Resume Previous',
	#    command=self.__restore,
	#)
	self.menubar.addmenuitem('Commands', 'command',
	    #label='Show details of data from selected sites',
	    label='Show observations history',
	    command=self.__details
	)
	self.menubar.addmenuitem('Commands', 'command',
	    #label='Update history and archive list from NCDC',
	    label='Update NCDC "ish" Files',
	    command=self.__helper
	)

        self.menubar.addmenuitem('Help', 'command',
            label='About...',
            command=self.showAbout,
            )
	self.menubar.addmenuitem('Help', 'command',
	    label='Usage',
	    command=self.__showHelpDialog,
	    )

	self.menubar.addmenuitem('File', 'command',
            label='Quit',
            command=self.exit,
            )

	self.menubar.addmenu('Tools','Examine climate data files' )
	self.menubar.addmenuitem('Tools', 'command',
	    label='METARs',
	    command=Avn.curry(self.__climate, 'metars'),
	    )
	self.menubar.addmenuitem('Tools', 'command',
	    label='Wind Rose',
	    command=Avn.curry(self.__climate, 'wrose'),
	    )
	self.menubar.addmenuitem('Tools', 'command',
	    label='CigVis Dist',
	    command=Avn.curry(self.__climate, 'cvdist'),
	    )
	self.menubar.addmenuitem('Tools', 'command',
	    label='CigVis Trend',
	    command=Avn.curry(self.__climate, 'cvtrend'),
	    )

    def listIdents(self, stn=''):
        self.messagebar().resetmessages('systemerror')
	idents = cdutils.getIdentsList()
	if idents:
	    self.idents.setlist(idents)
	else:
	    self.idents.clear()
            msg = 'Error reading ids.cfg file'
            self.messagebar().message('usererror', msg)
	if stn:
	    idx = list(self.idents.get()).index(stn)
	    self.idents.yview_scroll(idx, UNITS)
	    self.idents.selection_set(idx)
	    self.idents.invoke()

    def listIdnums(self):
	Busy.Manager.busy(self.interior())
	siteid = None
	siteids = self.idents.getvalue()
	self.__latestId()
	for akey in [key for key in self.idnums.keys() if key not in siteids]:
	    del self.idnums[akey]
	for siteid in siteids:
	    if not self.idnums.has_key(siteid):
		self.idnums[siteid] = cdutils.getIdnumsList(siteid)
	if self.idnums:
	    self.start_btn.configure(state=ACTIVE)
	    numslist = []
	    idnums_keys = self.idnums.keys()
	    idnums_keys.sort()
	    for stn in idnums_keys:
		for idnum in self.idnums[stn]:
		    #numslist.append('%s   %s-%s   %s-%s   %s-%s' % tuple([stn] + idnum))
		    numslist.append('%-7s%-15s%-12s%-9s' % tuple([stn] + ["%s-%s" % tuple(idnum[:2])] + ["%s-%s" % tuple(idnum[2:4])] + ["%s-%s" % tuple(idnum[4:])]))
		if not self.idnums[stn]:
		    numslist.append('%s   -- NO INFO AVAILABLE --' % stn)
	    deselected_list = [self.sites.get(x) for x in range(self.sites.size()) \
		if not self.sites.selection_includes(x)]
	    self.sites.setlist(numslist)
	    [self.sites.selection_set(x) for x in range(len(numslist)) if numslist[x] not in deselected_list]
	else:
	    self.start_btn.configure(state=DISABLED)
	    self.sites.clear()
	    if siteid != None:
		msg = 'Cannot read ID numbers for %s' % siteid
		self.messagebar().message('usererror', msg)
	if siteids: self.setSiteCreateInfo()
	Busy.Manager.notbusy()

    def setSiteCreateInfo(self):
	if self.append: return # don't do this if we're appending to a site
	cp = ConfigParser.SafeConfigParser()
        cp.read(self.IdsFile)
	try:
	    if self.newId not in cp.sections(): return
	    self.site_id.setvalue(self.newId)
	    self.pil.setvalue(cp.get(self.newId, 'pil'))
	    self.create_btn.configure(state=DISABLED)
	except:
	    pass

    def getdir(self):
	return self.__getdir()

    def restore(self):
	self.__restore()

    def start(self):
	self.__start()

    def validate(self,stn):
	self.__validate(stn)

    def make_qc(self, stn):
	script = 'qcstats_%s.sh' % stn
	command = os.path.join(TopDir, 'tmp', script)
	fh = open(command, 'w')
	fh.write(make_qc_script % stn)
	fh.close()
	os.chmod(command, stat.S_IRWXU|stat.S_IRWXG)
	os.system("%s &" % command)

    def exit(self):
	self.__stop()
        if int(self.option_get('confirmClose', '')):
            if Busy.askokcancel('Do you really want to quit?', self.interior()):
                self.quit()
        else:
            self.quit()	

    def __latestId(self):
	idList = list(self.idents.getvalue())
	try:
	    self.newId = [id for id in idList if id not in self.oldIdList][0]
	except IndexError:
	    try:
		self.newId = idList[-1]
	    except:
		self.newId = ''
	self.oldIdList = idList

    def __details(self):
	if not hasattr(self, 'cddetails'):
	    import ClimateDataDetails
	    self.cddetails = ClimateDataDetails.Details(self.interior(), self)
	    self.cddetails.setGeometry()
	self.cddetails.setIdents()
	self.cddetails.display()

    def __getdir(self):
	return
	if not hasattr(self, 'cdgetdir'):
	    return tkFileDialog.askdirectory(initialdir='tmp')

    def __restore(self):
	if not hasattr(self, 'cdresume'):
	    import ClimateDataResume
	    self.cdresume = ClimateDataResume.Resume(self.interior(), self)
	    self.cdresume.setGeometry()
	self.cdresume.display()

    def __create(self):
	stn = self.site_id.getvalue()
	pil = self.pil.getvalue()
	file = os.path.join(TopDir, 'data/climate/%s.hd5' % stn)
	cdutils.updateIdsCfg(stn, pil, file)
	self.listIdents(stn)

    def __helper(self):
	if not hasattr(self, 'cdhelper'):
	    import ClimateDataHelper
	    self.cdhelper = ClimateDataHelper.Helper(self.interior(), self)
	    self.cdhelper.setGeometry()
	self.cdhelper.display()

    def __scripts(self):
	if not hasattr(self, 'cdscripts') and hasattr(self, 'mythread'):
	    import ClimateDataScripts
	    self.cdscripts = ClimateDataScripts.Scripts(self.interior(),update=self.mythread)
	    self.cdscripts.setGeometry()
	self.cdscripts.display()

    def __start(self):
	self.__stop()
	self.mythread = cdupdate.ClimateDataUpdate(name="Climate Data Update Thread", win=self)
	self.mythread.start()
	self.start_btn.configure(state=DISABLED)

    def __continue(self):
	if hasattr(self, 'mythread'):
	    self.mythread.bypass = True
	else:
	    self.__start()
	    self.mythread.bypass = True
	    self.continue_btn.configure(state=DISABLED)

    def __stop(self):
	if hasattr(self, 'mythread'):
	    self.mythread.kill()
	    time.sleep(.1)
	    del self.mythread
	    time.sleep(.1)
	self.validate_btn.configure(state=DISABLED)

    def __saveLog(self):
	t = time.strftime('%Y%m%d%H%M',time.gmtime())
	fh = tkFileDialog.asksaveasfile(initialfile='climatedata_' + t, defaultextension='.log', initialdir='tmp')
	if fh:
	    fh.write(self.monitor.getvalue())
	    fh.close()

    def __showHelpDialog(self):
        dialog = HelpDialog()
        if dialog.winfo_ismapped():
            return
        #self.setGeometry(dialog)
        dialog.display(_Help)

    def __validate(self,stn=''):
	if hasattr(self, 'mythread'):
	    self.mythread.validate()
	else:
	    if stn == '': return
	    self.mythread = cdupdate.ClimateDataUpdate(name="Climate Data Update Thread", win=self)
	    self.mythread.validate(stn)

    def __climate(self, choice):
	AvnStart = os.path.join(TopDir, 'bin', 'avnstart.sh')
	os.system('%s %s &' % (AvnStart, choice))

    def __showCondClimate(self):
        opts = '-f %s' %Globals.Forecaster
        os.spawnlp(os.P_NOWAIT,Python, Python, 
                os.path.join(TopDir, 'py','avnclimate.py'),
                opts)

    def __commit(self):
	if hasattr(self, 'mythread'):
	    self.mythread.commit()

    def __reject(self):
	if hasattr(self, 'mythread'):
	    self.mythread.reject()

    def __setAppend(self, tag):
	if tag == "Append": 
	    self.site_id.component('entry').configure(state=DISABLED)
	    self.pil.component('entry').configure(state=DISABLED)
	    self.create_btn.configure(state=DISABLED)
	    self.append=True
	    return
	self.site_id.component('entry').configure(state=NORMAL)
	self.site_id.clear()
	self.pil.component('entry').configure(state=NORMAL)
	self.pil.clear()
	self.create_btn.configure(state=NORMAL)
	self.append=False
	self.setSiteCreateInfo()

    def __validateAFOS(self,text):
        text = text.strip()
        length = len(text)
        if length < 8:
            return Pmw.PARTIAL
        value = text.upper()
        if re.match('^[A-Z]{3,3}MTR[(0-9)|(A-Z)]{2,3}$', value):
            return Pmw.OK
    
        if length == 9:
            return Pmw.ERROR
        else:
            return Pmw.PARTIAL
