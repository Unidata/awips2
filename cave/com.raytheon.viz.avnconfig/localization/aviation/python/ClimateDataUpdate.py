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
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Jul 07, 2015    16907         zhao           Modified to work with new ids- files 
#    Dec 22, 2015    18341         zhao           Modified __writeHDFData to avoid 'bad' input

import sys
sys.argv = [__name__]

import logging, os, time, stat, types, gzip, string, pdb
import cPickle as pickle
import ftplib
import tables, numpy
import ClimateParser, ClimateDataFilePrep
import ClimateDataUtils as cdutils
import threading
import ClimateProcessLogger

_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)

###########################################################
#
# Variables set for now, but to be replaced with cfg vars
# ..before integrating this program
#
ish_inv = 'isd-inventory.txt'
ftp_site = 'ftp.ncdc.noaa.gov'
ftp_dir = '/pub/data/noaa'
climo_dir = '/data/adapt/avnfps/climate'
tmp_dir = 'tmp'
YEAR_LIMIT = 30
DATA_LIMIT_YEAR = time.gmtime().tm_year - YEAR_LIMIT + 1
DATA_ALL_YEARS = 1900

# Missing data in HDF file
Missingfloat = 1.0e+15 - 1.0
Missingint8 = (1 << 7) - 1
Missinguint8 = (1 << 8) - 1
Missingint16 = (1 << 15) - 1
Missinguint16 = (1 << 16) - 1
Missingint32 = (1 << 30) - 1    # 31 is too big
Missinguint32 = (1 << 32) - 1

# Missing data flags from Parser (this needs changed)
FLT_FILL = 9.99999986991e+14
INT_FILL = 1073741823
STR_FILL = '\0'

# Type codes from the raw data for obs we don't want to use
reject_codes = ['FM-12', 'NSRDB']

###########################################################
#
# Object to hold various information about a climate data station
#
class ClimateStation:
    
    _start_year = DATA_LIMIT_YEAR
    _ending_datetime = 0
    fh = None
    
    def __init__(self, stn):
	self._id_list = []
	self._f_list = []
	self._stn = stn

    def append_id(self, id):
	if id in self._id_list: return
	self._id_list.append(id)

    def append_file(self, fname):
	self._f_list.append(fname)
    
    def set_ending_datetime(self, date_time):
	self._ending_datetime = date_time

    def set_start_year(self, year):
	self._start_year = year

    def get_stn(self):
	return self._stn

    def get_id_list(self):
	return self._id_list

    def sort_f_list(self):
	self._f_list = sorted(self._f_list, key=lambda x:(x[2], x[1]))

    def get_f_list(self):
	# get the most recent (at most) 30 years
	self.sort_f_list()
	return ['-'.join(f) + '.gz' for f in self._f_list if int(f[2]) >= DATA_LIMIT_YEAR]

    def get_is_large_data(self):
	return (len(self._f_list) > YEAR_LIMIT)

    def get_ending_datetime(self):
	return self._ending_datetime

    def get_start_year(self):
	return self._start_year

    def get_year_limits(self):
	self.sort_f_list()
	return (self._f_list[-YEAR_LIMIT][2], self._f_list[-1][2])

###########################################################
#
# Object containing methods to download and write data
#

LARGE_DATA_MSG = """
************************************************************
More than 30 years of data are available for %s. This 
version of AvnFPS cannot process more than 30 years of data.
Limiting data set to %s-%s.
************************************************************
"""

class ClimateDataUpdate():
    
    fh = None
    
#    def __init__(self, name, win=None):
#        threading.Thread.__init__(self, name=name)
    def __init__(self, queue, stnPickle = None, climateDir = None):
        self.queue = queue
        if climateDir :
            self.climateDir = climateDir
        if stnPickle : 
            self.stnLoad(stnPickle)
            return
        
        self.stns = {}
        self.append = True
        self.has_data = True
        self.bypass = False
#        self.win = win
        self.killed = False
#        self.fh = file(ish_inv,'rb')
        self.fh = file(os.path.join(self.climateDir, ish_inv), 'rb')

    def __del__(self):
        pass


    #def run(self):
    def assessData(self, append = None, bypass = None, sites = None):
        #Modify states for future loads
        if append: self.append = append
        if bypass: self.bypass = bypass
        if not sites:
             sites = self.sites
        else:
            self.sites = sites
            
	#time.sleep(0.5) # needed to sync with Resume functionality (don't know why)
	self.__overwriteMonitor(" ")
	if not sites: return
	if not self.bypass:
            if not self.__testDirs():
                self.kill()
                return
	    for site in sites:
		try:
		    stn, id, yrs = site.split('   ')[:3]
		    if not self.stns.has_key(stn):
			self.stns[stn] = ClimateStation(stn)
			self.__updateMonitor('Getting Station IDs for: ' + stn + '\n')
		    self.stns[stn].append_id(id.replace('-', ' '))
		except ValueError:
		    self.__updateMonitor('No Station IDs for: ' + site.split(' ')[0] + '\n')
		except Exception, e:
		    self.__updateMonitor(str(e) + '\n')

	    for stn in self.stns:
		self.__updateMonitor("Preparing " + stn + "\n")
		self.__prepHDFFiles(stn)
		self.__getStationDataFiles(stn)

	if not self.has_data:
	    msg = """
************************************************************
You will need to download NCDC data files to continue. Click
on the 'Generate Scripts' button and you will be prompted to
create and save either a UNIX or Windows compatible script
to obtain the data. Either script needs to be moved to, and
run on a machine with access to the open Internet to get the
required NCDC data.
************************************************************
"""
	    self.__updateMonitor(msg)
	    self.queue.put({'method': 'scriptsSetEnabled', 'value' : 'True'})

#        while not self.has_data and not self.bypass:
#            time.sleep(1) # allows thread to operate correctly
#            if self.killed: break
#            continue
#        else:
        if self.has_data or self.bypass:
	    self.__parseDownloadedData()
	    self.bypass = False
        
        # Save state.
        self.stnDump()
	return

    def __testDirs(self):
        state = True
        if not os.path.exists(self.climateDir) :
            self.__updateMonitor('Directory: "%s" does not exist' % self.climateDir)
            state = False
        elif not os.path.isdir(self.climateDir):
            self.__updateMonitor('Must be a directory: %s' % self.climateDir)
            state = False
        elif not os.access(self.climateDir, os.R_OK | os.W_OK):
            self.__updateMonitor('You do not have read and/or write permission for the directory:\n    %s' % self.climateDir)
            state = False
    
        tmp_dir = os.path.join(self.climateDir,'tmp')
        if not os.path.exists(tmp_dir):
            try:
                os.mkdir(tmp_dir)
                os.chmod(tmp_dir, stat.S_IRWXU | stat.S_IRWXG)
            except OSError, e:
                self.__updateMonitor('Unable to create directory: %s\n' % str(e))
                state = False
        elif not os.path.isdir(tmp_dir):
            self.__updateMonitor('Must be a directory: %s' % tmp_dir)
            state = False
        elif not os.access(tmp_dir, os.R_OK | os.W_OK) :
            self.__updateMonitor('You do not have read and/or write permission for the directory:\n    %s' % tmp_dir)
            state = False
        return state

    def __prepHDFFiles(self, stn):
	self.__updateMonitor('Preparing HDF file for: ' + stn + '\n')
	climo_file = os.path.join(self.climateDir, stn + '.hd5')
	tmp_dir = os.path.join(self.climateDir,'tmp')
	tmp_file = os.path.join(tmp_dir, stn + '.hd5')
    
	if os.path.exists(climo_file) and self.append:
	    # if the data/climate file exists, then use it, regardless of what's in tmp/
	    self.__updateMonitor('Copying %s to %s\n' % (climo_file, tmp_file))
	    if os.path.exists(tmp_file): os.unlink(tmp_file)
	    os.system('/bin/cp -f ' + climo_file + ' ' + tmp_file)
	    os.chmod(tmp_file, stat.S_IRUSR | stat.S_IWUSR | stat.S_IWGRP | stat.S_IRGRP | stat.S_IROTH)
	    self.stns[stn].fh = tables.openFile(tmp_file, 'a')
	    table = self.stns[stn].fh.root.obs
	    try:
		self.stns[stn].set_start_year(table[table.nrows - 1]['year'])
		self.stns[stn].set_ending_datetime(table[table.nrows - 1]['date_time'])
	    except IndexError:
		self.stns[stn].set_start_year(DATA_LIMIT_YEAR)
	    except Exception, e:
		self.__updateMonitor(str(e) + '\n')
	elif os.path.exists(tmp_file) and self.append:
	    os.chmod(tmp_file, stat.S_IRUSR | stat.S_IWUSR | stat.S_IWGRP | stat.S_IRGRP | stat.S_IROTH)
	    self.stns[stn].fh = tables.openFile(tmp_file, 'a')
	    try:
		table = self.stns[stn].fh.root.obs
	    except:
		# sometimes the file exists from a user trying to create it but not starting the process
		# only to want to come back and append to it later; this will deal with errors arising from
		# that situation
		os.unlink(tmp_file)
		os.system('/bin/cp -f ' + climo_file + ' ' + tmp_file)
		os.chmod(tmp_file, stat.S_IRUSR | stat.S_IWUSR | stat.S_IWGRP | stat.S_IRGRP | stat.S_IROTH)
		self.stns[stn].fh = tables.openFile(tmp_file, 'a')
		table = self.stns[stn].fh.root.obs
	    try:
		self.stns[stn].set_start_year(table[table.nrows - 1]['year'])
		self.stns[stn].set_ending_datetime(table[table.nrows - 1]['date_time'])
	    except IndexError:
		self.stns[stn].set_start_year(DATA_LIMIT_YEAR)
	    except Exception, e:
		self.__updateMonitor(str(e) + '\n')
	else:
	    if os.path.exists(tmp_file):
		os.unlink(tmp_file)
	    self.stns[stn].set_start_year(DATA_LIMIT_YEAR)
	    ClimateDataFilePrep.PrepFiles(stn, tmp_file)
	    os.chmod(tmp_file, stat.S_IRUSR | stat.S_IWUSR | stat.S_IWGRP | stat.S_IRGRP | stat.S_IROTH)
	    self.stns[stn].fh = tables.openFile(tmp_file, 'a')
	return

    def __getStationDataFiles(self, stn):
	self.__updateMonitor('Determining Data Files for: ' + stn + '\n')
	try:
	    for id in self.stns[stn].get_id_list():
		self.fh.seek(1)
		count = self.fh.read().count(id)
		self.fh.seek(1)
		self.fh.seek(self.fh.read().index(id) + 1)
		lines = self.fh.readlines()[:count]
		for id, wban, yr in [[el.strip() for el in line.split(' ') if el.strip() != ''][:3] \
			for line in lines]:
		    if int(yr) in self.__getDataYears(stn):
			fyear = [id, wban, yr]
			self.stns[stn].append_file(fyear)
			if not os.path.exists(os.path.join(self.climateDir, 'tmp', '-'.join(fyear) + '.gz')):
                            self.__updateMonitor('Missing file ' + os.path.join(self.climateDir, 'tmp', '-'.join(fyear) + '.gz'))
			    self.has_data = False
	    if self.stns[stn].get_is_large_data(): 
		limits = self.stns[stn].get_year_limits()
	        self.__updateMonitor(LARGE_DATA_MSG % (stn, limits[0], limits[1]))
	except Exception, e:
	    self.__updateMonitor(str(e) + '\n')

    def __getDataYears(self, stn):
	return [x for x in range(self.stns[stn].get_start_year(), time.gmtime().tm_year + 1)]

    def __parseDownloadedData(self):
	datadir = ''
	total_lines = 0
	cp = ClimateParser.Parser()
	for stn in self.stns:
	    #first pass to count the number of lines
	    fh = []
	    for file in self.stns[stn].get_f_list():
		dirfile = os.path.join(self.climateDir, 'tmp', file) #default directory and file
	        try:
		    fh.append(gzip.open(dirfile))
		except:
		    self.__updateMonitor("%s: File not found\n" % os.path.basename(dirfile))
		    continue
		try:
		    num_lines = len(fh[-1].readlines())
		    fh[-1].seek(0) # return the iterator to the top of the file
		    self.__updateMonitor("%s: %6d lines\n" % (os.path.basename(fh[-1].filename), num_lines))
		    total_lines += num_lines
		except:
		    self.__updateMonitor("%s: Error reading file\n" % os.path.basename(fh[-1].filename))
		    continue
		
	    self.__updateMonitor('Started: ' + time.strftime('%m/%d/%y %H:%M', time.gmtime()) + '\n')
	    self.__updateMonitor("Writing %d Lines \n" % total_lines)
	    #est_min = (total_lines/2000) + (total_lines/10000) #formula achieved by trial and error
	    est_min = total_lines / 2750
	    self.__updateMonitor("Estimated time to complete: %d minutes\n" % est_min)
	    start = time.time()
	    prev_lines = 0
	    for file in fh: #self.stns[stn].get_f_list():
		data = []
		#dirfile = os.path.join('tmp',file.filename) #default directory and file
		dirfile = file.filename
		if not os.path.exists(dirfile):
		    #if not datadir:
		#	datadir = self.win.getdir()
		#    dirfile = os.path.join(datadir,file)
		#    if not os.path.exists(dirfile):
		    self.__updateMonitor("Data file: %s does not exist\n" % os.path.basename(file.filename))
		    continue
		self.__updateMonitor("Processing %s...%s\n" % (stn, os.path.basename(file.filename)))
		for line in file:
		    try:
			data.append(cp.process_line(line))
		    except Exception, e:
			self.__updateMonitor("Error processing line: %s\n" % str(e))
			continue
		    if self.killed: return
		prev_lines = self.__writeHDFData(stn, data, total_lines, prev_lines)
	    end = time.time()
	    self.__updateMonitor("Actual time to complete: " + str(int(round((end - start) / 60))) + " minutes\n\n")
	msg = '100% complete'
	self.__overwriteMonitor(msg)
	self.__updateMonitor("done" + "\n")
        self.queue.put({'method': 'validateSetEnabled', 'value' : 'True'})

    def __writeHDFData(self, stn, data, trows, prows):
	# drows: number of lines of data being written from a yearly NCDC data file
	# nrows: number of rows contained in the obs table of the climate file being written
	# prows: number of previously written rows(used only for appending)
	# trows: number of total lines of data being written from all NCDC data files
	info = self.stns[stn].fh.root.info
	obs = self.stns[stn].fh.root.obs
	nrows = obs.nrows
	if nrows == 0L:
	    yr = DATA_LIMIT_YEAR - 1
	else:
	    yr = info[info.nrows - 1]['year']
	drows = len(data)
	old_time = 0L
	while data:
	    if self.killed: break
	    if self.append:
		complete = (drows - len(data) + prows) * 100.0 / trows
	    else:
		complete = (nrows + drows - len(data)) * 100.0 / trows
	    if (len(data)) % 40 == 0:
	        msg = '%6.2f%% complete' % complete
		self.__overwriteMonitor(msg)
	    dataline = data.pop(0)
	    if old_time == 0L: old_time = dataline['md']['date_time']
	    if dataline['md']['type'] in reject_codes: continue
	    if dataline['md']['type'] == 'SY-MT' and dataline['md']['date_time'] <= old_time: continue
	    old_time = dataline['md']['date_time'] + (15 * 60)
	    if dataline['si']['year'] != yr:
                row = info.row
		for col in info.colnames:
		    row[col] = dataline['si'][col]
		row.append()
		info.flush()
		yr = dataline['si']['year']
	    if dataline['md']['date_time'] > self.stns[stn].get_ending_datetime():
		row = obs.row
		for col in obs.colnames:
		    try:
			f_col = obs.cols._f_col(col)
		    except Exception, e:
			return
		    if col in dataline['md'].keys():
			datum = dataline['md'][col]
		    elif col in dataline['ad'].keys():
			datum = dataline['ad'][col]
		    elif col == 'year':
			datum = time.gmtime(dataline['md']['date_time'] + 600.0).tm_year
		    elif col == 'hour':
			datum = time.gmtime(dataline['md']['date_time'] + 600.0).tm_hour
		    elif col == 'yday':
			datum = time.gmtime(dataline['md']['date_time'] + 600.0).tm_yday
		    else:
			datum = self.__get_msng(f_col.type)

		    try:
			#if f_col.shape == 1:
                        if f_col.descr._v_colObjects[col].shape == ():
			    datum = self.__convert2scalar(datum)
			    if datum in [FLT_FILL, INT_FILL, STR_FILL]:
				datum = self.__get_msng(f_col.type, True)
			    row[col] = datum
			else:
			    #shape = f_col.shape[0]-1
                            shape = f_col.descr._v_colObjects[col].shape[0] - 1
                            #datum = datum + [self.__get_msng(f_col.type)]*(f_col.shape[0]-len(datum))
			    datum = datum + [self.__get_msng(f_col.type)] * (f_col.descr._v_colObjects[col].shape[0] - len(datum))
                            if len(numpy.array(datum)) != len(row[col]):
                                continue
			    row[col] = numpy.array(datum).astype(f_col.type) 
		    except Exception, e:
			self.__updateMonitor(str(e) + '\n')
			continue
		try:
		    row.append()
		except Exception, e:
		    self.__updateMonitor(str(e) + '\n')
		    continue
	obs.flush()
	self.stns[stn].fh.flush()
	return prows + drows

    def __convert2scalar(self, datum):
	if isinstance(datum, types.ListType):
	    return datum[0]
	elif isinstance(datum, types.StringType):
	    return datum[:]
	return datum

    def __get_msng(self, type, scalar=False):
	type = str(type)
	if 'int' in type:
	    if scalar: return 0
	    return eval('Missing' + type)
	elif 'float' in type:
	    if scalar: return 0.0
	    return Missingfloat
	return ''

    def __updateMonitor(self, msg):
        #time.sleep(0.1)
        self.queue.put({'method': 'updateMonitor', 'msg': msg})

    def __overwriteMonitor(self, msg):
        #time.sleep(0.1)
        self.queue.put({'method': 'overwriteMonitor', 'msg': msg})
	#self.win.pct_complete.configure(text_state='normal')
	#time.sleep(0.1)
	#self.win.pct_complete.delete('1.0','1.17')
	#self.win.pct_complete.insert('1.0',msg)
	#"""
	#self.win.monitor.configure(text_state='normal')
	#time.sleep(0.1)
	#numlines = string.count(self.win.monitor.get(),"\n")
	#self.win.monitor.delete(str(numlines)+'.0',str(numlines)+'.17')
	#self.win.monitor.insert(str(numlines)+'.0',msg)
	#"""
	

    def validate(self, stn=''):
        self.queue.put({'method': 'validateSetEnabled', 'value' : 'False'})
        if not self.stns:
            self.stns = {stn: ClimateStation(stn)}
            self.__getStationDataFiles(stn)
        for stn in self.stns:
	    src = os.path.join(self.climateDir, 'tmp', stn + '.hd5')
	    dst = os.path.join(self.climateDir, stn + '.hd5')
	    bak = os.path.join(self.climateDir, stn + '.bak')
	    if os.path.exists(dst):
		self.__updateMonitor('Backing up...' + dst + '\n')
		os.rename(dst, bak)
	    os.system('/bin/cp ' + src + ' ' + dst)
	    self.__updateMonitor('Copying......' + src + ' to ' + dst + '\n')
	msg = """
************************************************************
The new HDF5 climate data file(s) have been copied into the
climate data directory. The old HDF5 file(s) are currently
saved with the extension "bak". Using any of the AvnFPS
climate tools under the "Tools" drop-down menu verify that
the new HDF5 file(s) are not corrupted and display the
latest year processed. Based on your results, press the
'Reject' or 'Commit' button.
************************************************************\n
"""
	self.__updateMonitor(msg)
        self.queue.put({'method': 'commitRejectSetEnabled', 'value' : 'True'})
        self.stnDump()
	return

    def commit(self):
        for stn in self.stns:
            src = os.path.join(self.climateDir, 'tmp', stn + '.hd5')
            bak = os.path.join(self.climateDir, stn + '.bak')
            if os.path.exists(bak):
                self.__updateMonitor('Deleting...' + bak + '\n')
                os.remove(bak)
            self.__updateMonitor('Deleting...' + src + '\n')
	    os.remove(src)
	    self.__updateMonitor('Deleting NCDC climate files\n')
            
            for f in self.stns[stn].get_f_list():
                ncdc_file = os.path.join(self.climateDir, 'tmp', f)
                if os.path.exists(ncdc_file):
                    os.remove(ncdc_file)

            # create .nc climate files
	    #self.win.make_qc(stn)
        #Handle all the stations in one call
        self.getStationsMap(False)
        self.queue.put({'method': 'make_qc'})

         
	self.__updateMonitor('Changes committed\n')
	self.queue.put({'method': 'commitRejectSetEnabled', 'value' : 'False'})
        self.kill()

    def reject(self):
        for stn in self.stns:
	    dst = os.path.join(self.climateDir, stn + '.hd5')
	    bak = os.path.join(self.climateDir, stn + '.bak')
	    if os.path.exists(dst):
                self.__updateMonitor('Deleting... ' + dst + '\n')
                os.remove(dst)
	    if os.path.exists(bak):
                self.__updateMonitor('Renaming... ' + bak + ' to ' + dst + '\n')
                os.rename(bak, dst)
            else:
                self.__updateMonitor('-----------------------')
                self.__updateMonitor('Unable to find %s cannot restore %s\n' % (bak, dst))
                self.__updateMonitor('-----------------------')
            self.__updateMonitor('Changes rejected\n')
            self.queue.put({'method': 'commitRejectSetEnabled', 'value' : 'False'})
	self.kill()

    def kill(self):
	self.killed = True
	for stn in self.stns:
	    try:
		filename = self.stns[stn].fh.filename
		self.stns[stn].fh.close()
		if not self.append: os.remove(filename)
	    except:
		continue
	self.__updateMonitor('\nProcessing Ended\n')
#	self.win.scripts_btn.configure(state=DISABLED)
#	self.win.continue_btn.configure(state=DISABLED)
        self.queue.put({'method' : 'scriptsSetEnabled', 'value' : "False"})
        self.queue.put({'method' : 'stnPickle', 'stnPickle' : None})

    def stnLoad(self, stnPickle):
        """Restore the stn state. Assumes stnPickle is a string generated by calling stnDump.
        """
        if not stnPickle : return
        o = pickle.loads(stnPickle)
        self.climateDir = o['climateDir']
        self.sites = o['sites']
        self.append = o['append']
        self.has_data = o['has_data']
        self.bypass = o['bypass']
        self.killed = o['killed']
        self.stns = o['stns']
        fhNames = o['fhNames']
        for stn in fhNames:
            try:
                self.stns[stn].fh = tables.openFile(fhNames[stn], 'a')
            except:
                pass
        try :
            self.fh = file(os.path.join(self.climateDir, ish_inv), 'rb')
        except :
            pass
        return

    def stnDump(self):
        """This method uses pickle to save current state information.
        This information is sent back to the java via the queue in order to allow the
        state to be restored
         """
        stnPickle = {}
        try :
            self.fh.close()
        except:
            pass
        self.fh = None
        stnPickle['climateDir'] = self.climateDir
        stnPickle['sites'] = self.sites
        stnPickle['append'] = self.append
        stnPickle['has_data'] = self.has_data
        stnPickle['bypass'] = self.bypass
        stnPickle['killed'] = self.killed
        
        fhNames = {}
        for stn in self.stns:
            try:
                fhNames[stn] = self.stns[stn].fh.filename
                self.stns[stn].fh.close()
            except:
                pass
            self.stns[stn].fh = None
        stnPickle['stns'] = self.stns
        stnPickle['fhNames'] = fhNames
        self.queue.put({'method': 'stnPickle', 'stnPickle': pickle.dumps(stnPickle)})
        return

    def getStationsMap(self, save=True):
        stnMap = {}
        for stn in self.stns:
            f_list = self.stns[stn].get_f_list()
            stnMap[stn] = f_list
        self.queue.put({'method': 'get_stations_map', 'map': stnMap})
        if save:
            self.stnDump()

if __name__ == '__main__':
    os.chdir('/home/gilmoredm/avnfps/')
    cd = ClimateDataUpdate("Climate Data Update Thread", None)
    cd.run()

