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
#   TODO: make the # climate years configurable
import ConfigParser
import logging, time, os

import sys
sys.argv = [__name__]

import tables

ish_inv = 'ish-inventory.txt'
ish_his = 'ish-history.txt'
_Logger=logging.getLogger('CLIMATE')

def get_climo_years(climateFilePath, stn='', lines = []):
    #finds the climate years for a station (any year in the last 30)
    #returns a list with station usaf & wban id, start year, end year
    if not lines: return lines

    curr_yr = time.gmtime()[0]
    lines.reverse()

    yrs = getStationYears(climateFilePath, stn)

    if not yrs: #we have to draw all our info from the ncdc inventory
	yrs = [[-1,-1,-1,-1] + [-1,-1]] #a list of lists
	while lines:
	    line = lines.pop(0)
	    if yrs[-1][0] != line[0] or yrs[-1][1] != line[1]:
		yrs.append([line[0], line[1], line[2], line[2]] + ['-','-'])
	    else:
		yrs[-1][2] = line[2]
	return yrs[1:]
    else: #we validate ncdc archive against local station_id
	while lines:
	    line = lines.pop(0)
	    for yr in yrs:
		if yr[0] == line[0] and yr[1] == line[1]:
		    if yr[3] == '-': 
			yr[3] = line[2]
		    yr[2] = line[2]
		    break
	    else:
		yrs.append([line[0], line[1], line[2], line[2]] + ['-','-'])
        
    return yrs

def getStationYears(climateFilePath, stn=''):
    def wban_convert(wban=''):
	#some wban #'s are ##### while others are 0####
	# since they're stored incorrectly(as int32) in the hd5 files, 
	# we have to adjust the wban id for that error
	if not wban: return ''
	if len(wban) == 5: return wban
	return '0' + wban

    if not stn: return []
    stn_years = []
    if os.path.exists(climateFilePath):
        import warnings
        warnings.simplefilter("ignore")
        fh = tables.openFile(climateFilePath)
        warnings.simplefilter("default")
        try:
            info = fh.root.info
            for x in range(info.nrows):
                station_id = str(info[x]['station_id'])
                wban_id = wban_convert(str(info[x]['wban_id']))
                if not stn_years or \
                stn_years[-1][0] != station_id or \
                stn_years[-1][1] != wban_id:
                    stn_years.append([station_id, \
                                      wban_id,	\
                                      '-', \
                                      '-', \
                                      int(info[x]['year']),  \
                                      int(info[x]['year'])])
                else:
                    stn_years[-1][5] = int(info[x]['year'])
        finally:
            fh.close()
    else:
        stn_years = None

    return stn_years 

def getIdnumsList(ishDirPath, climateFilePath, stn=''):
    
    #returns a list of TAF site USAF ID #s given a TAF site ID
    if not stn: return []
    
    ids = getHistory(ishDirPath, stn)

    
    fh = file(os.path.join(ishDirPath, ish_inv), 'r')
    lines = []

    for line in fh.readlines():
	if line[:12] in ids: 
	    lines.append([el for el in line.split(' ') if el != ''][:3])

    fh.close()
    if len(lines) == 0:
        return None
    
    return get_climo_years(climateFilePath, stn, lines) 

def getHistory(ishDirPath, stn=''):
    if stn == '': return []
    fh = open(os.path.join(ishDirPath, ish_his), 'r')
    return [line[:12] for line in fh.readlines() if stn == line[52:56] and line[:6] != '999999']
    
