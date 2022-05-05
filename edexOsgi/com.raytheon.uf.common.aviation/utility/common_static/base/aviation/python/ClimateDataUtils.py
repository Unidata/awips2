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
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Jul 07, 2015    16907         zhao           Modified to work with new ids- files
#    Jun 12, 2019    7835          dgilling       Rewrite to use h5py instead of PyTables.
#    Aug  6, 2019    7878          tgurney        Python 3 fixes
#

##
# This is a base file that is not intended to be overridden.
##


import logging
import os

import h5py

ish_inv = 'isd-inventory.txt'
ish_his = 'isd-history.txt'
_Logger = logging.getLogger('CLIMATE')


def get_climo_years(climateFilePath, listener, stn='', lines=[]):
    # finds the climate years for a station (any year in the last 30)
    # returns a list with station usaf & wban id, start year, end year
    if not lines:
        return []

    lines.reverse()

    yrs = getStationYears(climateFilePath, listener, stn)

    if not yrs: #we have to draw all our info from the ncdc inventory
        yrs = [[-1,-1,-1,-1] + [-1,-1]] #a list of lists
        while lines:
            line = lines.pop(0)
            if yrs[-1][0] != line[0] or yrs[-1][1] != line[1]:
                yrs.append([line[0], line[1], line[2], line[2]] + ['-','-'])
            else:
                yrs[-1][2] = line[2]
        return yrs[1:]
    else: # we validate ncdc archive against local station_id
        while lines:
            if listener.isCanceled():
                raise KeyboardInterrupt
            line = lines.pop(0)
            for yr in yrs:
                if listener.isCanceled():
                    raise KeyboardInterrupt
                if yr[0] == line[0] and yr[1] == line[1]:
                    if yr[3] == '-':
                        yr[3] = line[2]
                    yr[2] = line[2]
                    break
            else:
                yrs.append([line[0], line[1], line[2], line[2]] + ['-','-'])
    return yrs


def getStationYears(climateFilePath, listener, stn=''):
    def wban_convert(wban=''):
        #some wban #'s are ##### while others are 0####
        # since they're stored incorrectly(as int32) in the hd5 files,
        # we have to adjust the wban id for that error
        if not wban:
            return ''
        if len(wban) == 5:
            return wban
        return '0' + wban

    if not stn:
        return []
    stn_years = []
    if os.path.exists(climateFilePath):
        with h5py.File(climateFilePath) as fh:
            info = fh['info']
            for rec in info:
                if listener.isCanceled():
                    raise KeyboardInterrupt
                station_id = str(rec['station_id'])
                wban_id = wban_convert(str(rec['wban_id']))
                if not stn_years or stn_years[-1][0] != station_id or stn_years[-1][1] != wban_id:
                    stn_years.append([station_id, wban_id, '-', '-', int(rec['year']), int(rec['year'])])
                else:
                    stn_years[-1][5] = int(rec['year'])
    else:
        stn_years = None

    return stn_years


def getIdnumsList(ishDirPath, climateFilePath, listener, stn=''):
    #returns a list of TAF site USAF ID #s given a TAF site ID
    if not stn:
        return []

    ids = getHistory(ishDirPath, listener, stn)

    lines = []
    with open(os.path.join(ishDirPath, ish_inv), 'r') as f:
        for line in f:
            if listener.isCanceled():
                raise KeyboardInterrupt
            if line[:12] in ids:
                lines.append([el for el in line.split(' ') if el != ''][:3])

    if len(lines) == 0:
        return None
    return get_climo_years(climateFilePath, listener, stn, lines)


def getHistory(ishDirPath, listener, stn=''):
    if not stn:
        return []
    rval = []
    with open(os.path.join(ishDirPath, ish_his), 'r') as f:
        for line in f.readlines():
            if listener.isCanceled():
                raise KeyboardInterrupt
            if stn == line[51:55] and line[:6] != '999999':
                rval.append(line[:12])
    return rval
