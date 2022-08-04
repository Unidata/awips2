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
#       cvdata.py
#       GFS1-NHD:A9172.0000-SCRIPT;1
#
#    Status:
#       DELIVERED
#
#    History:
#       Revision 1 (DELIVERED)
#         Created:  16-MAY-2006 16:04:23      TROJAN
#           spr 7144
#
#    Change Document History:
#       1:
#               Change Document:   GFS1-NHD_SPR_7144
#               Action Date:       14-FEB-2007 12:27:26
#               Relationship Type: Affected
#               Status:           CLOSED
#               Title:             AvnFPS: Incorrect method to specify filtering criteria in Cig/Vis Monthly tool
#
#
# cvdata.py
# Generates ceiling/visibility distribution by month, hour and wind direction
# George Trojan, SAIC/MDL, December 2005
# last update: 03/14/06
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Dec 22, 2015  18342    zhao      Modified _process() to also pass 'jnt_count'
# Aug 07, 2019  7878     tgurney   Updated for multithreading
# Jan 08, 2020  7878     tgurney   Improve error handling
# May 14, 2020  8067     randerso  Fix checks for Calm and Variable winds.
#
##
# This is a base file that is not intended to be overridden.
##

import logging
import os
import sys
import time

import numpy
import tables

import Avn
import ClimLib

sys.argv = [__name__]

###############################################################################
_d = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 32]
Days = [sum(_d[:_n]) for _n in range(1, len(_d) + 1)]
MonthWindow = list(enumerate(Avn.window(Days)))
_tmp = list(range(len(ClimLib.FlightCats)))
CigCat = list(enumerate([ClimLib.FlightCats[_x]['cig'] for _x in _tmp]))
VisCat = list(enumerate([ClimLib.FlightCats[_x]['vis'] for _x in _tmp]))
# The values below mustbe the same as in CigVisDist.py
NumWindDir = 16
VrbDir = NumWindDir
CalmDir = NumWindDir + 1


def _in(value, from_, to):
    if from_ <= to:
        return from_ <= value <= to
    else:
        return not to < value < from_


def _periods(t, delta):
    '''Splits event timespan into hourly intervals'''
    h0, s0 = divmod(t % 86400, 3600.0)
    h1, s1 = divmod((t + delta) % 86400, 3600.0)
    if h1 == h0:            # consecutive reports within the same hour
        return [(h0, (s1 - s0) / 3600.0)]
    elif (h1 - h0) % 24 == 1:   # next hour
        if s1 == 0.0:
            return [(h0, 1.0 - s0 / 3600.0)]
        else:
            return [(h0, 1.0 - s0 / 3600.0), (h1, s1 / 3600.0)]
    elif (h1 - h0) % 24 == 2:   # 2 hours, but next one may be on the top
        if s1 <= 61.0:      # allow 1 min late
            return [(h0, 1.0 - s0 / 3600.0), ((h1 - 1) % 24, 1.0)]
        else:
            return []
    else:
        return []


def get_data(table, listener):
    _Elements = ['date_time', 'yday', 'cig', 'vis', 'wind_dir', 'wind_spd',
                 'wdir_type']

    def _get(row):
        d = {key: row[key] for key in _Elements}
        return row['date_time'], d

    def _filter(row):
        if row['vis'] == table.attrs.vis['fill']:
            return False
        if row['cig'] == table.attrs.cig['fill']:
            return False
        if (row['wind_spd'] == table.attrs.wind_spd['fill']
            and row['wdir_type'] != b'C'):
            return False
        if (row['wind_dir'] == table.attrs.wind_dir['fill']
            and row['wind_spd'] != 0.0 and row['wdir_type'] not in [b'C', b'V']):
            return False
        return True

    def _process(dt, data):
        for n, (day1, day2) in MonthWindow:
            if day1 < data['yday'] <= day2:
                month = n
                break
        else:
            logging.warning('Bad day: %s yday = %d',
                time.ctime(data['date_time']), data['yday'])
            return
        periods = _periods(data['date_time'], dt)
        if not periods:
            logging.warning('Gap in data on %s: %.0f',
                time.ctime(data['date_time']), dt)
            return
        for n, cig in CigCat:
            if data['cig'] < cig:
                cig_bin = n
                break
        else:
            cig_bin = num_cat
        for n, vis in VisCat:
            if data['vis'] < vis:
                vis_bin = n
                break
        else:
            vis_bin = num_cat
        if data['wdir_type'] == b'C' or data['wind_spd'] == 0:
            dd_bin = CalmDir
        elif data['wdir_type'] == b'V':
            dd_bin = VrbDir
        else:
            dd_bin = int((data['wind_dir'] * NumWindDir) / 360.0 + 0.4)
            if dd_bin == NumWindDir:
                dd_bin = 0

        for hour, delta in periods:
            h = int(hour)
            vis_count[month, h, dd_bin, vis_bin] += delta
            cig_count[month, h, dd_bin, cig_bin] += delta
            jnt_bin = min(vis_bin, cig_bin)
            jnt_count[month, h, dd_bin, jnt_bin] += delta
            obs_count[month, h] += 1

    tmp = [_get(row) for row in table if _filter(row)]
    # sort by date
    tmp.sort(key=lambda x: x[0])
    first_year = time.gmtime(tmp[0][0]).tm_year
    last_year = time.gmtime(tmp[-1][0]).tm_year
    num_cat = len(ClimLib.FlightCats)
    num_wind_dir = NumWindDir + 2
    cig_count = numpy.zeros((12, 24, num_wind_dir, num_cat + 1), numpy.float32)
    vis_count = numpy.zeros((12, 24, num_wind_dir, num_cat + 1), numpy.float32)
    jnt_count = numpy.zeros((12, 24, num_wind_dir, num_cat + 1), numpy.float32)
    obs_count = numpy.zeros((12, 24))
    for dt, data in [(y[0] - x[0], x[1]) for x, y in Avn.window(tmp) if y[0] - x[0] < 4200.0]:
        if listener.isCanceled():
            raise KeyboardInterrupt
        _process(dt, data)
    listener.sendObj([first_year, last_year])
    for month in range(12):
        for hour in range(24):
            for windDir in range(num_wind_dir):
                for flightCat in range(num_cat + 1):
                    if listener.isCanceled():
                        raise KeyboardInterrupt
                    sendObj = [month, hour, windDir, flightCat, float(cig_count[month][hour][windDir][flightCat]),
                               float(vis_count[month][hour][windDir][flightCat]), float(jnt_count[month][hour][windDir][flightCat])]
                    listener.sendObj(sendObj)
    listener.sendObj("done")

    return {'years': [first_year, last_year], 'obs': obs_count,
            'cig': cig_count, 'vis': vis_count, 'joint': jnt_count}

###############################################################################


def retrieveData(id_, fname, listener):
    if not os.path.isfile(fname):
        raise Avn.AvnError('File %s does not exist' % fname)

    with tables.open_file(fname, 'r') as fh:
        try:
            table = fh.get_node('/obs')
            tstart = time.time()
            get_data(table, listener)
            print('Elapsed time:', time.time() - tstart)
        except tables.exceptions.NoSuchNodeError:
            raise Avn.AvnError('Bad data in %s' % fname)
        except Exception as e:
            raise Avn.AvnError(e)
