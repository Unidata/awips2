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
#       CigVisTrend.py
#       GFS1-NHD:A9059.0000-SCRIPT;16
#
#    Status:
#       DELIVERED
#
#    History:
#       Revision 17
#         Created:  10-AUG-2012 15:00:00      GZHANG
#           DR 14702: Added fix for PyTables in Gui().trend()
#
#       Revision 16 (DELIVERED)
#         Created:  06-MAR-2008 17:01:20      OBERFIEL
#           Added fix for leap-years.
#
#       Revision 15 (DELIVERED)
#         Created:  20-MAR-2007 13:36:53      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#
#       Revision 14 (DELIVERED)
#         Created:  04-JAN-2007 12:52:14      OBERFIEL
#           Corrected misspelling.
#
#       Revision 13 (REVIEW)
#         Created:  28-DEC-2006 11:33:09      OBERFIEL
#           Made validate function less restrictive.
#
#       Revision 12 (DELIVERED)
#         Created:  30-JUN-2006 10:58:42      TROJAN
#           spr 7190: added plot area window parameters to
#           app-recources file
#
#       Revision 11 (DELIVERED)
#         Created:  30-MAY-2006 15:10:39      TROJAN
#           spr 7144: added auto-update feature, number of years in
#           database
#
#       Revision 10 (DELIVERED)
#         Created:  23-MAY-2006 08:47:13      TROJAN
#           spr 7152: fixed select days matching criteria - round time,
#           added history button in TWEB Editor's statusbar,
#           fixed spelling
#
#       Revision 9 (DELIVERED)
#         Created:  19-MAY-2006 08:32:07      TROJAN
#           SPR 7146: fixed select days matching criteria - round
#           observation time up
#
#       Revision 8 (DELIVERED)
#         Created:  02-MAY-2006 09:34:10      TROJAN
#           SPR 7133: fixed "where" selection expression for dates
#           wrapping around end of the year
#
#       Revision 7 (DELIVERED)
#         Created:  02-MAY-2006 08:52:49      TROJAN
#           SPR 7138: fixed "where" selection expression for dates
#           wrapping around end of the year
#
#       Revision 6 (DELIVERED)
#         Created:  27-MAR-2006 08:11:49      TROJAN
#           spr 7103: added ErrorRedirect method
#
#       Revision 5 (DELIVERED)
#         Created:  24-MAR-2006 17:45:44      TROJAN
#           spr 7106 Pmw code does not forward python exceptions to
#           AvnFPS _Logger
#
#       Revision 4 (DELIVERED)
#         Created:  23-FEB-2006 08:24:45      TROJAN
#           moved paths to text database commands to avnenv.sh
#
#       Revision 3 (DELIVERED)
#         Created:  16-FEB-2006 14:20:58      TROJAN
#           removed unnecessary imports, renamed shadowed variables
#
#       Revision 2 (APPROVED)
#         Created:  15-FEB-2006 14:34:43      TROJAN
#           fixes transient dialog behavior - spr 7091
#
#       Revision 1 (APPROVED)
#         Created:  13-FEB-2006 10:25:14      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#               Change Document:   GFS1-NHD_SPR_7373
#               Action Date:       02-JUN-2008 20:44:52
#               Relationship Type: In Response to
#               Status:           CLOSED
#               Title:             AvnFPS: AvnWatch monitoring not reliable.
#
#
##
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Mar 10, 2022  8808     randerso  Update ConfigParser to better work with
#                                  Java commons.configuration
#
##
# This is a base file that is not intended to be overridden.
##

import os
import sys

import numpy
import tables

import Avn
import AvnConfigParser
import ClimLib

_Help = {
    'title': 'AvnFPS - Ceiling/Visibility Trend Help',
    'content': """
This application displays ceiling/visibility trend based
on selected initial conditions.

Use "Get" button to retrieve METAR for a selected site.
The METAR can be modified.
Use "Decode" button to initialize selection widgets.
The initial conditions can be adjusted either by typing
in the "value" and "range" windows, or by mouse actions.
Left button moves value or an edge of range (red area on
the element widget). Middle button is used to move both
value and range. In the "Wind Direction" widget use right
button to toggle between wind arrow and a circle representing
calm and variable wind.
Use "Element" radiobuttons to select forecasted element.
Press "Draw" to display the forecast.

The displayed image can be printed or stored in a graphic file.
Use "File" menu for that purpose.

"""
}

sys.argv = [__name__]

CigCat = sorted((k, v['cig']) for k, v in ClimLib.FlightCats.items())
VisCat = sorted((k, v['vis']) for k, v in ClimLib.FlightCats.items())
NumCat = len(ClimLib.FlightCats)
Fill = 9

# used by Selection class
CatDict = {
    'dd': [30, 90, 150, 210, 270, 330],
    'ff': [5, 12, 32],
    'vsby': [0.4, 1.1, 3.1, 6.1],
    'cig': [210, 610, 1020, 3130, 5050],
    }

fields = {}


##############################################################################
def ff_range(value):
    seq = CatDict['ff']
    seq.insert(0, 0)
    seq.append(sys.maxsize)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad ff_range')
    return lo, hi


def vsby_range(value):
    seq = CatDict['vsby']
    seq.insert(0, 0)
    seq.append(sys.maxsize)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad vsby_range')
    return lo, hi


def cig_range(value):
    seq = CatDict['cig']
    seq.insert(0, 0)
    seq.append(sys.maxsize)
    for lo, hi in Avn.window(seq):
        if lo <= value < hi:
            break
        elif hi <= value < lo:
            break
    else:
        raise ValueError('Bad cig_range')
    return lo, hi


def _cigCat(v):
    for n, cig in CigCat:
        if v < cig:
            return n
    return NumCat


def _visCat(v):
    for n, vis in VisCat:
        if v < vis:
            return n
    return NumCat


def squeeze(matches, tref):

    # eliminate multiple events within the same time window
    def _num(t):
        return (t - tref) // 86400.0

    d = {_num(t): (n, t) for n, t in matches}
    return sorted(d.values())


def process_data_worst(count, data, ref_t, num_hours, table, listener):
    tmp_cig = numpy.full((num_hours,), Fill)
    tmp_vis = numpy.full((num_hours,), Fill)
    tmp_joint = numpy.full((num_hours,), Fill)
    for obs in data:
        if listener.isCanceled():
            raise KeyboardInterrupt
        dh = int((obs[0] - ref_t) // 3600.0)
        if not (0 <= dh < num_hours):
            raise Avn.AvnError('Bug in process_data_worst()')
        if (obs[1] != table.attrs.cig['fill'] and
            obs[2] != table.attrs.vis['fill']):
            tmp_cig[dh] = min(tmp_cig[dh], _cigCat(obs[1]))
            tmp_vis[dh] = min(tmp_vis[dh], _visCat(obs[2]))
            tmp_joint[dh] = min(tmp_cig[dh], tmp_vis[dh])
    for h in range(num_hours):
        if listener.isCanceled():
            raise KeyboardInterrupt
        if tmp_cig[h] < NumCat:
            count['cig'][h, int(tmp_cig[h])] += 1
        if tmp_vis[h] < NumCat:
            count['vis'][h, int(tmp_vis[h])] += 1
        if tmp_joint[h] < NumCat:
            count['joint'][h, int(tmp_joint[h])] += 1
        if tmp_joint[h] < Fill:
            count['total'][h] += 1


##############################################################################
class Gui():
    AppName = 'AvnFPS - Ceiling/Visibility Trend'
    Xresfile = os.path.join('etc', 'app-resources', 'XCigVisTrend')
    IdsFile = os.path.join('etc', 'ids.cfg')
    RowLabels = ['MVFR', 'IFR', 'LIFR', 'VLIFR', 'COUNT']
    MaxHours = 13   # hour_menu must not contain value >= MaxHours

    def __init__(self):
        pass

    def get_site_config(self):
        # get list of data files
        cp = AvnConfigParser.AvnConfigParser()
        cp.read(self.IdsFile)
        self.ids = {x: dict(cp.items(x)) for x in cp.sections()}
        if self.ids:
            tmp = sorted(self.ids.keys())
            self.station_w.setlist(tmp)
            self.station_w.selection_set(0)

    def get_metar(self, siteID):
        import JUtil
        import MetarData
        data = MetarData.retrieve([siteID], 1)
        if not data:
            raise Avn.AvnError('Cannot retrieve data for %s' % siteID)
        data = [{'header': d.header, 'text': d.text, 'dcd': d.dcd}
                for d in data
                ]
        data.sort(key=lambda x: x['dcd']['itime']['str'], reverse=True)
        return JUtil.pyValToJavaObj(data[0])

    def trend(self, table, selection, unltd_cig, listener):

        def _in(value, from_, to):
            if from_ <= to:
                return from_ <= value <= to
            else:
                return not (to < value < from_)

        def time_selector11(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('(yday0<=yday) & (yday<=yday1)'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o

        def time_selector12(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('(yday0<=yday) & (yday<=yday1)'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o

        def time_selector21(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('yday0<=yday'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o
            for o in table.where('yday<=yday1'):
                if sel.hour[0] <= o['hour'] <= sel.hour[1]:
                    yield o

        def time_selector22(sel):
            yday0 = sel.yday[0]
            yday1 = sel.yday[1]
            for o in table.where('yday0<=yday'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o
            for o in table.where('yday<=yday1'):
                if o['hour'] <= sel.hour[1] or sel.hour[0] <= o['hour']:
                    yield o

        def wx_selector(row, sel):
            if listener.isCanceled():
                raise KeyboardInterrupt
            ff = row['wind_spd']
            if ff == table.attrs.wind_spd['fill']:
                return False
            if not _in(ff, *sel.wind_speed):
                return False
            dd = row['wind_dir']
            if dd == table.attrs.wind_dir['fill']:
                if ff == 0:
                    dd = 0
                else:
                    typ = row['wdir_type']
                    if typ == 'V':
                        dd = 0
                    else:
                        return False
            else:
                dd = float(dd)
            if dd > 0:
                if not _in(dd, *sel.wind_dir):
                    return False
            cig = row['cig']
            if cig == table.attrs.cig['fill']:
                return False
            if not _in(cig, *sel.cig):
                return False
            vsby = row['vis']
            if vsby == table.attrs.vis['fill']:
                return False
            if not _in(vsby, *sel.vsby):
                return False
            pcp = [x for x in row['pres_wxm_code'][:] if 50 <= x < 100] != []
            if pcp and sel.pcp == 'no' or not pcp and sel.pcp == 'yes':
                return False
            return True

        # convert units
        sel = Avn.Bunch(wind_dir=selection.wind_dir,
            wind_speed=[ClimLib.us2hd_wind_speed(x)
                        for x in selection.wind_speed],
            cig=[ClimLib.us2hd_cig(x, unltd_cig) for x in selection.cig],
            vsby=[ClimLib.us2hd_vsby(x) for x in selection.vsby],
            hour=selection.hour,
            yday=selection.yday,
            pcp=selection.pcp,
            cur_hour=selection.cur_hour,
            )

        # fix hour selection - round to full hour
        def _round(h):
            return (h + 1.0 / 6.0) // 1.0     # 10 min before hour

        sel.cur_hour = _round(sel.cur_hour)
        sel.hour = [_round(x) for x in sel.hour]

        # create search selector
        if sel.yday[0] <= sel.yday[1]:
            if sel.hour[0] <= sel.hour[1]:
                time_selector = time_selector11
            else:
                time_selector = time_selector12
        else:
            if sel.hour[0] <= sel.hour[1]:
                time_selector = time_selector21
            else:
                time_selector = time_selector22

        # select days matching criteria
        matches = [(row.nrow, row['date_time'] + 600)
                   for row in time_selector(sel)
                   if wx_selector(row, sel)
                   ]
        tref = 3600.0 * sel.hour[0]
        count = {'cig': numpy.zeros((self.MaxHours, NumCat), numpy.float32),
                 'vis': numpy.zeros((self.MaxHours, NumCat), numpy.float32),
                 'joint': numpy.zeros((self.MaxHours, NumCat), numpy.float32),
                 'total': numpy.zeros((self.MaxHours,))
                 }
        delta = (sel.cur_hour - sel.hour[0]) % 24.0 * 3600.0 + tref
        # stats method
        for n, t in squeeze(matches, tref):
            if listener.isCanceled():
                raise KeyboardInterrupt
            # time corrresponding to the selected hour
            t0 = (t - tref) // 86400.0 * 86400.0 + delta
            # start search time
            t1 = t0 - 600.0
            # end search time
            t2 = t1 + self.MaxHours * 3600.0
            # small enough
            n1 = max(0, n - 2 * int((t - t1) // 3600.0) - 1)
            # big enough
            n2 = n1 + 5 * self.MaxHours
            data = [(row['date_time'], row['cig'], row['vis']) for row in
                table.where('(t1<=date_time) & (date_time<t2)', start=n1, stop=n2)]
            process_data_worst(count, data, t1, self.MaxHours, table, listener)
        # calculate frequencies
        args = list(count.keys())
        args.remove('total')
        tmp = 0.01 * count['total']
        for arg in args:
            for n in range(NumCat):
                count[arg][:, n] /= tmp
        return count

    def get_data(self, selectionDict, id_, hours, fname, listener):
        self.MaxHours = hours
        selection = Avn.Bunch(cig=selectionDict['cig'],
                vsby=selectionDict['vsby'],
                wind_speed=selectionDict['wind_speed'],
                wind_dir=selectionDict['wind_dir'],
                hour=selectionDict['hour'],
                yday=selectionDict['yday'],
                pcp=selectionDict['pcp'],
                cur_hour=selectionDict['cur_hour'],
                )

        if not os.path.isfile(fname):
            raise Avn.AvnError('File %s does not exist' % fname)

        with tables.open_file(fname) as fh:
            table = fh.get_node('/obs')
            unltd = ClimLib.Unlimited
            data = self.trend(table, selection, unltd, listener)
            cig_count = data['cig']
            vis_count = data['vis']
            joint_count = data['joint']
            total_count = data['total']
            dataDict = {}
            cigList = []
            visList = []
            jntList = []
            totalList = []
            for h in range(self.MaxHours):
                if listener.isCanceled():
                    raise KeyboardInterrupt
                totalList.append(float(total_count[h]))
                tmpCig = []
                tmpVis = []
                tmpJnt = []
                for c in range(NumCat):
                    if listener.isCanceled():
                        raise KeyboardInterrupt
                    tmpCig.append(float(cig_count[h][c]))
                    tmpVis.append(float(vis_count[h][c]))
                    tmpJnt.append(float(joint_count[h][c]))
                cigList.append(tmpCig)
                visList.append(tmpVis)
                jntList.append(tmpJnt)
            dataDict['cig'] = cigList
            dataDict['vis'] = visList
            dataDict['joint'] = jntList
            dataDict['total'] = totalList
            listener.sendObj(dataDict)
            listener.sendObj("done")
        return data

##############################################################################
