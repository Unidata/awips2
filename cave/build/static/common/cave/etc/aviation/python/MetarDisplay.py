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
#       MetarDisplay.py
#       GFS1-NHD:A9183.0000-SCRIPT;9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 9 (DELIVERED)
#         Created:  22-APR-2009 09:40:08      OBERFIEL
#           Removed hardwired initial year.  Use previous year instead.
#           Expanded the Year: text field valid range to 1940 to 2020.
#       
#       Revision 8 (REVIEW)
#         Created:  17-APR-2009 10:23:31      GILMOREDM
#           Changed to only include the 24 hour max/min group in the
#           decoded metar
#       
#       Revision 7 (REVIEW)
#         Created:  08-APR-2009 14:35:33      GILMOREDM
#           Added code for max/min temps
#       
#       Revision 6 (DELIVERED)
#         Created:  20-MAR-2007 13:36:54      OBERFIEL
#           Updated text input fields and printer dialogs to be
#           consistent.  Frame tag removed.
#       
#       Revision 5 (DELIVERED)
#         Created:  03-JAN-2007 08:03:30      OBERFIEL
#           If any EntryText validate function fails, do not update.
#       
#       Revision 4 (DELIVERED)
#         Created:  30-AUG-2006 10:56:09      OBERFIEL
#           Added auto update function when year and month are changed
#       
#       Revision 3 (DELIVERED)
#         Created:  30-MAY-2006 15:10:40      TROJAN
#           spr 7144: added auto-update feature, number of years in
#           database
#       
#       Revision 2 (DELIVERED)
#         Created:  18-MAY-2006 10:51:18      TROJAN
#           fixed rounding of mslp
#       
#       Revision 1 (DELIVERED)
#         Created:  18-MAY-2006 10:14:54      TROJAN
#           spr 7150
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7422
#       	Action Date:       15-AUG-2009 20:19:42
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Balloon Message does not appear when mouse over 'wnd' indicator
#       
#
import logging, os, time
import ConfigParser, Queue
#import tables
#import Tkinter as tk
#import Pmw
#import Avn, AvnLib, Busy, ClimLib, ErrorRedirect, MessageBar
import Avn, AvnLib, Busy, ClimLib
#from HelpDialog import HelpDialog

_Help = {
    'title': 'METAR Display Help',
    'content': """
This dialog is used to display METARs reconstructed from climate data.

Menu Bar
File:
    Print:      invokes printer selection dialog.
    Save As:    invokes file selection dialog.

Options:
    Show Decoded:   toggles between METAR and decoded (ARONET) display 
                format
    Update on Selection: when selected, "Station", "Month", "Day" 
                and "Num Days" changes update the display without 
                pressing "Show"

Date Selection
    Year:       select start year.
    Month:      select start month.
    Day:        select start day. Range of days is always 1-31, 
                year 2000, month 2, day 31 results in data for 
                March 2, 2000.
    Num Days:   number of days of data to display.

Show:   displays reconstructed METARs for the selected dates and 
        display format.
"""
}

_Logger = logging.getLogger()
Month = ['', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', \
    'Oct', 'Nov', 'Dec']
        
items = {}
        
##############################################################################
# functions to build METAR from climate data
def _mk_time(row):
    return time.strftime('%d%H%M', time.gmtime(row[items['date_time']]))

def _mk_wind(row):
    wspeed = ClimLib.hd2us_wind_speed(row[items['wind_spd']])
    wgust = ClimLib.hd2us_wind_speed(row[items['wd_gust']])
    if wspeed < 0:
        return ''
    wspeed_str = '%02d' % wspeed
    if wgust < 0:
        wgust_str = ''
    else:
        wgust_str = 'G%02d' % wgust
    if row[items['wdir_type']] == 'C' or wspeed == 0:
        return '00000'
    elif row[items['wdir_type']] == 'V':
        return 'VRB'+wspeed_str+wgust_str
    else:
        wdir = row[items['wind_dir']]
        if not 0 <= wdir <= 360:
            return ''
        wdir = (wdir+5)//10*10
        if wdir == 0:
            wdir = 360
        return ('%03d' % wdir)+wspeed_str+wgust_str

def _mk_vis(row):
    if row[items['vis']] > 100000:
        return '     '
    vis = ClimLib.hd2us_vsby(row[items['vis']])
    vi = int(vis)
    vf = vis - vi
    if vi == 0:
        if vf < 0.2:
            return 'M1/4'
        elif vf < 0.3:
            return '1/4'
        elif vf < 0.6:
            return '1/2'
        else:
            return '3/4'
    elif vi == 1:
        if vf < 0.3:
            return '1 1/4'
        elif vf < 0.6:
            return '1 1/2'
        else:
            return '1 3/4'
    else:
        return str(vi)

def _mk_wx(row):
    codes = row[items['pres_wxm_code']]
    # first, precipitation
    tstm = [ClimLib.MWxTable[x] for x in codes if 91<=x<=99]
    pcp = [ClimLib.MWxTable[x] for x in codes if 50<=x<=90]
    pcp_ = []
    if tstm:
        pcp_.append(tstm[0])
    if pcp:
        tmp = [(len(p['str']), p) for p in pcp]
        tmp.sort()
        tmp.reverse()
        for n, p in tmp:
            for p_ in pcp_:
                if p['str'][-2:] == p_['str'][-2:]:
                    break
            else:
                p['str'] = p['str'][-2:]
                pcp_.append(p)
    if pcp_:
        i = max([p['int'] for p in pcp_])
        if i & ClimLib.PInt.hvy:
            i_str = '+'
        elif i & ClimLib.PInt.mod:
            i_str = ''
        else:
            i_str = '-'
        pcp_str = i_str + ''.join([p['str'] for p in pcp_])
    else:
        pcp_str = ''
    # other weather
    obv = [ClimLib.MWxTable[x] for x in codes if x<=49]
    obv_ = []
    if 'TS' in [o['str'] for o in obv]:
        obv_.append(o['str'])
    for o in obv:
        for o_ in obv_:
            if o['str'] in o_:
                break
            # fix for FG/BR
            if o['str'] == 'BR' and o_ == 'FG' or \
                o['str'] == 'FG' and o_ == 'BR':
                break
        else:
            obv_.append(o['str'])
    if obv_:
        # fix for FG/BR
        vis = row[items['vis']]
        try:
            i = obv_.index('FG')
            if 1000 <= vis < 100000:
                obv_[i] = 'BR'
        except ValueError:
            pass
        try:
            i = obv_.index('BR')
            if vis < 1000:
                obv_[i] = 'FG'
        except ValueError:
            pass
        obv_str = ' '.join(obv_)
    else:
        obv_str = ''
    return (pcp_str + ' ' + obv_str).strip()

def _mk_sky(row):
    cld = {1: 'FEW', 2: 'SCT', 3: 'BKN', 4: 'OVC', 5: 'VV'}
    sky = []
    for cvr, h in zip(row[items['cov_sum_st_code']], row[items['cov_sum_st_dim']]):
        if cvr == 0 or cvr > 6:
            break 
        hgt = ClimLib.hd2us_cig(h)/100
        if hgt < 0:
            break
        if cvr == 6:    # partially obscured
            continue
        if hgt < 30:
            h1 = int(hgt+0.49)
        elif hgt < 50:
            h1 = (hgt+2)//5 * 5
        else:
            h1 = (hgt+5)//10 * 10
        sky.append('%s%03d' % (cld[cvr], h1))
    if not sky:
        sky.append('SKC')
    return ' '.join(sky)

def _mk_temp(row):
    tt, td = row[items['temp']], row[items['dewt']]
    if tt < 0.0:
        tt_str = 'M%02d/' % -tt
    elif tt < 60.0:
        tt_str = '%02d/' % tt
    else:
        tt_str = '/'
    if td < 0.0:
        td_str = 'M%02d' % -td
    elif td <= tt < 60.0:
        td_str = '%02d' % td
    else:
        td_str = ''
    return tt_str+td_str

def _mk_maxmintemp(row):
    try:
	maxt, mint = row[items['x_tp']]
    except:
	print row[items['x_tp']], len(row[items['x_tp']])
	return
    if not (-80.0 < maxt < 70.0 and -80.0 < mint < 70.0): return ''
    if not (row[items['x_tp_period']][0] == row[items['x_tp_period']][1] == 24.): return ''
    if maxt < 0.0:
	maxt_str = "1%03d" % (-maxt * 10)
    else:
	maxt_str = "0%03d" % (maxt * 10)
    if mint < 0.0:
	mint_str = "1%03d" % (-mint * 10)
    else:
	mint_str = "0%03d" % (mint * 10)
    return "4%s%s" % (maxt_str, mint_str)

def _mk_alt(row):
    alt = row[items['altimeter']]
    if not 900 <= alt < 1100:
        return ''
    return 'A%4.0f' % (2.953*alt)

def _mk_alt_dcd(row):
    alt = row[items['altimeter']]
    if not 900 <= alt < 1100:
        return ''
    return ('%4.0f' % (2.953*alt))[1:]

def _mk_mslp(row):
    if 900 <= row[items['pres']] < 1100:
        p = int(row[items['pres']]+0.49)
        if p >= 1000.0:
            p -= 1000.0
        return '%03d' % p 
    else:
        return ''

def _mk_tt_us(row):
    if -80.0 < row[items['temp']] < 70.0:
        return '%.0f' % (row[items['temp']]*1.8+32.0)
    else:
        return ''

def _mk_td_us(row):
    if -80.0 < row[items['dewt']] < 70.0:
        return '%.0f' % (row[items['dewt']]*1.8+32.0)
    else:
        return ''

def _mk_past_pcp(row):
    tok = []
    for p, d, c in zip(row[items['prec_period']], row[items['prec_depth']], 
        row[items['prec_code']]):
        if d == 0.0 and c > 1 or d > 1000.0:
            continue
        if p == 1:
            tok.append('P%04.0f' % (d/0.254))
        elif p in [3, 6]:
            tok.append('R%04.0f' % (d/0.254))
        elif p == 24:
            tok.append('7%04.0f' % (d/0.254))
    return ' '.join(tok)

def _mk_past_pcp_dcd(row):
    tok = []
    for p, d, c in zip(row[items['prec_period']], row[items['prec_depth']], 
        row[items['prec_code']]):
        if d == 0.0 and c > 1 or d > 1000.0:
            continue
        if p == 1:
            return '%04.0f' % (d/0.254)
        else:
            break
    return ''

###############################################################################
# for decoded METAR
Items = [ \
    {'item': 'time',  'length': 6,  'left': '',  \
        'header': 'TIME', 'fun': _mk_time}, \
    {'item': 'wind',  'length': 8,  'left': '-', \
        'header': 'WIND', 'fun': _mk_wind}, \
    {'item': 'vsby',  'length': 5,  'left': '-', \
        'header': 'VSBY', 'fun': _mk_vis}, \
    {'item': 'wx',    'length': 11, 'left': '-', \
        'header': 'WX', 'fun': _mk_wx}, \
    {'item': 'sky',   'length': 28, 'left': '-', \
        'header': 'SKY CONDITION', 'fun': _mk_sky}, \
    {'item': 'slp',   'length': 3,  'left': '',  \
        'header': 'SLP', 'fun': _mk_mslp}, \
    {'item': 'temp',  'length': 3,  'left': '',  \
        'header': ' TT', 'fun': _mk_tt_us}, \
    {'item': 'dewp',  'length': 3,  'left': '',  \
        'header': ' DP', 'fun': _mk_td_us}, \
    {'item': 'altim', 'length': 3,  'left': '',  \
        'header': 'ALT', 'fun': _mk_alt_dcd}, \
    {'item': 'pcp1h', 'length': 3,  'left': '',  \
        'header': 'PCP', 'fun': _mk_past_pcp_dcd}, \
]

def mk_metar_dcd(id_, row):
    return ' '.join(['%%%s%ds' % (it['left'], it['length']) % it['fun'](row) \
        for it in Items])

def mk_metar(id_, row):
    tok = ['METAR', id_, _mk_time(row)+'Z']
    wind = _mk_wind(row)
    if wind:
        tok.append(wind+'KT')
    vis = _mk_vis(row)
    if vis:
        tok.append(vis+'SM')
    tok.append(_mk_wx(row))
    tok.append(_mk_sky(row))
    tok.append(_mk_temp(row))
    tok.append(_mk_alt(row))
    atok = ['RMK']
    mslp = _mk_mslp(row)
    if mslp:
        atok.append('SLP'+mslp)
    atok.append(_mk_past_pcp(row))
    atok.append(_mk_maxmintemp(row))
    atok = filter(None, atok)
    if len(atok) > 1:
        tok.extend(atok)
    metar = ' '.join(filter(None, tok))
    if len(metar) > 70:
        i = metar.rfind(' ', 0, 71)
        return metar[:i]+'\n    '+metar[i+1:]
    else:
        return metar

#def get_decoded(id_, table, tstart, tend):
#    data = [(row['date_time'], mk_metar_dcd(id_, row)) \
#        for row in table.where(tstart<=table.cols.date_time<tend)]
#    data.sort()
#    return [x[1] for x in data]

def get_decoded(id_, table, tstart, tend):
    dt = table['date_time']
    dt1 = (tstart <= dt)
    dt2 = (dt < tend)
    import numpy
    index = numpy.where(dt1&dt2)[0]
    rows = []
    for i in index:
        rows.append(table[i])
    data = [(row[items['date_time']], mk_metar_dcd(id_, row)) for row in rows]
    data.sort()
    return [x[1] for x in data]

#def get_formatted(id_, table, tstart, tend):
#    data = [(row['date_time'], mk_metar(id_, row)) \
#        for row in table.where(tstart<=table.cols.date_time<tend)]
#    data.sort()
#    return [x[1] for x in data]

def get_formatted(id_, table, tstart, tend):
    dt = table['date_time']
    dt1 = (tstart <= dt)
    dt2 = (dt < tend)
    import numpy
    index = numpy.where(dt1&dt2)[0]
    rows = []
    for i in index:
        rows.append(table[i])
    data = [(row[items['date_time']], mk_metar(id_, row)) for row in rows]
    data.sort()
    return [x[1] for x in data]

##############################################################################

def get_metars(id_, year, month, day, ndays, decoded, fname, queue):
    try:
        import os
        if not os.path.isfile(fname):
            raise Avn.AvnError('File %s does not exist' % fname)
        import h5py
        fh = h5py.File(fname, 'r')
        try:
            table = fh['obs']
            names = table.dtype.names
            for i in range(len(names)):
                items[names[i]] = i
            tms = (int(year), int(month), int(day), 0, 0, 0, 0, 0, 0)
            tstart = time.mktime(tms)-600.0
            tend = tstart + int(ndays)*86400.0 + 600.0
            if decoded:
                data = get_decoded(id_, table, tstart, tend)
            else:
                data = get_formatted(id_, table, tstart, tend)
            
            if data:
                if decoded:
                    header_dcd = ' '.join(['%%-%ds' % it['length'] % it['header'] \
                              for it in Items])
                else:
                    header_dcd = ''
                
                header = '%s METARs for %s %d\n%s' % (id_, Month[int(month)], int(year), header_dcd)
                if decoded:
#                    h = ' '.join(['%%-%ds' % it['length'] % it['header'] for it in Items])
#                    header = header + '\n' + h
                    queue.put(header)
                    for d in data:
                        queue.put(d)
                else:
                    queue.put(header)
                    for d in data:
                        queue.put(d)
            else:
                msg = 'No %s data for %s %d %d' % \
                    (id_, Month[int(month)], int(day), int(year))
                errMsg = 'Date %s %d %d not present in climate file %s.hd5' % \
                    (Month[int(month)], int(day), int(year), id_)
                queue.put(errMsg)
        finally:
            fh.close()            
    except Exception, e:
        print e
