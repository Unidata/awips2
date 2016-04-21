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
#       TafPlotP.py
#       GFS1-NHD:A8539.0000-SCRIPT;4
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 4 (DELIVERED)
#         Created:  11-AUG-2006 10:29:23      OBERFIEL
#           Checked to make sure log(0) isn't performed
#       
#       Revision 3 (DELIVERED)
#         Created:  07-MAY-2005 11:39:04      OBERFIEL
#           Added Item Header Block
#       
#       Revision 2 (DELIVERED)
#         Created:  25-APR-2005 20:40:04      TROJAN
#           stdr917
#       
#       Revision 1 (DELIVERED)
#         Created:  02-APR-2005 17:09:24      TROJAN
#           spr 6763
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7206
#       	Action Date:       07-SEP-2006 06:35:21
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Display NAM-WRF guidance in WxPlot results in an error.
#       
#
# TafPlotP.py
# George Trojan, SAIC/MDL, March 2005
# last update: 04/25/05

import math
import GraphWidget

def plot(data, tticks, vticks, cticks, color):
    graphics = {'cig': [], 'vsby': [], 'wind': []}
    cig = []
    vsby = []
    wind = []
    wind_t = []
    for g in data._data:
        try:
            lo = max(cticks[0], g['sky']['lo'])
            hi = min(cticks[-1], g['sky']['hi'])
            if lo > hi:
                lo = max(cticks[0],hi)
            if hi < cticks[0]:
                hi = cticks[0]
            lo_cig = math.log(lo)
            hi_cig = math.log(hi)
        except KeyError:
            lo_cig, hi_cig = None, None
        try:
            lo = max(vticks[0], g['vsby']['lo'])
            hi = min(vticks[-1], g['vsby']['hi'])
            if lo > hi:
                lo = max(vticks[0],hi)
            if hi < vticks[0]:
                hi = vticks[0]
            lo_vsby = math.log(lo)
            hi_vsby = math.log(hi)
        except KeyError:
            lo_vsby, hi_vsby = None, None
        try:
            dd = g['wind']['dd']['prev']
            ff = g['wind']['ff']['prev']
        except KeyError:
            dd, ff = None, None
        if g['from'] < tticks[0] < g['to']:
            t = tticks[0]
            if cig:
                cig[0] = (t, lo_cig, hi_cig)
            else:
                cig.append((t, lo_cig, hi_cig))
            if vsby:
                vsby[0] = (t, lo_vsby, hi_vsby)
            else:
                vsby.append((t, lo_vsby, hi_vsby))
            if wind_t:
                wind_t[0] = t
                wind[0] = {'dd': dd, 'ff': ff}
            else:
                wind_t.append(t)
                wind.append({'dd': dd, 'ff': ff})
        elif g['from'] < tticks[-1] <= g['to']:
            t = g['from']
            cig.append((t, lo_cig, hi_cig))
            vsby.append((t, lo_vsby, hi_vsby))
            wind_t.append(t)
            wind.append({'dd': dd, 'ff': ff})
            t = tticks[-1]
            cig.append((t, lo_cig, hi_cig))
            vsby.append((t, lo_vsby, hi_vsby))
            break
        elif tticks[0] <= g['from'] and g['to'] <= tticks[-1]:
            t = g['from']
            cig.append((t, lo_cig, hi_cig))
            vsby.append((t, lo_vsby, hi_vsby))
            wind_t.append(t)
            wind.append({'dd': dd, 'ff': ff})
    else:
        t = g['to']
        cig.append((t, lo_cig, hi_cig))
        vsby.append((t, lo_vsby, hi_vsby))
    graphics['cig'] = [GraphWidget.StepsRange(cig, fill=color, 
        stipple='gray25', width=1)]
    graphics['vsby'] = [GraphWidget.StepsRange(vsby, fill=color, 
        stipple='gray25', width=1)]
    graphics['wind'] = [GraphWidget.Winds(wind_t, wind, fill=color, width=1)]
    return graphics
