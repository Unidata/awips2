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
#       MetarPlot.py
#       GFS1-NHD:A8543.0000-SCRIPT;6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 6 (DELIVERED)
#         Created:  06-JUL-2005 18:16:40      TROJAN
#           spr 6548
#       
#       Revision 5 (DELIVERED)
#         Created:  12-MAY-2005 14:30:21      TROJAN
#           spr 6838
#       
#       Revision 4 (REVIEW)
#         Created:  07-MAY-2005 11:35:59      OBERFIEL
#           Added Item Header Block
#       
#       Revision 3 (DELIVERED)
#         Created:  18-APR-2005 17:32:12      OBERFIEL
#           Changes to support gamin
#       
#       Revision 2 (DELIVERED)
#         Created:  06-APR-2005 11:41:07      TROJAN
#           spr 6763
#       
#       Revision 1 (APPROVED)
#         Created:  02-APR-2005 17:13:55      TROJAN
#           spr 6763
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6548
#       	Action Date:       09-AUG-2005 14:09:33
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS:  Data acquistion change in OB6
#       
#
# MetarPlot.py
# George Trojan, SAIC/MDL, March 2005
# last update: 06/07/05

import math
import Avn, AvnLib, Globals, GraphWidget
        
def _plot(data, tticks, vticks, cticks, color='black'):
    graphics = {'cig': [], 'vsby': [], 'wind': []}
    cig = []
    vsby = []
    wind = []
    wind_t = []
    for g in data:
        try:
            c = math.log(max(vticks[0], min(cticks[-1], g['sky']['cig'])))
            v = math.log(max(vticks[0], min(vticks[-1], g['vsby']['vsby'])))
            dd = g['wind']['dd']
            ff = g['wind']['ff']['lo']
            gg = g['wind']['ff']['hi']
        except KeyError:    # missing data
            if g['time'] < tticks[-1]:
                continue
            else:
                break
        if g['time'] < tticks[0]:
            t = tticks[0]
            if cig:
                cig[0] = (t, c)
            else:
                cig.append((t, c))
            if vsby:
                vsby[0] = (t, v)
            else:
                vsby.append((t, v))
            if wind_t:
                wind_t[0] = t
                if gg > ff:
                    wind[0] = {'dd': dd, 'ff': ff, 'gg': gg}
                else:
                    wind[0] = {'dd': dd, 'ff': ff}
            else:
                wind_t.append(t)
                if gg > ff:
                    wind.append({'dd': dd, 'ff': ff, 'gg': gg})
                else:
                    wind.append({'dd': dd, 'ff': ff})
        elif g['time'] < tticks[-1]:
            t = g['time']
            cig.append((t, c))
            vsby.append((t, v))
            wind_t.append(t)
            if gg > ff:
                wind.append({'dd': dd, 'ff': ff, 'gg': gg})
            else:
                wind.append({'dd': dd, 'ff': ff})
        else:
            break
    graphics['cig'] = [GraphWidget.Steps(cig, fill=color, width=1)]
    graphics['vsby'] = [GraphWidget.Steps(vsby, fill=color, width=1)]
    graphics['wind'] = [GraphWidget.Winds(wind_t, wind, fill=color, width=1)]
    return graphics

class Plot:
    def __init__(self, **args):
        self.color = args.get('color', 'black')
        self.data = None

    def plot(self, tafid, sites, tref, tticks, vticks, cticks, header):
        metarid = sites['metar']    # must be defined
        metars = Globals.DRC.getMetars(metarid, True, tref-86400.0)
        data = [AvnLib.makeMetarData(m.dcd) for m in metars if 'itime' in m.dcd]
        if not data:
            raise Avn.AvnError('Cannot access METARs for %s' % tafid)
        tmp = [(x['time'], x) for x in data]
        tmp.reverse()
        self.data = [x[1] for x in tmp]
        return _plot(self.data, tticks, vticks, cticks, self.color)
