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
#       GridMonitor.py
#       GFS1-NHD:A6631.0000-SCRIPT;1.18
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.21 (INITIALIZE)
#         Created:  18-APR-2007 12:38:15      SOLSON
#           Removed CR characters from the previous rev of this item.
#       
#       Revision 1.20 (DELIVERED)
#         Created:  07-DEC-2006 12:18:10      OBERFIEL
#           Fixed code mixup and PVCS doc block.
#       
#       Revision 1.19 (DELIVERED)
#         Created:  17-NOV-2006 13:45:10      BLI
#           Fixed for uninitialized variable
#       
#       Revision 1.18 (DELIVERED)
#         Created:  17-NOV-2006 13:10:57      BLI
#           Fixed an uninitialized variable
#       
#       Revision 1.17 (DELIVERED)
#         Created:  26-SEP-2006 09:34:47      BLI
#           Modified to work with new TafGen
#       
#       Revision 1.16 (DELIVERED)
#         Created:  21-APR-2006 11:29:21      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.15 (DELIVERED)
#         Created:  20-APR-2006 15:47:30      TROJAN
#           made compare() wrapper in base class handling exceptions
#           renamed compare() to __compare()
#       
#       Revision 1.14 (DELIVERED)
#         Created:  23-JAN-2006 08:23:13      TROJAN
#           stdr 956
#       
#       Revision 1.13 (APPROVED)
#         Created:  03-NOV-2005 13:39:36      TROJAN
#           spr 7052
#       
#       Revision 1.12 (DELIVERED)
#         Created:  16-AUG-2005 13:53:04      TROJAN
#           spr 6988
#       
#       Revision 1.11 (APPROVED)
#         Created:  09-AUG-2005 14:00:48      TROJAN
#           spr 6974
#       
#       Revision 1.10 (DELIVERED)
#         Created:  06-JUL-2005 18:16:38      TROJAN
#           spr 6548
#       
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:34:02      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.8 (DELIVERED)
#         Created:  02-APR-2005 17:47:23      TROJAN
#           spr 6772
#       
#       Revision 1.7 (DELIVERED)
#         Created:  15-FEB-2005 18:12:21      TROJAN
#           spr 6561
#       
#       Revision 1.6 (DELIVERED)
#         Created:  21-OCT-2004 19:36:12      TROJAN
#           spr 6420
#       
#       Revision 1.5 (APPROVED)
#         Created:  30-SEP-2004 18:56:02      TROJAN
#           stdr 874
#       
#       Revision 1.4 (APPROVED)
#         Created:  19-AUG-2004 20:44:35      OBERFIEL
#           Code change
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:27      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:03      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:38      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7227
#       	Action Date:       26-FEB-2007 11:02:05
#       	Relationship Type: Affected
#       	Status:           DONE
#       	Title:             AvnFPS: Grid Monitoring Fails due to unassigned variable
#       
#
# GridMonitor.py
# IFPS grids monitoring module
# Author: George Trojan, SAIC/MDL, August 2003
# last update: 04/20/06
#**
#* 
#* 
#* <pre>
#* SOFTWARE HISTORY
#* Date         Ticket#     Engineer    Description
#* ------------ ----------  ----------- --------------------------
#*                                      Initial creation.
#* Mar 07, 2013 1735        rferrel     Use SiteGridManager to limit calls to server.
##  

import logging, time, cPickle
import Avn, AvnLib, Globals, GridData, MonitorP, TafDecoder, TafGen, JUtil
from com.raytheon.viz.aviation.monitor import SiteGridManager
_Logger = logging.getLogger(Avn.CATEGORY)

##############################################################################
class Monitor(MonitorP.Monitor):
    Source = 'grids'
    Namespace = globals()

    def __init__(self, info, args):
        MonitorP.Monitor.__init__(self, info, args)
        self.decoder = TafDecoder.Decoder()

    def __makeData(self, t):
        try:
            siteID = self.info['ident']
            timeSeconds = long(t)
            if SiteGridManager.needData(timeSeconds) :
                siteIDs = JUtil.javaStringListToPylist(SiteGridManager.getSiteIDs())
                containersMap = GridData.retrieveMapData(siteIDs, timeSeconds)
                SiteGridManager.setContainersMap(containersMap)
                return None
            
            o = SiteGridManager.getData(siteID, timeSeconds)
            if o is None :
                return None
            
            ndata = cPickle.loads(o)
            data = GridData.formatData(siteID, timeSeconds, ndata)
                
            bbb = 'RRA'
            tc=TafGen.TafGen('grid',data,bbb)
            taf=tc.createTaf(False)
            dcd = self.decoder(taf, bbb)
            if 'fatal' in dcd:
                raise Avn.AvnError
            return {'hourly': AvnLib.TafData(dcd['group']), \
                'text': '\n'.join(taf)}
        except Avn.AvnError:    
            return None

    def __compare(self, taf):
        now = time.time()
        msg = None
        result = {}
        currentTime = time.gmtime()
        requestTime = time.mktime((currentTime[0], currentTime[1], currentTime[2], currentTime[3],
                               0, 0, currentTime[6], currentTime[7], currentTime[8])) - time.timezone
        fcst = self.__makeData(requestTime)
        if fcst is None:
            msg = 'Missing grid forecast'       
            _Logger.info('%s for %s' % (msg, self.info['ident']))            
        else:
            result['text'] = fcst['text']
        if msg:
            result['status'] = self.setMissing(msg)
            return result
        try:
            beg = now + 3600.0*self.args['from']
            nhours = self.args['to'] - self.args['from'] + 1
            delta_h = {}
            t = beg
            for n in range(nhours):
                delta_h[n] = MonitorP.applyRules(self.rules, t,
                    taf.hourly.get(t), fcst['hourly'].get(t))
                t += 3600.0
            delta = delta_h[0][:]
            for m in range(len(delta)):
                if delta[m] != 0:   # pick first occurence
                    continue
                for n in range(1, nhours):
                    if delta_h[n][m] != 0:
                        delta[m] = delta_h[n][m]
                        break
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            _Logger.exception(self.info['ident'])
            result['status'] = self.setNIL()
        return result

def _WX(s, wx):
    """Returns TRUE if s contains string wx"""
    if wx == '':
        return 0
    # special case for blowing/drifting snow
    ix = s.find(wx)
    if wx == 'SN' and ix > 1:
        if s[ix-2:ix] in ('BL', 'DR'):
            return 0
    return ix >= 0
        
###############################################################################
# Section containing editable rules
##############################################################################
class DDDelta(MonitorP.Rule):
    """TAF and GRIDs winds directions differ by DD with 
either wind speed >= FF1.
Arguments: DD FF1"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = True
        self.severity = 3
        self.args = {'dd': 3, 'ff': 12}
    
    def method(self, taf, grids):
        try:
            gw, tw = grids['wind'], taf['wind']
            gdd = gw['dd']['prev']
            tddp = tw['dd'].get('prev', None)
            tddo = tw['dd'].get('ocnl', None)
            # variable wind: always matches
            if 'VRB' in (gdd, tddp, tddo):
                return False 
            if tddp is None:
                delta1= 999
            else:
                delta1 = abs(tddp - gdd)
                if delta1 > 180:
                    delta1 = 360 - delta1
            if tddo is None:
                delta2= 999
            else:
                delta2 = abs(tddo - gdd)
                if delta2 > 180:
                    delta2 = 360 - delta2
            delta = min(delta1, delta2)
            if delta == 999:
                raise Avn.AvnMissing
            if delta < self.args['dd']:
                return False
            if gw['ff']['lo'] >= self.args['ff']:
                self.setmsg('Wind directions differ by %d deg, grids wind' \
                    ' exceeds %d kt', delta, self.args['ff'])
                return True
            elif tw['ff']['hi'] >= self.args['ff']:
                self.setmsg('Wind directions differ by %d deg, TAF wind' \
                    ' exceeds %d kt', delta, self.args['ff'])
                return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class FFDelta(MonitorP.Rule):
    """TAF and GRIDs wind speeds/gusts differ by FF with
either wind speed >= FF1.
Arguments: FF FF1"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = True
        self.severity = 3
        self.args = {'ff': 10, 'ff1': 15}

    def method(self, taf, grids):
        try:
            gff = grids['wind']['ff']['hi']
            tfflo = taf['wind']['ff']['lo']
            tffhi = taf['wind']['ff']['hi']
            if gff < tfflo:
                self.setmsg('Wind speeds differ by %d, TAF wind exceeds %d',
                    self.args['ff'], self.args['ff1'])
                return tfflo-gff >= int(self.args['ff']) and \
                    tfflo >= self.args['ff1']
            elif gff > tffhi:
                self.setmsg('Wind speeds differ by %d, grids wind exceeds %d',
                    self.args['ff'], self.args['ff1'])
                return gff-tffhi >= int(self.args['ff']) and \
                    gff >= self.args['ff1']
            else:
                return 0
        except KeyError:
            raise Avn.AvnMissing

class VsbyCatDelta(MonitorP.Rule):
    """TAF and grids visibilities differs by #categories
Arguments: ncat"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'vsby'
        self.unique = True
        self.severity = 3
        self.args = {'ncat': 1}

    def method(self, taf, grids):
        self.setmsg('Visibility categories differs by %d', self.args['ncat'])
        try:
            thresholds = self.sitedata['thresholds']['vsby']
            tclo = Avn.category(taf['vsby']['lo'], thresholds)
            tchi = Avn.category(taf['vsby']['hi'], thresholds)
            gclo = Avn.category(grids['vsby']['lo'], thresholds)
            gchi = Avn.category(grids['vsby']['hi'], thresholds)
            if gchi < tclo:
                return tclo-gchi >= self.args['ncat']
            elif gclo > tchi:
                return gclo-tchi >= self.args['ncat']
            else:
                return False
        except KeyError:
            raise Avn.AvnMissing

class CigCatDelta(MonitorP.Rule):
    """TAF and grids ceiling differs by #categories
Arguments: ncat"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'sky'
        self.unique = True
        self.severity = 3
        self.args = {'ncat': 1}

    def method(self, taf, grids):
        self.setmsg('Ceilings categories differs by %d', self.args['ncat'])
        try:
            thresholds = self.sitedata['thresholds']['cig'] 
            tclo = Avn.category(taf['sky']['lo'], thresholds)
            tchi = Avn.category(taf['sky']['hi'], thresholds)
            gclo = Avn.category(grids['sky']['lo'], thresholds)
            gchi = Avn.category(grids['sky']['hi'], thresholds)
            if gchi < tclo:
                return tclo-gchi >= self.args['ncat']
            elif gclo > tchi:
                return gclo-tchi >= self.args['ncat']
            else:
                return False
        except KeyError:
            raise Avn.AvnMissing
   
class SkyMismatch(MonitorP.Rule):
    """TAF and grids sky cover differs by #categories
Arguments: ncat"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'sky'
        self.unique = True
        self.severity = 3
        self.args = {'ncat': 1}

    def method(self, taf, grids):
        try:
            tclo = taf['cover']['lo']
            tchi = taf['cover']['hi']
            gclo = grids['cover']['lo']
            gchi = grids['cover']['hi']
            self.setmsg('TAF and grids sky cover differs by %d categories',
                self.args['ncat'])
            if gchi < tclo:
                return tclo-gchi >= self.args['ncat']
            elif gclo > tchi:
                return gclo-tchi >= self.args['ncat']
            else:
                return False
        except KeyError:
            raise Avn.AvnMissing

class WxTafDelta(MonitorP.Rule):
    """Weather (any of the list) occurs in TAF and not in GRIDs.
Arguments: WX (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['FZRA', 'FZDZ', 'PL']}

    def method(self, taf, grids):
        self.setmsg('%s forecasted but not in grids',
            ' or '.join(self.args['wx']))
        if 'wx' not in taf or 'pstr' not in taf['wx']:
            return False
        wx = taf['wx']['pstr']  # only prevailing conditions
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            return False
        if 'wx' not in grids:
            return True
        if 'pstr' in grids['wx']:
            wx = grids['wx']['pstr']
            if Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return False
        if 'ostr' in grids['wx']:
            wx = grids['wx']['ostr']
            if Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return False
        return True

class WxGridsDelta(MonitorP.Rule):
    """Weather (any of the list) occurs in GRIDs and not in TAF.
Arguments: WX (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['FZRA', 'FZDZ', 'PL']}

    def method(self, taf, grids):
        self.setmsg('%s occured in grids but not forecasted',
            ' or '.join(self.args['wx']))
        if 'wx' not in grids or 'pstr' not in grids['wx']:
            return 0
        wx = grids['wx']['pstr']    # only prevailing conditions
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            return 0
        if 'wx' not in taf:
            return 1
        if 'pstr' in taf:
            wx = taf['wx']['pstr']
            if Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return 0
        if 'ostr' not in taf['wx']:
            return 1
        wx = taf['wx']['ostr']
        return not Avn.any(self.args['wx'], Avn.curry(_WX, wx))
