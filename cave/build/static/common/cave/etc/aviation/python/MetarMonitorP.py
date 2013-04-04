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
#       MetarMonitorP.py
#       GFS1-NHD:A7808.0000-SCRIPT;1.38
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.38 (DELIVERED)
#         Created:  05-NOV-2009 20:41:00      OBERFIEL
#           Removed superfluous try/except cases when TEMPO conditions
#           are being evaluated.
#       
#       Revision 1.37 (DELIVERED)
#         Created:  21-OCT-2009 13:12:40      OBERFIEL
#           Revised default severity of FuelAlternate and FltCatDelta
#           rules upward.
#           Removed dead code.
#       
#       Revision 1.36 (DELIVERED)
#         Created:  09-OCT-2009 14:50:00      OBERFIEL
#           Removal of much cruft.  Whew!
#       
#       Revision 1.35 (DELIVERED)
#         Created:  28-SEP-2009 08:12:44      OBERFIEL
#           Final implementation of CAC rules.
#       
#       Revision 1.34 (DELIVERED)
#         Created:  24-AUG-2009 14:38:06      OBERFIEL
#           Fixed head/tailwind computations. Minor changes to balloon
#           messages.
#       
#       Revision 1.33 (DELIVERED)
#         Created:  11-AUG-2009 10:41:18      OBERFIEL
#           Minor changes to balloon messages. Made sure that cig/vis
#           thresholds have the same number of breakpoints.
#       
#       Revision 1.32 (DELIVERED)
#         Created:  17-APR-2009 12:05:51      OBERFIEL
#           For 'wx' rules raise AvnUnknwnPcp when UP is detected in
#           the observation.
#       
#       Revision 1.31 (DELIVERED)
#         Created:  12-NOV-2008 22:45:07      OBERFIEL
#           Added 'strict' flag to FltCatDelta rule.
#       
#       Revision 1.30 (DELIVERED)
#         Created:  14-MAR-2008 10:06:38      OBERFIEL
#           Added code to make the use of variability in the METAR
#           remarks section optional.
#       
#       Revision 1.29 (DELIVERED)
#         Created:  19-NOV-2007 20:31:26      OBERFIEL
#           Removed carriage return characters in files
#       
#       Revision 1.28 (INITIALIZE)
#         Created:  19-NOV-2007 11:51:11      GILMOREDM
#           Backed out previous changes to WxMetar class
#       
#       Revision 1.27 (REVIEW)
#         Created:  16-NOV-2007 14:11:26      GILMOREDM
#           added code to WxMetar rule that fixes problem of message
#           appearing incorrectly
#       
#       Revision 1.26 (DELIVERED)
#         Created:  03-OCT-2007 13:56:13      OBERFIEL
#           Change logic on FuelAlternate and AirportOpsThresh to use
#           LT instead of LTE.
#           Changed slightly the logic to calculate differences btw
#           observed and forecasted flight category.
#       
#       Revision 1.25 (DELIVERED)
#         Created:  21-SEP-2007 17:26:08      OBERFIEL
#           Reorganized alert messages for flight category alerts in
#           balloon popup.  Changed wording and added additional
#           dynamic info flt cat msgs.
#       
#       Revision 1.24 (DELIVERED)
#         Created:  18-SEP-2007 10:55:04      OBERFIEL
#           Change logic so that flight category conditions forecasted
#           in the TAF can bracket the observed flight category.
#       
#       Revision 1.23 (DELIVERED)
#         Created:  27-JUN-2007 13:14:15      OBERFIEL
#           MetarMonitorP.py revamped logic and created new rule for
#           Alaska region.
#           TafDecoder.py changed to allow variable AMD LTD element
#           list
#       
#       Revision 1.22 (DELIVERED)
#         Created:  29-MAY-2007 12:19:40      OBERFIEL
#           Updated colophon.  Fixed logic within MetarMonitorP.py to
#           produce same behavior as 3.4 for
#           for vis and cig monitoring.  Fixed AvnWatch to better
#           detect flight category type messages.
#       
#       Revision 1.21 (DELIVERED)
#         Created:  25-MAY-2007 14:27:10      OBERFIEL
#           Update to support additional information in remarks
#       
#       Revision 1.20 (DELIVERED)
#         Created:  15-MAY-2007 08:28:06      GILMOREDM
#           Changed SKC to CIGNO
#       
#       Revision 1.19 (UNDER WORK)
#         Created:  21-MAR-2007 09:54:48      OBERFIEL
#           For AvnClimate.py: Updated the Help text; MetarMonitorP.py:
#           fix potential permission problem;
#           WindRose.py: Added Auto Update feature, by default always
#           on.
#       
#       Revision 1.18 (DELIVERED)
#         Created:  02-MAR-2007 13:31:25      OBERFIEL
#           Removed carriage returns from code.
#           No code changes in this revision.
#       
#       Revision 1.17 (REVIEW)
#         Created:  02-MAR-2007 12:18:45      OBERFIEL
#           Added head/tail wind monitoring rule.
#       
#       Revision 1.16 (DELIVERED)
#         Created:  19-NOV-2006 09:25:25      OBERFIEL
#           Change runway indexing to be consistent with 3.3 and prior
#           code.
#       
#       Revision 1.15 (DELIVERED)
#         Created:  19-NOV-2006 09:19:49      OBERFIEL
#           Corrected indexing of runways to be consistent with 3.2 and
#           previous versions.
#       
#       Revision 1.14 (DELIVERED)
#         Created:  29-AUG-2006 08:59:30      OBERFIEL
#           Corrected arguments for Crosswind monitoring
#       
#       Revision 1.13 (DELIVERED)
#         Created:  02-JUN-2006 10:35:29      TROJAN
#           spr 7161: changed logic in rules FltCatDelta() and
#           FuelAlternate()
#       
#       Revision 1.12 (DELIVERED)
#         Created:  02-JUN-2006 08:57:30      TROJAN
#           spr 7159:changed logic in rules FltCatDelta() and
#           FuelAlternate()
#       
#       Revision 1.11 (DELIVERED)
#         Created:  23-APR-2006 11:54:00      TROJAN
#           spr 7125 - changes to TEMPO and category monitoring
#       
#       Revision 1.10 (DELIVERED)
#         Created:  23-APR-2006 10:52:19      TROJAN
#           spr 7126 - changes to tempo and category alerts
#       
#       Revision 1.9 (DELIVERED)
#         Created:  23-JAN-2006 08:23:14      TROJAN
#           stdr 956
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-JUL-2005 13:01:13      TROJAN
#           spr 6904
#       
#       Revision 1.7 (DELIVERED)
#         Created:  07-MAY-2005 11:35:50      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.6 (DELIVERED)
#         Created:  04-APR-2005 15:51:08      TROJAN
#           spr 6775
#       
#       Revision 1.5 (APPROVED)
#         Created:  25-MAR-2005 12:00:46      TROJAN
#           spr 6749
#       
#       Revision 1.4 (DELIVERED)
#         Created:  24-JAN-2005 15:51:13      TROJAN
#           spr 6259
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 18:56:02      TROJAN
#           stdr 874
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 18:09:23      OBERFIEL
#           Updated to fix problem with VCTS
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:42:18      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7432
#       	Action Date:       06-NOV-2009 08:30:19
#       	Relationship Type: In Response to
#       	Status:           NEXTRELEASE
#       	Title:             OB9.2 AvnFPS - TPO/FuelAlternate Rule Doesn't work
#
#################################
# Date           DR. #   Engineer   Description
# Dec. 27, 2012  15583   zhao       Fixed a bug with Wind Dir. when wind is calm     
#
import copy, logging, math, sets
import Avn, MonitorP

_Logger = logging.getLogger(__name__)

###############################################################################
class Monitor(MonitorP.Monitor):
    Namespace = globals()

###############################################################################
def _WX(s, wx):
    """Returns TRUE if s contains string wx"""
    if wx == '':
        return False
    # special case for blowing/drifting snow
    ix = s.find(wx)
    if wx == 'SN' and ix > 1:
        if s[ix-2:ix] in ('BL', 'DR'):
            return False
    return ix >= 0

def _fmt(cig, vsby):
    if vsby < 0.01:
        vstr = '0SM'
    elif vsby < 0.07:
        vstr = '1/16SM'
    elif vsby < 0.14:
        vstr = '1/8SM'
    elif vsby < 0.28:
        vstr = '1/4SM'
    elif vsby < 0.56:
        vstr = '1/2SM'
    elif vsby < 0.9:
        vstr = '3/4SM'
    elif vsby < 1.1:
        vstr = '1SM'
    elif vsby < 1.3:
        vstr = '1 1/4SM'
    elif vsby < 1.6:
        vstr = '1 1/2SM'
    elif vsby < 1.8:
        vstr = '1 3/4SM'
    elif vsby < 2.1:
        vstr = '2SM'
    elif vsby < 2.7:
        vstr = '2 1/2SM'
    elif vsby < 80.0:
        vstr = '%.0fSM' % vsby
    else:
        vstr = 'P6SM'

    if cig >= Avn.CLEAR:
        return 'CIGNO|%s' % vstr
    else:
        return '%03d|%s' % (cig/100, vstr)

def format_msg(taf, mtr):
    #
    # Print the observation first
    cats = [':METAR: %s, TAF:' % _fmt(mtr['sky']['cig'], mtr['vsby']['vsby'])]
    #
    # If temporary conditions are present, print that out too.
    #
    if taf['vsby']['lo'] < taf['vsby']['hi'] or \
           taf['sky']['lo'] < taf['sky']['hi']:
        cats.append('%s /' % _fmt(taf['sky'].get('ocnl',taf['sky']['prev']),
                    taf['vsby'].get('ocnl',taf['vsby']['prev'])))
    #
    # Append the prevailing conditions in the TAF.
    cats.append(_fmt(taf['sky']['prev'], taf['vsby']['prev']))
    return ' '.join(cats)

###############################################################################
# Section containing editable rules
###############################################################################
class DDDelta(MonitorP.Rule):
    """TAF and METAR winds directions differ by "dd" with either wind speed >= "ff1."

Arguments: dd ff1"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = True
        self.severity = 3
        self.args = {'ff': 10, 'dd': 30}

    def method(self, taf, mtr):
        try:
            mw, tw = mtr['wind'], taf['wind']
            mdd = mw['dd']
            tddp = tw['dd'].get('prev', None)
            tddo = tw['dd'].get('ocnl', None)
            # variable wind: always matches
            if 'VRB' in (mdd, tddp, tddo):
                return False
            if mw['ff']['lo'] == 0:
                return False 
            if tddp is None:
                delta1= 999
            else:
                delta1 = abs(tddp - mdd)
                if delta1 > 180:
                    delta1 = 360 - delta1
            if tddo is None:
                delta2= 999
            else:
                delta2 = abs(tddo - mdd)
                if delta2 > 180:
                    delta2 = 360 - delta2
            delta = min(delta1, delta2)
            if delta == 999:
                raise Avn.AvnMissing
            if delta < self.args['dd']:
                return False
            if mw['ff']['lo'] >= tw['ff']['lo']:
                if mw['ff']['lo'] >= self.args['ff']:
                    self.setmsg('Wind directions differ by %d deg, METAR wind' \
                        ' >= %d KTS', delta, self.args['ff'])
                    return True
            else:
                if tw['ff']['lo'] >= self.args['ff']:
                    self.setmsg('Wind directions differ by %d deg, TAF wind' \
                        ' >= %d KTS', delta, self.args['ff'])
                    return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class FFDelta(MonitorP.Rule):
    """TAF and METAR wind speeds/gusts differ by "ff" with either wind speed >= "ff1."

Arguments: ff ff1"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = True
        self.severity = 3
        self.args = {'ff': 10, 'ff1': 15}

    def method(self, taf, mtr):
        try:
            mff = mtr['wind']['ff']['hi']
            tfflo = taf['wind']['ff']['lo']
            tffhi = taf['wind']['ff']['hi']
            if mff < tfflo:
                self.setmsg('Wind speeds differ by %d, TAF wind >= %d KTS',
                            self.args['ff'], self.args['ff1'])
                return tfflo-mff >= int(self.args['ff']) and \
                    tfflo >= self.args['ff1']
            elif mff > tffhi:
                self.setmsg('Wind speeds differ by %d KTS, METAR wind >= %d KTS',
                            self.args['ff'], self.args['ff1'])
                return mff-tffhi >= int(self.args['ff']) and \
                       mff >= self.args['ff1']
            else:
                return False
        except KeyError:
            raise Avn.AvnMissing

class XFFMetar(MonitorP.Rule):
    """METAR runway cross wind speed >= ff kts

Arguments: runway - index to runway array in site config file
ff - crosswind speed"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = False
        self.severity = 3
        self.args = {'runway': -1, 'ff': 15}

    def method(self, taf, mtr):
        rway = int(self.args['runway'])
        try:
            angle = self.sitedata['geography']['runway'][rway-1]
        except IndexError:
            angle = 0

        if angle <= 0:      # don't bother
            return False
        try:
            mdd = mtr['wind']['dd']
            mff = mtr['wind']['ff']['hi']
            if mdd == 'VRB':
                return mff >= self.args['ff']
            # add 0.1 for roundoff error
            xff = mff * abs(math.sin(math.radians(angle-mdd))) + 0.1
            if xff >= self.args['ff']:
                self.setmsg('Crosswind %.0f KTS on runway %02d >= %d KTS',
                            xff, angle/10, self.args['ff'])
                return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class LFFMetar(MonitorP.Rule):
    """METAR runway head or tail wind meets or exceeds "ff" KT

Arguments: runway = index to runways[] in info.cfg;
+ff = tailwind, -ff = headwind"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = False
        self.severity = 3
        self.args = {'runway': -1, 'ff': 0}

    def method(self, taf, mtr):
        rway = int(self.args['runway'])
        try:
            angle = self.sitedata['geography']['runway'][rway-1]
        except IndexError:
            angle = 0
            
        if angle <= 0:      # don't bother
            return False
        try:
            mdd = mtr['wind']['dd']
            mff = mtr['wind']['ff']['hi']
            if mdd == 'VRB':
                return mff >= self.args['ff']
            #
            # Negative sign is there because runway headings point _towards_ a direction
            # METAR report winds coming _from_ a direction.
            #
            lff = -mff * math.cos(math.radians(angle-mdd)) + 0.1
            if self.args['ff'] > 0:
                if lff >= self.args['ff']:
                    self.setmsg('Tailwind %.0f KTS on runway %02d >= %d KTS',
                                lff, angle/10, self.args['ff'])
                    return True
            elif self.args['ff'] < 0:
                if lff <= self.args['ff']:
                    self.setmsg('Headwind %.0f KTS on runway %02d >= %d KTS',
                                -lff, angle/10, -(self.args['ff']))
                    return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class CigCatDelta(MonitorP.Rule):
    """TAF and METAR ceiling differ by #categories.

Arguments: ncat - number of category differences;
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'sky'
        self.unique = True
        self.severity = 3
        self.args = {'ncat': 1, 'remarks': 'Y'}

    def method(self, taf, mtr):
        try:
            thresholds = self.sitedata['thresholds']['cig']
            msg = 'ncat,thresholds = (%d,%s)' % (self.args['ncat'],thresholds)
            _Logger.debug(msg)
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour
            #
            use_rmks = False
            try:
                if taf['sky'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass

            _Logger.debug({ True: 'Using remarks',False:'Ignoring remarks'}.get(use_rmks))
            #
            # Gather the lowest ceilings
            tclo = Avn.category(taf['sky']['lo'], thresholds)
            mclo = [Avn.category(mtr['sky']['cig'], thresholds)]
            if use_rmks:
                try:
                    mclo.append(Avn.category(mtr['vcig']['lo'], thresholds))
                except KeyError:
                    pass
            #
            # Now gather the highest ceilings
            tchi = Avn.category(taf['sky']['hi'], thresholds)
            mchi = [Avn.category(mtr['sky']['cig'], thresholds)]
            
            if use_rmks:
                try:
                    mchi.append(Avn.category(mtr['vcig']['hi'], thresholds))
                except KeyError:
                    pass
            #
            # If the same category is found between both observation and
            # forecast, return early.
            #
            msg='metar sky categories',mchi,mclo
            _Logger.debug(msg)
            msg='taf sky categories',tchi,tclo
            _Logger.debug(msg)
            
            for mcig in mchi+mclo:
                if mcig == tchi or mcig == tclo:
                    return False
                
            result1 = [(x-tchi)>=self.args['ncat'] for x in mchi if x > tchi]
            msg='result1',result1
            _Logger.debug(msg)

            result2 = [(tclo-x)>=self.args['ncat'] for x in mclo if x < tclo]
            msg='result2',result2
            _Logger.debug(msg)
            
            for result in result1+result2:
                if result:
                    self.setmsg('TAF and METAR differ by %s', { 1:"1 category",
                                                                2:"2 categories",
                                                                3:"3 categories",
                                                                4:"4 categories"}.get(int(self.args['ncat']),
                                                                                      ">4 categories"))
                    return True
            return False
        
        except KeyError:
            raise Avn.AvnMissing

class VsbyCatDelta(MonitorP.Rule):
    """TAF and METAR visibilities differ by #categories, 

Arguments: ncat - number of category differences;
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'vsby'
        self.unique = True
        self.severity = 3
        self.args = {'ncat': 1, 'remarks':'Y'}

    def method(self, taf, mtr):
        try:
            thresholds = self.sitedata['thresholds']['vsby']
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour
            #
            use_rmks = False
            try:
                if taf['vsby'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass
            #
            # Gather the lowest visibilities first
            tvlo = Avn.category(taf['vsby']['lo'], thresholds)
            mvlo = [Avn.category(mtr['vsby']['vsby'], thresholds)]
            if use_rmks:
                try:
                    mvlo.append(Avn.category(mtr['vvsby']['lo'], thresholds))
                except KeyError:
                    pass
            #
            # Now gather the highest visibilities
            tvhi = Avn.category(taf['vsby']['hi'], thresholds)
            mvhi = [Avn.category(mtr['vsby']['vsby'], thresholds)]
            if use_rmks:
                try:
                    mvhi.append(Avn.category(mtr['vvsby']['hi'], thresholds))
                except KeyError:
                    pass            
            #
            # If the same category is found between both observation and
            # forecast, return early.
            #
            for mvis in mvhi+mvlo:
                if mvis == tvhi or mvis == tvlo:
                    return False             
            #
            # See if differences between forecast and observation differ
            # less than the threshold allowed.
            #
            result1 = [(x-tvhi)>=self.args['ncat'] for x in mvhi if x > tvhi]
            result2 = [(tvlo-x)>=self.args['ncat'] for x in mvlo if x < tvlo]
            for result in result1+result2:
                if result:
                    self.setmsg('TAF and METAR differ by %s',{ 1:"1 category",
                                                               2:"2 categories",
                                                               3:"3 categories",
                                                               4:"4 categories"}.get(int(self.args['ncat']),
                                                                                     ">4 categories"))
                    return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class VsbyTafThresh(MonitorP.Rule):
    """TAF visibility <= vsby1 and METAR visibility > vsby2

Arguments: vsby1 vsby2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'vsby'
        self.unique = False
        self.severity = 3
        self.args = {'vsby1': 3.0, 'vsby2': 3.0, 'remarks':'Y'}

    def method(self, taf, mtr):
        try:
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour
            #
            use_rmks = False
            try:
                if taf['vsby'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass

            if taf['vsby']['hi'] <= self.args['vsby1']:
                vsbys = [mtr['vsby']['vsby']]
                
                if use_rmks:
                    try:
                        vsbys.append(mtr['vvsby']['lo'])
                    except KeyError:
                        pass

                for v in vsbys:
                    if v <= self.args['vsby2']:
                        return False
                    
                self.setmsg('TAF visibility <= %.1f and METAR visibility > %.1f',
                            self.args['vsby1'], self.args['vsby2'])
                return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class VsbyMetarThresh(MonitorP.Rule):
    """METAR visibility <= vsby1 and TAF visibility > vsby2

Arguments: vsby1 vsby2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'vsby'
        self.unique = False
        self.severity = 3
        self.args = {'vsby1': 3.0, 'vsby2': 3.0, 'remarks': 'Y'}

    def method(self, taf, mtr):
        #
        # Use METAR RMK information only if its turned on
        use_rmks = False
        try:
            if taf['vsby'].has_key('ocnl'):
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass

        try:            
            if taf['vsby']['lo'] > self.args['vsby2']:
                vsbys = [mtr['vsby']['vsby']]
                
                if use_rmks:
                    try:
                        vsbys.append(mtr['vvsby']['hi'])
                    except KeyError:
                        pass
                
                for v in vsbys:
                    if v > self.args['vsby1']:
                        return False
                    
                self.setmsg('METAR visibility <= %.1f and TAF visibility > %.1f',
                            self.args['vsby1'], self.args['vsby2'])
                return True
            return False
        except KeyError:
            raise Avn.AvnMissing

class CigTafThresh(MonitorP.Rule):
    """TAF ceiling <= cig1 and METAR ceiling > cig2

Arguments: cig1 cig2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'sky'
        self.use_remarks = True
        self.unique = False
        self.severity = 3
        self.args = {'cig1': 3100, 'cig2': 3100, 'remarks':'Y'}

    def method(self, taf, mtr):
        #
        # Use METAR RMK information only if its turned on and there's variability
        # in the TAF for the given hour
        #
        use_rmks = False
        try:
            if taf['sky'].has_key('ocnl'):
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass

        try:
            if taf['sky']['hi'] <= self.args['cig1']:
                cigs = [mtr['sky']['cig']]

                if use_rmks:
                    try:
                        cigs.append(mtr['vcig']['lo'])
                    except KeyError:
                        pass
	                
                    try:
                        if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                            cigs.append(mtr['vsky']['cig'])
                    except KeyError:
                        pass
                                    
                for c in cigs:
                    if c <= self.args['cig2']:
                        return False
                    
                self.setmsg('TAF ceiling <= %d and METAR ceiling > %d',
                            self.args['cig1'], self.args['cig2'])
                return True
            return False            
        except KeyError:
            raise Avn.AvnMissing

class CigMetarThresh(MonitorP.Rule):
    """METAR ceiling <= cig1 and TAF ceiling > cig2

Arguments: cig1 cig2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'sky'
        self.unique = False
        self.severity = 3
        self.args = {'cig1': 3100, 'cig2': 3100, 'remarks':'Y'}

    def method(self, taf, mtr):
        #
        # Use METAR RMK information only if its turned on and there's variability
        # in the TAF for the given hour
        #
        use_rmks = False
        try:
            if taf['sky'].has_key('ocnl'):
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass
        
        try:                    
            if taf['sky']['lo'] > self.args['cig2']:
                cigs = [mtr['sky']['cig']]
                
                if use_rmks:
                    try:
                        cigs.append(mtr['vcig']['hi'])
                        cigs.append(mtr['vcig']['lo'])
                    except KeyError:
                        pass
                
                    try:
                        if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                            cigs.append(mtr['vsky']['cig'])
                    except KeyError:
                        pass

                for c in cigs:
                    if c > self.args['cig1']:
                        return False
                    
                self.setmsg('METAR ceiling <= %d and TAF ceiling > %d',
                            self.args['cig1'], self.args['cig2'])
                return True
            return False

        except KeyError:
            raise Avn.AvnMissing

class WxTafDelta(MonitorP.Rule):
    """Weather (any of the list) occurs in TAF and not in METAR.
Arguments: wx (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['FZRA', 'FZDZ', 'PL']}

    def method(self, taf, mtr):
        if 'wx' in mtr and mtr['wx']['str'][:2] == 'UP':
            raise Avn.AvnUnknwnPcp
        
        if 'wx' not in taf or 'pstr' not in taf['wx']:
            return False
        wx = taf['wx']['pstr']  # only prevailing conditions
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            return False
        
        self.setmsg('%s forecasted but does not occurs in METAR',
                    ' or '.join(self.args['wx']))
        if 'wx' not in mtr:
            return True
        wx = mtr['wx']['str']
        return not Avn.any(self.args['wx'], Avn.curry(_WX, wx))

class WxMetarDelta(MonitorP.Rule):
    """Weather (any of the list) occurs in METAR and not in TAF.

Arguments: wx (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['FZRA', 'FZDZ', 'PL']}

    def method(self, taf, mtr):
        if 'wx' not in mtr:
            return False
        wx = mtr['wx']['str']
        if wx[:2] == 'UP':
            raise Avn.AvnUnknwnPcp
        
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            return False
        # special case: VCTS
        if 'TS' in self.args['wx'] and 'ts' in taf:
            return False

        self.setmsg('%s occurred in METAR but not forecasted',
                    ' or '.join(self.args['wx']))
        if 'wx' not in taf:
            return True
        if 'pstr' in taf['wx']:
            wx = taf['wx']['pstr']
            if Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return False
        if 'ostr' not in taf['wx']:
            return True
        wx = taf['wx']['ostr']
        return not Avn.any(self.args['wx'], Avn.curry(_WX, wx))

class WxMetar(MonitorP.Rule):
    """Checks for occurrence of weather in METAR.
Arguments: WX (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['TS']}

    def method(self, taf, mtr):
        if 'wx' not in mtr:
            return False
        self.setmsg('%s occurred in METAR', ' or '.join(self.args['wx']))
        wx = mtr['wx']['str']
	if self.wx_not_in_mtr(wx):
	    return False
        return Avn.any(self.args['wx'], Avn.curry(_WX, wx))

    def wx_not_in_mtr(self,wx):
	for el in self.args['wx']:
	    if el not in wx:
		continue
	    else:
		return False #weather found in wx
	return True #weather not found in wx

class WxVsbyDelta(MonitorP.Rule):
    """Checks for occurrence of weather in METAR while not in TAF,
       with visibility <= vsby.

Arguments: vsby, wx (list) 
remarks - use variability information, if given, in the METAR remarks"""

    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'vsby': 3.0, 'wx': ['DZ'], 'remarks':'Y'}

    def method(self, taf, mtr):
        #
        # Use METAR RMK information only if its turned on
        use_rmks = False
        try:
            if taf['vsby'].has_key('ocnl'):
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass

        try:
            vsbys = [mtr['vsby']['vsby']]
            if use_rmks:
                try:
                    vsbys.append(mtr['vvsby']['hi'])
                    vsbys.append(mtr['vvsby']['lo'])
                except KeyError:
                    pass
            
            for v in vsbys: 
                if v > self.args['vsby']:
                    return False
        except KeyError:
            raise Avn.AvnMissing
        
        if 'wx' not in mtr:
            return False
        wx = mtr['wx']['str']
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            return False

        self.setmsg('%s occurred in METAR but not forecasted\n'
                    'visibility <= %.1f', ' or '.join(self.args['wx']), 
                    self.args['vsby'])
        
        if 'wx' not in taf:
            return True
        if 'pstr' in taf['wx']:
            wx = taf['wx']['pstr']
            if Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return False
        if 'ostr' not in taf['wx']:
            return True
        wx = taf['wx']['ostr']
        return not Avn.any(self.args['wx'], Avn.curry(_WX, wx))

class FltCatDelta(MonitorP.Rule):
    """Comparing TAF and observations with respect to aviation flight categories. Severity and
message determined by algorithm
Arguments: remarks - yes to use variability information, if given in the METAR remarks
           strict - yes to alert if forecasted and observed flight category do not match exactly"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = True
        self.args = {'remarks':'True','strict':'True'}
        self.severity = 3

    def method(self, taf, mtr):
        
        try:
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour
            #
            use_rmks = False
            try:
                if taf['vsby'].has_key('ocnl') or taf['sky'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass
            #
            # Check to see if exact matching is desired.
            try:
                if type(self.args['strict']) == type(' '):
                    strict = self.args['strict'].lower()[0] in ['a','y','t','1']
                else:
                    strict = self.args['strict'] == 1
            except KeyError:
                strict = True

            cig_thresholds = self.sitedata['thresholds']['cig']
            vis_thresholds = self.sitedata['thresholds']['vsby']
            c=len(cig_thresholds)
            v=len(vis_thresholds)
            if v > c:
                cig_thresholds=[-1]*(v-c)+self.sitedata['thresholds']['cig']
            elif v < c:
                vis_thresholds=[-1]*(c-v)+self.sitedata['thresholds']['vis']
            
            mCset = sets.Set()
            mVset = sets.Set()
            
            mVset.add(Avn.category(mtr['vsby']['vsby'], vis_thresholds))

            if use_rmks:
                try:
                    mVset.add(Avn.category(mtr['vvsby']['lo'], vis_thresholds))
                    mVset.add(Avn.category(mtr['vvsby']['hi'], vis_thresholds))
                except KeyError:
                    pass
            
            mCset.add(Avn.category(mtr['sky']['cig'], cig_thresholds))
            
            if use_rmks:
                try:
                    mCset.add(Avn.category(mtr['vcig']['lo'], cig_thresholds))
                    mCset.add(Avn.category(mtr['vcig']['hi'], cig_thresholds))
                except KeyError:
                    pass

                try:
                    if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                        mCset.add(Avn.category(mtr['vsky']['cig'], cig_thresholds))
                except KeyError:
                    pass
            #
            # Observation can span a range of categories
            lo_mtrFltCat = min(min(mCset),min(mVset))
            hi_mtrFltCat = min(max(mCset),max(mVset))
            #
            prev_list = [Avn.category(taf['vsby']['prev'],vis_thresholds),
                         Avn.category(taf['sky']['prev'],cig_thresholds)]
            t_p_cat = min(prev_list)            
            ocnl_list = []
            try:
                ocnl_list.append(Avn.category(taf['vsby']['ocnl'],vis_thresholds))
            except KeyError:
                ocnl_list.append(prev_list[0])

            try:
                ocnl_list.append(Avn.category(taf['sky']['ocnl'],cig_thresholds))
            except KeyError:
                ocnl_list.append(prev_list[1])
                
            t_o_cat = min(ocnl_list)
            #
            # If prevailing or temporary forecast brackets the observation
            # w.r.t flight category
            #            
            if not strict and (min(t_p_cat,t_o_cat) <= lo_mtrFltCat <= max(t_p_cat,t_o_cat) or \
                               min(t_p_cat,t_o_cat) <= hi_mtrFltCat <= max(t_p_cat,t_o_cat)):
                
                if prev_list[0] in mVset and prev_list[1] in mCset:
                    return False
                
                if ocnl_list[0] in mVset and ocnl_list[1] in mCset:
                    return False

                tVis, tCig = prev_list[0],prev_list[1]
                group = 'Prevailing'
                
                pmin = min([abs(prev_list[0]-m) for m in mVset]) + \
                       min([abs(prev_list[1]-m) for m in mCset])

                omin = min([abs(ocnl_list[0]-m) for m in mVset]) + \
                       min([abs(ocnl_list[1]-m) for m in mCset])

                if pmin > omin:
                    tVis, tCig = ocnl_list[0],ocnl_list[1]
                    group = 'Occasional'
                    
                items = []
                if tVis not in mVset:
                    items.append('visibility')
                if tCig not in mCset:
                    items.append('ceiling')
                #
                # If there was a disagreement on ceiling or visibility
                # category, raise a minor flag
                #
                if items:
                    if len(items) == 1:
                        self.severity = 2   #Light green
                        self.setmsg('%s %s category differs %s', 
                                    group, items[0],
                                    format_msg(taf, mtr))
                    else:
                        self.severity = 3   #Yellow
                        self.setmsg('%s %s categories differ %s', 
                                    group, ' and '.join(items),
                                    format_msg(taf, mtr))                        
                    return True
            #
            # For those situations where flight categories of forecast and
            # observations don't agree
            #
            # Get the lowest (worse) flight category forecasted
            taf_lo = min(t_p_cat,t_o_cat)
            if hi_mtrFltCat > taf_lo > lo_mtrFltCat:
                delta_lo = taf_lo - lo_mtrFltCat
            else:
                delta_lo = min(abs(lo_mtrFltCat-taf_lo),
                               abs(hi_mtrFltCat-taf_lo))
            #
            # A generic message first
            self.setmsg('Flight categories differ %s', format_msg(taf, mtr))
            #
            # The severity color is dependent on the difference between what
            # is observed and forecasted.
            #
            if delta_lo > 3:
                self.severity = 6    #Purple
            elif delta_lo == 3:
                self.severity = 5    #Red
            elif delta_lo == 2:
                self.severity = 4    #Orange
            elif delta_lo == 1:
                self.severity = 3    #Yellow
            else:
                t_p_vis, t_p_cig = prev_list[0], prev_list[1]
                t_o_vis, t_o_cig = ocnl_list[0], ocnl_list[1]
                t_vis = min(t_p_vis, t_o_vis)
                t_cig = min(t_p_cig, t_o_cig)
                items = []
                if t_cig not in mCset:
                    items.append('ceiling')
                if t_vis not in mVset:
                    items.append('visibility')
                #
                # If there was a disagreement on ceiling or visibility
                # category, raise a minor flag
                #
                if items:
                    self.severity = 2 # Light green
                    items[0] = items[0].capitalize()
                    if len(items) == 1:
                        self.setmsg('%s category differs %s',
                                    items[0], format_msg(taf, mtr))
                    else:
                        self.setmsg('%s categories differ %s',
                                    ' and '.join(items),
                                    format_msg(taf, mtr))
                    return True
                return False
            return True
        except KeyError:
            raise Avn.AvnMissing

class FuelAlternate(MonitorP.Rule):
    """Either TAF and METAR weather falls below alternate fuel requirements.

Arguments: vsby, cig - ceiling and visibility thresholds for alternate fuel loading for aircaft
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = True
        self.severity = 3
        self.args = {'vsby': 3, 'cig': 2000, 'remarks':'Y'}

    def method(self, taf, mtr):
        try:
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour
            #
            use_rmks = False
            try:
                if taf['vsby'].has_key('ocnl') or taf['sky'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass

            mvlist = [mtr['vsby']['vsby']]
            if use_rmks:
                try:
                    mvlist.append(mtr['vvsby']['lo'])
                except KeyError:
                    pass

            mclist = [mtr['sky']['cig']]
            if use_rmks:
                try:
                    mclist.append(mtr['vcig']['lo'])
                except KeyError:
                    pass

            if use_rmks:
                try:
                    if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                        mclist.append(mtr['vsky']['cig'])
                except KeyError:
                    pass

            metar = min(mvlist) < self.args['vsby'] or min(mclist) < self.args['cig']
            
            tvlist = [taf['vsby']['prev']]
            try:
                tvlist.append(taf['vsby']['ocnl'])
            except KeyError:
                pass
            
            tclist = [taf['sky']['prev']]
            try:
                tclist.append(taf['sky']['ocnl'])
            except KeyError:
                pass

            fcst = min(tvlist) < self.args['vsby'] or min(tclist) < self.args['cig']

            if metar and fcst:
                return False
            
            if metar:
                self.setmsg('METAR LT %03d|%sSM requires additional fuel & alternate airport %s',
                            int(self.args['cig'])/100,self.args['vsby'],format_msg(taf,mtr))
                return True
                                 
            if fcst:
                self.setmsg('TAF LT %03d|%sSM requires additional fuel & alternate airport %s',
                            int(self.args['cig'])/100,self.args['vsby'],format_msg(taf,mtr))
                return True
            return False
        
        except KeyError:
            raise Avn.AvnMissing

class AirportOpsThresh(MonitorP.Rule):
    """Alert when either TAF or METAR cig/vis falls below an airport
operations criteria and obs and forecast disagree.

Arguments: vsby, cig - visibility/ceiling thresholds that affect operations.
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = True
        self.severity = 5
        self.args = {'vsby': 1, 'cig': 500, 'remarks': 'Y'}

    def method(self, taf, mtr):
        try:
            #
            # Use METAR RMK information only if its turned on and there's variability
            # in the TAF for the given hour a==affirmative,y=yes,t=true
            #
            use_rmks = False
            try:
                if taf['vsby'].has_key('ocnl') or taf['sky'].has_key('ocnl'):
                    if type(self.args['remarks']) == type(' '):
                        use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                    else:
                        use_rmks = self.args['remarks'] == 1
            except KeyError:
                pass

            mvlist = [mtr['vsby']['vsby']]
            if use_rmks:
                try:
                    mvlist.append(mtr['vvsby']['lo'])
                except KeyError:
                    pass

            mclist = [mtr['sky']['cig']]
            if use_rmks:
                try:
                    mclist.append(mtr['vcig']['lo'])
                except KeyError:
                    pass

            if use_rmks:
                try:
                    if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                        mclist.append(mtr['vsky']['cig'])
                except KeyError:
                    pass

            metar = min(mvlist) < self.args['vsby'] or min(mclist) < self.args['cig']
            
            tvlist = [taf['vsby']['prev']]
            try:
                tvlist.append(taf['vsby']['ocnl'])
            except KeyError:
                pass
            
            tclist = [taf['sky']['prev']]
            try:
                tclist.append(taf['sky']['ocnl'])
            except KeyError:
                pass

            fcst = min(tvlist) < self.args['vsby'] or min(tclist) < self.args['cig']
            #
            if metar and fcst:
                return False
            elif metar or fcst:
                if metar:
                    self.setmsg('METAR LT airport operations criteria (%03d|%sSM) %s',
                                int(self.args['cig'])/100,self.args['vsby'],format_msg(taf,mtr))
                if fcst:
                    self.setmsg('TAF LT airport operations criteria (%03d|%sSM) %s',
                                int(self.args['cig'])/100,self.args['vsby'],format_msg(taf,mtr))
                return True
            else:
                return False
  
        except KeyError:
            raise Avn.AvnMissing

class CAC_FltCatDelta(MonitorP.Rule):
    """Comparing TAF and observations with respect to ceiling and visibility categories.
Severity and message returned determined by algorithm.  This rule satisfies CAC
requirements.

Arguments: remarks - yes to use variability information, if given in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = True
        self.args = {'remarks':'True'}
        self.severity = 3

    def method(self, taf, mtr):        
        try:
            tempoCheck = taf.get('tempoCheck', False)
            use_rmks = [False, False]
            try:
                if type(self.args['remarks']) == type(' '):
                    use_rmks[0] = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks[0] = self.args['remarks'] == 1
                if taf['vsby'].has_key('ocnl') or taf['sky'].has_key('ocnl'):
                    use_rmks[1] = use_rmks[0]
                    pass
            except KeyError:
                pass

            cig_thresholds = self.sitedata['thresholds']['cig']
            vis_thresholds = self.sitedata['thresholds']['vsby']
            c=len(cig_thresholds)
            v=len(vis_thresholds)
            if v > c:
                cig_thresholds=[-1]*(v-c)+self.sitedata['thresholds']['cig']
            elif v < c:
                vis_thresholds=[-1]*(c-v)+self.sitedata['thresholds']['vis']
            
            mCset = sets.Set()
            mVset = sets.Set()
            
            mVset.add(Avn.category(mtr['vsby']['vsby'], vis_thresholds))
            mCset.add(Avn.category(mtr['sky']['cig'], cig_thresholds))

            mCset1 = copy.deepcopy(mCset)
            mVset1 = copy.deepcopy(mVset)

            mVset1.add(
                Avn.category(
                    mtr.get('vvsby', {}).get('lo', 10.0), vis_thresholds))
            mVset1.add(
                Avn.category(
                    mtr.get('vvsby', {}).get('hi', 10.0), vis_thresholds))

            mCset1.add(
                Avn.category(
                    mtr.get('vcig', {}).get('lo', 99999), cig_thresholds))
            mCset1.add(
                Avn.category(
                    mtr.get('vcig', {}).get('hi', 99999), cig_thresholds))

            try:
                if max(mtr['vsky']['cvr1'], mtr['vsky']['cvr2']) > 2:
                    mCset1.add(
                        Avn.category(mtr['vsky']['cig'], cig_thresholds))
            except KeyError:
                pass

            # Observation can span a range of categories
            lo_mtrFltCat = min(min(mCset),min(mVset))
            hi_mtrFltCat = min(max(mCset),max(mVset))
            #
            try:
                tafVisPrev = Avn.category(taf['vsby']['prev'], vis_thresholds)
            except KeyError:
                raise Avn.AvnMissing
            try:
                tafCigPrev = Avn.category(taf['sky']['prev'], cig_thresholds)
            except KeyError:
                raise Avn.AvnMissing

            prev_list = [tafVisPrev, tafCigPrev]
            t_p_cat = min(prev_list)
            ocnl_list = []
            try:
                ocnl_list.append(Avn.category(taf['vsby']['ocnl'],vis_thresholds))
            except KeyError:
                ocnl_list.append(prev_list[0])

            try:
                ocnl_list.append(Avn.category(taf['sky']['ocnl'],cig_thresholds))
            except KeyError:
                ocnl_list.append(prev_list[1])
                
            t_o_cat = min(ocnl_list)
            #
            # For those situations where flight categories of forecast and
            # observations don't agree
            #
            # Get the lowest (worse) flight category forecasted
            taf_lo = min(t_p_cat,t_o_cat)
            if hi_mtrFltCat > taf_lo > lo_mtrFltCat:
                delta_lo = taf_lo - lo_mtrFltCat
            else:
                delta_lo = min(abs(lo_mtrFltCat-taf_lo),
                               abs(hi_mtrFltCat-taf_lo))
            #
            if use_rmks[1]:
                if delta_lo > 0:
                    lo_mtrFltCat = min(min(mCset1),min(mVset1))
                    hi_mtrFltCat = min(max(mCset1),max(mVset1))
                    # Get the lowest (worse) flight category forecasted
                    if hi_mtrFltCat > taf_lo > lo_mtrFltCat:
                        if taf_lo - lo_mtrFltCat == 0:
                            if tempoCheck:
                                return False
                            self.severity = 2
                            self.setmsg('METAR variable remarks verified the TEMPO flight category')
                            return True
                    else:
                        if min(abs(lo_mtrFltCat-taf_lo),
                                       abs(hi_mtrFltCat-taf_lo)) == 0:
                            if tempoCheck:
                                return False
                            self.severity = 2
                            self.setmsg('METAR variable remarks verified the TEMPO flight category')
                            return True

            # The severity color is dependent on the difference between what
            # is observed and forecasted.
            #
            if delta_lo > 3:
                self.severity = 6    #Purple
            elif delta_lo == 3:
                self.severity = 5    #Red
            elif delta_lo == 2:
                self.severity = 4    #Orange
            elif delta_lo == 1:
                self.severity = 3    #Yellow
            else:
                if use_rmks[0]:
                    if min(min(mCset1), min(mVset1)) < \
                       min(min(mCset), min(mVset)):
                        if tempoCheck:
                            return False
                        self.severity = 2
                        self.setmsg('METAR variable remarks are in a lower flight category '\
                                    'than the lowest TAF category')
                        return True

                t_p_vis, t_p_cig = prev_list[0], prev_list[1]
                t_o_vis, t_o_cig = ocnl_list[0], ocnl_list[1]
                t_vis = min(t_p_vis, t_o_vis)
                t_cig = min(t_p_cig, t_o_cig)
                items = []
                if t_cig not in mCset:
                    items.append('ceiling')
                if t_vis not in mVset:
                    items.append('visibility')
                #
                # If there was a disagreement on ceiling or visibility
                # category, raise a minor flag
                #
                if items:
                    if tempoCheck:
                        return False
                    self.severity = 2 # Light green
                    items[0] = items[0].capitalize()
                    if len(items) == 1:
                        self.setmsg('%s category differs %s',
                                    items[0],format_msg(taf, mtr))
                    else:
                        self.setmsg('%s categories differ %s',
                                    ' and '.join(items),format_msg(taf, mtr))
                    return True
                return False
            # A generic message
            if not tempoCheck:
                self.setmsg('Categories differ %s ',format_msg(taf, mtr))
                
            return True
        except KeyError:
            raise Avn.AvnMissing

class CAC_AirportOpsThresh(MonitorP.Rule):
    """Alert when either TAF or METAR cig/vis falls below an airport
operations criteria. This rule satisfies CAC requirements.

Arguments: vsby, cig - visibility/ceiling thresholds that affect operations.
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = True
        self.severity = 5
        self.args = {'vsby': 1, 'cig': 500, 'remarks': 'Y'}

    def method(self, taf, mtr):
        try:
            cigThresh = self.args['cig']
            visThresh = self.args['vsby']

            cigVisThreshStr = _fmt(cigThresh, visThresh)
            if (cigThresh == 600  or cigThresh == 800) and \
                visThresh == 2:
                msg = 'below use as an alternate airfield minimums'
            elif cigThresh == self.sitedata['thresholds']['cig'][0] and \
                 visThresh == self.sitedata['thresholds']['vsby'][0]:
                msg = 'below airfield minimums'
            else:
                msg = ''
            tempoCheck = taf.get('tempoCheck', False)
            use_rmks = [False, False]
            try:
                if type(self.args['remarks']) == type(' '):
                    use_rmks[0] = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks[0] = self.args['remarks'] == 1
                if taf['vsby'].has_key('ocnl') or taf['sky'].has_key('ocnl'):
                    use_rmks[1] = use_rmks[0]
            except KeyError:
                pass
            except IndexError:
                pass

            vVis = mtr.get('vvsby', {}).get('lo', 10.0)
            vCig = mtr.get('vcig', {}).get('lo', 99999)
            vCvr1 = mtr.get('vsky', {}).get('cvr1', -1)
            vCvr2 = mtr.get('vsky', {}).get('cvr2', -1)
            if max(vCvr1, vCvr2) > 2:
                vCig = min(
                    vCig, mtr.get('vsky', {}).get('cig', 99999))

            mvlist = [mtr['vsby']['vsby']]
            mclist = [mtr['sky']['cig']]
            metar = min(mvlist) < visThresh or min(mclist) < cigThresh

            try:
                tvlist = [taf['vsby']['prev']]
            except KeyError:
                raise Avn.AvnMissing
            try:
                tvlist.append(taf['vsby']['ocnl'])
            except KeyError:
                pass

            try:
                tclist = [taf['sky']['prev']]
            except KeyError:
                raise Avn.AvnMissing
            try:
                tclist.append(taf['sky']['ocnl'])
            except KeyError:
                pass

            fcst = min(tvlist) < visThresh or min(tclist) < cigThresh
            #
            # 4 cases:
            # fcst and metar are True: Good, no need to check remarks
            # fcst and metar are False: Good, but check remarks to see if they
            #                           they are below threshold
            # fcst False and metar True: Bad, no need to check remarks
            # fcst True and metar False: Bad, but check remarks to see if they
            #                            save the TAF.
            if metar and fcst:
                return False

            if not metar and not fcst:
                if use_rmks[0]:
                    # Always check to see if the variable remarks, if present,
                    # put the METAR in a lower category than the lowest TAF
                    # category.
                    if vVis < visThresh or vCig < cigThresh:
                        if tempoCheck:
                            # "cat" box alerts and messages not appropriate for
                            # "tpo" box checks.
                            return False
                        self.severity = 2
                        self.setmsg('METAR variable remarks LT %s (%s)',
                                    cigVisThreshStr,msg)
                        return True
                return False

            if metar:
                if not tempoCheck:
                    # Create "cat" box messages only when doing "cat" box
                    # checking.
                    self.setmsg('METAR LT %s %s %s',cigVisThreshStr,msg,
                                format_msg(taf, mtr))
                return True

            if fcst:
                if use_rmks[1]:
                    mvlist.append(vVis)
                    mclist.append(vCig)
                    metar1 = min(mvlist) < visThresh or min(mclist) < cigThresh
                    if metar1 and fcst:
                        if tempoCheck:
                            return False
                        self.severity = 2
                        self.setmsg('METAR variable remarks verified the TEMPO LT %s (%s)',
                                    cigVisThreshStr, msg)
                        return True
                    
                if not tempoCheck:
                    self.setmsg('TAF LT %s %s %s', cigVisThreshStr,msg,
                                format_msg(taf, mtr))
                return True
            return False

        except KeyError:
            raise Avn.AvnMissing

class CAC_VsbyMetarThresh(MonitorP.Rule):
    """METAR visibility < vsby1 and TAF visibility >= vsby2.  This rule satisfies
CAC requirements.
    
Arguments: vsby1 vsby2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = False
        self.severity = 3
        self.args = {'vsby1': 3.0, 'vsby2': 3.0, 'remarks': 'Y'}

    def method(self, taf, mtr):
        tempoCheck = taf.get('tempoCheck', False)
        use_rmks = False
        try:
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass
        except IndexError:
            pass

        # 4 cases:
        # Both below: Good, remarks do not need to be checked.
        # METAR above, TAF below: Good, remarks do not need to be checked.
        # METAR below, TAF above: Bad, remarks do no need to be checked.
        # Both above: Good, but check remarks to see if that makes METAR
        #             below. If so, light green.
        try:
            tempoCheck = taf.get('tempoCheck', False)
            fcst = taf['vsby']['lo']
            metar = mtr['vsby']['vsby']
            tafThresh = self.args['vsby2']
            metarThresh = self.args['vsby1']
            tafThreshStr = _fmt(99999, tafThresh).split('|')[1]
            metarThreshStr = _fmt(99999, metarThresh).split('|')[1]

            if (fcst < tafThresh and metar < metarThresh) or \
                (fcst < tafThresh and metar >= metarThresh):
                    return False

            if fcst >= tafThresh and metar < metarThresh:
                if not tempoCheck:
                    self.setmsg('METAR visibility < %s and TAF visibility >= %s %s',
                                metarThreshStr, tafThreshStr,format_msg(taf, mtr))
                return True

            if fcst >= tafThresh and metar >= metarThresh:
                if use_rmks:
                    if tempoCheck:
                        return False
                    metar1 = mtr['vsby'].get('vvsby', 99999.0)
                    if metar1 < metarThresh:
                        self.severity = 2
                        self.setmsg('METAR variable remarks < %s while TEMPO > %s %s',
                                    metarThreshStr,tafThreshStr,format_msg(taf, mtr))
                        return True
                return False
        except KeyError:
            raise Avn.AvnMissing

class CAC_VsbyTafThresh(MonitorP.Rule):
    """TAF visibility < vsby1 and METAR visibility >= vsby2.  This rule satisfies
CAC requirements.

Arguments: vsby1 vsby2
remarks - use variability information, if given, in the METAR remarks"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'cat'
        self.unique = False
        self.severity = 3
        self.args = {'vsby1': 3.0, 'vsby2': 3.0, 'remarks':'Y'}

    def method(self, taf, mtr):
        tempoCheck = taf.get('tempoCheck', False)
        # Use METAR RMK information only if it's turned on and there is
        # variablility in the TAF.
        use_rmks = False
        try:
            if taf['vsby'].has_key('ocnl'):
                if type(self.args['remarks']) == type(' '):
                    use_rmks = self.args['remarks'].lower()[0] in ['a','y','t','1']
                else:
                    use_rmks = self.args['remarks'] == 1
        except KeyError:
            pass
        except IndexError:
            pass

        # 4 cases:
        # Both below: Good, no need to check remarks
        # TAF above, METAR below: Good, no need to check remarks
        # Both above: Good, no need to check remarks
        # TAF below, METAR above: Bad, but check remarks to see if METAR "saves"
        # the TAF
        try:
            fcst = taf['vsby']['lo']
            metar = mtr['vsby']['vsby']
            tafThresh = self.args['vsby1']
            metarThresh = self.args['vsby2']
            tafThreshStr = _fmt(99999, tafThresh).split('|')[1]
            metarThreshStr = _fmt(99999, metarThresh).split('|')[1]

            if (fcst < tafThresh and metar < metarThresh) or \
               (fcst >= tafThresh and metar < metarThresh) or \
               (fcst >= tafThresh and metar >= metarThresh):
                return False

            # Only the bad case is left
            if use_rmks:
                metar = mtr['vsby'].get('vvsby', 99999.0)
                if metar < metarThresh:
                    self.severity = 2
                    self.setmsg('METAR variable remarks < %s verified TEMPO visibility < %s %s',
                                metarThreshStr,tafThreshStr,format_msg(taf, mtr))
                    return True

            if not tempoCheck:
                self.setmsg('TAF visibility < %s and METAR visibility >= %s %s',
                            tafThreshStr,metarThreshStr,format_msg(taf, mtr))
            return True
        except KeyError:
            raise Avn.AvnMissing

class CAC_WxTafDelta(MonitorP.Rule):
    """Weather (any of the list) occurs in the TAF, conditional group included, but not in METAR.
       
Arguments: wx (list)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = False
        self.severity = 3
        self.args = {'wx': ['FZRA', 'FZDZ', 'PL']}

    def method(self, taf, mtr):
        if 'wx' in mtr and mtr['wx']['str'][:2] == 'UP':
            raise Avn.AvnUnknwnPcp

        if 'wx' not in taf:
            return False
        wx = taf['wx'].get('pstr', '')
        if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
            wx = taf['wx'].get('ostr', '')
            if not Avn.any(self.args['wx'], Avn.curry(_WX, wx)):
                return False

        self.setmsg('%s forecasted but does not occurs in METAR',
                    ' or '.join(self.args['wx']))
        if 'wx' not in mtr:
            return True
        wx = mtr['wx']['str']
        return not Avn.any(self.args['wx'], Avn.curry(_WX, wx))
