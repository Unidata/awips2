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
#       RadLtgMonitor.py
#       GFS1-NHD:A7816.0000-SCRIPT;1.9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.9 (DELIVERED)
#         Created:  21-APR-2006 11:29:27      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.8 (DELIVERED)
#         Created:  20-APR-2006 15:52:54      TROJAN
#           made compare() called from a base class
#       
#       Revision 1.7 (DELIVERED)
#         Created:  23-JAN-2006 08:23:17      TROJAN
#           stdr 956
#       
#       Revision 1.6 (DELIVERED)
#         Created:  06-JUL-2005 18:16:41      TROJAN
#           spr 6548
#       
#       Revision 1.5 (DELIVERED)
#         Created:  07-MAY-2005 11:37:17      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.4 (DELIVERED)
#         Created:  04-APR-2005 15:51:08      TROJAN
#           spr 6775
#       
#       Revision 1.3 (DELIVERED)
#         Created:  14-FEB-2005 20:54:51      TROJAN
#           spr 6649
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 18:56:03      TROJAN
#           stdr 874
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:43:51      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7124
#       	Action Date:       26-FEB-2007 09:50:32
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: DUP 7123 Exceptions from threads not captured in log files.
#       
#
# RadLtgMonitor.py
# NCEP lightning probability monitoring module
# Author: George Trojan, SAIC/MDL, October 2003
# last update: 04/20/06

import logging, time
import Avn, Globals, MonitorP
import RadLtgData

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
class Monitor(MonitorP.Monitor):
    Source = 'rltg'
    Namespace = globals()

    def __compare(self, taf):
        now = time.time()
        #fcst = Globals.DRC.getFcstLightning(self.info['ident'])
        fcst = RadLtgData.retrieve(self.info['ident'])
        msg = None
        result = {}
        if fcst is None or fcst['to'] < now+3600.0:
            msg = 'Missing lightning forecast'
            _Logger.info('%s for %s' % (msg, self.info['ident']))
        else:   
            f = time.strftime('%d%H', time.gmtime(fcst['from']))
            t = time.strftime('%d%H', time.gmtime(fcst['to']))
            result['text'] = '%s-%sZ  %d%%' % (f, t, fcst['prob'])
        if msg:
            result['status'] = self.setMissing(msg)
            return result
        try:
            t = taf.dcd['vtime']['from']
            n0 = max(int((now-t)//3600.0), 0)
            n1 = int((fcst['to']-t)//3600.0)
            if n0 > n1:
                result['status'] = self.setMissing(msg)
                return result
            delta_h = {}
            t = now
            for n in range(n0, n1+1):
                delta_h[n] = MonitorP.applyRules(self.rules, t, 
                    taf.hourly.get(t), fcst['prob'])
                t += 3600.0
            # if any of delta is 0, then ok
            delta = [0]*len(delta_h[n0])
            for m in range(len(delta)):
                delta[m] = min([delta_h[n][m] for n in range(n0, n1+1)])
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            result['status'] = self.setNIL()
        return result

###############################################################################
class TSInTaf(MonitorP.Rule):
    """Thunder in TAF but low probability in guidance.
Arguments: threshold-probability"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = True
        self.severity = 3
        self.args = {'prob': 20}
    
    def method(self, taf, prob):
        if prob < self.args['prob'] and 'ts' in taf:
            self.setmsg('Thunder in TAF but guidance probability %.0f', prob)
            return True
        return False

class TSNotInTaf(MonitorP.Rule):
    """Thunder not forecast but high probability in guidance.
Arguments: threshold-probability"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = True
        self.severity = 3
        self.args = {'prob': 40}
    
    def method(self, taf, prob):
        if 'ts' not in taf and prob >= self.args['prob']:
            self.setmsg('Thunder not forecast but guidance probability %.0f',
                prob)
            return True
        return False
