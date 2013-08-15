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
#       LtgMonitor.py
#       GFS1-NHD:A6836.0000-SCRIPT;12
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 12 (DELIVERED)
#         Created:  21-APR-2006 11:29:23      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 11 (DELIVERED)
#         Created:  20-APR-2006 15:47:31      TROJAN
#           made compare() wrapper in base class handling exceptions
#           renamed compare() to __compare()
#       
#       Revision 10 (DELIVERED)
#         Created:  23-JAN-2006 08:23:14      TROJAN
#           stdr 956
#       
#       Revision 9 (DELIVERED)
#         Created:  06-JUL-2005 18:16:39      TROJAN
#           spr 6548
#       
#       Revision 8 (DELIVERED)
#         Created:  07-MAY-2005 11:35:06      OBERFIEL
#           Added Item Header Block
#       
#       Revision 7 (DELIVERED)
#         Created:  04-APR-2005 15:51:07      TROJAN
#           spr 6775
#       
#       Revision 6 (DELIVERED)
#         Created:  14-FEB-2005 20:54:50      TROJAN
#           spr 6649
#       
#       Revision 5 (APPROVED)
#         Created:  24-JAN-2005 22:09:28      TROJAN
#           spr 6529
#       
#       Revision 4 (APPROVED)
#         Created:  30-SEP-2004 18:56:02      TROJAN
#           stdr 874
#       
#       Revision 3 (APPROVED)
#         Created:  01-JUL-2004 14:59:35      OBERFIEL
#           Update
#       
#       Revision 2 (DELIVERED)
#         Created:  13-APR-2004 17:09:18      OBERFIEL
#           Turned off alarms for lightning activity
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:30:52      PCMS
#           Initial version
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
# LtgMonitor.py
# lightning data monitoring module
# Author: George Trojan, SAIC/MDL, December 2003
# last update: 04/20/06

import logging, time
import Avn, Globals, MonitorP

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
class Monitor(MonitorP.Monitor):
    Source = 'ltg'
    Namespace = globals()
    Period = 600        # 10 minutes of data, should be less than
                        # LtgThread.Period
    def __compare(self, taf):
        now = time.time()
        #nltgs = Globals.DRC.getLightning(self.info['ident'], now-self.Period)
        from com.raytheon.viz.aviation.monitor import LtgDataMgr
        nltgs = LtgDataMgr.getLtgData(self.info['ident']).getNumberOfStrikes()
        if nltgs is None:   # should not happen
            return {}
        elif nltgs == 0:
            result = {'text': 'No strikes'}
        elif nltgs == 1:
            result = {'text': '1 strike'}
        else:
            result = {'text': '%d strikes' % nltgs}
        try:
            delta = MonitorP.applyRules(self.rules, now, taf.hourly.get(now), 
                nltgs)
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            result['status'] = self.setNIL()
        return result

###############################################################################
class TSObsDelta(MonitorP.Rule):
    """Thunder present but not in TAF.
Arguments: number of strikes"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = True
        self.severity = 3
        self.args = {'num': 3}
    
    def method(self, taf, num):
        if num < self.args['num']:
            return False
        if 'ts' not in taf:
            self.setmsg('Thunder present (%d strikes) but not in TAF', num)
            return True
        return False
