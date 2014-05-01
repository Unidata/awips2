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
#       LLWSMonitor.py
#       GFS1-NHD:A8132.0000-SCRIPT;1.10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.10 (DELIVERED)
#         Created:  21-APR-2006 11:29:22      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.9 (DELIVERED)
#         Created:  20-APR-2006 15:47:30      TROJAN
#           made compare() wrapper in base class handling exceptions
#           renamed compare() to __compare()
#       
#       Revision 1.8 (DELIVERED)
#         Created:  23-JAN-2006 08:23:13      TROJAN
#           stdr 956
#       
#       Revision 1.7 (APPROVED)
#         Created:  28-DEC-2005 16:35:20      OBERFIEL
#           Fixed text posting bug
#       
#       Revision 1.6 (APPROVED)
#         Created:  15-AUG-2005 14:53:47      OBERFIEL
#           Increased timeout to 75 minutes
#       
#       Revision 1.5 (DELIVERED)
#         Created:  06-JUL-2005 18:16:38      TROJAN
#           spr 6548
#       
#       Revision 1.4 (DELIVERED)
#         Created:  07-MAY-2005 11:34:40      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  04-APR-2005 15:51:06      TROJAN
#           spr 6775
#       
#       Revision 1.2 (DELIVERED)
#         Created:  14-FEB-2005 20:54:49      TROJAN
#           spr 6649
#       
#       Revision 1.1 (APPROVED)
#         Created:  10-NOV-2004 17:27:18      OBERFIEL
#           date and time created 11/10/04 17:27:06 by oberfiel
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
# LLWSMonitor.py
# low level wind shear data monitoring module
# Author: George Trojan, SAIC/MDL, October 2004
# last update: 04/20/06

import logging, time
import Globals, MonitorP, Avn
import LLWSData

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
class Monitor(MonitorP.Monitor):
    Source = 'llws'
    Namespace = globals()

    def __compare(self, taf):
        now = time.time()
        text, maxval = [], -1.0
        data = LLWSData.retrieve(self.info['ident'], self.info)
        if data is None:
            data = []
        for ident in data:
            llws = data[ident]
            if llws['time'] < now-4500.0:
                continue
            maxval = max(maxval, llws['value'])
            text.append('%s %sZ %s (%.2f 1/s)' % \
                (ident, time.strftime('%d%H%M', \
                time.gmtime(llws['time'])), llws['str'], llws['value']))
        if maxval < 0.0:
            msg = 'Missing LLWS forecast'
            _Logger.info('%s for %s' % (msg, self.info['ident']))
            return {'status': self.setMissing(msg)}
        result = {'text': '\n'.join(text)}
        try:
            delta = MonitorP.applyRules(self.rules, now, taf.hourly.get(now), 
                maxval)
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            result['status'] = self.setNIL()
        return result

###############################################################################
class WSinRadar(MonitorP.Rule):
    """LLWS potential, but not in TAF.
Arguments: wind shear threshold"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wind'
        self.unique = True
        self.severity = 3
        self.args = {'value': 0.2}
    
    def method(self, taf, value):
        if value < self.args['value']:
            return False
        if 'llws' not in taf:
            self.setmsg('Measured LLWS = %.1f 1/s, LLWS not in TAF', value)
            return True
        return False
