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
#       PersistMonitor.py
#       GFS1-NHD:A7812.0000-SCRIPT;1.8
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.8 (DELIVERED)
#         Created:  21-APR-2006 11:29:25      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.7 (DELIVERED)
#         Created:  20-APR-2006 15:52:53      TROJAN
#           made compare() called from a base class
#       
#       Revision 1.6 (DELIVERED)
#         Created:  23-JAN-2006 08:23:17      TROJAN
#           stdr 956
#       
#       Revision 1.5 (DELIVERED)
#         Created:  06-JUL-2005 18:16:40      TROJAN
#           spr 6548
#       
#       Revision 1.4 (DELIVERED)
#         Created:  07-MAY-2005 11:36:51      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  14-FEB-2005 20:54:50      TROJAN
#           spr 6649
#       
#       Revision 1.2 (APPROVED)
#         Created:  30-SEP-2004 18:56:03      TROJAN
#           stdr 874
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:43:07      OBERFIEL
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
# PersistMonitor.py
# METAR monitoring module
# Author: George Trojan, SAIC/MDL, June 2003
# last update: 04/20/06
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06JUL2012       15153         zhao           Retrieve latest METAR record in database
#

import logging, os, time
import Avn, AvnLib, Globals, MonitorP, MetarMonitorP
import MetarData

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
class Monitor(MetarMonitorP.Monitor):
    Source = 'mtrs'

    def __compare(self, taf):
        now = time.time()
#        self._metars = Globals.DRC.getMetars(self.info['sites']['metar'], 
#            True, now-7200.0)

# For DR15153: use 'maxSize=0' to indicate that the latest record is to be retrieved  
        self._metars = MetarData.retrieve(self.info['sites']['metar'],0)
        msg = None
        result = {}
        if not self._metars: 
            msg = 'Missing METAR'
            _Logger.warning('%s for %s', msg, self.info['sites']['metar'])
        else:
            rpt = self._metars[0]
            result['header'] = rpt.header
            result['text'] = rpt.text
            if 'fatal' in rpt.dcd:
                msg = 'Cannot decode METAR'
                _Logger.error('%s for %s:\n%s', msg, 
                    self.info['sites']['metar'], rpt.text)
        if msg:
            result['status'] = self.setMissing(msg)
            return result
        try:
            future = now + 3600.0*self.args['nhours']
            mtrdata = AvnLib.makeMetarData(rpt.dcd)
            delta = MonitorP.applyRules(self.rules, future,
                taf.hourly.get(future), mtrdata)
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            result['status'] = self.setNIL()
        return result
