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
#       TriggerThread.py
#       GFS1-NHD:A7826.0000-SCRIPT;1.15
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.15 (DELIVERED)
#         Created:  01-AUG-2008 15:44:47      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.14 (DELIVERED)
#         Created:  11-JUN-2008 14:51:59      OBERFIEL
#           Removed references to obsolete products
#       
#       Revision 1.13 (DELIVERED)
#         Created:  19-SEP-2005 13:47:39      TROJAN
#           spr 7011
#       
#       Revision 1.12 (DELIVERED)
#         Created:  06-JUL-2005 18:16:43      TROJAN
#           spr 6548
#       
#       Revision 1.11 (DELIVERED)
#         Created:  01-JUN-2005 17:41:32      OBERFIEL
#           Bug fixes since RHE3 initial snapshot
#       
#       Revision 1.10 (DELIVERED)
#         Created:  07-MAY-2005 11:39:49      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.9 (DELIVERED)
#         Created:  28-APR-2005 19:30:44      TROJAN
#           spr 6816
#       
#       Revision 1.8 (DELIVERED)
#         Created:  04-APR-2005 15:51:09      TROJAN
#           spr 6775
#       
#       Revision 1.7 (DELIVERED)
#         Created:  11-MAR-2005 15:55:32      TROJAN
#           spr 6717
#       
#       Revision 1.6 (DELIVERED)
#         Created:  15-FEB-2005 13:47:38      TROJAN
#           spr 6650
#       
#       Revision 1.5 (APPROVED)
#         Created:  24-JAN-2005 15:51:14      TROJAN
#           spr 6259
#       
#       Revision 1.4 (APPROVED)
#         Created:  07-DEC-2004 18:28:39      TROJAN
#           spr 6485
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:22:11      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  19-AUG-2004 20:58:07      OBERFIEL
#           code change
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:45:51      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7385
#       	Action Date:       11-OCT-2008 12:56:11
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Handle missing LLWS sources better
#       
#

import logging, os, time
import Avn, TextThreadP

_Logger = logging.getLogger(__name__)

##############################################################################
class Server(TextThreadP.Server):
    """Processes TAFs, TWEBs and METARs
Expects files from acqserver. File name determines data type.
"""
    def processFile(self, code, direct, fname):
        # Dispatches file to proper methods based on name
        if len(fname) not in [8, 9]:
            # don't process files with an extension
            return
        _Logger.debug('Processing %s', fname)
        path = os.path.join(direct, fname)
        try:
            data = file(path).read()
            bulletin = filter(None, [x.rstrip() for x in data.split('\n')])
        except IOError:
            _Logger.error('Cannot access file %s', path)
            return
        type = fname[3:6]
        if type == 'MTR':
            rcode = self.doMetar(bulletin)
        elif type == 'TAF':
            rcode = self.doTaf(bulletin)
        elif type == 'CFP':
            rcode = self.doCCFP(bulletin)
        else:
            _Logger.info('Unknown data type: %s' % fname)
            rcode = 0
        _Logger.debug('Done %s', fname)
        self.dispose(rcode, direct, fname)

    def onStartup(self):
        self.getSites()

    def paths(self):
        return [self.datapath]
