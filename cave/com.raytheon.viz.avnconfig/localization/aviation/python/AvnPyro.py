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
#       AvnPyro.py
#       GFS1-NHD:A7854.0000-SCRIPT;1.7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.7 (DELIVERED)
#         Created:  06-JUL-2005 18:16:34      TROJAN
#           spr 6548
#       
#       Revision 1.6 (DELIVERED)
#         Created:  07-MAY-2005 11:30:09      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.5 (DELIVERED)
#         Created:  11-MAR-2005 15:55:29      TROJAN
#           spr 6717
#       
#       Revision 1.4 (DELIVERED)
#         Created:  23-JAN-2005 18:42:21      TROJAN
#           spr 6604
#       
#       Revision 1.3 (APPROVED)
#         Created:  30-SEP-2004 20:53:42      TROJAN
#           stdr 873
#       
#       Revision 1.2 (APPROVED)
#         Created:  19-AUG-2004 20:25:36      OBERFIEL
#           Code change
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 16:44:31      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
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
# AvnPyro.py
# Pyro validation stuff specific to AvnFPS
# Author: George Trojan, SAIC/MDL, May 2004
# last update: 05/25/05

import logging, os
from fnmatch import fnmatch
import Pyro, Pyro.core, Pyro.protocol, Pyro.naming, Pyro.constants
from Pyro.errors import *
import AvnParser

_Logger = logging.getLogger(__name__)

try:
    cfg = AvnParser.getServerCfg()
    _ValidHosts = cfg['valid']
except:
    _Logger.exception('Cannot access server configuration')

###############################################################################
#----- required global funcs that return validator objects ------
def BCGuard():
    return BCReqValidator()

def NSGuard():
    v = newConnValidator()
#   FIXME: use passphrases?
#   v.setAllowedIdentifications([ACCEPTED_ID])
    return v

# NS Broadcast Request Validator
# Must inherit from the base class as shown,
# because dispatcher code is in there.
class BCReqValidator(Pyro.naming.BCReqValidator):
    # we have:
    # self.addr = address of client (ip, port)
    # self.sock = reply socket (used by self.reply method)
    def acceptLocationCmd(self):
        if filter(lambda x, a=self.addr[0]: fnmatch(a, x), _ValidHosts):
            return 1
        else:
            self.reply('denied!')  # send this back to client
            return 0

    def acceptShutdownCmd(self):
        return 1

class newConnValidator(Pyro.protocol.DefaultConnValidator):
    def acceptHost(self, daemon, conn):
        if filter(lambda x, a=conn.addr[0]: fnmatch(a, x), _ValidHosts):
            return Pyro.protocol.DefaultConnValidator.acceptHost( \
                self, daemon, conn)
        else:
            return (0,Pyro.constants.DENIED_HOSTBLOCKED) # not ok
