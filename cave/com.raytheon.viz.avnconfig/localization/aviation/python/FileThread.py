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
#       FileThread.py
#       GFS1-NHD:A7798.0000-SCRIPT;1.7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.7 (DELIVERED)
#         Created:  29-JUL-2005 19:11:54      TROJAN
#           spr 6953
#       
#       Revision 1.6 (APPROVED)
#         Created:  06-JUL-2005 18:16:37      TROJAN
#           spr 6548
#       
#       Revision 1.5 (DELIVERED)
#         Created:  07-MAY-2005 11:33:18      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.4 (DELIVERED)
#         Created:  11-MAR-2005 15:55:31      TROJAN
#           spr 6717
#       
#       Revision 1.3 (DELIVERED)
#         Created:  15-FEB-2005 13:47:37      TROJAN
#           spr 6650
#       
#       Revision 1.2 (APPROVED)
#         Created:  21-OCT-2004 19:36:11      TROJAN
#           spr 6420
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:40:15      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6953
#       	Action Date:       09-AUG-2005 14:09:19
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Delivery of multiple Pythons
#       
#
# FileThread.py
# sends notifications for files dumped to data/xxxx
# Author: George Trojan, SAIC/MDL, June 2004
# last update: 07/29/05

import logging, os, re, stat, time, Queue
import Pyro.errors
import Avn, AvnPyro
if os.path.exists('/usr/lib/libglib-2.0.so.0'):
     import gamin
else:
     import fam as gamin

_Logger = logging.getLogger(__name__)

###############################################################################
class Server(object):
    def __init__(self, inqueue, outqueue, info):
        self.inqueue = inqueue
        self.outqueue = outqueue
        self.name = info['name'].upper()
        self.datapaths = [x.strip() for x in info['source'].split(',')]

    def paths(self):
        return self.datapaths

    def run(self):
        while 1:
            try:
                code, fname, direct = self.inqueue.get(True, 6)
                if code == 0:   # end thread
                    _Logger.info('Got exit request')
                    raise SystemExit
                path = os.path.join(direct, fname)
                if code == gamin.GAMExists:        # skip old (12 hour +) files
                    if os.stat(path)[stat.ST_MTIME] < time.time()-43200:
                        continue
                _Logger.info('Processing file %s', path)
                src = direct.split('/')[-1]
                self.outqueue.put(Avn.Bunch(src=src, ident=fname))
            except Queue.Empty:
                pass
