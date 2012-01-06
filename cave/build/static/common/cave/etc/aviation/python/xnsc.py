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
#       xnsc.py
#       GFS1-NHD:A7838.0000-SCRIPT;1.2
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.2 (DELIVERED)
#         Created:  07-MAY-2005 11:42:38      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.1 (DELIVERED)
#         Created:  01-JUL-2004 14:48:12      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6832
#       	Action Date:       07-JUN-2005 13:13:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Add PVCS doc blocks
#       
#
# xnsc.py
# a simple wrapper around Pyro name server GUI
# George Trojan, SAIC/MDL May 2004
# last update: 05/20/04

import Startup
import Pyro.xnsc, sys

Pyro.xnsc.main(sys.argv[1:])
