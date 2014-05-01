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
#       Globals.py
#       GFS1-NHD:A6628.0000-SCRIPT;1.9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.9 (DELIVERED)
#         Created:  06-JUL-2005 18:16:37      TROJAN
#           spr 6548
#       
#       Revision 1.8 (DELIVERED)
#         Created:  07-MAY-2005 11:33:36      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.7 (DELIVERED)
#         Created:  04-APR-2005 15:51:05      TROJAN
#           spr 6775
#       
#       Revision 1.6 (DELIVERED)
#         Created:  11-MAR-2005 15:55:31      TROJAN
#           spr 6717
#       
#       Revision 1.5 (DELIVERED)
#         Created:  30-SEP-2004 20:22:10      TROJAN
#           stdr 873
#       
#       Revision 1.4 (APPROVED)
#         Created:  19-AUG-2004 20:40:56      OBERFIEL
#           Change code
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:24      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:39:58      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:27      OBERFIEL
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
# Globals.py
# global variables
# Author: George Trojan, SAIC/MDL, July 2003
# last update: 05/25/05

Forecaster = ''     # forecaster
Products = []       # Currently monitored products
DRC = None          # data request client
ServerStatus = {}   # last 'ALIVE' message time 
Viewers = []        # viewers in TAF editor
Colors = []         # colors in main GUI alert labels
EditTag = {}        # colors in forecast editors used to highlight errors
