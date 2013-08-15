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
#       avnclimate.py
#       GFS1-NHD:A7896.0000-SCRIPT;1.4
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.4 (DELIVERED)
#         Created:  29-NOV-2007 09:54:10      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 1.3 (DELIVERED)
#         Created:  16-FEB-2006 15:43:55      PCMS
#           test update for DB fix, no code change.  Mcgovern
#       
#       Revision 1.2 (DELIVERED)
#         Created:  07-MAY-2005 11:40:44      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.1 (DELIVERED)
#         Created:  09-JUL-2004 19:53:30      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7351
#       	Action Date:       19-MAR-2008 08:14:54
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Remove dependency on shared library, climmodule.so
#       
#
#    Purpose:
# 	A driver for the AvnFPS setup GUI

import os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend(['%s/%s' % (TopDir, dir) for dir in ['sitepy', 'py', 'toolpy']])
import Startup
import AvnClimate

def main():
	os.chdir(TopDir)
	gui = AvnClimate.AvnClimate()
	gui.run()

if __name__ == '__main__':
	main()
