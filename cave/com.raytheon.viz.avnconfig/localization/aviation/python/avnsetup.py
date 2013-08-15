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
#       avnsetup.py
#       GFS1-NHD:A3663.0000-SCRIPT;11
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 11 (DELIVERED)
#         Created:  29-NOV-2007 09:54:12      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 10 (DELIVERED)
#         Created:  24-MAR-2006 12:46:08      TROJAN
#           spr 7106: redirect all error messages to a log file
#       
#       Revision 9 (DELIVERED)
#         Created:  24-MAR-2006 09:28:28      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 8 (DELIVERED)
#         Created:  07-MAY-2005 11:41:54      OBERFIEL
#           Added Item Header Block
#       
#       Revision 7 (DELIVERED)
#         Created:  16-NOV-2004 20:14:30      PCMS
#           Restoring history
#       
#       Revision 6 (DELIVERED)
#         Created:  05-NOV-2003 18:59:52      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 5 (DELIVERED)
#         Created:  16-MAY-2003 12:36:30      TROJAN
#           spr 5160
#       
#       Revision 4 (DELIVERED)
#         Created:  17-JUL-2002 13:38:11      PCMS
#           Updating with TWEB QC and reorganized directory structure
#           to allow for multiple versions.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 19:04:09      PCMS
#           Enabled garbage collection
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:32      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:39:18      MOELLER
#           Initial version
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
#	 A driver for the AvnFPS Setup GUI

import os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
	['sitepy', 'py', 'toolpy']])

import Startup

def main():
    try:
        os.chdir(TopDir)
        import AvnSetup
        gui = AvnSetup.AvnSetup()
        gui.run()
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
