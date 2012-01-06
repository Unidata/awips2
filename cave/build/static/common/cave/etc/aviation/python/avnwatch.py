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
#       avnwatch.py
#       GFS1-NHD:A3664.0000-SCRIPT;13
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 13 (DELIVERED)
#         Created:  29-NOV-2007 09:54:12      OBERFIEL
#           Removed obsolete directory search path
#       
#       Revision 12 (DELIVERED)
#         Created:  24-MAR-2006 12:46:09      TROJAN
#           spr 7106: redirect all error messages to a log file
#       
#       Revision 11 (DELIVERED)
#         Created:  24-MAR-2006 09:28:29      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 10 (DELIVERED)
#         Created:  07-MAY-2005 11:42:12      OBERFIEL
#           Added Item Header Block
#       
#       Revision 9 (DELIVERED)
#         Created:  01-JUL-2004 15:00:09      OBERFIEL
#           Update
#       
#       Revision 8 (DELIVERED)
#         Created:  08-JAN-2004 21:40:42      PCMS
#           Updating for code cleanup
#       
#       Revision 7 (APPROVED)
#         Created:  03-DEC-2003 18:42:58      TROJAN
#           spr 5681
#       
#       Revision 6 (APPROVED)
#         Created:  05-NOV-2003 19:00:24      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 5 (DELIVERED)
#         Created:  17-JUL-2002 13:38:09      PCMS
#           Updating with TWEB QC and reorganized directory structure
#           to allow for multiple versions.
#       
#       Revision 4 (DELIVERED)
#         Created:  11-JUN-2002 18:38:07      PCMS
#           Fixed problem starting AVN Watch if a forecaster is
#           selected.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 19:04:07      PCMS
#           Enabled garbage collection
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:29      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:39:43      MOELLER
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
#	A driver for the AvnFPS Monitoring GUI

import getopt, os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
    ['sitepy', 'py', 'toolpy']])
import Startup

def main():
    try:
        opts, pargs = getopt.getopt(sys.argv[1:], 'f:')
    except:
        print 'python avnmenu.py [-f forecaster_id] [product ...]'
        raise SystemExit
    try:
        os.chdir(TopDir)
        import AvnWatch, Globals
        if opts:
            gui = AvnWatch.AvnWatch(msghistory=1, forecaster=opts[0][1])
        else:
            gui = AvnWatch.AvnWatch(msghistory=1)
        if pargs:
            Globals.Products = pargs
        gui.run()
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
