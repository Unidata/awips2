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
#       wrose.py
#       GFS1-NHD:A9009.0000-SCRIPT;3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 3 (DELIVERED)
#         Created:  24-MAR-2006 12:46:11      TROJAN
#           spr 7106: redirect all error messages to a log file
#       
#       Revision 2 (DELIVERED)
#         Created:  24-MAR-2006 09:37:56      TROJAN
#           spr 7103: redirect all error messages to a log file
#       
#       Revision 1 (DELIVERED)
#         Created:  30-JAN-2006 07:57:30      TROJAN
#           stdr 945
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7106
#       	Action Date:       26-FEB-2007 09:50:31
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: DUP 7103 Pmw code does not forward python exceptions to AvnFPS _Logger
#       
#
# wrose.py
# A driver for Wind Rose GUI
# Author: George Trojan, SAIC/MDL, January 2006
# last update: 03/15/06

import os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
	['sitepy', 'py', 'toolpy']])
import Startup

def main():
    try:
        os.chdir(TopDir)
        from WindRose import WindRose
        WindRose().run()
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
