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
#       cvdist.py
#       GFS1-NHD:A9173.0000-SCRIPT;1
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1 (DELIVERED)
#         Created:  16-MAY-2006 16:07:05      TROJAN
#           spr 7144
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7144
#       	Action Date:       14-FEB-2007 12:27:26
#       	Relationship Type: Affected
#       	Status:           CLOSED
#       	Title:             AvnFPS: Incorrect method to specify filtering criteria in Cig/Vis Monthly tool
#       
#
# cvdist.py
# A driver for ceiling/visibility distribution GUI
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
        from CigVisDist import Gui
        Gui().run()
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
