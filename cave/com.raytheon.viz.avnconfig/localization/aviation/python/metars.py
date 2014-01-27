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
#       metars.py
#       GFS1-NHD:A9182.0000-SCRIPT;1
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1 (DELIVERED)
#         Created:  18-MAY-2006 10:13:28      TROJAN
#           spr 7150
#
#    Change Document History:
#       Not related to any Change Document
#
# metars.py
# A driver for MetarDisplay GUI
# Author: George Trojan, SAIC/MDL, May 2006
# last update: 05/08/06

import os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend([os.path.join(TopDir, dir) for dir in \
	['sitepy', 'py', 'toolpy']])
import Startup

def main():
    try:
        os.chdir(TopDir)
        from MetarDisplay import MetarDisplay
        MetarDisplay().run()
    except SystemExit:
        raise
    except Exception:
        import logging
        logging.getLogger(__name__).exception('Uncaught exception')

if __name__ == '__main__':
    main()
