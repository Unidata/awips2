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
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%
#
#    Purpose:
# 	A driver for the AvnFPS setup GUI

import os, sys
TopDir = os.environ['TOP_DIR']
sys.path = sys.path[1:]
sys.path.extend(['%s/%s' % (TopDir, dir) for dir in ['sitepy', 'py', 'toolpy']])
import Startup
import ClimateDataDialog

def main():
	os.chdir(TopDir)
	gui = ClimateDataDialog.Editor()
	gui.run()

if __name__ == '__main__':
	main()
