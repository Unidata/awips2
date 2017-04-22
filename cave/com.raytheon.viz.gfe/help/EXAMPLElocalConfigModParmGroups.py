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
# This is an example localConfig.py file to modify the weather element
# and time constraint relationship for the Fcst and Official Databases
# to the following:
# T, Td, Wind, Wx, Sky: 3 hours long, no gaps 
# MaxT : 4am - 10pm local time
# MinT : 4pm - 10am local time

# Always include these lines at the top of your localConfig.py file.
from serverConfig import *
import serverConfig

# define the new time constraints for MaxT and MinT.  Doesn't really matter
# what I call them.
MaxTTC18 = localTC(4*HOUR, 24*HOUR, 18*HOUR, 0)
MinTTC18 = localTC(16*HOUR, 24*HOUR, 18*HOUR, 0)

# define the new parm groupings.  Can only use one parms= in the config file.
parms = [([T, Td, Wind, Wx, Sky], TC3NG), 
  ([MaxT], MaxTTC18), ([MinT], MinTTC18)]




