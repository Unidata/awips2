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
# This is an example localConfig.py file

# Always include these lines at the top of your localConfig.py file.
from serverConfig import *
import serverConfig

# In this example, we want to add a new field called Clearing, and have
# its time constraints be every 4 hours, aligned with midnight local time.
# Since there isn't a time constraint for this, we need to define a new one
# first, before defining the new weather element.
LT4 = localTC(0*HOUR, 4*HOUR, 4*HOUR, 0)

# Now we define the new "Clearing" weather element.
elem1 = ("Clearing", SCALAR, "pot", "Clearing Potential", 100.0, 0.0, 0, NO)
parms = [([elem1], LT4)]


