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

# We wish to have 5 km resolution for our site (SLC).  The approximate
# grid size will be 144.
SITES['SLC'] = ([144, 144], (30.00, 28.00), (10.0, 10.0), 'MST7MDT', Grid211,
 'wfo')


