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
# Example ifpIMAGE configuration file. 

# Must always import from gfeConfig (or your site configuration)
from gfeConfig import *
import gfeConfig


# Defines the T and Wind Official database weather elements for display
Png_parms = ['T_SFC:_Official -1', 'Wind_SFC:_Official -1']

# Defines the T should be the image
Png_image = 'T'

# We want the legend to be in localtime, not Zulu time
Png_localTime = 1

# We only want the States map background
MapBackgrounds_default = ['States']

# Clip the data to the edit area called "BOU"
BOU_mask = "BOU"

# Limit the color table to a range of 20 to 90.
T_maxColorTableValue = 90.0
T_minColorTableValue = 20.0


