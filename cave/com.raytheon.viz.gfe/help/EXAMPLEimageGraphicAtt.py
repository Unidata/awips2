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


# Defines the T, Wind as the weather elements to display
Png_parms = ['T_SFC:_Official -1', 'Wind_SFC:_Official -1']

# Make the Temperature contour labels larger, make the Temperature
# contours blue, decrease the density of the contours
Contour_font = 4
T_graphicColor = 'blue'
T_density = -2

# Make the Wind red, make the wind barbs bigger, increase the
# density (packing) of the wind barbs.
Wind_graphicColor = 'red'
Wind_fontOffset = 2
Wind_density = 20






