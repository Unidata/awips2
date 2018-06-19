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
# Example ifpIMAGE configuration file

# Must always import from gfeConfig (or your site configuration)
from gfeConfig import *
import gfeConfig

# make the legend smaller than the default
SELegend_font = 2

# set the width to 350
Png_width = 350

# MaxT is what we are generating images for
Png_image = 'MaxT'
Png_parms = ['MaxT_SFC:_Fcst -1']

# Turn on the logo
Png_logo = 1
Png_logoString = "NWS Timbuktoo"

