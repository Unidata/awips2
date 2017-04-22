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


# Defines Wx as the display and make it an image
Png_parms = ['Wx_SFC:_Official -1']
Png_image = 'Wx'

# Change the color/pattern mapping
WeatherCoverage_names = ["Iso", "WSct", "Sct", "Num", "Wide", "Ocnl", "SChc",
                         "Chc", "Lkly", "Def", "Patchy", "<NoCov>"]
WeatherCoverage_fillPatterns = ["OCNL", "WIDE_SCATTERED",
                                "SCATTERED", "LKLY", "WHOLE", "OCNL",
                                "WIDE_SCATTERED", "SCATTERED", "LKLY",
                                "WIDE", "SCATTERED", "WHOLE"]
WeatherType_names = ["<NoWx>", "T", "A", "R", "RW", "L", "ZR", "ZL",
                     "S", "SW", "IP", "F", "H", "BS", "K", "BD",
                     "SA", "LC", "FR", "AT", "TRW"]
WeatherType_colors = ["brown", "red3", "DeepPink", "Green",
                      "ForestGreen", "CadetBlue1", "darkorange1",
                      "goldenrod1", "blue", "Grey65", "plum1",
                      "khaki4", "Gray75", "snow", "grey30", "Brown",
                      "blue1", "coral1", "pale turquoise", "DeepPink",
                      "red3"]

