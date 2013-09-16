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
# Test file for ifpIMAGE

from gfeConfig import *
import gfeConfig

HideConfigFile = 1

# Defines the T as the displayable weather element
Png_parms = ['T_SFC:_Fcst -1', 'Td_SFC:_Fcst -1', 'Wind_SFC:_Fcst -1']
Png_image = 'T'

#Png_parms = ['Wx_SFC:_Fcst -1']
#Png_image = 'Wx'
#WeatherCoverage_names = ["Iso", "WSct", "Sct", "Num", "Wide", "Ocnl", "SChc",
#                         "Chc", "Lkly", "Def", "Patchy", "<NoCov>"]
#WeatherCoverage_fillPatterns = ["OCNL", "WIDE_SCATTERED",
#                                "SCATTERED", "LKLY", "WHOLE", "OCNL",
#                                "WIDE_SCATTERED", "SCATTERED", "LKLY",
#                                "WIDE", "SCATTERED", "WHOLE"]
#WeatherType_names = ["<NoWx>", "T", "A", "R", "RW", "L", "ZR", "ZL",
#                     "S", "SW", "IP", "F", "H", "BS", "K", "BD",
#                     "SA", "LC", "FR", "AT", "TRW"]
#WeatherType_colors = ["brown", "red3", "DeepPink", "Green",
#                      "ForestGreen", "CadetBlue1", "darkorange1",
#                      "goldenrod1", "blue", "Grey65", "plum1",
#                     "khaki4", "Gray75", "snow", "grey30", "Brown",
#                      "blue1", "coral1", "pale turquoise", "DeepPink",
#                      "red3"]



#Png_baseTimeFormat = "%a_" 


# mask
#mask = 'ISC_Send_Area'

#bgColor = 'blue' 

#Png_interval = 3

# Change the format of the legend
Png_localTime = 0
Png_legendFormat_dur = '([UNITS]) '
Png_snapshotTime = 0
#Png_descriptiveWeName = "LONG"

Png_smoothImage = 1

# set the width to 800
Png_width = 600
Png_height = 600

Td_graphicColor = 'green'
Td_lineWidth = 1
Wind_graphicColor = 'blue'
#T_defaultColorTable="GFE/Warm To Cold"
#T_maxColorTableValue=100.0
#T_minColorTableValue = -30.0
#T_fitToDataColorTable = "Single Grid"
#Png_fitToDataArea = "NE_Douglas"
DefaultSamples = ['pngSamples']
#OfficeDomain_expandLeft = 5
#OfficeDomain_expandRight = 20
#OfficeDomain_expandTop = 10
#OfficeDomain_expandBottom = 30

Png_logo = 1
Png_logoString = "NWS Valley"

#T_ColorBarLabels = [32.0, 42.0, 52.0, 62.0]
#Png_omitColorBar = 1

# MapLabelXOffset and MapLabelYOffset are the number of pixels you
# wish to move map city labels relative to their "normal" position.
#MapLabelXOffset = 0
#MapLabelYOffset = 0

MapBackgrounds_default = ['States', 'Counties']
States_graphicColor = 'red'
States_lineWidth = 2
States_linePattern = 'DASHED'
States_fontOffset = 1
# The attribute to use in map labels. Not all maps support this setting.
#States_labelAttribute = 'state'

# Add a prefix to the filename
Png_filenamePrefix = 'Test1_'


