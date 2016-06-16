# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# XXXStormSurgeThreat
#
# Author: M. Volkmer/P. Santos
# ----------------------------------------------------------------------------

from gfeConfig import *

#HideConfigFile = 1

#------------------------------------------------------------------------
# GIF/PNG Graphic Product Generation (ifpIMAGE program)
#------------------------------------------------------------------------

# IMAGE SIZE
# Both default to 400
#Png_height    = 720
Png_width     = 492

#NAME OF ELEMENT DISPLAYED AS IMAGE
Png_image = 'StormSurgeThreat'

Png_parms = ['StormSurgeThreat_SFC:_Official -1']
StormSurgeThreat_spatialImageType = ["Image"]

# Smoothing...
#Png_smoothImage = 1

# NWS Logos
Png_logo = 1
Png_logoString = "Storm Surge Threat - NWS Miami FL"

# Turn off Color Bar
Png_omitColorBar = 0

# COLOR TABLES
StormSurgeThreat_defaultColorTable='GFE/gHLS_new'
#LocalSurgeThreat_maxColorTableValue = 6.0 
#LocalSurgeThreat_minColorTableValue = 0.0 
ImageLegend_color = 'black'
bgColor = 'white'
# Counties_graphicColor = 'grey'
Counties_XXX_graphicColor = 'black'
Zones_XXX_graphicColor = 'black'
Marine_Zones_XXX_graphicColor = 'black'
Interstates_graphicColor = 'blue'
States_graphicColor = 'gray'
Lakes_graphicColor = 'blue'


# MAP BACKGROUNDS
# MapBackgrounds_default = ['Counties','Marine_Zones_XXX','Interstates']
MapBackgrounds_default = ['Zones_XXX','Marine_Zones_XXX','Interstates','States','Lakes']
XXX_mask = "StormSurgeWW_EditArea"

DefaultSamples = ['XXXStormSurgeThreat']
# Customize FONT SIZES here
#TextFont0 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont1 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont2 =  "Bitstream Vera Sans Mono-bold-11"
#TextFont3 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont4 =  "Bitstream Vera Sans Mono-bold-10"

SESample_font = 1
SELegend_font = 2
SEColorBar_fgTextColor = "black"

#CONFIGURATION FILE SETTINGS
Png_filenamePrefix = 'XXXStormSurgeThreat' 
Png_baseTimeFormat = ''

#LEGENDS
# Ability to turn on/off legends for the graphic generation.  Applies
# only to graphic product generation and not GFE.  Defaults to on
# if not specified.  Do not include a decimal point after the number.
Png_legend = 1   #1 for visible, 0 for invisible

# Legends display mode - 0 for UTC, 1 for local time
# Do not include a decimal point after the number.
Png_localTime = 1 # legend displays time in local or UTC (default to UTC)

# Legend weather element name mode - 0 for weather element name,
# 1 for weather element descriptive name
#Png_descriptiveWeName = 1
Png_descriptiveWeName = "ALT"
Png_StormSurgeThreat_AltName="Updated"
Png_legendFormat_LT_dur = ""
#Png_legendFormat_LT_start = "%A %b %d"
Png_legendFormat_LT_start = "%I %p %a %h %d"
# Png_legendFormat_LT_start = ""
Png_legendFormat_LT_end = ""
