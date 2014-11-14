
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# gHLSMFLTornadoThreat
#
# Author: M. Volkmer/P. Santos
# ----------------------------------------------------------------------------

from gfeConfig import *

#------------------------------------------------------------------------
# GIF/PNG Graphic Product Generation (ifpIMAGE program)
#------------------------------------------------------------------------

# IMAGE SIZE
#Png_height    = 720
Png_width     = 492

#NAME OF ELEMENT DISPLAYED AS IMAGE
Png_image = 'TornadoThreat'

Png_parms = ['TornadoThreat_SFC:_Official -1']
TornadoThreat_spatialImageType = ["Image"]

# Smoothing...
#Png_smoothImage = 1

# NWS Logos
Png_logo = 1
Png_logoString = "Tornado Threat - NWS Miami FL"

# Turn off Color Bar
Png_omitColorBar = 0

# COLOR TABLES
TornadoThreat_defaultColorTable='GFE/gHLS_new'
#MFLTornadoThreat_maxColorTableValue = 6.0 
#MFLTornadoThreat_minColorTableValue = 0.0 
ImageLegend_color = 'black'
bgColor = 'white'
Counties_MFL_graphicColor = 'black'
Zones_MFL_graphicColor = 'black'
Marine_Zones_MFL_graphicColor = 'black'
Interstates_graphicColor = 'blue'
States_graphicColor = 'gray'
Lakes_graphicColor = 'blue'

# MAP BACKGROUNDS
#MapBackgrounds_default = ['Counties','Marine_Zones_MFL','Interstates']
MapBackgrounds_default = ['Zones_MFL','Marine_Zones_MFL','Interstates','States','Lakes']
MFL_mask = "MFL_CWA"

DefaultSamples = ['MFLTornadoThreat']
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
Png_filenamePrefix = 'MFLTornadoThreat' 
Png_baseTimeFormat = ''

#LEGENDS
# Ability to turn on/off legends for the graphic generation.  Applies
# only to graphic product generation and not GFE.  Defaults to on
# if not specified.  Do not include a decimal point after the number.
Png_legend = 1   #1 for visible, 0 for invisible

# Legends display mode - 0 for UTC, 1 for local time
# Do not include a decimal point after the number.
Png_localTime = 1 # legend displays time in local or UTC (default to UTC)

Png_descriptiveWeName = "ALT"
Png_TornadoThreat_AltName = "Updated"
Png_legendFormat_LT_dur = ""
#Png_legendFormat_LT_start = "%A %b %d"
Png_legendFormat_LT_start = "%I %p %a %h %d"
# Png_legendFormat_LT_start = ""
Png_legendFormat_LT_end = ""
