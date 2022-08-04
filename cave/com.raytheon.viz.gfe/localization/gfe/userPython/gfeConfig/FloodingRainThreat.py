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

# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
# 
# XXXFloodingRainThreat
#
# Author: M. Volkmer/P. Santos
#
# SOFTWARE HISTORY
#  Date       Ticket#   Engineer    Description
#  ---------- --------  ----------- --------------------------
#  07/21/17   DCS19921  PSantos     Enable TCStormSurgeThreat Tool to Run
#                                   with P-ETSS data  
# ----------------------------------------------------------------------------

##
# This is a base file that is not intended to be overridden.
#
# This file can be imported to override configuration settings. Please see the
# Configuration Guides->GFE Configuration section of the GFE Online Help for
# guidance on creating a new configuration file.
##

from gfeConfig import *

HideConfigFile = 1

#------------------------------------------------------------------------
# GIF/PNG Graphic Product Generation (ifpIMAGE program)
#------------------------------------------------------------------------

# IMAGE SIZE
# You can set the height and width (in pixels) for the Png images.
# It is only necessary to set one of these, as the other will
# be calculated using the aspect ratio of your office domain.
# Do not include decimal points after the numbers.
# Both default to 400
#Png_height    = 720
Png_width     = 492

#NAME OF ELEMENT DISPLAYED AS IMAGE
Png_image = 'FloodingRainThreat'


Png_parms = ['FloodingRainThreat_SFC:_Official -1']
FloodingRainThreat_spatialImageType = ["Image"]

# Smoothing...
#Png_smoothImage = 1

# NWS Logos
Png_logo = 1
Png_logoString = "Flooding Rain Threat - NWS Miami FL"

# Turn off Color Bar
Png_omitColorBar = 1

# COLOR TABLES
FloodingRainThreat_defaultColorTable='GFE/gHLS_new'
# FloodThreat_maxColorTableValue = 6.0 
# FloodThreat_minColorTableValue = 0.0 
ImageLegend_color = 'black'
bgColor = 'white'
Counties_XXX_graphicColor = 'black'
Zones_XXX_graphicColor = 'black'
Marine_Zones_XXX_graphicColor = 'black'
Interstates_graphicColor = 'blue'
States_graphicColor = 'gray'
Lakes_graphicColor = 'blue'

# MAP BACKGROUNDS
#MapBackgrounds_default = ['Counties','Marine_Zones_XXX','Interstates']
MapBackgrounds_default = ['Zones_XXX','Marine_Zones_XXX','Interstates','States','Lakes']
XXX_mask = "XXX"

#DefaultSamples = ['XXXFloodingRainThreat']
# Customize FONT SIZES here.
#TextFont0 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont1 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont2 =  "Bitstream Vera Sans Mono-bold-11"
#TextFont3 =  "Bitstream Vera Sans Mono-bold-10"
#TextFont4 =  "Bitstream Vera Sans Mono-bold-10"

SESample_font = 1
SELegend_font = 2
SEColorBar_fgTextColor = "black"

#CONFIGURATION FILE SETTINGS
Png_filenamePrefix = 'XXXFloodingRainThreat' 
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
Png_FloodingRainThreat_AltName = "Updated"
Png_legendFormat_LT_dur = ""
#Png_legendFormat_LT_start = "%A %b %d"
Png_legendFormat_LT_start = "%I %p %a %h %d"
# Png_legendFormat_LT_start = ""
Png_legendFormat_LT_end = ""
