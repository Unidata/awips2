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
# -*-python-*-
# NOTE: THIS FILE SHOULD NOT BE MODIFIED BY THE USER.  INSTEAD, REFER TO
# THE BASE,SITE,USER and GFE CONFIGURATION DOCUMENTATION ON HOW TO
# OVERRIDE ENTRIES IN THIS FILE.

#------*-python-*-------------------------------------------------------------
# Config file for the GFE (Graphical Forecast Editor).
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/20/2013      2488          randerso       Changed to use DejaVu fonts

GFESUITE_HOME = "/awips2/GFESuite"
GFESUITE_PRDDIR = "/tmp/products"
yes = True
no  = False


#------------------------------------------------------------------------
# Hiding the configuration file
#------------------------------------------------------------------------
# The gfe configuration file can be hidden in the Start Up Dialog by
# using the HideConfigFile keyword and setting it to 1, or by commenting
# out the following line.
# HideConfigFile = 1

#------------------------------------------------------------------------
# Mutable Parameter and Viewable Database Configurations
#------------------------------------------------------------------------
# mutableModel indicates the one database that can be modified.  Format
# is "type_model_time".  If time isn't important (for a singleton db),
# then the format is "type_model".  If there isn't a type, then the
# format is "_model".
mutableModel = "_Fcst"

# dbTypes is a list of database types which the gfe should "see".
dbTypes = ['', 'D2D', 'V']

# The GFE supports filtering of the displayed data by site ID.
# If a config entry of the form SITEID_mask is set (to a named edit area),
# then the gfe will use the area as a mask in displaying data in the
# spatial editor. The user also can set masks for individual weather
# elements by adding config entries of the form SITEID_PARMNAME_mask.
# Sipmlified formats also available are PARMNAME_mask and just mask.
# The software first looks for a specific site/parmName entry, then
# for the site entry, then parmName, then just mask.  If you want all of
# the weather elements clipped except one, then specify an empty name ("")
# of the edit area associated with that weather element.
#BOU_Wind_mask = "BOU"
#BOU_mask = "CWA"
#Wind_mask = "CWA"
#mask = "CWA"

#------------------------------------------------------------------------
# Initial GFE Startup Weather Elements, Samples, and Edit Action List
# Configurations
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Ordering the Weather Element Groups

# Defines the initial set of parameters to be loaded when starting
# the GFE.  The name of the Group is specified.
# This is also the name of the default group of Edit Actions.
DefaultGroup = "Public"

# To provide an order for the weather elements, list an order
# preference in the list variable 'WEList'.  Any elements not listed in
# 'WEList', will be listed at the bottom of the weather element group menu
# in alphabetical order.
# WEList = ["FireWx","Gweight","Public","Temps"]

# Defines the initial set of sample sets that are displayed when
# starting the GFE.
#DefaultSamples = ['DefaultSamples']

# Defines the Smart Tools to be displayed on the Spatial Editor button-3 pop up menu.
# All smart tools (screened by active element) will appear if
#    AllEditActionsOnPopUp = yes
# Alternatively, you can set AllEditActionsOnPopUp = no and specify a list of smart tools (screened by active element) to appear.
AllEditActionsOnPopUp = yes
PopUpEditActions = ["Assign_Value","AdjustValue_Down","AdjustValue_Up","Smooth"]

# Defines the Smart Tools to be displayed on the Grid Manager button-3 pop up menu.
#GridManagerEditActions = ['nameOfTool1', 'nameOfTool2', 'nameOfTool3']

# Define keyboard shortcuts.
# You are allowed up to 200 shortcuts.
# IMPORTANT:  You should test your shortcuts on your system as many
#  keys are already bound by the system.  For example, F10 is bound by some Tk
#  widgets to bring up menus.
# Each shortcut is defined by a list with entries:
#   Shortcut key
#   State of ShortCut key
#       None
#       Ctrl  (control key)
#       Alt  (alt key)
#       Shift (shift key)
#       key states can be combined (e.g. Ctrl+Alt)
#   Action type:
#       EditTool
#       SmartTool
#       Procedure
#       Toggle
#   Name of the action.
#
# The possible EditTool actions are:
#   Sample
#   Pencil
#   Contour
#   MoveCopy
#   DrawEditArea
#
# The possible Toggle actions are:
#   ISC
#   TEGM  (Temporal Editor/Grid Manager)
#   HorizVert  (Horizontal/Vertical Display)
#
# Examples:
#
#ShortCut1 = ["F1", "None", "SmartTool","Assign_Value"]              # F1
#ShortCut2 = ["NUMPAD_SUBTRACT", "None", "SmartTool","AdjustValue_Down"] # Keypad -
#ShortCut3 = ["NUMPAD_ADD", "None", "SmartTool","AdjustValue_Up"]        # Keypad +
#ShortCut4 = ["F2", "None", "SmartTool","Smooth"]
#ShortCut5 = ["F3", "None", "Procedure","ISC_Discrepancies"]
#ShortCut6 = ["F5", "None", "EditTool", "Sample"]
#ShortCut7 = ["F6", "None", "EditTool", "Contour"]
#ShortCut8 = ["F7", "None", "EditTool", "Pencil"]
#ShortCut9 = ["F8", "None", "EditTool", "MoveCopy"]
#ShortCut10 = ["F9", "None", "EditTool", "DrawEditArea"]
#
#ShortCut11 = ["F5", "Alt", "EditTool", "Sample"]
#ShortCut12 = ["F6", "Ctrl", "EditTool", "Contour"]
#ShortCut13 = ["F7", "Shift", "EditTool", "Pencil"]

# Defines the initial set of edit area groups to appear in the edit
# area and query dialog.  If not specified, the default is Misc.
EditAreaGroups = ['Misc']


#------------------------------------------------------------------------
# Misc. Configuration
#------------------------------------------------------------------------
# This list of Weather element names will be used to sort the GridManager.
# Elements in this list will occur first.  All others will be sorted
# by name.
GridManagerSortOrder = ['T', 'Td', 'RH', 'MaxT', 'MinT', 'MaxRH', 'MinRH',
  'WindChill', 'HeatIndex', 'Wind', 'WindGust',  'FreeWind',
  'TransWind', 'Sky', 'Wx', 'LAL', 'PoP', 'CWR', 'QPF', 'SnowAmt',
  'StormTotalSnow', 'SnowLevel', 'MaxTAloft', 'WetBulb', 'Hazards',
  'FzLevel', 'Haines', 'MixHgt']

# This algorithm determines the sorting order of weather elements in the
# Grid Manager, Samples, and Spatial Editor Legends. It contains of up to
# 5 characters in the order of sort importance. The characters are:
# 'm' for mutable, 'N' for parm name, 'M' for model name, 't' for model time,
# and 'o' for model optional type. For example, "mNMto" will result in
# the mutables first, then parm name, then model name, then model time, then
# optional type. This means that all of the weather elements with the same
# name from different models will be grouped together (except for the mutable).
#GridManagerSortAlgorithm = "mNMto"

# Auto Save Interval
# The Auto Save Interval entry defines the interval in minutes that is
# used to automatically save modified weather elements.
AutoSaveInterval = 0

# This is the list of entries that appear on the Publish Dialog.  The
# entries are the names of the user-defined selection time ranges. The
# order of entries on the dialog will match this list.
PublishTimes = ['Today', 'Tonight', 'Tomorrow', 'Tomorrow Night', 'Day 3',
  'Day 4', 'Day 5', 'Day 6', 'Day 7', 'Hour 0-240']

#Preselect a weather group to be loaded in the Publish Dialog
#PublishDialogInitialWEGroup = "Public"

# Interpolation Dialog defaults. By default, the dialog is shown
# with a minimum interval and duration.  This can be changed. If the
# duration is specified, then the interval must also be specified.
# The units are hours and must range between 1 and 24.
#InterpolateDefaultInterval = 1
#InterpolateDefaultDuration = 1

# Create from Scratch Dialog defaults. By default, the dialog is shown
# with a minimum interval and duration.  This can be changed. If the
# duration is specified, then the interval must also be specified.
# The units are hours and must range between 1 and 24.
#CreateScratchDefaultInterval = 1
#CreateScratchDefaultDuration = 1

# Defines the product file purge in hours
#ProductPurgeHours = 6

#------------------------------------------------------------------------
# Map Background Configuration
#------------------------------------------------------------------------
# Defines the initial loaded set of map backgrounds.  The name of each
# background should match the name (without ".xml") of a map file in the 
# CAVE/Bundles/maps directory under the Localization perspective.
MapBackgrounds_default = ['States','CWA']

# Specific Colors for a map background
# The user may specify a specific color to be used for a map background,
# rather than getting a random color assigned.
# Format is mapName_graphicColor = color.
#States_graphicColor = 'green'

# Specific Graphic Line Widths for a map
# Default line widths can be set for each map background based on
# map name. Zero is the default value, which represents thin lines.
# The larger the number, the wider the line.  The format is mapName_lineWidth.
# Do not include a decimal point after these entries.
#States_lineWidth = 1

# Specific Line Pattern definitions for a map
# Default line patterns can be set up for each map background.  The
# possible strings are "SOLID", "DASHED", "DOTTED", "DASHED_DOTTED".  The
# values must be enclosed within quotes.   The format is mapName_linePattern.
#States_linePattern = "SOLID"

# Specific Font Offsets for a map background.
# The font offset (called magnification on the GFE menus) allows the
# default font size to be increased or decreased on a per map basis.
# Numbers can range from -2 through +2.  Format is mapName_fontOffset.
# Do not include a decimal point after these entries.
#States_fontOffset = 0


#------------------------------------------------------------------------
# Graphics Hardware Configurations
#------------------------------------------------------------------------
#
# general default X resources can be set here.
#

# Fonts.  These are the various fonts that the GFE uses.  They can be
# changed to increase/decrease the size of the text on the GFE.  The
# fonts are in ascending sizes.  A better way to override the fonts
# is to use the config items under UI Configuration.
# A valid font data representation is a string of the form fontname-style-height 
# where fontname is the name of a font, 
#       style is a font style (one of "regular", "bold", "italic", or "bold italic") 
#       height is an integer representing the font height. 
# Example: Times New Roman-bold-36. 
TextFont0 =  "DejaVu Sans Mono-regular-9"
TextFont1 =  "DejaVu Sans Mono-regular-9"
TextFont2 =  "DejaVu Sans Mono-bold-12"
TextFont3 =  "DejaVu Sans Mono-bold-14"
TextFont4 =  "DejaVu Sans Mono-bold-20"

# The color which will be used as the background for all of the display
# panes.
bgColor = "black"

#------------------------------------------------------------------------
# System Time Range Configuration
#------------------------------------------------------------------------
# These parameters indicate the span of the Grid Manager and Temporal
# Editor in relation to the current time.  Units are in hours.  If grids
# are present, the displayable time range may be expanded by the software.
SystemTimeRange_beforeCurrentTime = 48
SystemTimeRange_afterCurrentTime =  168


#------------------------------------------------------------------------
# UI Configuration
#------------------------------------------------------------------------

# Defines the color and pattern used in the Grid Manager to indicate
# a time selection.
Selected_color = 'LightSkyBlue'
Selected_fillPattern = 'TRANS_25PC_45DEG'

# Defines the color and pattern of the time scale lines in the Grid
# Manager and Temporal Editor
TimeScaleLines_color = 'Blue'
TimeScaleLines_pattern = 'DOTTED'

# Defines the color, width, and pattern used for the editor time line
# that runs through the Grid Manager and Temporal Editor
EditorTimeLine_color = 'Yellow'
EditorTimeLine_width = 2
EditorTimeLine_pattern = 'DOTTED'

# Defines the color used by the Grid Manager to indicate the
#current system time
CurrentSystemTime_color = 'Green'

# Defines the colors used in the Grid Manager to indicate that a
# time period is locked by you, or by another person.
LockedByMe_color = 'forestgreen'
LockedByMe_pattern = 'WHOLE'
LockedByOther_color = 'tomato2'
LockedByOther_pattern = 'WHOLE'

# Defines the visible, invisible,  and active colors used in the Grid
# Manager to indicate when a grid block is either visible, invisible,
# and/or active. Defines the color used to indicate which grids
# may be modified during an edit action.(Preview_color)
TimeBlockVisible_color = 'White'
TimeBlockActive_color = 'Yellow'
TimeBlockInvisible_color = 'Gray50'
TimeBlockPreview_color = 'Cyan'

# Defines the color used to indicate the Edit Area on the spatial editor.
ReferenceSet_color = 'Gray80'

# Defines the border width used to indicate the Edit Area on the spatial editor
ReferenceSet_width = 0

# Defines the initial horizontial size of the grid manager when first
# starting the GFE in pixels.  Do not place a decimal point after the number.
TimeScale_horizSize = 350

# Initial Legend Mode. Can be GRIDS for all weather elements (default),
# MUTABLE for just the Fcst weather elements,
# ACTIVE for just the active weather element, MAPS for just the maps,
# or SETIME for just the spatial editor time.
LegendMode = 'GRIDS'

# Initial Grid Manager Mode.  Can be "Normal", "History", "Saved",
# "Modified", "Published", or "Sent". Default is "Normal".
InitialGMDisplayMode = 'Normal'

# Defines the number of Edit Area Quick Set Buttons. Do not place a
# decimal point after the buttons
#QuickSetButtons = 4

# Sets the maximum number of menu items before the menu will cascade
# with a 'More >'. Do not place a decimal point after the number.
MaxMenuItemsBeforeCascade = 30

# Defines the percent that the office domain will be expanded for the
# spatial editor full-screen view.  The user can specify the expansion
# for each of the four directions.  If not specified, the default is 10%.
OfficeDomain_expandLeft = 10
OfficeDomain_expandRight = 10
OfficeDomain_expandTop = 10
OfficeDomain_expandBottom = 10

# Initial location of Edit Action Dialog
# These are absolute screen coordinates (not relative to GFE window)
# To put Edit Action Dialog in lower left corner, set Ycoord to 600
#EditActionDialog_Xcoord = 99
#EditActionDialog_Ycoord = 74

# Initial layout up of Grid Manager/Temporal Editor:
# Values:  "OnTop" (default)
#          "OnLeft"
#GM_TE_Layout = "OnTop"

# Default setting for temporal editor weather elements. Choices are
# ALL for all weather elements are displayed in the temporal
# editor, ALL_NOISC is for all weather elements except ISC (intersite coord)
# elements, MUTABLE for just the mutable weather elements (e.g., Fcst)
# displayed in the temporal editor, VISIBLE (default) for all visible
# elements in the grid manager and ACTIVE for just the single
# active weather element to be displayed in the temporal editor.
TemporalEditorWEMode = "VISIBLE"

# Extra categories for the formatter launcher.
# Products beginning with this name will get their own
# cascade.
#FormatterLauncherDialog_Categories = []

# Default setting for the Wx/Discrete Show Description option.  Setting it
# to 1 will enable the descriptions, setting it to 0 will disable the
# descriptions.
#WxDiscrete_Description = 1

# Default setting for the font and colors for the Product Output Dialog.
#ProductOutputDialog_font = TextFont2
#ProductOutputDialog_fgColor = "#000000"
#ProductOutputDialog_bgColor = "#d0d0d0"
#ProductOutputDialog_wrapMode = 1  #default, if not listed in wrapPils, nonWrap
ProductOutputDialog_wrapPils = []
ProductOutputDialog_nonWrapPils = ['AFM','PFM','FWF','SFT','WCN','FWS']
#ProductOutputDialog_wrapSize = 66
#ProductOutputDialog_lockColor = "blue"
#ProductOutputDialog_frameColor = "red"
#ProductOutputDialog_insertColor = "cyan"

# The initial size of the Call to action dialog (in pixels)
#ProductOutputDialog_CTAWidth = 575
#ProductOutputDialog_CTAHeight = 300

# Default directory to use for the ProductOutputDialog editor when
# {prddir} is not set in the product definition.
#ProductEditorDirectory = '/tmp'

#------------------------------------------------------------------------
# Process Monitor Options
#------------------------------------------------------------------------
#
# The maximum number of pending tasks to queue.
#ProcessMonitorMaxPendingTasks = 20

# The maximum number of finished tasks to keep around (so you can
# see their output).
#ProcessMonitorMaxOldTasks = 10

# The maximum number of Tasks to run at one time (user can still
# start more via the TaskMonitorDialog).
#ProcessMonitorMaxTasks = 1

#------------------------------------------------------------------------
# Sample and Legend Colors, Sample Shadows
#------------------------------------------------------------------------
# This section provides some control over the sample colors and
# the image legend color.  Normally these values are set to "white",
# but might need to be changed if the background color for the drawing
# panes color is changed.  The sample shadow may also be turned on
# or off.

# Alternative sample color. This is used primarily for ifpIMAGE when
# you want a specific color for the sample, rather than the default which
# is the graphic color.   Format is parmname_Sample_color = "color".
# Note that this applies only if the data is displayed as a graphic.
# T_Sample_color = "#ff0672"

# Alternative legend color. This is used primarily for ifpIMAGE when
# you want a specific color for the legend, rather than the default which
# is the graphic color.   Format is parmname_Legend_color = "color".
# Note that this applies only if the data is displayed as a graphic.
# T_Legend_color = "#ff0672"

# Sample LatLon and + Color. This affects the color of the '+' drawing,
# plus the color of the latitude/longitude samples on the spatial editor.
# SampleLLPlus_color = "white"

# Image Legend color. This affects the color of the legend when a weather
# element is displayed as an image.    This also affects the sample color
# for weather element displayed as an image.
#ImageLegend_color = "white"

# Sample Shadows. The samples can have a shadow character written in
# black offset from the sample text.  This improves constrast when the
# main sample color is light and the background color (e.g., image) is
# also fairly light.  Acceptable entries are yes and no.
#ShowSampleShadows = yes

# Sample Shadow Color.  The color of the shadows defaults to black.
# You can override this with any valid color.
#SampleShadow_color = "#000000"

# SampleLabelXOffset and SampleLabelYOffset are the number of pixels you
# wish to move sample labels relative to their "normal" position.
#SampleLabelXOffset = 0
#SampleLabelYOffset = 0

# Limiting Samples to Specific Weather Elements
# Controls the weather elements that will be displayed as samples.
# This feature is normally only used in conjunction with the creation
# of PNG imagery. If not specified, then all visible weather elements
# will have a sample value.
#SampleParms = ['T', 'Wind']

# Using descriptive names instead of the pretty Wx strings for samples.
# This set of parallel lists translate a pretty Wx string into a
# more descriptive name for the sample labels.
#AltWxSampleLabels_prettyWx = ['Sct RW-', 'Sct SW-']
#AltWxSampleLabels_label = ['Rain Showers', 'Snow Showers']

# ISC Update Time. The samples can show the ISC Update Time if in ISC mode.
# Acceptable entries are yes and no.
ShowISCUpdateTime = yes

# ISC Site Id. The samples can show the ISC Site Id if in ISC mode.
# Acceptable entries are yes and no.
ShowISCSiteID = yes

# Enable ISC Markers.  ISC Markers are only shown
# if ISC mode or an ISC grid is displayed.  Acceptable entries are yes and no.
ShowISCMarkers = yes

# ISC Update Time for Marker. The sample markers can show the ISC
# Update Time if in ISC mode.
# Acceptable entries are yes and no.
ShowISCUpdateTimeMarker = yes

# ISC Site Id Marker. The sample markers can show the ISC Site Id
# if in ISC mode.  # Acceptable entries are yes and no.
ShowISCSiteIDMarker = yes

# ISC Official Symbol Marker. The sample markers can show the "P" symbol
# for the # official database data or not.  Acceptable entries are yes and no.
ShowISCOfficialSymbolMarker = yes

# ISC Official Symbol. The samples can show the "P" symbol for the
# official database data or not.  Acceptable entries are yes and no.
ShowISCOfficialSymbol = yes

# Spatial Editor Color Bar Label/Tick Colors
# Controls the tick, foreground text colors for the labels,
# and the foreground/background text colors for the pickup value. There
# is a special set of colors for the Wx/Discrete (WEATHER/DISCRETE) values.
#SEColorBar_tickColor = "white"
#SEColorBar_fgTextColor = "white"
#SEColorBar_fgPickUpColor = "white"
#SEColorBar_bgPickUpColor = "black"
#SEColorBar_fgWxPickUpColor = "white"
#SEColorBar_bgWxPickUpColor = "purple"

# Configure additional labels on the SE Color Bar for WEATHER.
# The format is an array of strings which represent the ugly weather
# string.
#Wx_AdditionalColorBarLabels = [ \
#  "<NoCov>:<NoWx>:<NoInten>:<NoVis>:<NoAttr>" ]

#------------------------------------------------------------------------
# GFE Font Sizes
#------------------------------------------------------------------------
# This section provides the user the capability of changing the font
# sizes in various components of the GFE.  The font numbers can range
# from 0 through 4 with 0 being the smallest.

# These font entries define the fonts used by various components of
# the GFE.
#ColorBarScale_font = 1
#ColorBarWxLabel_font = 2
#ColorBarPickUp_font = 3
#SESample_font = 2
#SEMarker_font = 3
#SELegend_font = 3
#TEDataSelector_font = 1
#TESample_font = 1
#TimeBlockLabel_font = 3
#TimeBlockSource_font = 1
#TimeScale_font = 2
#SetValueContLabel_font = 2
#SetValuePickUp_font = 3

# Defines the default labeling size on the Bounded Area display for
# weather, the contour tool depiction font, the map background font,
# and the contour labeling font. These fonts can also be modified on
# a per-parm basis using the fontOffset capability defined in the
# parameter configuration.
#BoundedArea_font = 2
#Cline_font = 2
#Contour_font = 2

#------------------------------------------------------------------------
# Grid Manager Saved, Published, Sent configurations
#------------------------------------------------------------------------
# Defines the colors and times used to color the Grid Manager when
# in the last saved, last modified, last published, or last sent display mode.

# parallel list of minutes and colors.   If the last save, modified,
# published, sendISC time is less than the given time (in minutes),
# then that color is used to display the box.   The default if the
# last xxx time is greater than the final "minutes" in the list, is Gray75.
Modified_minutes = [60, 180, 360, 720, 1440, 2880 ]
Modified_colors = ["#0bc71e", "#60c7b8", "#417fc7", "#e17c10",
  "#ebdf00", "#e11a00"]

Saved_minutes = [60, 180, 360, 720, 1440, 2880 ]
Saved_colors = ["#0bc71e", "#60c7b8", "#417fc7", "#e17c10",
  "#ebdf00", "#e11a00"]

Published_minutes = [60, 180, 360, 720, 1440, 2880 ]
Published_colors = ["#0bc71e", "#60c7b8", "#417fc7", "#e17c10",
  "#ebdf00", "#e11a00"]

Sent_minutes = [60, 120, 180, 240, 300, 360]
Sent_colors = ["#0bc71e", "#60c7b8", "#417fc7", "#e17c10",
  "#ebdf00", "#e11a00"]


#------------------------------------------------------------------------
# Grid Data History configuration
#------------------------------------------------------------------------
# Defines the characters, colors, and patterns  that will appear
# in the Grid Manager grid blocks
# to indicate the source, origin, and modification states.
#
# If the grid has been modified by me or someone else, the text in the
# grid block and grid pattern is modified to that specified below:
HistoryUserModText_Me = "m"            #Text for modified by me
HistoryUserModText_Other = "o"         #Text for modified by other
HistoryUserModPattern_Me = "TRANS_25PC_45DEG"  #Pattern for mod by me
HistoryUserModPattern_Other = "TRANS_25PC_135DEG"  #Pattern for mod by other

# The text in the grid block and the grid color will represent the origin:
# Note that the user can also override the populated in the next section.
HistoryOriginText_Populated = "P"
HistoryOriginText_Calculated = "C"
HistoryOriginText_Scratch = "S"
HistoryOriginText_Interpolated = "I"
HistoryOriginText_Other = "?"
HistoryOriginColor_Populated = "wheat"
HistoryOriginColor_Calculated = "red"
HistoryOriginColor_Scratch = "magenta"
HistoryOriginColor_Interpolated = "blue"
HistoryOriginColor_Other = "gray75"

# This next section applies to the text and the color of the grid blocks
# that have an origin of Populated.  The model determines the text and color.
# The format of the color entry is HistoryModelColor_modelname.  The format
# of the text entry is: HistoryModelText_modelname.  If a model is not
# listed here, then the HistoryOriginText_Populated and
# HistoryOriginColor_Populated is used.
HistoryModelColor_gfsLR = '#30df10'
HistoryModelColor_NGM80 = 'orange'
HistoryModelColor_NGM95 = 'orange'
HistoryModelColor_RUC80 = '#00ffff'
HistoryModelColor_MAVMOS = '#e6c8a1'
HistoryModelColor_GFSMOS = '#e6d8a1'
HistoryModelColor_METMOS = '#e6b8a1'
HistoryModelColor_MEXMOS = '#e6a8a1'
HistoryModelColor_NGMMOS = '#e608a1'
HistoryModelColor_NAM80 = '#ffff52'
HistoryModelColor_NAM95 = '#ffff52'
HistoryModelColor_NAM40 = '#ff99ff'
HistoryModelColor_NAM12 = '#ffcaa0'
HistoryModelColor_GFS80 = 'pink'
HistoryModelColor_GFS40 = 'pink'
HistoryModelColor_GFS190 = 'pink'
HistoryModelColor_GWW = '#a0a0ff'
HistoryModelColor_HPCStn = '#d0d0a0'
HistoryModelColor_HPCGrid = '#d0d0b0'
HistoryModelColor_ISC = '#b43aee'
HistoryModelColor_LAPS = '#b06b72'
HistoryModelColor_HPCQPF = '#3dc9ff'
HistoryModelColor_HPCGuide = '#3dc9ff'
HistoryModelColor_RFCQPF = '#3bffb7'
HistoryModelColor_Restore = '#e0a0ff'
HistoryModelColor_DGEX = 'orange'
HistoryModelColor_MOSGuide = '#e608ff'
HistoryModelColor_OPCTAFBE = '#a0a0cc'
HistoryModelColor_OPCTAFBSW = '#a0a0cc'
HistoryModelColor_OPCTAFBNW = '#a0a0cc'
HistoryModelColor_RTMA = '#a0522d'
HistoryModelColor_NamDNG5 = '#808000'

HistoryModelText_GFS80 = 'GFS'
HistoryModelText_GFS40 = 'GFS'
HistoryModelText_GFS190 = 'GFS'
HistoryModelText_NGM80 = 'NGM'
HistoryModelText_NGM95 = 'NGM'
HistoryModelText_RUC80 = 'RUC'
HistoryModelText_GFSMOS = 'GFSMOS'
HistoryModelText_MEXMOS = 'MEXMOS'
HistoryModelText_MAVMOS = 'MAVMOS'
HistoryModelText_METMOS = 'METMOS'
HistoryModelText_NGMMOS = 'NGMMOS'
HistoryModelText_NAM80 = 'N80'
HistoryModelText_NAM95 = 'N95'
HistoryModelText_NAM40 = 'N40'
HistoryModelText_NAM20 = 'N20'
HistoryModelText_NAM12 = 'N12'
HistoryModelText_gfsLR = 'gfsLR'
HistoryModelText_HPCStn = 'HPCs'
HistoryModelText_HPCGrid = 'HPCg'
HistoryModelText_GWW = 'GWW'
HistoryModelText_ISC = 'ISC'
HistoryModelText_LAPS = 'LAPS'
HistoryModelText_HPCQPF = 'HPCQPF'
HistoryModelText_HPCGuide = 'HPCGuide'
HistoryModelText_RFCQPF = 'RFCQPF'
HistoryModelText_Restore = 'Restore'
HistoryModelText_DGEX = 'DGEX'
HistoryModelText_MOSGuide = 'GMOS'
HistoryModelText_OPCTAFBE = 'OPC'
HistoryModelText_OPCTAFBSW = 'OPC'
HistoryModelText_OPCTAFBNW = 'OPC'
HistoryModelText_RTMA = 'RTMA'
HistoryModelText_NamDNG5 = 'Nd5'


#------------------------------------------------------------------------
# Algorithm Configuration
#------------------------------------------------------------------------
# Smart tools can access time-weighted averages of multiple grids.  Since
# weather is discrete, time weighted average for weather is based on
# all values of weather at that grid point as long as they occupy at least
# the given percentage of all grids.  Do not place a decimal point after
# the number.
SignificantWeatherTimeWeightAverage_percent = 40

# The default width of the pencil tool can be specified in grid cells
# on a per weather element basis.  The format is parmName_pencilWidth.
# If not specified, the value defaults to 4.
#T_pencilWidth = 4

# Pencil Tool influence sizes are specified here
PencilToolInfluence_list = [1, 2, 4, 8, 12, 16]

# Smooth algorithm default value
SmoothSize = 3

# Smooth Size Choices
SmoothSizeList = [3, 5, 7, 9]

# User can control the interpolation algorithm for each weather element.
# The format of the string is parmName_interpolateAlgorithm.  The available
# options, which must be quoted, are "CUBIC_ADVECT", "LINEAR_ADVECT",
# "CUBIC_NOADVECT", and "LINEAR_NOADVECT".  By default, most elements use
# CUBIC_NOADVECT, except for Wx, PoP, Sky, and QPF which use CUBIC_ADVECT.
# Wind and Wx cannot be changed.
# T_interpolateAlgorithm = "CUBIC_NOADVECT"


#------------------------------------------------------------------------
# Menu and Dialog Configuration
#------------------------------------------------------------------------
# Entries allow the specification of the zoom factor (click 1) over the
# Pickup Value Dialog.  There is only one zoom step.  If not specified,
# the default is set to a zoom factor of 4.  You can also specify specific
# zoom factors based on the parameter name.
# SetValue_zoom is the generic zoom value.  parmName_SetValue_zoom is
# the parameter-specific zoom value.  Do not place a decimal point
# after the numbers.
SetValue_zoom = 4
QPF_SetValue_zoom = 10

# The maximum value on the Set Delta Dialog may be set on a
# per weather element basis.  Format is weName_MaxDeltaDialogValue.
# The floating-point entry requires a decimal point in the value.
# The default is 20% of the weather element data range.
#Sky_MaxDeltaDialogValue = 30.0


# The default value of the Interpolate Dialog mode may be set to
# either "Gaps" or "Edited", which refer to "By Gaps" and "Based on
# Edited Data" on the dialog.  The default if not specified is "By Gaps".
#InterpolateDialogMode = "Gaps"

# In Formatter Launcher, whether to highlight framing codes and the
# text enclosed by them in the text editor component.
HighlightFramingCodes = no


#------------------------------------------------------------------------
# Weather Element Configuration
#------------------------------------------------------------------------

# generic colors for graphics -----------------------------------
# These colors will be the colors assigned to the graphics, unless
# specific colors are assigned to each parameter.
Generic_colors = ['#00ff00', '#ff8e59', '#00ffff', '#e6c8a1',
                  '#ffff52', '#ff99ff', '#aeb370', '#ff4000',
                  '#e6c8a1']

# Specific Graphic Colors for a parameter
# The user may specify a specific color to be used for a parameter, rather
# than getting a random color assigned.  This color will be assigned, if
# available.  Format is parmName_graphicColor = color.  The color does
# not need to be in the Generic_colors list.
#T_graphicColor = 'green'
Wx_graphicColor = '#ffffff'

# Specific Graphic Line Widths for a parameter
# Default line widths can be set for each weather element, which will
# be used to draw their graphics on the spatial editor.  0 is the default
# value, which represents thin lines.  The larger the number, the wider
# the line.  The format is parmName_lineWidth.  Do not include a decimal
# point after these entries.
#T_lineWidth = 1

# Specific Line Pattern definitions for a parameter.
# Default line patterns can be set up for each weather element.  The
# possible strings are "SOLID", "DASHED", "DOTTED", "DASH_DOTTED".  The
# values must be enclosed within quotes.   The format is parmName_linePattern.
#T_linePattern = "SOLID"

# Specific Font Offsets for a parameter.
# The font offset (called magnification on the GFE menus) allows the
# default font size to be increased or decreased on a per-parameter
# basis.  Note that for wind arrows/barbs, the fontOffset controls the
# size of the wind arrows/barbs.  Numbers can range from -2 through +2.
# Format is parmName_fontOffset. Do not include a decimal point
# after these entries.
#T_fontOffset = 0

# Specific Density definitions for a parameter.
# The density controls the packing of wind barbs and arrows for the vector
# spatial editor displays, and the packing of contour intervals for the
# scalar spatial editor displays.  The default is zero.  Densities and
# contour values are related to each other.  Typical values can range
# from -2 through +2.  You can use values outside of this range if
# desired.  Format is parmName_density.  Do not include a
# decimal point after these entries.
#T_density = 0

# temporal editor sizes -----------------------------------------
# the initial size of temporal editor panes may be defined on a
# per parameter basis.  If not specified, the default is 150 pixels.
# Format is: parmName_temporalDataPaneSize = size
# Do not place a decimal point after the numbers.
# Wx_temporalDataPaneSize = 200

# contour values -----------------------------------------------
# contour values may be defined on a per-parameter basis.  If not
# defined, then contour values are automatically computed.
# Format is wxElementName_contourValues = [c1, c2, c3, c4, ... ]
# Be sure to include decimal points in each entry.
# This overrides any entries that may exist in contour interval.
QPF_contourValues = [0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50,
  0.60, 0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8,
  3.0, 3.2, 3.4, 3.6, 3.8, 4.0, 4.2, 4.4, 4.6, 4.8, 5.0]

Topography_contourValues = [5.0, 10.0, 20.0, 30.0, 40.0, 50.0,
  60.0, 70.0, 80.0, 90.0, 100.0, 125.0, 150.0, 175.0, 200.0, 250.0,
  300.0, 350.0, 400.0, 450.0, 500.0, 600.0, 700.0, 800.0, 900.0,
  1000.0, 1250.0, 1500.0, 1750.0, 2000.0, 2500.0, 3000.0, 3500.0,
  4000.0, 4500.0, 5000.0, 5500.0, 6000.0, 6500.0, 7000.0, 7500.0,
  8000.0, 8500.0, 9000.0, 9500.0, 10000.0, 11000.0, 12000.0, 13000.0,
  14000.0, 15000.0, 16000.0, 17000.0, 18000.0, 19000.0, 20000.0]

# contour intervals  -----------------------------------------------
# contour intervals may be defined on a per-parameter basis.  If not
# defined, then contour values are automatically computed.
# Format is wxElementName_contourInterval = value.
# Be sure to include decimal points in the entry.
# Note, you can also specify wxElementName_contourValues, which
# will override the entry for contour interval.
Sky_contourInterval = 10.0
PoP_contourInterval = 10.0
MinT_contourInterval = 5.0
MaxT_contourInterval = 5.0
T_contourInterval = 5.0
Td_contourInterval = 5.0

# delta values
# Delta values define the default delta (adjust up, adjust down) value
# for the adjust operations.  The user can redefine this at any time
# through the GUI.  If not specified, the delta value defaults to
# the precision value.  For example, a precision of 0 indicates a delta of 1.
# and a precision of 1 indicates a delta of 0.1.
# Format is parmName_deltaValue = value.
# Be sure to include a decimal point.
#parmName_deltaValue = 10.0
FzLevel_deltaValue = 100.0
SnowLevel_deltaValue = 100.0

# fuzz values
# fuzz values define the value considered to be the same during a
# homogenous area select using the GridPoint Tool. For example, if the
# fuzz is 2.0 degrees for Temperature and you click on 40 degrees, then
# all points between 38 and 42 will be selected as long as they are
# contiguous to the click point.  If not specified, the fuzz is set
# to 1/100 of the parm range.  Format is parmName_fuzzValue = value.
# Be sure to include a decimal point.
#parmName_fuzzValue = 10.0

# visual types
# This section defines the spatial and temporal editor visualization
# types for the scalar, vector, and weather parameters. There are two
# modes, graphic and image.  For example, the weather parameter may be
# viewed as a bounded area and an image.  Available types:
# Spatial Editor Options:
# Scalar: Image, Contour
# Vector: Image, WindBarb, WindArrow
# Weather: Image, BoundedArea
# Discrete: Image, BoundedArea
# Temporal Editor Options:
# Scalar: TEColorBar, TEColorRangeBar, TimeBar, RangeBar
# Vector: TEColorBar, TEColorRangeBar, TimeBar, RangeBar
# Weather: TEColorBar, TEColorRangeBar
# Discrete: TEColorBar, TEColorRangeBar
#
# format is: parmName_editorImageType = [ types ] or
# parmName_editorGraphicType = [ types ] where 'editor' is replaced with
# spatial or temporal.
# For example, to make wind appear as wind arrows on the spatial editor
# in graphic mode:  Wind_spatialGraphicType = ["WindArrow"].
Wx_spatialImageType = [ "Image", "BoundedArea" ]
Headlines_spatialImageType = [ "Image", "BoundedArea" ]
Swell_spatialImageType = [ "Image", "WindArrow" ]
Swell2_spatialImageType = [ "Image", "WindArrow" ]
Swell_spatialGraphicType = [ "WindArrow" ]
Swell2_spatialGraphicType = [ "WindArrow" ]


# Bounded Area Visual attributes
# The user may turn on/off the boundary, and the text labels, for
# the bounded area visual. Allowables values are yes and no (or 1 and 0).
# By default, then are both enabled.
#BoundedArea_Labels = yes
#BoundedArea_Boundary = yes

# Wind Barb and Arrow Default Sizes.
# The user may specify the default wind barb and arrow default sizes,
# for the GFE, or by the weather element name.  The default size is 60
# pixels.  The entry format for a particular weather element definition
# of arrow or barb size is parmName_windArrowDefaultSize and
# parmName_windBarbDefaultSize.
WindArrowDefaultSize = 60
WindBarbDefaultSize = 60
#Wind_windArrowDefaultSize = 60
#Wind_windBarbDefaultSize = 60

# Wind Arrow Scaling
# The user may specify the default scaling for the wind arrow.  If not
# specified, then the wind arrows will grow linearly with an increase
# in magnitude.  To emphasize the lower ranges, the user may set the
# wind arrow logarithmic scaling.  The lower the number,
# the steeper the log curve will appear.  Refer to on-line documentation
# for example values. Include decimal points with the numbers.
# Note that the factor needs to be greater than 0.  The format of the
# entry is parmName_arrowScaling.
Wind_arrowScaling = 0.03
Swell_arrowScaling = 0.001
Swell2_arrowScaling = 0.001

# Wind Sample Format
# The user may specify the default sample format for vector weather elements.
# If not specified, then the format is "ddff". The user may specify a format
# for all vector elements, or can specify the format for a particular weather
# element.  The four possible formats are "ddff", "8pt", "16pt", and "d/f".
# The configuration entry for the default sample format for all vector
# elements is WindFormat = "type".  The entry format to define the format for
# a specific entry is parmName_windFormat = "type".
WindFormat = "ddff"
#Swell_windFormat = "8pt"

# Default Values (for create from scratch)
# The default values for SCALAR, VECTOR, WEATHER, and DISCRETE may be
# specified on a per-weather element basis.  By default, SCALAR has the
# weather element's minimum value, VECTOR has a magnitude and direction of 0,
# WEATHER has <NoWx>, and DISCRETE has the first defined discrete key
# (always <None> for Hazards grids, user-defined DISCRETE grids may vary).
# Format of the entry is parmName_defaultValue, or parmName_level_defaultValue
# for non-surface based SCALAR, WEATHER, or DISCRETE elements. For VECTOR,
# the format is slightly different: parmName_magDefaultValue has the
# magnitude, and parmName_dirDefaultValue has the direction in degrees. 
# A decimal point is required for SCALAR and VECTOR, strings are required for 
# WEATHER and DISCRETE.
#
#T_defaultValue = 32.0
#Wx_defaultValue = "<NoCov>:<NoWx>:<NoInten>:<Novis>:"
#Wind_dirDefaultValue = 90.0

#------------------------------------------------------------------------
# Weather/Discrete Common Value Definitions
#------------------------------------------------------------------------
# the following describes common types that appear on the temporal
# editor popup menu and the spatial editor color bar popup menu.
# For WEATHER, the format is the "ugly" string of the Weather Key. For
# DISCRETE, the format is the key string of the Discrete Key.
# Prefixing an string with other strings that end with a vertical
# bar (|) will make these strings in a cascade,
# such as "Winter|Wide:S:--:<NoVis>:<NoAttr>",
# which will put the widespread snow under a Winter cascade.  The format
# of this entry is parmName_commonValues, and applies to Weather and
# Discrete only.

Wx_commonValues = [ \
  "<NoCov>:<NoWx>:<NoInten>:<NoVis>:<NoAttr>",
  "Wide:R:-:<NoVis>:<NoAttr>",
  "Wide:S:--:<NoVis>:<NoAttr>",
  "Wide:R:-:<NoVis>:<NoAttr>^Wide:S:-:<NoVis>:<NoAttr>",
  "Sct:RW:-:<NoVis>:<NoAttr>",
  "Sct:SW:-:<NoVis>:<NoAttr>",
  "Sct:T:<NoInten>:<NoVis>:<NoAttr>^Sct:RW:-:<NoVis>:<NoAttr>",
  "Patchy:F:<NoInten>:<NoVis>:<NoAttr>"]


Hazards_commonValues = [ \
    "Watches|Fire Weather|FW.A",
    "Watches|Hydrology|FF.A",
    "Watches|Hydrology|FA.A",
    "Watches|Coastal Flooding|CF.A",
    "Watches|Coastal Flooding|LS.A",
    "Watches|Marine|GL.A",
    "Watches|Marine|HF.A",
    "Watches|Marine|SE.A",
    "Watches|Marine|SR.A",
    "Watches|Marine|UP.A", 
    "Watches|Non-Precipitation|EH.A",
    "Watches|Non-Precipitation|FZ.A",
    "Watches|Non-Precipitation|HW.A",
    "Watches|Non-Precipitation|HZ.A",
    "Watches|Non-Precipitation|EC.A",
    "Watches|Winter Storm|BZ.A",
    "Watches|Winter Storm|LE.A",
    "Watches|Winter Storm|WC.A",
    "Watches|Winter Storm|WS.A",
    "Warnings|Fire Weather|FW.W",
    "Warnings|Coastal Flooding|CF.W",
    "Warnings|Coastal Flooding|LS.W",
    "Warnings|Coastal Flooding|SU.W",
    "Warnings|Marine|MH.W",
    "Warnings|Marine|HF.W",
    "Warnings|Marine|GL.W",
    "Warnings|Marine|UP.W",
    "Warnings|Marine|SR.W",
    "Warnings|Marine|SE.W",
    "Warnings|Non-Precipitation|AF.W",
    "Warnings|Non-Precipitation|DS.W",
    "Warnings|Non-Precipitation|EH.W",
    "Warnings|Non-Precipitation|FZ.W",
    "Warnings|Non-Precipitation|HW.W",
    "Warnings|Non-Precipitation|HZ.W",
    "Warnings|Non-Precipitation|EC.W",
    "Warnings|Winter Storm|BZ.W",
    "Warnings|Winter Storm|IS.W",
    "Warnings|Winter Storm|LE.W",
    "Warnings|Winter Storm|WC.W",
    "Warnings|Winter Storm|WS.W",
    "Advisories|Marine|UP.Y",
    "Advisories|Marine|LO.Y",
    "Advisories|Marine|SC.Y",
    "Advisories|Marine|SW.Y",
    "Advisories|Marine|BW.Y",
    "Advisories|Marine|RB.Y",
    "Advisories|Marine|SI.Y",
    "Advisories|Marine|MF.Y",
    "Advisories|Marine|MS.Y",
    "Advisories|Marine|MH.Y",
    "Advisories|Coastal Flooding|CF.Y",
    "Advisories|Coastal Flooding|LS.Y",
    "Advisories|Coastal Flooding|SU.Y",
    "Advisories|Non-Precipitation|AS.O",
    "Advisories|Non-Precipitation|AS.Y",
    "Advisories|Non-Precipitation|AQ.Y",
    "Advisories|Non-Precipitation|DU.Y",
    "Advisories|Non-Precipitation|FG.Y",
    "Advisories|Non-Precipitation|SM.Y",
    "Advisories|Non-Precipitation|ZF.Y",
    "Advisories|Non-Precipitation|FR.Y",
    "Advisories|Non-Precipitation|HT.Y",
    "Advisories|Non-Precipitation|LW.Y",
    "Advisories|Non-Precipitation|AF.Y",
    "Advisories|Non-Precipitation|WI.Y",
    "Advisories|Winter Weather|ZR.Y",
    "Advisories|Winter Weather|LE.Y",
    "Advisories|Winter Weather|WC.Y",
    "Advisories|Winter Weather|WW.Y",
    "Statements|Coastal Flooding|CF.S",
    "Statements|Coastal Flooding|LS.S",
    "Statements|Coastal Flooding|RP.S",
    "Statements|Marine|MA.S",
    ]


#------------------------------------------------------------------------
# Weather Dialog Default Values
#------------------------------------------------------------------------
# the following describes the intensity and coverage/probability defaults
# that appear in the Set Value dialog for Weather data.  The format is
# the weather type (e.g., RW), followed by the keyword.  The actual value
# is a string surrounded in quotes.

# Define the weather dialog default coverage/probabilities
R_defaultCoverage = "Wide"
RW_defaultCoverage = "Sct"
S_defaultCoverage = "Wide"
SW_defaultCoverage = "Sct"
T_defaultCoverage = "Sct"

# Define the weather dialog default intensities
R_defaultIntensity = "-"
RW_defaultIntensity = "-"
S_defaultIntensity = "-"
SW_defaultIntensity = "-"
L_defaultIntensity = "-"
ZR_defaultIntensity = "-"
ZL_defaultIntensity = "-"
IP_defaultIntensity = "-"

#------------------------------------------------------------------------
# Default Discrete Color Table Algorithm Configuration
#------------------------------------------------------------------------
# DiscreteOverlapPatterns are used for overlapping (non-exclusive)
# Discrete weather elements when two or more values are overlapping.
# Each entry denotes the fill pattern to use when it is overlapping
# another pattern.  The available types are: WHOLE, WIDE, SCATTERED,
# WIDE_SCATTERED, ISOLATED, TRANS_25PC_45DEG, SELECTED_AREA, OCNL,
# LKLY, TRANS_25PC_135DEG, DUALCURVE, CURVE, VERTICAL, CROSS, HORIZONTAL,
# BIGCROSS.  DiscreteOverlapPatterns are used for all discrete weather
# elements, unless a parmName_level_DiscreteOverlapPatterns is found.
#------------------------------------------------------------------------
DiscreteOverlapPatterns = ['TRANS_25PC_45DEG', 'TRANS_25PC_135DEG', 'CROSS']
#pName_level_DiscreteOverlapPatterns = ['pat1', 'pat2', 'pat3']

# DiscreteComplexColor is used when there aren't enough fill patterns
# defined for overlap.  This color is used when a very complex overlapping
# situation occurs.  DiscreteComplexColor applies to all discrete
# weather elements, unless a parmName_level_DiscreteComplexColor
# value is found. Default is "White".
#DiscreteComplexColor = 'White'
#pName_level_DiscreteComplexColor = 'color'

# DiscreteComplexPattern is used when there aren't enough fill patterns
# defined for overlap.  This pattern is used when a very complex overlapping
# situation occurs.  DiscreteComplexPattern applies to all discrete
# weather elements, unless a parmName_level_DiscreteComplexPattern
# value is found. Default is "SCATTERED".
#DiscreteComplexPattern = 'SCATTERED'
#pName_level_DiscreteComplexPattern = 'pattern'

#------------------------------------------------------------------------
# Default (non-weather) Color Table Algorithm Configuration
#------------------------------------------------------------------------
# The default color table is used for all parameters unless overridden in
# this configuration file.  The left wavelength defines the left side
# value for the color in nanometers. 380 is roughly purple.  The right
# wavelength defines the right side value for the color in nanometers.
# 650 is red.  The number of colors indicate the number of color bins
# that will be used when the default color table is displayed.
# Use decimal points after the wavelengths, but not the numColors.
DefaultColorTable_leftWavelength = 380.0
DefaultColorTable_rightWavelength = 650.0
DefaultColorTable_numColors = 150

# color table default entries -----------------------------
# Entries can be made to define a default color table for a particular
# parameter. If a default color table is not defined for a parameter, then
# the spectrum defined in DefaultColorTable* will be used for the parameter.
# Entries are of the form parmName_defaultColorTable="colortablename".
# For example, if you want MaxT to always have a "Low-Enhanced" color table,
# then the entry would be as shown below.
# MaxT_defaultColorTable = "Low-Enhanced"
# You can determine the possible color tables that are on the system by
# displaying any scalar image and selecting "Change Color Table To".

T_defaultColorTable="GFE/Mid Range Enhanced"
Td_defaultColorTable="GFE/Mid Range Enhanced"
MaxT_defaultColorTable="GFE/Mid Range Enhanced"
MinT_defaultColorTable="GFE/Mid Range Enhanced"
Sky_defaultColorTable="GFE/Cloud"
Wind_defaultColorTable="GFE/Low Range Enhanced"
Wind20ft_defaultColorTable="GFE/Low Range Enhanced"
PoP_defaultColorTable="GFE/ndfdPoP12"
QPF_defaultColorTable="GFE/Gridded Data"
Ttrend_defaultColorTable = "GFE/Discrepancy"
RHtrend_defaultColorTable = "GFE/Discrepancy"
Wetflag_defaultColorTable = "GFE/YesNo"
DeltaMinT_defaultColorTable = "GFE/Discrepancy"
DeltaMaxT_defaultColorTable = "GFE/Discrepancy"
DeltaWind_defaultColorTable = "GFE/Discrepancy"
DeltaSky_defaultColorTable = "GFE/Discrepancy"
DeltaPoP_defaultColorTable = "GFE/Discrepancy"
# Default Satellite weather element color tables
visibleEast_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
ir11East_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir13East_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir39East_defaultColorTable = "Sat/IR/CIRA (IR Default)"
waterVaporEast_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"
visibleCentral_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
ir11Central_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir13Central_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir39Central_defaultColorTable = "Sat/IR/CIRA (IR Default)"
waterVaporCentral_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"
visibleWest_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
ir11West_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir13West_defaultColorTable = "Sat/IR/CIRA (IR Default)"
ir39West_defaultColorTable = "Sat/IR/CIRA (IR Default)"
waterVaporWest_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"

VisibleE_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
IR11E_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR13E_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR39E_defaultColorTable = "Sat/IR/CIRA (IR Default)"
WaterVaporE_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"
FogE_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"

VisibleC_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
IR11C_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR13C_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR39C_defaultColorTable = "Sat/IR/CIRA (IR Default)"
WaterVaporC_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"
FogC_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"

VisibleW_defaultColorTable = "Sat/VIS/ZA (Vis Default)"
IR11W_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR13W_defaultColorTable = "Sat/IR/CIRA (IR Default)"
IR39W_defaultColorTable = "Sat/IR/CIRA (IR Default)"
WaterVaporW_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"
FogW_defaultColorTable = "Sat/WV/Gray Scale Water Vapor"

Hazards_defaultColorTable = "GFE/Hazards"

# TopDownWx
MaxTAloft_defaultColorTable="WarmNoseTemp"
WetBulb_defaultColorTable="WetBulbTemp"

# Logarithmic Color Table Assignments
# By default, all color tables are linear.  Certain parameters may lend
# themselves to a logarithmic color table.  To enable a logarithmic
# color table for a parameter, an entry in the form of
# parmName_LogFactor=factor is required.  The closer the value is to zero,
# the steeper the log curve will appear.  Refer to on-line documentation
# for example values. Include decimal points with the numbers.
# Note that the factor needs to be greater than 0
QPF_LogFactor = 0.03
SnowAmt_LogFactor = 0.6

# Default Max/Min Ranges for Color Tables
# By default, all colors tables (except for WEATHER) are spread out over
# the range of the minimum to maximum weather element possible value, as
# defined by serverConfig.py.  The initial range of the color table can
# be specified through these entries.  The form of the two entries are:
# parmName_maxColorTableValue and parmName_minColorTableValue.  These
# values are floats and MUST have a decimal point in them.
#T_maxColorTableValue = 120.0
#T_minColorTableValue = -30.0
WetBulb_maxColorTableValue = 50.0
WetBulb_minColorTableValue = 20.0

# Fit To Data Color Tables
# Automatic Fit To Data color tables can be set up for the initial set
# of data in a weather element.  The form of the entry is:
# parmName_fitToDataColorTable.  The fit to data overrides any
# specified max/min color table values. There are several algorithms
# available: "None", "All Grids", "Single Grid", "All Grids over Area",
# and "Single Grid over Area". The Single Grid options are not
# available for the GFE and only apply to the ifpIMAGE program.
# Note that the ifpIMAGE program can specify an edit area to use for
# the "All Grids over Area" and "Single Grid over Area" algorithms.
# See Png_fitToDataArea.  For the GFE, the active edit area is used in
# the fit to data scheme.
#T_fitToDataColorTable = "None"

# Configure the desired labels on the SE Color Bar on a per-parameter basis.
# The format is parmName + "_ColorBarLabels".  For example, the color bar
# would be labeled at 10, 20, 40 & 60 for temperature with the following
# entry. Note that the values need to be entered as floats for all parameters.
# This is only used for SCALAR or VECTOR parameters.
# For WEATHER or DISCRETE parameters, use parmName_additionalColorBarLabels.
#T_ColorBarLabels = [10.00, 20.00, 40.00, 60.00]

#------------------------------------------------------------------------
# Weather Color Algorithm Configuration
#------------------------------------------------------------------------
# Color Tables for Weather are handled differently than scalar and
# vector data.  Coverages are denoted by fill patterns.  Composite
# types by colors.  Complex weather of more than two coverages will
# result in a solid fill pattern and can't be configured.


# The WeatherCoverage_names and WeatherCoverage_fillPatterns indicate
# the fill pattern used for a particular weather coverage or probability.
# These are parallel lists.  For example, if "Iso" coverage is in the 1st
# entry of the list and ISOLATED appears in the first entry of the
# fill patterns, the for Iso coverage, the fill pattern ISOLATED will
# be used.
WeatherCoverage_names = ["Iso", "Sct", "Num", "Wide", "Ocnl", "SChc",
                         "Chc", "Lkly", "Def", "Patchy", "<NoCov>", "Areas",
                         "Frq", "Brf", "Pds", "Inter"]
WeatherCoverage_fillPatterns = ["WIDE_SCATTERED", "SCATTERED", "LKLY", "WIDE",
                                 "OCNL", "WIDE_SCATTERED", "SCATTERED", "LKLY",
                                "WIDE", "CURVE", "WHOLE", "DUALCURVE",
                                 "OCNL", "OCNL", "OCNL", "OCNL"]

# The weather type entries are generic entries without intensities.
# Combinations are permitted.  The WeatherType_names and WeatherType_colors
# are parallel lists of names and colors.  The default weather color table
# algorithm looks at the weather type or combination of types, as listed
# in the _names, and matches the list with the specified color.  For
# example, if T appears in the names as the first entry and brown2 appears
# in the colors for the first entry, then for weather type T, the color
# shown will be brown2.

WeatherType_names = ["<NoWx>", "T", "R", "RW", "L", "ZR", "ZL",
                     "S", "SW", "IP", "F", "H", "BS", "K", "BD",
                     "SA", "LC", "FR", "AT", "TRW"]
WeatherType_colors = ["Gray40", "red3", "ForestGreen",
                      "ForestGreen", "CadetBlue1", "darkorange1",
                      "goldenrod1", "Grey65", "Grey65", "plum1",
                      "khaki4", "Gray75", "snow", "grey30", "Brown",
                      "blue1", "coral1", "pale turquoise", "DeepPink",
                      "red3"]

# The weather type entries are specific entries that contain intensities.
# Combinations are permitted.  The WeatherTypeInten_names and
# WeatherTypeInten_colors are parallel lists of names and colors.  The
# algorithm looks first at this list to find a specific type/intensity
# match.  If not found, then the algorithm looks in the WeatherType_names
# and WeatherType_colors list for a match.  If not found, then a generic
# color is assigned.
# The weather type with intensity entries are specific entries

WeatherTypeInten_names = ["T+", "Rm", "R+", "RWm", "RW+"]
WeatherTypeInten_colors = ["red1", "green", "green", "green", "green"]

# Colors to use for weather which was not defined using any of the methods
# found above. The colors in this list will be used before a "random" color
# is chosen.
WeatherGeneric_colors = ["Coral", "CadetBlue2", "Aquamarine", "DarkKhaki",
                         "DodgerBlue", "IndianRed1", "PaleGreen", "MistyRose",
                         "chartreuse3", "PapayaWhip"]

#------------------------------------------------------------------------
# Preference Defaults
#------------------------------------------------------------------------
# Default setting for changing the active grid to an image-type display.
# This occurs when "Edit Grid" from the Grid Manager or setting a parameter
# active from the legend.
ImageOnActiveSE = yes

# Default visibility setting for showing the time scale lines in the
# Grid Manager and Temporal Editor
TimeScaleLines = yes

# Default visibility setting for showing the editor time lines in the
# Grid Manager and Temporal Editor.  The editor time line is always on
# for the Time Scale.
EditorTimeLines = yes

# Default visibility setting for showing the split boundary or time
# constraints in the Grid Manager and Temporal Editor for mutable parameters.
SplitBoundaryDisplay = yes

# Default setting for combining like parameters (same units) in the
# temporal editor when loading parameters.
TemporalEditorOverlay = yes

# Default setting for temporal editor edits.  Choices are absolute mode
# or relative mode which is defined by "yes" or "no".
TemporalEditorAbsoluteEditMode = no

# Initial statistics mode for temporal editor range statistics dialog. 
# Choices are "ABSOLUTE", # "MODERATED", or "STANDARD_DEVIATION".
TemporalEditorStatisticsMode = "ABSOLUTE"
# Initial minimum and maximum values for scales on temporal editor range
# statistics dialog in moderated and stadard deviation operation modes 
# (dialog is not shown in absolute mode). Do NOT include a decimal point
# for moderated mode values, you MUST include a decimal point for standard
# deviation values.
TemporalEditorStatisticsModeModeratedMin = 15
TemporalEditorStatisticsModeModeratedMax = 15
TemporalEditorStatisticsModeStandardDeviationMin = 1.0
TemporalEditorStatisticsModeStandardDeviationMax = 1.0

# Default setting for editing components of vector parameters.  Choices
# are MAG, DIR, or BOTH.
WindEditMode = "BOTH"

# Default setting for automatic combining of existing weather/discrete and new
# weather/discrete when editing.  For example, if the setting is yes
# and existing weather is Rain, then setting the value to Snow will result in
# a Rain/Snow mix.
WeatherDiscreteCombineMode = no

# Default setting for Missing Data Mode. Possible values are:
# Stop: Stop execution of a smart tool if there is missing data.
# Skip: Skip grids for which there is  missing data.
#   A User Alert message will report which grids were skipped.
# Create: Create grids to supply the missing data.
#   A User Alert message will report which gridswere created.
MissingDataMode = "Stop"

# Default setting for showing the dialog box when the user attempts to
# edit grids when a selection time range is active.  Editing grids when
# a selection time range is active may cause multiple grids to be
# edited.
ShowTimeRangeWarning = yes

# Default setting for showing the dialog box when the user attempts to
# edit grids without an edit area being set.  The behavior is to edit
# the entire domain.
ShowEmptyEditAreaWarning = yes

# Specifies the default contour to grid algorithm.  Can be set to
# "Contour Analyzer", "Internal SIRS Server"
ContourServer = "Contour Analyzer"

# The Countour Analyzer algorithm can run over a subsampled grid
# to improve performance.  This is usually ok since the contour tool
# is mostly used where there is not much detail due to topography.
# The value of ContourSubSample is used to divide the x and y dimensions
# of the original grid to get the dimensions of the subsampled grid.
# So, setting ContourSubSample to 4 would cause the Contour Analyzer to
# reduce a 400x400 grid to a 100x100 grid for contouring purposes.
# This can greatly speed up the algorithm.  Setting ContourSubSample to
# 1 will cause no reduction.
# The default value is 4.  If ContourSubSample is set to a value less than
# or equal to 0 then it will go back to 4.  If it is set to a value large
# enough to make the subsampled grid have an x or y dimension less than 5
# then it will be reduced so that the minimum dimension for x or y will be
# 5.
ContourSubSample = 4

# Specifies whether the selection time range will track the spatial
# editor time when time stepping using the toolbar buttons or keyboard.
SelectGridsWhenStepping = no

# Default Time Scale Periods that are shown on the time scale.  These
# are names of the selection time ranges (SELECTTR).
TimeScalePeriods = ['Today', 'Tonight', 'Tomorrow', 'Tomorrow Night',
  'Day 3', 'Day 4', 'Day 5', 'Day 6', 'Day 7']

# Contour Tool drawing color. Defaults to "White"
#ContourToolDrawing_color = "White"

# Move/Copy, Pencil, SelectPoints drawing color. Defaults to "White"
#Drawing_color = "White"


#------------------------------------------------------------------------
# PNG Graphic Product Generation (ifpIMAGE program)
#------------------------------------------------------------------------
# Defines what kind of files ifpIMAGE will produce.  The default is
# png.  But you may also choose from the following list.  Note that
# these are case sensitive and only png, svg, and gif have been really
# tested. [ 'png', 'pnm', 'gif', 'svg', 'ai', 'ps',
#           'cgm', 'fig', 'pcl', 'hpgl', 'regis',
#           'tek', 'meta' ]
#
#Png_fileType = 'ps'

# Legends display mode - 0 for UTC, 1 for local time
# Do not include a decimal point after the number.
#Png_localTime = 1 # legend displays time in local or UTC (default to UTC)

# You can set the height and width (in pixels) for the Png images.
# It is only necessary to set one of these, as the other will
# be calculated using the aspect ratio of your office domain.
# Do not include decimal points after the numbers.
# Both default to 400
#Png_height    = 400
#Png_width     = 400

# Name of the weather element which will be displayed as
# an image in the png. If nothing is specified here, then all weather
# elements will be displayed as a graphic.  Topo may also be added
# using the string "Topo"
#Png_image = 'T'

# Indicates that a snapshot time should be displayed instead of the valid time
# of the grid.
Png_snapshotTime = 0    # ifpIMAGE only
# Default format of the snapshot time if the Png_snapshotTime = 1
#Png_legendFormat_Zulu_snapshot = "%b%d%H%MZ"
# Default format of the snapshot itme if the Png_snapshotTime = 1 and
# Png_localTime = 1
#Png_legendFormat_LT_snapshot = "%d %b %I:%M %p %Z"

# Indicate if the Png image displayed should be smoothed (1 = smoothing
# enabled, 0 = smoothing disabled).  Note that smoothing will only apply
# to scalar and vector images.
Png_smoothImage = 0     # ifpIMAGE only

# Alternate way of specifying the weather elements to be displayed.
# If this entry is specified, then the DefaultGroup is ignored (for
# ifpIMAGE). Format is a list of weather elements in a psudo weather
# element bundle formats, which consist
# of "parmName_level:optType_modelName seq", where the
# seq is normally -1 for singleton databases, 0 for model databases for
# the most recent version, 1 for the prev. version of a model database.
# If you wish, you may add Topo to this list.  For it just use
# the string "Topo" (none of the other nonsense is needed).
#Png_parms = ['FzLevel_SFC:_Fcst -1', 'Sky_SFC:_Fcst -1', 'QPF_SFC:_Fcst -1']

# Ability to turn on/off legends for the graphic generation.  Applies
# only to graphic product generation and not GFE.  Defaults to on
# if not specified.  Do not include a decimal point after the number.
#Png_legend = 1   #1 for visible, 0 for invisible

# Legend weather element name mode - SHORT for weather element name,
# LONG for weather element descriptive name, ALT for alternate,
# OFF for no name
#Png_descriptiveWeName = 'SHORT'

# Alternate weather element name. Png_descriptiveWeName must be set to ALT.
# These entries define the weather element name to be displayed based
# on the weather element name (e.g., T).  The string
# format is Png_wxelem_AltName. For example, Png_MaxT_AltName = "Highs" will
# display "Highs" for the wx element name rather than MaxT or
# Maximum Temperature.  If not defined and ALT is set, then the weather
# element name will be the 'SHORT' name.
#Png_MaxT_AltName = "Highs"

# Legend format for Pngs. See strftime(3) for time string formats
# or ifpIMAGE documentation. If the duration, start time, or ending
# time is not desired, then the entry should be set to "".  There are
# separate entries for Zulu and LocalTime.  The duration formats
# can use the %H (hours) %M (minutes) formats.
Png_legendFormat_Zulu_dur = ""                      # ifpIMAGE only
Png_legendFormat_Zulu_start = "%b %d %H%MZ to "     # ifpIMAGE only
Png_legendFormat_Zulu_end = "%b %d %H%MZ"           # ifpIMAGE only
Png_legendFormat_LT_dur = ""                        # ifpIMAGE only
Png_legendFormat_LT_start = "%b %d %I:%M %p %Z to " # ifpIMAGE only
Png_legendFormat_LT_end = "%b %d %I:%M %p %Z"       # ifpIMAGE only

# Png filename prefix
# Specifies the prefix to be applied to all generated png imagery
#Png_filenamePrefix = 'desiredPrefix'

# Png filename format
# Specifies the format to be used for the date/time string in the
# generated png imagery. See strftime(3) for time string formats
# or the ifpIMAGE documentation. Default is yyyymmdd_hhmm
#Png_baseTimeFormat = '%Y%m%d_%H%M'

#By default, png images are generated for each and every possible change
#in the generated grids.  For example, if you are generating a grid for T
#and WaveHeight, and the T is a one hour grid and the WaveHeight a 6 hour
#grid, that starts at the same time (e.g., 12z), two different images will
#be generated. The first will have T and WaveHeight together and will be
#time stamped to 12z; the second will just have WaveHeight and will be time
#stamped to 13z.  This is identical behavior to running the GFE with
#multiple visible weather elements.

#You can override this behavior for the creation of the Png imagery by
#specifying an interval for which to generate imagery.  The interval is
#specified in hours.  Setting the value to 6 will generate grids at 00z,
#06z,12z and 18z, assuming there is data available to generate the imagery.
#The configuration line to set the generation to every 6 hours is:
#Png_interval = 6

#Png imagery intervals can be offset by the amount set in the
#Png_intervalOffset option.  If the Png_intervalOffset is 1 and Png_interval =6,
#(specified in hours) grids will be generated at  01z, 07z, 13z, etc.,
#assuming there is data available to generate the imagery.  Png_intervalOffset
#is 0.
#Png_intervalOffset = 0

# If using fit to data for ifpIMAGE, and the option "All Grids over Area",
# or "Single Grid over Area" is enabled, then the ifpIMAGE program needs to
# know the name of the edit area.
#Png_fitToDataArea = "BOU"

# Add a "logo bar" to the bottom of each image.  If this flag is set to 1,
# then a bar containing the noaa and nws logos will be inserted at the bottom
# of the image.
#Png_logo = 0

# If Png_logo is enabled, then this can be set to a string you would
# like to have in the "logo bar".  The string will be centered in the bar.
#Png_logoString = ""

# If an alternate legend language is desired, then enter that here.
# Acceptable values those defined in the locale command (part of Unix).
# Checked values are "spanish" and "french".
#Png_legendLanguage = "spanish"

# If set to 1, then the colorbar will not be rendered for images.
#Png_omitColorBar = 0

# Disables Automatic Zooming feature when ifpIMAGE clipping is enabled.
# Default is that ifpIMAGE will automatically zoom.  Set to yes or 1 to
# disable automatic zooming.
#Png_wholeDomain = 0

# Enables the creation of the PNG *.info files.  Set to yes or 1 to enable
# the creation.  Set to no or 0 to disable the creation.
#Png_infoFiles = 1

# Enables the special masking for ifpIMAGE to use the ISC grid data history
# information.  This is used when creating imagery with ISC data.  Areas
# not containing current ISC data will be blanked out. 0 for off, 1 for on.
# This entry overrides the other masking.
#Png_historyMask = 0

#------------------------------------------------------------------------
# INTERSITE COORDINATION
#------------------------------------------------------------------------
# Moved to serverConfig/localConfig for OB8.3

#------------------------------------------------------------------------
# ZONE COMBINER CONFIGURATION
#------------------------------------------------------------------------
# Specifies the height and width of the zone combiner.  It can be resized
# larger, but not smaller in the GFE.  Defaults are 400 pixels
#ZoneCombiner_height = 400
#ZoneCombiner_width = 400

# Specifies the zone combiner colors for the background,
# and the no zone color, which is used when a zone is not included
# in any combination.
#ZoneCombiner_backgroundColor = 'gray40'
#ZoneCombiner_noZoneColor = 'black'

# If set true, then these options will be set when the zone combiner
# starts for each product.
#ZoneCombiner_LabelZones = 0
#ZoneCombiner_LabelGroups = 1


#------------------------------------------------------------------------
# PRODUCT GENERATION SCRIPTS
#------------------------------------------------------------------------
# Product Generation Scripts appear under the product generation menu
# on the GFE.
Scripts = [
    "Send Grids to NDFD..:" +
    "sendGridsToNDFD.sh {site} &",

    "Send Point and Click Grids to Consolidated Web Farm..:" +
    "/awips2/GFESuite/bin/rsyncGridsToCWF_client.sh {site} &",
    
    "Png Images...:" +
    "ifpIMAGE " +\
    "-h {host} -c {entry:ConfigFile:imageTest1} -o {prddir}/IMAGE",
    
    "Ascii Grids...: " +
    "ifpAG -h {host} -r {port} -o {prddir}/AG/{ztime}.ag " +\
      "-d {productDB} ",

    "Official Grids to LDAD: " +
    "ifpAG -h {host} -r {port} -o - -d {productDB} | gzip -9 > " +
    " /data/fxa/LDAD/ifp/Official/.incoming; " +
    "mv /data/fxa/LDAD/ifp/Official/.incoming /data/fxa/LDAD/ifp/Official/{ztime} &"
    ]

##   Note: Please define TextProducts through
##   the DefineTextProducts dialog (Product Generation Menu)
##   within the GFE.

# Ordering Product Generation
# To provide an order for the products generated, list an order
# preference in the list variable 'ProductList'.  Any products not listed in
# 'ProductList' will be listed at the bottom of the product generation menu
# in alphabetical order.
#ProductList = ["Png Images", "Ascii Grids", "YourProduct"]

#------------------------------------------------------------------------
# Product Generation Script Notes
#
# Each script entry is a text string of the form:
#    "<Entry Name>: " +
#    "<command line script> "
#
#    where:
#     <Entry Name> will appear in the Product Generation menu
#     <command line script> is the command line that will be submitted when
#       the script is chosen.
#
# The following variables can be used in scripts and the GFE will fill
# in the appropriate information before executing the script:
#
#       {host}                   -- server hostname
#       {port}                   -- server port
#       {site}                   -- site identifier
#       {productDB}              -- product database -- this is the
#                                   Official Database if it exists.
#                                   Otherwise, it's the Mutable (Fcst) database.
#       {SEstart}                -- Start of Spatial Editor time:
#                                    format of all times: YYYYMMDD_HHMM
#       {SEend}                  -- Spatial Editor time plus one second
#       {SelectedStart}          -- Start of Selected Time range
#       {SelectedEnd}            -- End of Selected Time range
#       {time}                   -- Current local time in format: YYYYMMDD_HHMM
#       {ztime}                  -- Current zulu time in format: YYYYMMDD_HHMM
#       {module:<module name>}   -- The correct path of the module will
#                                   be substituted in the command line.
#                                   The module must have a .py extension.
#       {home}                   -- Substitutes the home GFESuite directory
#                                 at runtime (may differ from local to server)
#       {prddir}                 -- Substitutes the product directory
#                                 at runtime (may differ from local to server)
#
# Note that the directory {} values should be used, rather than hard-coding
# them, if you want to be able to run a process locally as well as remotely.
#
#    If the following variables are used in a script,
#    a dialog will appear for the user to make selections from a simple GUI
#    before the script is executed:
#       {parmsMutable}  (Those listed in Forecast database)
#       {refsets}
#       {maps}
#       {databases}
#       {output file}
#       {output directory}
#       {startTime}
#       {endTime}
##
#   Named Variable
#    To have the user prompted for a named variable, use the following
#    in your script:
#       {entry: <name of variable>: <default value>}
#    For example, to have the user prompted for "width" use:
#       {entry: width: 350}
#    in your script.
#
#   Radio Button list of values
#    To have the user prompted for a list of radiobutton variables, use
#       {entryButtons: <name of variable>: <list of values separated by commas>}
#    E.g.
#       {entryButtons: ReportType: GeneralImages,CustomizedImages}
#
#   Check Button list of values
#    To have the user prompted for a list of radiobutton variables, use
#       {entryChecks: <name of variable>: <list of values separated by commas>}
#    E.g.
#       {entryChecks: EditAreas: Area1,Area2,Area3}

#   Edit Areas and  Groups
#    If the name of the entryButtons or entryChecks is "EditAreas",
#    the system will accept edit area OR edit area group names.
#    The system will check for groups and will automatically expand
#    them to the appropriate areas
#       {entryChecks: EditAreas: Group1,Group2,Area3,Area4}
#       {entryButtons: EditAreas: Group1,Group2}

# Scripts with Multiple Command Lines
#   To string multiple command lines together, use the following format for
#   your command line script:
#      "csh -c (<command line 1>; <command line 2>; <command line 3>)"
#------------------------------------------------------------------------
