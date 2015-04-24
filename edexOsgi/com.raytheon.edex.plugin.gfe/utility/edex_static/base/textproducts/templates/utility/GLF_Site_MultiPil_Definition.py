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
# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# GLF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  GLF formatter for a site. 
#
# ---------------------------------------------------------------------

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************
#####################################################
# Override VariableList if desired
#
#VariableList = []

#----- WFO <site> GLF Definition -----
# Definition Statements must start in column 1.
#Definition['displayName'] = None
Definition['displayName'] = "GLF_<MultiPil>"

# DEFAULT EDIT AREAS -- change for forecasted lake
# Superior, Erie, or Ontario
Definition["defaultEditAreas"] = [("west_half", "WEST HALF\n\n"),
                                 ("east_half", "EAST HALF\n\n")]
#    # Michigan or Huron
#    Definition["defaultEditAreas"] = [("north_half", "NORTH HALF\n\n"),
#                                     ("south_half", "SOUTH HALF\n\n")]
#    # St Clair
#Definition["defaultEditAreas"] = [("St_Clair", "\n\n"),]
#Definition["lake_name"] = "Superior" # use -- Superior, Huron, Erie, Ontario, Michigan, St_Clair

# HEADER AND ZONE CONFIG ITEMS
#Definition["productName"] = "Open Lake Forecast"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil in AWIPS XX is the 
                                     #lake abbreviation "LE, LS, LO, SC, LM"
#Definition["areaName"] = "Lake Superior" # Name of lake, such 
                                         # as "Lake Superior"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/GLF_<MultiPil>.txt"

#Definition["lakezone"] = "LSZ260" # lake zone code
#Definition["maforzone"] = "LSZ261" # mafor zone code
#Definition["headerphrase"] = "Lake Superior forecast beyond five nautical miles from shore" # header phrase

#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

##### OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["useAbbreviations"] = 0   # If 1, use marine abbreviations, if 0, no abbreviations

#Definition["processmafor"] = 0 # process mafor? 1=yes, 0=no (default is yes)
#Definition["useHolidays"] = 1

#Definition["lineLength"] = 66   #Product line length

# Weather-related flags
#Definition["hoursSChcEnds"] = 24

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers -- not needed for GLF
#Definition["language"] = "english"                 

# Trouble-shooting items
#Definition["passLimit"] = 20              # Limit on passes allowed through
                                          # Narrative Tree
#Definition["trace"] = 1                   # Set to 1 to turn on trace through
