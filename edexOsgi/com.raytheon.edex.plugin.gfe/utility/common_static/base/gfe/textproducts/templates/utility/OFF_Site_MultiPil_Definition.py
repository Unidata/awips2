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
# OFF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  OFF formatter for a site. 
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

#----- WFO <site> OFF Definition -----
# Definition Statements must start in column 1.
# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "OFF"

Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
Definition["defaultEditAreas"] = "Combinations_OFF_<site>_<MultiPil>"
Definition["mapNameForCombinations"] = "Marine_Zones_<site>" # Map background for creating Combinations

# Header configuration items
#Definition["productName"] = "Coastal Waters Forecast"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>"  # Name of state, such as "Georgia"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city st
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/OFF.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["editAreaSuffix"] = "_pt"

#Definition["lineLength"] = 66   #Product line length
#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

#Definition["periodCombining"] = 1     # If 1, do period combining
#Definition["includeEveningPeriod"] = 0  # If 1, include Evening Period
#Definition["useAbbreviations"] = 1      # If 1, use marine abbreviations

# Weather-related flags
#Definition["hoursSChcEnds"] = 24

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers
#Definition["language"] = "english"
#Definition["useHolidays"] = 1

# Trouble-shooting items
#Definition["passLimit"] = 20             # Limit on passes allowed through Narrative Tree
#Definition["trace"] = 1                  # Set to 1 to turn on trace through
                                         # Narrative Tree for trouble-shootin
