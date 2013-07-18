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
# FWF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  FWF formatter for a site. 
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

#----- WFO <site> FWF Definition -----
# Definition Statements must start in column 1.
# REQUIRED CONFIGURATION ITEMS 
Definition['displayName'] = "FWF_<MultiPil>"

Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
Definition["defaultEditAreas"] = "Combinations_FWF_<site>_<MultiPil>"
Definition["mapNameForCombinations"] = "FireWxZones_<site>" # Map background for creating Combinations

#Special multiple product domains for certain sites:
if "<site>" == "AFG":
    if "_<MultiPil>" == "_AFG":
        Definition["subDomainUGCs"] = ["AKZ218","AKZ219","AKZ220","AKZ221",
                                       "AKZ222","AKZ223","AKZ224","AKZ225",
                                       "AKZ226"]
    elif "_<MultiPil>" == "_NSB":
        Definition["subDomainUGCs"] = ["AKZ201","AKZ202","AKZ203","AKZ204",
                                       "AKZ205","AKZ206"]
    elif "_<MultiPil>" == "_WCZ":
        Definition["subDomainUGCs"] = ["AKZ207","AKZ208","AKZ209","AKZ210",
                                       "AKZ211","AKZ212","AKZ213","AKZ214",
                                       "AKZ215","AKZ216","AKZ217"]

#Definition["tempLocalEffects"] = 1   # Set to 1 to enable Temp and RH local effects AFTER
                                      # creating AboveElev and BelowElev edit areas
#Definition["windLocalEffects"] = 1   # Set to 1 to enable wind local effects AFTER
                                      # creating Ridges and Valleys edit areas

# Header configuration items
#Definition["productName"] = "FIRE WEATHER PLANNING FORECAST"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"      # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>"     # Name of state, such as "GEORGIA"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city st
Definition["textdbPil"] = "<textdbPil>"        # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"    # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/FWF_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
#Definition["editAreaSuffix"] = "_pt"

#Definition["periodCombining"] = 1     # If 1, do period combining
#Definition["useRH"] = 1                # Use RH grids instead of MaxRH, MinRH
#Definition["summaryExtended"] = 0
#Definition["summaryArea"] = "FireWxAOR_<site>_<MultiPil>"
#Definition["individualExtended"] = 1
#Definition["extendedLabel"] = 1
#Definition["useHolidays"] = 1       # Will use holidays in time period labels
#Definition["includeTrends"] = 0     # Set to 1 to include Temp and RH trends

#Definition["lineLength"] = 66   #Product line length

# Set the following variable to 1 if you want Lightning Activity
# reported with phrases like "1-8 STRIKES", "9-15 STRIKES", etc.
#Definition["lightningPhrases"] = 1

# The following variable sets a wind adjustment factor for surface
# (20 ft) winds.  Wind speeds will be multiplied by this factor.
# Winds reported by RAWS sites are frequently lower than ASOS winds
# due to the fact that they use a 10-min average.  A common adjustment
# factor is 80% (0.80).  If you want no adjustment to the winds
# then set this variable to 1.00
#Definition["windAdjustmentFactor"] = 1.00

#Definition["includeMultipleElementTable"] = 1       # Will include a MultipleElementTable
#Definition["includeMultipleElementTable_perPeriod"] = 1 # Will include a MultipleElementTable
                                                     # per area per period.
                                                     # ("singleValueFormat" must be 1)
# Uncomment just one elementList below      
#Definition["elementList"] = ["Temp", "Humidity", "PoP"] # Default
#Definition["elementList"] = ["Temp", "PoP"]         
#Definition["singleValueFormat"] = 1                     # Default is 0

# Weather-related flags
#Definition["hoursSChcEnds"] = 24

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers
#Definition["language"] = "english"                 

# Trouble-shooting items
#Definition["passLimit"] = 20              # Limit on passes allowed through
                                          # Narrative Tree
#Definition["trace"] = 1                   # Set to 1 to turn on trace 
