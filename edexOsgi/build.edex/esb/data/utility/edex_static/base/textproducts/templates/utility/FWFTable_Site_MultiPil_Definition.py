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
# FWFTable_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  FWFTable formatter for a site. 
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

#----- WFO <site> FWFTable Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "FWFTabular_<MultiPil>"

Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
Definition["defaultEditAreas"] = "Combinations_FWFTable_<site>_<MultiPil>"
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
                                       "AKZ215","AKZ216","AKZ217","AKZ227"]

# Header configuration items
#Definition["productName"] = "FIRE WEATHER PLANNING FORECAST"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>" # Name of state, such as "GEORGIA"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/FWF_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["editAreaSuffix"] = "_pt"

#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

#Definition["lineLength"] = 66   #Product line length

#Definition["periodCombining"] = 1     # If 1, do period combining
#Definition["columnJustification"] = "r"  # Left (l) or right (r) justification for columns
#Definition["useRH"] = 1                  # Use RH grids instead of MaxRH, MinRH
#Definition["summaryExtended"] = 0
#Definition["summaryArea"] = "FireWxAOR_<site>_<MultiPil>"
#Definition["individualExtended"] = 1
#Definition["extendedLabel"] = 1

# Set the following variable to 1 if you want Mixing Height,
# Transport Wind and Vent Index reported in night periods.
#Definition["mixingParmsDayAndNight"] = 1    
# Use "Max" or "Avg" for mixHgt
#Definition["mixHgtMethod"] = "Max"

# Set the following variable to 1 if you want Lightning Activity
# reported with phrases like "1-8 STRIKES", "9-15 STRIKES", etc.
#Definition["lightningPhrases"] = 1

# Winds are reported from the Wind20ft grid if available.
# Otherwise, the Wind grid is used with the magnitude multiplied
# by this wind adjustment factor.
# Winds reported by RAWS sites are frequently lower than ASOS winds
# due to the fact that use a 10-min average.  A common adjustment
# factor is 80% (0.80).  If you want no adjust ment to the winds
# then set this variable to 1.00
#Definition["windAdjustmentFactor"] = 1.00

# Threshold for a light wind string in the table
#Definition["tableLightWindThreshold"] = 5
# Light wind string in the table
#Definition["tableLightWindPhrase"] = "LGT/VAR"
# Use a range for the winds in the table 1=yes
#Definition["tableWindRanges"] = 1
# Gusts will not be reported below this value.
#Definition["minGustMph"] = 17
# Gust - wind must exceed this threshold to be reported.
#Definition["windGustDiffMph"] = 7

# If max humidity is above this percentage, humidity recovery
# will be EXCELLENT.
#Definition["humidityRecovery_percentage"] = 50

# Set to MinRH value below which you would like a MinRH phrase in the Extended. 
# Default (-1) is no MinRH phrase.
#Definition["rhPhraseThreshold"] = 20

# Set the following variable to 1 to include long-range outlook
# placeholders at the end of the product.  These are appended by
# _postProcessProduct
#Definition["includeOutlooks"] = 1
#Definition["useHolidays"] = 1       # Will use holidays in time period labels
#Definition["areaDictionary"] = "AreaDictionary"     # For product headers

# Weather-related flags
#Definition["hoursSChcEnds"] = 36

