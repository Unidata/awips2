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
# ZFP_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  ZFP formatter for a site. 
#
# ---------------------------------------------------------------------

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

#----- WFO <site> ZFP Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "ZFP_<MultiPil>"

Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
Definition["defaultEditAreas"] = "Combinations_ZFP_<site>_<MultiPil>"
Definition["mapNameForCombinations"] = "Zones_<site>" # Map background for creating Combinations

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
                                                                            
# Header configuration items
#Definition["productName"] = "ZONE FORECAST PRODUCT"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>"  # Name of state, such as "GEORGIA"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/ZFP_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
#Definition["editAreaSuffix"] = "_pt"

#Definition["periodCombining"] = 1          # If 1, do period combining
#Definition["directiveType"] = "C11"       
#Definition["directiveType"] = "10-503"     # Can be "C11"
#Definition["arealSkyAnalysis"] = 1         # Set to 1 for areal Sky analysis (binnedPercent)
#Definition["useStormTotalSnow"] = 1        # Set to 1 to use StormTotalSnow grid

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers
#Definition["language"] = "english"                 

#Definition["lineLength"] = 66   #Product line length

# Apply to C11 only:
#Definition["includeExtended"] = 0        # To include extended forecast
#Definition["extendedLabel"] = 0          # To include extended label
#Definition["includeEveningPeriod"] = 0   # To turn off evening period

#Definition["includeMultipleElementTable"] = 1       # Will include a MultipleElementTable
#Definition["elementList"] = ["Temp", "PoP"]         # Default
#Definition["singleValueFormat"] = 1                 # Default is 0
#Definition["cityDictionary"] = "CityDictionary"     # For MultipleElementTable

# Weather-related flags
#Definition["hoursSChcEnds"] = 24

# Trouble-shooting items
#Definition["passLimit"] = 20             # Limit on passes allowed through
                                          # Narrative Tree
#Definition["trace"] = 1                   # Set to 1 to turn on trace through
                                          # Narrative Tree for trouble-shooting
