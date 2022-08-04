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

##
# This is a base file that is not intended to be overridden.
##

# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# CWF_Pacific_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  CWF_Pacific formatter for a site. 
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

#----- WFO <site> CWF_Pacific Definition -----
# Definition Statements must start in column 1.
# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "CWFPacific_<MultiPil>"

Definition["showZoneCombiner"] = 1 # 1 to cause zone combiner to display
Definition["defaultEditAreas"] = "Combinations_CWF_<site>_<MultiPil>"
Definition["mapNameForCombinations"] = "Marine_Zones_<site>" # Map background for creating Combinations

#Special multiple product domains for certain sites:
if "<site>" == "AJK":
    if "_<MultiPil>" == "_AJK":
        Definition["subDomainUGCs"] = ["PKZ011","PKZ012","PKZ013","PKZ021",
                                       "PKZ022","PKZ031","PKZ032","PKZ033",
                                       "PKZ034","PKZ035","PKZ036"]
    elif "_<MultiPil>" == "_AEG":
        Definition["subDomainUGCs"] = ["PKZ041","PKZ042","PKZ043","PKZ051",
                                       "PKZ052"]
elif "<site>" == "GUM":
    if "_<MultiPil>" == "_MY":
        Definition["subDomainUGCs"] = ["PMZ151","PMZ152","PMZ153","PMZ154"]
    elif "_<MultiPil>" == "_PQ":
        Definition["subDomainUGCs"] = ["PMZ161","PMZ171","PMZ172","PMZ173",
                                       "PMZ174","PMZ181","PMZ191"]

elif "<site>" == "AFG":
    if "_<MultiPil>" == "_NSB":
        Definition["subDomainUGCs"] = ["PKZ225","PKZ230","PKZ235","PKZ240",
                                       "PKZ245"]
    elif "_<MultiPil>" == "_WCZ":
        Definition["subDomainUGCs"] = ["PKZ200","PKZ210","PKZ215","PKZ220"]



# Header configuration items
#Definition["productName"] = "Coastal Waters Forecast"  # name of product
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["areaName"] = "<state>"  # Name of state, such as "Georgia"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city st
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/CWF_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)
#Definition["editAreaSuffix"] = "_pt"

#Definition["periodCombining"] = 1     # If 1, do period combining
#Definition["includeEveningPeriod"] = 0 # If 1, include Evening Period
#Definition["useAbbreviations"] = 0     # If 1, use marine abbreviations 

# Weather-related flags
#Definition["hoursSChcEnds"] = 24
# River Bar Zones
#Definition["riverBarZones"] = []

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers
#Definition["language"] = "english"
#Definition["lineLength"] = 66
#Definition["useHolidays"] = 1

# Trouble-shooting items
#Definition["passLimit"] = 20       # Limit on passes allowed through Narrative Tree
#Definition["trace"] = 1            # Set to 1 to turn on trace through
                                   # Narrative Tree for trouble-shooting
