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
# SAF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  SAF formatter for a site. 
#
# ---------------------------------------------------------------------

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

# If you comment out this VariableList section,
# You will get whatever is set up in your ZFP.
VariableList = [
         (("Issuance Type", "issuanceType") , "ROUTINE", "radio",
                    ["ROUTINE", "UPDATE", "CORRECTION"]),
         (("Number of Periods", "numPeriods"),  "All", "radio",
          ["All", "5", "4", "3", "2", "1"]),
         #  If Flooding is "Yes", the system will insert a statement in the
         #  product when FFA, FLW, or FLS is in effect.
         #("Flooding?" , "No", "radio", ["Yes","No"]),
         #(("Include Tropical?", "includeTropical") , "No", "radio", ["Yes","No"]),
     ]

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

#----- WFO <site> SAF Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 

# Change the "_" to a "-" in the name and get rid of the pesky submenus
# on the Formatter Launcher!
Definition['displayName'] = "SAF_<MultiPil>"

# You really can't use this formatter with interactive zone combinations
# due to the way the areaName must be prepended with the lac ID
# (see Definition["defaultEditAreas"])
Definition["mapNameForCombinations"] = None 

# Header configuration items
# If you are using the lacFileDict,
# you do not need the following entries since they apply to the
# Formatter Launcher interface and the SAF write
# each tower's forecast to the text data base and directly
# to a file in the NWR pending directory

#Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
#Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
#Definition["outputFile"] =  "{prddir}/TEXT/SAF_<MultiPil>.txt"

# Instead you will probably want these set as follows:
Definition["textdbPil"] = ""
Definition["awipsWANPil"] = "DONOTSEND!"
Definition["outputFile"] = ""

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Automatic Functions
# You really don't want to turn these on since the SAF is normally only
# sent to CRS
Definition["autoSend"] = 0   #set to 1 to automatically transmit product
Definition["autoSendAddress"] = "000"   #transmission address
Definition["autoStore"] = 0   #set to 1 to store product in textDB
Definition["autoWrite"] = 0   #set to 1 to write product to file

######################################################################
# REQUIRED SET-UP:

# The following entries are set up as examples and so the SAF will run
# "out-of-the-box".  However, ALL must be set up correctly for your site.
#
# Note: The code is designed to be backward compatible such that if
# you have an "lac" and "pil" entry from a previous version, they will be used
# instead of the "lacList" and "pilDict."
# Thus BE SURE AND REMOVE YOUR "lac" entry in your SAF Site Definition file
# if you choose to use a "lacList"!

# lac - Listening area code. Must be a list of LACs to create
# forecasts for multiple towers.

Definition["lacList"] = ["VAC910c", "NCC940c"]

Definition["defaultEditAreas"] = [
            ("area1","Southwest mountains including yourTown"),
            ("area2","Eastern Virginia"),
            ("area3","Northern North Carolina"),
          ]

# lacAreaDict - For each defaultEditArea, enter it's associated LAC
Definition["lacAreaDict"] = {
    "area1": "VAC910c",
    "area2": "VAC910c",
    "area3": "NCC940c",
    }

# pilDict
# This must be set up as a dictionary with keys of the LAC ID's
# as set in the Definition['laciList'] and values of
# the CRS ID to be put in the CRS header:
#
Definition["pilDict"] = {"VAC910c" : "WBCSAFNW1",
                         "NCC940c" : "WBCSAFNW4",
                        }

############################
# OPTIONAL SET-UP:

# Flag to repeat first period at the end. 1 or 0
#Definition["repeat1stPeriod"] = 1

# summaryExtended - flag 0 or 1 to generate a summary extended
# forecast. If 1, you must define summaryAreaDict.
# Also turn off extendedLabel and includeExtended
#Definition["summaryExtended"] = 1

# summaryAreaDict - dictionary with keys of the LAC ID
# and values of a tuple of (editAreaName, areaLabel) where
# areaLabel is a label string such as "Western Virginia"
# editAreaName must be the name of a GFE defined edit area
##Definition["summaryAreaDict"] = {
##        "VAC910c":("area1", "The Roanoke Area."),
##        "NCC940c":("area3",
##                   "The northern foothills and mountains of North Carolina."),
##        }

# summaryExtendedIntro is a string to introduce the extended
# such as "The extended forecast for"
#Definition["summaryExtendedIntro"] = "The extended forecast for"

# lacFileDict - Dictionary with keys of LAC and values
# of full pathname of where to store the data on disk.
# Use this to store to the NWR Pending directory
##Definition["lacFileDict"] = {
##        "VAC910c" : "/data/fxa/workFiles/nwr/pending/WBCSAFNW1",
##        "NCC940c" : "/data/fxa/workFiles/nwr/pending/WBCSAFNW4",
##        }

#----------------------------
# ZFP-related Overrides

#Definition["periodCombining"] = 1     # If 1, do period combining

#Definition["directiveType"] = "C11"       
#Definition["directiveType"] = "10-503"     # Can be "C11"
# Can use a method name you have set up in your SAF_XXX_Overrides file or 
# "_five12hr_24hrExtended_issuance_list" from SAF_Overrides
#Definition["directiveType"] = "_five12hr_24hrExtended_issuance_list"

#Definition["arealSkyAnalysis"] = 1         # Set to 1 for areal Sky analysis (binnedPercent)
#Definition["useStormTotalSnow"] = 1        # Set to 1 to use StormTotalSnow grid
# Weather-related flags
##Definition["hoursSChcEnds"] = 24

#Definition["areaDictionary"] = "AreaDictionary"     # For product headers
#Definition["language"] = "english"                 

# Apply to C11 only:
#Definition["includeExtended"] = 0        # To include extended forecast
#Definition["extendedLabel"] = 0          # To include extended label
#Definition["includeEveningPeriod"] = 0   # To turn off evening period

#Definition["includeMultipleElementTable"] = 1       # Will include a MultipleElementTable
#Definition["elementList"] = ["Temp", "PoP"]         # Default
#Definition["singleValueFormat"] = 1                 # Default is 0
#Definition["cityDictionary"] = "CityDictionary"     # For MultipleElementTable

# Trouble-shooting items
#Definition["passLimit"] = 20             # Limit on passes allowed through Narrative Tree
#Definition["trace"] = 1                  # Set to 1 to turn on trace through
                                          # Narrative Tree for trouble-shooting
#Definition["debug"] = 1 # flag for debug messages
