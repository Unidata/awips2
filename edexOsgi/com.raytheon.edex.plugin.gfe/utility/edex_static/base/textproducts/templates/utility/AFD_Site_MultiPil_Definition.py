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
# AFD_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  AFD formatter for a site. 
#
# ---------------------------------------------------------------------

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

####################################################################
#   VariableList overrides (if desired) below.
####################################################################
#VariableList = []

#----- AFD Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS
#Definition["displayName"] = "None"
Definition["displayName"] = "AFD_<MultiPil>"

#Definition["defaultEditAreas"] = "EditAreas_PublicZones_<site>_<MultiPil>"
#Definition["editAreaSuffix"] = "_pt"

# Edit Areas for creating the optional Preliminary Point Temp/PoPs
#Definition["pointEditAreas"] = [
#    ("area1", "AREA1"),
#    ("area2", "AREA2"),
#    ]

# product identifiers
#Definition["productName"] =  "Area Forecast Discussion"      # full station idr (e.g., KSLC)
Definition["fullStationID"] =  "<fullStationID>"      # full station idr (e.g., KSLC)
Definition["wmoID"] = "<wmoID>"                       # wmoID
Definition["pil"] = "<pil>"                           # product pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/AFD_<MultiPil>.txt"
#Definition["state_IDs"] = ["ST"]

#Definition["hazardSamplingThreshold"] = (10, None)  #(%cov, #points)

# Optional Configuration Items
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1
#Definition["useZoneNames"] = 1
#Definition["abbreviateUGCs"] = 0
#Definition["fcstrNumberFormat"] = "Brief"  # Brief, Verbose, or None
#Definition["shortTermForecasters"] = ["99","01","02","03"]
#Definition["longTermForecasters"] = ["99","01","02","03"]
#Definition["aviationForecasters"] = ["99","01","02","03"]
#Definition["popStartZ_AM"] = 12     # hour UTC

#Definition["tieUpdateToPreviousAFD"] = 1 # If 1, UPDATE section will appear IFF
                                          # Include Previous AFD is chosen.

                             
##   NOTE: The order of dividers in the list determines their order of appearance in the product.
##Definition["topicDividers"] = [
##       # topicName,   topicDivider,   alwaysInclude, includeInGUI

##       ("Update",     ".UPDATE...",          0,          1),
##       ("Synopsis",   ".SYNOPSIS...",        0,          1),

##       # EITHER Discussion OR ShortTerm/LongTerm should always be included.
##       ("Discussion", ".DISCUSSION...",      0,          0),
##       ("ShortTerm",  ".SHORT TERM...",      1,          0),
##       ("LongTerm",   ".LONG TERM...",       1,          0),

##       # Optional dividers
##       ("Aviation",   ".AVIATION...",        0,          1),
##       ("Marine",     ".MARINE...",          0,          1),
##       ("FireWeather",".FIRE WEATHER...",    0,          1),
##       ("Hydro",      ".HYDROLOGY...",       0,          1),
##       ("Climate",    ".CLIMATE...",         0,          1),

##       # Controlled by "includePreviousAFD"
##       ("PrevDisc",   ".PREV DISCUSSION...", 0,          0),

##       # Controlled by "pointEditAreas"
##       ("Prelim", ".PRELIMINARY POINT TEMPS/POPS...", 0, 0),
##       ]




