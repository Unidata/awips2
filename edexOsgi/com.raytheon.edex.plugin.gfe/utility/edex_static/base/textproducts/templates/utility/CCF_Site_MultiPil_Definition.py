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
# CCF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  CCF formatter for a site. 
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

#----- WFO <site> CCF Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS
#Definition['displayName'] = None
Definition['displayName'] = "CCF_<MultiPil>"

Definition["defaultEditAreas"] = [
    ("area1", "AREA1"),
    ("area2", "AREA2"),
    ]
# product identifiers
Definition["fullStationID"] =  "<fullStationID>"      # full station idr (e.g., KSLC)
Definition["wmoID"] = "<wmoID>"                       # wmoID
Definition["pil"] = "<pil>"                           # product pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/CCF_<MultiPil>.txt"

# Optional Configuration Items
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1

#Definition["alwaysIncludeSnow"] = 0    # include snow always (1=yes,0=no)
#Definition["popStartZ_AM"] = 12        # start time for PoP for AM in Zulu 
#Definition["wxStartLT_AM"] = 6         # start time for Wx in AM in LT
#Definition["wxStopLT_AM"] = 18         # stop time for Wx in AM in LT
#Definition["AMnumPeriods"] = 13        # set to 14 if using the C-20


