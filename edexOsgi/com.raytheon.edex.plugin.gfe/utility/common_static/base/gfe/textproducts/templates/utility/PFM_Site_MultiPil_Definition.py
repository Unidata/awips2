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
# PFM_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  PFM formatter for a site. 
#
# ---------------------------------------------------------------------

#**********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
#**********************************************************************

#----- WFO <site> PFM Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "PFM_<MultiPil>"

Definition["productType"] = "PFM"      #Must be PFM

# format: editarea name, (area name, description, latlon, elevation)
Definition["defaultEditAreas"] = [
                ("area1", "Area 1\narea1 description\n40.12N 111.22W\n52"),
                ("area2", "Area 2\narea2 description\n40.44N  90.00W\n256"),
                ]

# Identifiers for product
Definition["fullStationID"] = "<fullStationID>"    # full station id (e.g., KSLC)
Definition["wfoCityState"] = "<wfoCityState>"  # city,state of wfo
Definition["wmoID"] = "<wmoID>"       # product WMO ID code
Definition["pil"] = "<pil>"         # product pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/PFM_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1

# range vs. single value thresholds
#Definition["tempRangeThreshold"] = 5     # for temperatures
#Definition["qpfRangeThreshold"] = 0.05   # for QPF
#Definition["snowRangeThreshold"] = 3     # for SnowAmt

# Snow Amt Options
#Definition["includeSnowAmt"] = 0      # include=1,exclude=0

# Heat Index Options
#Definition["includeHeatIndex"] = 0    # include=1,exclude=0
#Definition["heatIndexDifference"] = 0 # indicates HI-T threshold
#Definition["heatIndexLimit"] = 80     # absolute heat index threshold

# Wind Chill Options
#Definition["includeWindChill"] = 0     # include=1, exclude = 0
#Definition["windChillDifference"] = 5  # T-WindChill threshold
#Definition["windChillLimit"] = 40     # absolute wind chill threshold

