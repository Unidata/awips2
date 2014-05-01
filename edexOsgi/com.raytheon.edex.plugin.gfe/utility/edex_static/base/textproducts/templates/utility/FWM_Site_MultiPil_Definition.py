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
# FWM_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  FWM formatter for a site. 
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

#----- WFO <site> FWM Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "FWM_<MultiPil>"

Definition["defaultEditAreas"] = [
       ("area1", "086401"),
       ("area2", "086402"),
       ("area3", "668"),
       ]

# Header configuration items
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/FWM_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1

#Definition["reportAsTrendsForZONE"] = 0 # data values as trends for ZONE
#Definition["reportAsTrendsForFCST"] = 0 # data values as trends for FCST
#Definition["reportTandRHforZONE"] = 0   # T and RH values for ZONE
#Definition["reportTandRHforFCST"] = 0   # T and RH values for FCST
#Definition["reportWindDirectionForZONE"] = 1  # If 1, insert placeholder comma for wind dir
#Definition["reportWindDirectionForFCST"] = 0  # If 1, include wind direction
#Definition["textDirection"] = 0           # Winddir as 16-pt compass
#Definition["fuelMoisturePlaceHolder"] ="" # String to hold the place for fuel moisture

# Adjustment for Wind if no Wind20ft grid available
#Definition["windAdjustmentFactor"] = 1.00
#Definition["wxDurPopThreshold"] = 70      # Pop threshold for reporting wx duration
