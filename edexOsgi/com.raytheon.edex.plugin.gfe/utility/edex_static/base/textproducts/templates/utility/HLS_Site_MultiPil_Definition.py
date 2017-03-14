# ---------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without
# technical  support, and with no warranty, express or implied, as to
# its usefulness for any purpose.
#
# HLS_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the 
#  HLS formatter for a site. 
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

#----- WFO <site> HLS Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS 
#Definition['displayName'] = None
Definition['displayName'] = "HLS_<MultiPil> (Hurricane Local Statement)"

# Header configuration items
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"        # WMO ID
Definition["pil"] = "<pil>"          # product pil
Definition["textdbPil"] = "<textdbPil>"       # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"   # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] =  "{prddir}/TEXT/HLS_<MultiPil>.txt"

# OPTIONAL CONFIGURATION ITEMS
#Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
#Definition["debug"] = 1

