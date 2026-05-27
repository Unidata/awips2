# ESF_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the
#  ESF formatter for a site.
#
# ---------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# 03/15/2020       DCS21339       NFTF        Add NFTF ESF to baseline
#
##
# **********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
# **********************************************************************

# ----- WFO <site> ESF Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS
# Definition["displayName"] = None
Definition["displayName"] = "ESF_<MultiPil>"

# Header configuration items
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"  # WMO ID
Definition["pil"] = "<pil>"  # product pil
Definition["stateName"] = "<state>"  # Name of state, such as "Georgia"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
Definition["textdbPil"] = "<textdbPil>"  # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"  # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] = "{prddir}/TEXT/ESF.txt"

# OPTIONAL CONFIGURATION ITEMS
# Definition["database"] = "Official"    # Source database. "Official", "Fcst", or "ISC"
# Definition["debug"] = 1

# See ESF.py TextProduct for additional Definition settings that can be set
