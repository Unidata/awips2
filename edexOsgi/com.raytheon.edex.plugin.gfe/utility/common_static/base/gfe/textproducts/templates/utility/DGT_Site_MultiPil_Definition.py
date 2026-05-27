# DGT_<site>_<MultiPil>_Definition.TextUtility
#
#  This file sets up all the Product Definition overrides for the
#  DGT formatter for a site.
#
# ---------------------------------------------------------------------
##
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# 03/15/2020       DCS21339       NFTF        Add NFTF DGT to baseline
#
##
# **********************************************************************
# MAKE NO CHANGES HERE
# The minimum content of this file is the following Definition statement

Definition = {}

# End MAKE NO CHANGES HERE
# **********************************************************************

# ----- WFO <site> DGT Definition -----
# Definition Statements must start in column 1.

# REQUIRED CONFIGURATION ITEMS
# Definition["displayName"] = None
Definition["displayName"] = "DGT_<MultiPil>"

# Header configuration items
Definition["fullStationID"] = "<fullStationID>"  # full station identifier (4letter)
Definition["wmoID"] = "<wmoID>"  # WMO ID
Definition["pil"] = "<pil>"  # product pil
Definition["stateName"] = "<state>"  # Name of state, such as "Georgia"
Definition["wfoCityState"] = "<wfoCityState>"  # Location of WFO - city state
Definition["textdbPil"] = "<textdbPil>"  # Product ID for storing to AWIPS text database.
Definition["awipsWANPil"] = "<awipsWANPil>"  # Product ID for transmitting to AWIPS WAN.
Definition["outputFile"] = "{prddir}/TEXT/DGT.txt"

# Uncomment areaType and mapNameForCombinations if issuing for zones. Default is counties.
# Definition["areaType"] = "Zones"
# Definition["mapNameForCombinations"] =  "Zones_<site>"

# Define the AWIPS text database PIL used to retrieve the last issued DGT.
Definition["textdbPil"] = "<textdbPil>"

# This defines whatever local websites you would like to have in the "RELATED WEB SITES" section
Definition["localWebsites"] = [
    "Northeast Regional Climate Center:\nhttp://www.nrcc.cornell.edu",
    "New York State Climate Office:\nhttp://nysc.eas.cornell.edu",
]

# This defines any acknowledgements to local, regional or national agencies
Definition["acknowledgements"] = """
The Drought Monitor is a multi-agency effort involving the
National Weather Service and National Centers for Environmental
Information, the USDA, state and regional center climatologists
and the National Drought Mitigation Center. Information for this
statement has been gathered from NWS and FAA observation sites,
state cooperative extension services, the USDA, USACE and USGS.
""".strip()

# Your local WFO address that can be appended at the bottom of the product

Definition["addressWFO"] = """
National Weather Service
1111 Forecast Ave
City ST 12345
Phone...555-123-4567
nws.city@noaa.gov
""".strip()


# OPTIONAL CONFIGURATION ITEMS
# Definition["debug"] = 1
# Definition["purgeTime"] = 336   # Default expiration time

# See DGT.py TextProduct for additional Definition settings that can be set
