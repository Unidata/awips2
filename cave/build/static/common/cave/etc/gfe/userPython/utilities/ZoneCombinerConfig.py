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

# Table for conversions from the old map file names to table names in the maps
# database. The map file name from the config file has the CWA removed and is
# converted to uppercase.
# For example, if the old entry was mapNameForCombinations = 'FireWxZones_OAX',
# then the FIREWXZONES entry in tables would be used to obtain the name of the
# database table.   
tables = {
    'FIPS': 'county',
    'FIREWXZONES': 'firewxzones',
    'MARINE_ZONES': 'marinezones',
    'ZONES': 'zone',
    'OFFSHORE': 'offshore'
    }

# SQL definitions of the additional data columns (other than the zone geometry) needed for
# ZoneCombinerComp.java to manipulate the onscreen map. Dictionary keys are the
# database table names, values are a list of strings with the SQL for the fields.
# Generally speaking, these entries should be left alone, but changes to the
# database might require modifications.
# In the current implementation, a single database query using UNION clauses is performed to
# obtain the map. Therefore, the column definitions for all the tables must be union-compatible.
tableColumns = {
    'county': ["'county'", "state || 'C' || substr(fips,3) AS zonelabel", "CWA"],
    'firewxzones': ["'firewxzones'", "state || 'Z' || zone AS zonelabel", "CWA"],
    'marinezones': ["'marinezones'", "id as zonelabel", "wfo as CWA"],
    'zone': ["'zone'", "state || 'Z' || zone AS zonelabel", "CWA"],
    'offshore': ["'offshore'", "id as zonelabel", "wfo as CWA"]
    }

# Latitude and longitude at which the map center should be originally positioned.
# These must be numeric and within the scrollable map for the site. When values are given, if either
# value cannot be interpreted as a number, or the point is outside the scrollable map, an error message
# will be generated and the values will be ignored.
# The site default is always the centerpoint of the scrollable area; these settings only modify 
# the initial position of the scrollbars.  
#centerLat = 49.5
#centerLon = -95.5

# Zoom level at which the map should initially be displayed. If a value is given but cannot be understood,
# an error message will be generated and "No zoom" will be selected. Otherwise, the value will be rounded 
# up to the nearest value on the zoom menu, or to "x8" if the value is above 8. 
# zoom = "x4"