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
# Example localMaps.py configuration file

# You must include the following line
from Maps import * 

# The following changes the configuration of the CWAzones maps.  It overrides
# every possible item that can be overridden.

# Rename by name of the CWAzones map
CWAzones.name = "MyCWAZones" 

# Change the filter to only include "OH" (Ohio) zones
CWAzones.filter(lambda x : x['STATE'] == "OH")

# We don't want any edit areas to be automatically generated, so we
# turn off the edit area name.
CWAzones.editAreaName = []

# Expand the map by 2 degrees in the east and south, and 1.5 degrees
# in the west
CWAzones.expandDomain = (0, 2, 2, 1.5)

# The following changes the configuration of the Cities map.  Instead
# of filtering off of the population field, it will produce 
# a map of the given cities.
cityset = ['Akron', 'Denver', 'Boulder', 'Loveland', 'Longmont']
cities.filter(lambda x : x['NAME'] in cityset)

