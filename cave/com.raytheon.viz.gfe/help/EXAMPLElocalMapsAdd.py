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

# Example of adding a new map background.  This one is called "WYCounties"
# on the GFE maps menu, uses the uscounty shapefile, is filtered to include
# just the Wyoming counties.  Edit areas are automatically generated"
# and named WY_countyName. The clip region is expanded from the default
# by 2 degrees in the north and east directions
WYcounties = ShapeFile(MAPDIR)
WYcounties.filename(CountyMapName) 
WYcounties.filter(lambda x : x['STATE'] == "WY") 
WYcounties.name = 'WYCounties' 
WYcounties.editAreaName = ['STATE','COUNTYNAME'] 
WYcounties.groupName = 'WYCounties' 
WYcounties.expandDomain = (2, 2, 0, 0)
maps.append(WYcounties) 


