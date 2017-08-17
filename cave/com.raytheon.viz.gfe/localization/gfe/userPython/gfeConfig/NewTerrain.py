# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# NewTerrain.py
#
# GFE configuration file for use when editing GFE Topo for Standard Terrain initiative
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
# 01/13/2015    #3955     randerso     Copied from EditTopo GFE config file and updated
# 10/13/2015    #4961     randerso     Renamed parameters from Terrain to Topo
#
# Author:   romberg
# ----------------------------------------------------------------------------

# This special GFE configuration file is for modifying the NewTopo
# grid that is stored on the ifpServer.  Starting the GFE
# with this configuration file should bring up a special GFE with
# just the Topo parameters

# Include this line to override gfeConfig (BASE configuration):
from gfeConfig import *

# Hide this configuration file from users
HideConfigFile = 1

DefaultGroup = "NewTerrain"

mutableModel = "EditTopo_NewTerrain"

dbTypes = ["EditTopo", ""]

# Turn off the split boundary display -- this is very important for performance due to the
# fact that these grids have a 1 second resolution
SplitBoundaryDisplay = no

GMTED_fitToDataColorTable = "All Grids"
GTOPO_fitToDataColorTable = "All Grids"
NewTopo_fitToDataColorTable = "All Grids"
Topo_fitToDataColorTable = "All Grids"

GridManagerSortOrder = ['NewTopo', 'Topo', 'GMTED', 'GTOPO']

GM_TE_Layout = "OnLeft"
