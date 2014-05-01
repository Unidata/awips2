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
# -*-python-*-
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Gweight -- used for modifying the topo grid.
#
# Author:   romberg
# ----------------------------------------------------------------------------

# This special GFE configuration file is for modifying the topo
# grid that is stored on the ifpServer.  Starting the GFE
# with this configuration file should bring up a special GFE with
# just the Topo parameter.

# Include this line to override gfeConfig (BASE configuration):
from gfeConfig import *

# Hide this configuration file from users
HideConfigFile = 1

# Auto-load the group containing the geo weight weather elements
DefaultGroup = "EditTopo"

# The mutable model is set to "CG_Slider", to bring up the geoweight parameters
mutableModel = "EditTopo_Topo"

# The viewable database types is "CG" for the gweight.
dbTypes = ["EditTopo"]

# Turn off the split boundary display -- this is very important for performance due to the
# fact that these grids have a 1 second resolution
SplitBoundaryDisplay = no
