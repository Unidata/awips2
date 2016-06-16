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
# NewTerrain.py
#
# GFE configuration file for use when editing GFE Topo for Standard Terrain initiative
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
# Jan 13, 2015  #3955     randerso     Copied from EditTopo GFE config file and updated
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
#HideConfigFile = 1

DefaultGroup = "NewTerrain"

mutableModel = "EditTopo_Topo"

dbTypes = ["EditTopo", ""]

# Turn off the split boundary display -- this is very important for performance due to the
# fact that these grids have a 1 second resolution
SplitBoundaryDisplay = no

Topo_fitToDataColorTable = "All Grids"
NewTerrain_fitToDataColorTable = "All Grids"
OldTerrain_fitToDataColorTable = "All Grids"

GM_TE_Layout = "OnLeft"
