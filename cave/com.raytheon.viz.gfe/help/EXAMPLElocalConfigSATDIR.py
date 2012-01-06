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
# This is an example localConfig.py file

# Always include these lines at the top of your localConfig.py file.
from serverConfig import *
import serverConfig

# modifying the list of SATDIRS directories seen from the GFE, and scanned
# by the ifpServer
pfix = "/data/fxa/sat/SBN/netCDF/"
serverConfig.SATDIRS = [(pfix + "westCONUS/conus_vis/regClip", "visibleWest"),
           (pfix + "westCONUS/conus_i11/regClip", "ir11West"),
           (pfix + "westCONUS/conus_i12/regClip", "ir13West"),
           (pfix + "westCONUS/conus_i39/regClip", "ir39West"),
           (pfix + "westCONUS/conus_iwv/regClip", "waterVaporWest"),



