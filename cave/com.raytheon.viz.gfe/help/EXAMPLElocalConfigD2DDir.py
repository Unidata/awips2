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

# modifying the list of D2D directories seen from the GFE, and scanned
# by the ifpServer.  This is not the recommended approach since it overrides
# all entries.    

serverConfig.D2DDIRS = [('/data/fxa/Grid/SBN/netCDF/AK216/MesoEta', 'NAM40'),
               ('/data/fxa/Grid/SBN/netCDF/AK217/MesoEta', 'NAM20'),
               ('/data/fxa/Grid/SBN/netCDF/NAT203/AVN', 'GFS190'),
               ('/data/fxa/Grid/SBN/netCDF/NAT203/MRF', 'gfsLR'),
               ('/data/fxa/Grid/SBN/netCDF/REG207/NGM', 'NGM95'),
               ('/data/fxa/Grid/SBN/netCDF/REG207/Eta', 'NAM95'),
               '/data/fxa/Grid/SBN/netCDF/REG233/GWW',
               ('/data/fxa/Grid/SBN/netCDF/GRID242/Eta', 'NAM12'),

# the recommended approach is usually to add a new entry, such as this:
serverConfig.D2DDIRS.append(('/data/fxa/Grid/Local/netCDF/LAPS/LAPS', 'LocalL'))




