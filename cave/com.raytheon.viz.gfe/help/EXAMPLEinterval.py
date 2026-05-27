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
# Example ifpIMAGE configuration file.

# Must always import from gfeConfig (or your site configuration)
from gfeConfig import *
import gfeConfig


# Defines T to be display and as an image
Png_parms = ['T_SFC:_Official -1']
Png_image = 'T'

#To display the snapshotTime at the interval offsets
Png_snapshotTime = 1

#Set the interval to 6 and the offset to 1.  This will produce grids at
#01z, 07z, 13z and 19z.
Png_interval = 6
Png_intervalOffset = 1



