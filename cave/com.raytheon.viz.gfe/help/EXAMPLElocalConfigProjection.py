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

# We want to change the projection so that North is up for our longitude.
# Basically we still want to keep the Grid211 and define a new one.
# Here is the definition for the new projection
Local211 = ('Local211', LAMBERT_CONFORMAL,
           (-133.459, 12.190), (-49.385, 57.290), (-116.0, 25.0),
           25.0, 25.0, (1, 1), (93, 65), 0.0, 0.0, 0.0)

# Now we need to tell the server that our site (Boise in this example)
# should use this new projection.  We need to completely redefine the
# grid domain information for the site and include the new projection.
SITES['BOI'] = ([45, 45], (25.00, 34.00), (11.0, 11.0), 'MST7MDT', Local211, 'wfo')




