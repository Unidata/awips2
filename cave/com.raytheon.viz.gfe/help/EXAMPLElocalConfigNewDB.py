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

# Adding a new database is fairly complicated.  Multiple steps must be
# defined and done in the correct order.

# We want T, Td, rh, and maxrh in this database.  Since rh and max rh are
# not defined in serverConfig, we add it in localConfig.py.
rh = ("RH", SCALAR, "%", "Relative Humidity", 100.0, 0.0, 0, NO)
maxrh = ("MaxRH", SCALAR, "%", "Maximum Relative Humidity", 100.0, 0.0, 0, NO)

# We want the maxRH to have a special time constraint that is not defined
# in serverConfig.  The other elements use already defined configurations.
# This time constraint starts at 6am and is 4 hours long, repeats daily.
LT4gap = localTC(6*HOUR, 24*HOUR, 4*HOUR, 0)

# Now we define the database attributes. Our database is called Ag, is 
# model-based (not singleton), and we only want 1 version of it.
Ag = ('Ag', GRID, '', NO, NO, 1, 0)

# Now we define the new parm groupings which relate weather elements to
# time constraints. The T and Td come from serverConfig.py.  The T, Td, and
# rh get the basic 1 hourly time constraints.
AG_MODEL = [([Temp,Td,rh], TC1), ([maxrh], LT4gap)]

# Very important step so that server sees this new definition.
# There can only be ONE dbs line in the localConfig.py
dbs = [(Ag, AG_MODEL)]




