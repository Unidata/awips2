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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    18 Feb 2010     #4502         jelkins        Initial Creation.

from numpy import array, nanmin

def execute(*args):
    """ Return the min value at each grid point
    
    @param args
                a list of grids or a single 3D grid. If a single 3D variable is passed in, 
                will compute without considering the vertical coordinate information. 
    """
    
    if len(args) == 1 and isinstance(args[0], list):
        if len(args[0]) == 2 and len(args[0][0].shape) == 3:
            # we have been passed 3D data
            return nanmin(args[0][0], 0)
        else:
            return execute(*args[0])
    else:
        return nanmin(array(list(args)), 0)