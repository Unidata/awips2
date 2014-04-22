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
from Add import execute as Add
from numpy import multiply, copy

def execute(*args):
    """ Perform scalar or vector subtraction.
    
    """
    diffArgs = list(args)

    if type(diffArgs[0]) == tuple:
        for i in range(1, len(diffArgs)):
            diffArgs[i] = (-diffArgs[i][0], -diffArgs[i][1])
        return apply(Add, diffArgs)
    else:
        result = 0
        result += diffArgs[0]
        for i in range(1, len(diffArgs)):
            result -= diffArgs[i]
        return result