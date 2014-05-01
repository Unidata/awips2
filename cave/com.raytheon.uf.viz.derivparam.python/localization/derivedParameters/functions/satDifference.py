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

# ----------------------------------------------------------------
# 
# 
# ----------------------------------------------------------------
import numpy

##
# Calculate the difference between the 11u and 12u IR.
# This function accepts numpy arrays of the appropriate values.
#
# @param physicalElement1
# @param physicalElement2
# @return: The difference of the two sat imagers
# @rtype: numpy array or scalar
# 
def execute(physicalElement1, physicalElement2):
    
    result = physicalElement2 - physicalElement1
    result = numpy.where(result < 0, physicalElement1 - physicalElement2, result)
    return result