##
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