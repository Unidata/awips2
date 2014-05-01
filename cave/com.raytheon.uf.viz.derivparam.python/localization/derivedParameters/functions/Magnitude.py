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
# Calculate magnitude
# ----------------------------------------------------------------
from numpy import hypot

def execute(*args):
    """ Return the magnitude of a given vector or vector components
    
    """
    
    if type(args[0]) == tuple:
        return vectorMagnitude(args[0])
    else:
        return componentMagnitude(args[0], args[1])

def vectorMagnitude(vector):
    """ Return the first item in the vector tuple (its the magnitude)
    
    vector tuples are assumed to follow the tuple returned by the
    Vector.execute method.  That is (mag,dir,u,v)
    
    """
    return vector[0]

def componentMagnitude(u, v):
    return hypot(u, v)

def test():
    
    from Vector import execute as Vector
    from numpy import array
    
    testVector = Vector(array([1., 2.]), array([180., 270.]), True)
    result = execute(testVector)
    if not(all(result == array([1.,2.]))):
        raise Exception
    
    result = execute(testVector[2],testVector[3])
    if not(all(result == array([1.,2.]))):
        raise Exception
    
    print "Magnitude Test Complete"