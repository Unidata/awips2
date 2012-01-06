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
from Vector import execute as Vector
from numpy import array
from numpy import dot

def execute(*args):
    """Rotate a vector
    
    Rotate vector by a number of degrees, or transform the vector with a matrix.
    The arguments after the vector need to be constants. 
    
    """
    
    if type(args[0]) != tuple:
        raise ValueError("first argument must be a vector")
    elif len(args) == 2:
        return rotateDegrees(args)
    else:
        return vectorTransformMatrix(args)
    
def rotateDegrees(args):
    
    magnitude = args[0][0]
    direction = Add(args[0][1], args[1]) % 360
    
    return Vector(magnitude, direction, True)

def vectorTransformMatrix(args):
    
    uComponent = args[0][2]
    vComponent = args[0][3]
    
    uComponentShape = uComponent.shape
    vComponentShape = vComponent.shape 
    
    # in-place flatten the arrays
    uComponent.resize((uComponent.size,))
    vComponent.resize((vComponent.size,))
    
    vector = array([uComponent, vComponent])
    transform = array([[args[1], args[2]], [args[3], args[4]]],dtype=uComponent.dtype)
    
    (u,v) = dot(transform, vector)
    
    # resize the arrays back to appropriate size
    u.resize(uComponentShape)
    v.resize(vComponentShape)
    
    return Vector(u, v)

def test():
    
    from numpy import all
    from numpy import array
    
    try:
        execute(1, 2, 3, 4);
        raise Exception
    except ValueError:
        pass
    
    (mag, dir, u, v) = execute((array([1., 2.]), array([90., 270.])), 180.)
    if not(all(mag == array([1., 2.])) and all(dir == array([270., 90.]))):
        raise Exception
        
    (mag, dir, u, v) = execute((None, None, array([1., 3.]), array([2., 4.])), 3., 4., 5., 6.)
    if not(all(u == array([11., 25.])) and all(v == array([17., 39.]))):
        raise Exception
    
    print "Rotate Test Complete"

    