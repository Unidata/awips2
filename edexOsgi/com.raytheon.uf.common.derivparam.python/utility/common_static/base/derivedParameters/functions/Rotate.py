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
from math import radians,cos,sin
from numpy import array,dot
import Vector

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
    v = args[0]
    r = radians(args[1])
    s = sin(r)
    c = cos(r)
    return vectorTransformMatrix(v,c,-s,s,c)

def vectorTransformMatrix(args):
    
    uComponent,vComponent = args[0]
    
    uComponentShape = uComponent.shape
    vComponentShape = vComponent.shape 
    
    # in-place flatten the arrays
    uComponent = uComponent.reshape((uComponent.size,))
    vComponent = vComponent.reshape((vComponent.size,))
    
    vector = array([uComponent, vComponent])
    transform = array([[args[1], args[2]], [args[3], args[4]]],dtype=uComponent.dtype)
    
    u,v = dot(transform, vector)
    
    # resize the arrays back to appropriate size
    u.resize(uComponentShape)
    v.resize(vComponentShape)
    
    return Vector.componentsTo(u, v)
    