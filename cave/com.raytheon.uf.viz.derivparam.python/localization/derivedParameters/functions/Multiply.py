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

from numpy import multiply
from Vector import execute as Vector

def execute(*args):
    """ Perform multiplication of any number of scalars or of a vector and a scalar.
    
    """
    
    if type(args[0]) == tuple:
        return vectorMultiply(args)
    else:
        return scalarMultiply(args)
    
def scalarMultiply(args):
    
    return reduce(multiply, args)

def vectorMultiply(args):
    
    vector = args[0]
    scalar = args[1]

    return Vector(scalarMultiply((vector[2], scalar)), scalarMultiply((vector[3], scalar)))

def test():
    
    from numpy import array
    
    if not(all(execute(array([2., 4.]), 2.) == array([4., 8.]))):
        raise Exception
    
    vector = Vector(array([2., 4.]), array([0., 270.]), True)
    (mag, dir, u, v) = execute(vector, 2.)
    if not(all(mag == array([4., 8.])) and all(dir == array([0., 270.]))):
        raise Exception

    print "Multiply Test Complete"
    