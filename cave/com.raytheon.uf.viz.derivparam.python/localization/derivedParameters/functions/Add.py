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

from numpy import add
from numpy import array

from Vector import execute as Vector

def execute(*args):
    """ Perform scalar or vector addition
    
    """
    if len(args) == 1 and isinstance(args[0], list):
        return execute(*args[0])
    elif isinstance(args[0], tuple):
        return vectorAddition(args)
    else:
        return scalarAddition(args)
    
def scalarAddition(args):    
    return reduce(add, args)

def vectorAddition(args):

    from numpy import zeros
    
    targetShape = args[0][2].shape
    targetType = args[0][2].dtype
    
    uResult = zeros(shape=(targetShape), dtype=targetType)
    vResult = zeros(shape=(targetShape), dtype=targetType)
    
    for arg in args:
        uResult = scalarAddition((uResult, arg[2]))
        vResult = scalarAddition((vResult, arg[3]))
    
    return Vector(uResult, vResult)

def test():
    
    if not( all(execute(array([1., 2.]), array([3., 4.])) == array([4., 6.]))):
        raise Exception
    
    Vector1 = Vector(array([1.,2.]),array([0.,270.]),True)
    Vector2 = Vector(array([3.,4.]),array([180.,90.]),True)
    (mag, dir, u, v) = execute(Vector1,Vector2)
    if not( all(mag == array([2., 2.])) and all(dir.round(0) == array([180., 90.]))):
        raise Exception
        
    if not( all(execute(array([1., 2.]), 3.) == array([4., 5.]))):
        raise Exception

    print "Add Test Complete"