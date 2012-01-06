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
            diffVectorArg = list(diffArgs[i])
            diffVectorArg[2] = -diffVectorArg[2]
            diffVectorArg[3] = -diffVectorArg[3]
            diffArgs[i] = tuple(diffVectorArg)
        return apply(Add, diffArgs)
    else:
        result = 0
        result += diffArgs[0]
        for i in range(1, len(diffArgs)):
            result -= diffArgs[i]
        return result

def test():
    
    from numpy import array
                     
    if not(all(execute(array([1., 2.]), array([3., 4.])) == array([ - 2, - 2]))):
        raise Exception
    
    from Vector import execute as Vector
    
    # using meteoroligcal directions
    Vector1 = Vector(array([1., 2.]), array([0., 270.]), True)
    Vector2 = Vector(array([3., 4.]), array([180., 90.]), True)
    (mag, dir, u, v) = execute(Vector1, Vector2)
    if not(all(mag == array([2., 2.])) and all(dir.round(0) == array([180., 90.]))):
        print mag
        print dir
        raise Exception

    print "Difference Test Complete"