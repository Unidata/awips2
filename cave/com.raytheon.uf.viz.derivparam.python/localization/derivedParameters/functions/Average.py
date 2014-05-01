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
from Divide import execute as Divide


def execute(*args):
    """ Calculate the arithmetic average of any number of arguments.
    
    """ 
    if len(args) == 1 and isinstance(args[0], list):
        return execute(*args[0])
    return Divide(Add(*args),float(len(args)))

def test():
    
    from numpy import array

    if not( all(execute(array([1.,2.]),array([3.,4.])) == array([2,3]))):
        raise Exception
    
    from Vector import execute as Vector
    
    Vector1 = Vector(array([1.,2.]),array([0.,270.]),True)
    Vector2 = Vector(array([3.,4.]),array([180.,90.]),True)
    (mag, dir, u, v) = execute(Vector1,Vector2)
    if not( all(mag == array([1., 1.])) and all(dir.round(0) == array([180., 90.]))):
        raise Exception
    
    print "Average Test Complete"