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

from Multiply import execute as Multiply
from numpy import NaN, where

#  
def execute(*args):
    """ Divide a scalar by a scalar or a vector by a scalar.
    
    """
    
    divArgs = list(args)
    
    for i in range(1,len(divArgs)):
        divArgs[i] = where(divArgs[i] == 0, NaN, 1/divArgs[i] )
    
    return apply(Multiply,divArgs)

def test():
    
    from numpy import array
    
    if not( all(execute(array([2.,4.]),array([2.,2.])) == array([1.,2.]))):
        raise Exception
    
    from Vector import execute as Vector
    
    vector = Vector(array([2.,4.]),array([0.,270.]),True)
    (mag, dir, u, v) = execute(vector,2.)
    if not( all(mag == array([1.,2.])) and all(dir == array([0.,270.]))):
        raise Exception
        
    if not( all(execute(array([2.,4.]),array([2.,0.])) == array([1.,NaN]))):
        raise Exception

    print "Divide Test Complete"
    