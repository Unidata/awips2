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

from numpy import power
from numpy import multiply

def execute(*args):
    """Combine the arguments into a polynomial result
    
    @return result of arg1*arg2^arg3 + arg4*arg5^arg6 + arg7*arg8 
    
    """

    result = 0
    
    terms = len(args) / 3
    if len(args) % 3 != 0:
        terms = terms + 1
    
    for i in range(terms):
                
        coefficient = args[i * 3]    
        variable = 1 if i * 3 + 1 >= len(args) else args[i * 3 + 1]
        exponent = 1 if i * 3 + 2 >= len(args) else args[i * 3 + 2]
        
        result += multiply(coefficient, power(variable, exponent))
        
    return result 

def test():
    
    from numpy import array
    
    if not(all(execute(array([1., 2.])) == array([1., 2.]))):
        raise Exception
        
    if not(all(execute(array([1., 2.]), array([3., 4.])) == array([3., 8.]))):
        raise Exception
        
    if not(all(execute(array([1., 2.]), array([3., 4.]), array([5., 6.])) == array([243., 8192.]))):
        raise Exception

    print "Poly Test Complete"