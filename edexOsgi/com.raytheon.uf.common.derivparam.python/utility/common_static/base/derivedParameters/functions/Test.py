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

from numpy import all
from numpy import any
from numpy import add
from numpy import subtract
from numpy import multiply
from numpy import divide
from numpy import copy

def execute(*args):
    ''' Search and replace values of the initial input argument 
    
    This function takes five arguments at a minimum. There is an initial input
    argument, followed by a group of four arguments which can cause the input
    argument to be modified. There can be any number of these groups, each of
    which modifies the result of the previous argument group.
    
    For a single argument group with four arguments: operation, lowTest,
    highTest, and replacement:
    
    operation
               : a constant operation type in which the first argument determines
               how values in the input argument are tested against those in the
               second and third arguments
    
               The type of operation to perform depends on the given operation's
               magnitude and polarity.
    
               positive :: When the first argument is positive, then the test is
               passed when values in the input argument fall within the range of
               values of the second and third.
    
               negative :: If it is negative, the test is passed if values in the
               input argument fall outside the range of values of the second and
               third.
    
               1 :: When the magnitude of the first argument is one, then testing
               and replacing is done on each individual corresponding grid point.
    
               2 :: When the magnitude of the first argument is two, the entire
               input grid is tested point by point and if ANY test is passed then
               the entire grid is replaced.
    
               3 :: If the magnitude of the first argument is three, then the
               entire grid is tested point by point and if EVERY test is passed
               then the entire grid is replaced.
    
               Non-zero digits in the thousands place of the operation type
               cause the argument to be used in an operation on the input data
               rather than just replace it.
               
               100? :: add replacement to input data
               200? :: subtract replacement from input data
               300? :: multiply input data by replacement
               400? :: divide input data by replacement
    
    lowTest
               : the lower end of the test range
    hiTest
               : the upper end of the test range
    
    replacement
               : when the test is passed, values in the input argument are
               replaced with values in the replacement argument
    
    NOTE:  once a particular value in a grid has been altered by a test, all 
           memory of its original value is lost for the purposes of future tests
           in the group. This can mean that two tests that work fine independently
           can fail to do what the user intended if strung together.
    
    '''
    
    result = copy(args[0])
    
    termLength = 4
    
    terms = (len(args) - 1) / termLength
    
    for i in range(terms):
        operation = int(args[i * termLength + 1])    
        lowTest = args[i * termLength + 2]
        highTest = args[i * termLength + 3]
        replacement = args[i * termLength + 4]
        
        if type(replacement) != type(result):
            replacementFillValue = replacement
            replacement = copy(result)
            replacement[:] = replacementFillValue
        
        comparisonType = abs(operation) % 10
        replacementType = abs(operation) / 1000
        replaceMask = (lowTest <= result) & (result <= highTest)
        
        if operation < 0:
            replaceMask = (lowTest > result) | (result > highTest)
            
        if replacementType == 1:
            replacement = add(result, replacement)
        elif replacementType == 2:
            replacement = subtract(result, replacement)
        elif replacementType == 3:
            replacement = multiply(result, replacement)
        elif replacementType == 4:
            replacement = divide(result, replacement)

        if comparisonType == 1:
            result[replaceMask] = replacement[replaceMask]
        elif comparisonType == 2 and any(replaceMask):
            result = replacement
        elif comparisonType == 3 and all(replaceMask):
            result = replacement
    
    return result

def test():
    
    from numpy import array
    
    if not(all(execute(array([1., 2., 3., 4.]), 1., 1., 3., array([3., 4., 5., 6.])) == array([3., 4., 5., 4.]))):
        raise Exception
    
    if not(all(execute(array([1., 2., 3., 4.]), 2., 1., 3., array([3., 4., 5., 6.])) == array([3., 4., 5., 6.]))):
        raise Exception
    
    if not(all(execute(array([1., 2., 3., 4.]), 3., 1., 3., array([3., 4., 5., 6.])) == array([1., 2., 3., 4.]))):
        raise Exception
        
    if not(all(execute(array([1., 2., 3., 4.]), - 1., 1., 3., array([3., 4., 5., 6.])) == array([1., 2., 3., 6.]))):
        raise Exception
        
    if not(all(execute(array([1., 2., 3., 4.]), - 1., 1., 3., array([3., 4., 5., 6.]), 1., 2., 6., array([7., 8., 9., 0.])) == array([1., 8., 9., 0.]))):
        raise Exception
        
    if not(all(execute(array([1., 2., 3., 4.]), 1001., 1., 3., array([3., 4., 5., 6.])) == array([4., 6., 8., 4.]))):
        raise Exception
    
    if not(all(execute(array([1., 2., 3., 4.]), 2001., 1., 3., array([3., 4., 5., 6.])) == array([ - 2., - 2., - 2., 4.]))):
        raise Exception
        
    if not(all(execute(array([1., 2., 3., 4.]), 3001., 1., 3., array([3., 4., 5., 6.])) == array([3., 8., 15., 4.]))):
        raise Exception
        
    if not(all(execute(array([3., 2., 3., 4.]), 4001., 1., 3., array([3., 4., 5., 6.])) == array([1., 0.5, 0.6, 4.]))):
        raise Exception

    if not(all(execute(array([1., 2., 3., 4.]), 1., 1., 3., 2.) == array([2., 2., 2., 4.]))):
        raise Exception
    
    print "Test Test Complete"