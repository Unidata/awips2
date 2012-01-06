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

import numpy

def execute(SFEXC):
    #linear mapping 0->0 through 60->19.68, y = 0.328x
    
    #61->252 goes to pixel 229, data point is 19.7728
    #253 goes to pixel 0, data point is -1.48528
    #254 goes to pixel 240, data point is 20.7939
    
    #if data < 61 multiply by .328
    #if data is > 60 and < 253 set to 19.7728
    #if data is 253 set to -1.48528
    #if data is 254 set to 20.7939
    
    #mask values above 60
    result = numpy.ma.masked_greater(SFEXC, 60)
    #transform unmasked values
    result *= 0.328
    #remove mask
    result.mask = numpy.ma.nomask
    
    #mask all values below 61 and above 252
    result = numpy.ma.masked_less(result, 61)
    result = numpy.ma.masked_greater(result, 252)
    result *= 0
    result += 19.7728
    result.mask = numpy.ma.nomask
    
    #mask all but 253
    result = numpy.ma.masked_not_equal(result, 253)
    result *= 0
    result += -1.48528
    result.mask = numpy.ma.nomask
    
    #mask all but 254
    result = numpy.ma.masked_not_equal(result, 254)
    result *= 0
    result += 20.7939
    result.mask = numpy.ma.nomask
    
    return result


def main():
    testin = numpy.array([[12.0,36,253,64,53,24,33],[253,254,112,29,42,1,55],[12,123,253,18,254,22,212]])
    testout = execute(testin)
    print(testout)
    
if __name__ == "__main__":
    main()
