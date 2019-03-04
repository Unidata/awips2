##
##

#
# SOFTWARE HISTORY
#
# Date           Ticket#      Engineer      Description
# ------------   ----------   -----------   -----------
#                             ???           Initial creation
# Aug 05, 2015   4703         njensen       cast -128 to int8
#

from Difference import execute as Difference
from numpy import logical_and, where, int8

# Calculate Satellite 11u-3.9u, basically does difference with some fancy flagging
def execute(arg1, arg2):
    # if both args are 0, the result will be -128 otherwise it is the Difference
    # The idea is that the args treat 0 as NaN so we treat -128 as NaN
    # Byte data and NaN don't mix any better than this.
    return where(logical_and(arg1 == 0, arg2 == 0), int8(-128), Difference(arg1, arg2))
