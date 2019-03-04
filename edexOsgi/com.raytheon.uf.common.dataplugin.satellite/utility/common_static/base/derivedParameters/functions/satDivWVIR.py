##
##

# ----------------------------------------------------------------
# Calculate the derived parameter WV/IR
#
# ----------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date           Ticket#      Engineer      Description
# ------------   ----------   -----------   -----------
#                             ???           Initial creation
# Aug 05, 2015   4703         njensen       cast 0 to uint8
#

import numpy

##
# Calculate the derived parameter WV/IR
# This function accepts numpy arrays of the appropriate values.
#
# @param Imager_6pp7hh6pp5_micron_IR_ooWVcc: Water Vapor
# @param Imager_11_micron_IR : Imager 11 micron IR
# @return:
# @rtype: numpy array or scalar
#
def execute(Imager_6pp7hh6pp5_micron_IR_ooWVcc, Imager_11_micron_IR):

    uWV = Imager_6pp7hh6pp5_micron_IR_ooWVcc.astype(numpy.uint8)
    WV = (uWV * 0.708).astype(numpy.uint8)
    uIR = Imager_11_micron_IR.astype(numpy.uint8)
    IR = numpy.where(uIR < 180, numpy.uint8(0), uIR)
    result = IR
    mask = (IR == 0)
    result[mask] = WV[mask]
    return result.astype(numpy.int8)