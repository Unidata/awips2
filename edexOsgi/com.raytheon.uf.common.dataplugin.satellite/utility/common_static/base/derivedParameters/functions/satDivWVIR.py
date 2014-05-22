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

# ----------------------------------------------------------------
# Calculate the derived parameter WV/IR
# 
# ----------------------------------------------------------------
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
    IR = numpy.where(uIR < 180, 0, uIR)
    result = numpy.where(IR == 0, WV, IR)
    return result.astype(numpy.int8)

       
