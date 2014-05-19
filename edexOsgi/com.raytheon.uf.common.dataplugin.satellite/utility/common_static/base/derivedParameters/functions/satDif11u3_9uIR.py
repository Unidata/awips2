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
from Difference import execute as Difference
from numpy import logical_and, where

# Calculate Satellite 11u-3.9u, basicall does difference with some fancy flagging
def execute(arg1, arg2):
    # if both args are 0, the result will be -128 otherwise it is the Difference
    # The idea is that the args treat 0 as NaN so we treat -128 as NaN
    # Byte data and NaN don't mix any better than this.
    return where(logical_and(arg1 == 0, arg2 == 0), -128, Difference(arg1, arg2))
