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

from numpy import where

def execute(TP3hrtPlus0, TP3hrtMinus10800):
    TP3hrtPlus0 = where((TP3hrtPlus0 > 4e13),0,TP3hrtPlus0);
    TP3hrtMinus10800 = where((TP3hrtMinus10800 > 4e13),0,TP3hrtMinus10800);
    result = TP3hrtPlus0+TP3hrtMinus10800
    return result
        