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
from numpy import zeros

def execute(values, *mappings):
    tmp = zeros(values.shape, 'float32')
    
    size = len(mappings)
    i = 0
    while i < size:
        checkFor = mappings[i]
        i += 1
        setTo = mappings[i]
        i += 1
        tmp[values == checkFor] = setTo
    return tmp



       
