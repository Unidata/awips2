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

#cig[:,index::6]

import numpy as np
from numpy import *

def execute(cig, index):
    outerLen=len(cig)
    innerLen=len(cig[0])
    endElement=outerLen*innerLen
    for counter1, i in enumerate(cig):
        for counter2, j in enumerate(i):
            if ("BKN" in j) or ("OVC" in j) or ("VV" in j):
                cig[counter1][counter2]=''
    cig = cig.flatten() 
    return cig[index:endElement:6]
