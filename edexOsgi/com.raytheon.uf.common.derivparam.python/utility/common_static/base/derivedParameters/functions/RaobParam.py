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

from numpy import array, NaN, float32

def execute(levelsList, index):
    maxLen = 0
    results = []
    for i in range(len(levelsList)):
        levels = levelsList[i]
        values = []
        for level in levels:
            values.append(level.getParam(index))
        if len(levels) > maxLen:
            maxLen = len(levels)
        results.append(values)
    for result in results:
        while len(result) < maxLen:
            result.append(NaN)
    return array(results, float32)