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
import os
from com.raytheon.uf.common.message.response import ResponseMessageGeneric
from java.util import ArrayList

def parseFsiRadarListLine(line):
    try:
        line = line.strip()
        if line[0] != '/':
            values = line.split()
            if int(values[2]) == 1:
                return values[0]
    except:
        pass
    return False

def makeResponse(resultList):
    al = ArrayList()
    for value in resultList:
        al.add(value)
    return ResponseMessageGeneric(al)

def makeList(arrayList):
    return [arrayList.get(i) for i in range(0, arrayList.size())]

# Try fsiRadarList.txt first
try:
    f = open(os.path.join(os.environ['FXA_DATA'], 'tstorm', 'fsiRadarList.txt'), 'r')
    try:
        result = [x for x in [parseFsiRadarListLine(l) for l in f]
                  if x]
        return makeResponse(result)
    finally:
        f.close()
except:
    pass

# Fallback: Use radars on menu
from com.raytheon.edex.util.radar import RadarsInUseUtil

result = makeList(RadarsInUseUtil.getSite(RadarsInUseUtil.LOCAL_CONSTANT))
return makeResponse(result)
