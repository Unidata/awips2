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
##
# This is a helper script for use by TestSmartScript.java.
# It is meant to be run from within Jepp.
# It creates a DataManager and uses it to create a SmartScript.
import numpy as np
import SmartScript
    

# Create a numpy array of floats for the scalar test
scalarGrid = np.array(list(range(16)), dtype=np.float32).reshape((4,4))

# Create a "discrete grid" tuple for the discrete test
idxA = np.array(list(range(4)), dtype='int8') * np.ones((4,4), dtype='int8')
idxKL = ["<None>", "one", "two", "three"]
discreteGrid = (idxA, idxKL)

numpyByteArray = np.ones((3,3), dtype=np.byte)
dimx = 3
dimy = 3

def testGetGridShape(dataMgr):
    smartScript = SmartScript.SmartScript(dataMgr)
    valueExpected = (145, 145)
    valueObtained = smartScript.getGridShape()
    if not valueExpected==valueObtained:
        print("valueObtained =", valueObtained)
    return (valueExpected==valueObtained)

def testGetGridInfo(dataMgr, model, element, level, timeRange, mostRecentModel=0):
    smartScript = SmartScript.SmartScript(dataMgr)
    rtnInfo = smartScript.getGridInfo(model, element, level, timeRange, mostRecentModel)
    if rtnInfo is not None:
        gridTime = rtnInfo[0].gridTime()
        if gridTime is None:
            raise RuntimeError("gridTime is None")
    return True

def testSortUglyStr(dataMgr, uglyStr):
    smartScript = SmartScript.SmartScript(dataMgr)
    return smartScript.sortUglyStr(uglyStr)

def testGetIndex(dataMgr, keyList, uglyStr):
    smartScript = SmartScript.SmartScript(dataMgr)
    return smartScript.getIndex(uglyStr, keyList)

def getParm(dataMgr, model, element, level):
    smartScript = SmartScript.SmartScript(dataMgr)
    return smartScript.getParm(model, element, level)