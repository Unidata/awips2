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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MakeHazard.py
#
# Author: wdougherty
# ----------------------------------------------------------------------------
import numpy
import JUtil
import LogStream
import TimeRange
from HazardUtils import HazardUtils
from HazardUtils import ELEMENT
from HazardUtils import MODEL
from HazardUtils import LEVEL
from HazardUtils import SUCCESS
from HazardUtils import FAIL_REDUNDANT

def makeHazard(hazard, dbss, timeRange, zones, segment, selectedTimeRange, defaultAreaList, defaultHazard, defaultSegment):
    hazardUtils = HazardUtils(dbss, None)
    # hazard has already been validated
    # timeRange has already been validated

    # convert zones to a Python list
    if zones is None:
        zones = []
    if defaultAreaList is None:
        defaultAreaList = []
        
    if not isinstance(zones, list):
        zones = JUtil.javaStringListToPylist(zones)
    if not isinstance(defaultAreaList, list):
        defaultAreaList = JUtil.javaStringListToPylist(defaultAreaList)

    # Find the mask that describes the area covered by the hazard
    if [] == zones:
        ea = hazardUtils.getActiveEditArea()
        if ea is None:
            mask = None
        else:
            mask = hazardUtils.encodeEditArea(ea)
    else:
        mask = hazardUtils._makeMask(zones)

    # Hazards need to be separated for makeHazard's temp hazard grid.
    # If the user hasn't already separated them, it's up to us.
    if hazardUtils._tempWELoaded():
        pass # Hazards are already separated
    else:
        hazParm = hazardUtils.getParm(MODEL,ELEMENT,LEVEL)
        # If the inventory span is invalid, the hazards grid is empty.
        if hazParm.getInventorySpan().isValid():
            if SUCCESS != hazardUtils._separateHazardGrids():
                return False
        
    # Build the hazard key
    hazardKey = hazard
    defaultHazardKey = defaultHazard
    
    if segment is None:
        segment = ""
    if defaultSegment is None:
        defaultSegment = ""

    segment = segment.strip()
    defaultSegment = defaultSegment.strip()
    
    if "" != segment:
        hazardKey = hazardKey + ":" + segment
    if "" != defaultSegment and defaultHazardKey is not None:
        defaultHazardKey = defaultHazardKey + ":" + defaultSegment

    # Create the name of the temp weather element from hazardKey
    weName = hazardUtils._makeTempWEName(hazardKey)
    
    # if we're modifying, remove the old grid first
    if defaultAreaList != [] and hazardKey == defaultHazardKey:
        hazardUtils.deleteCmd([weName], selectedTimeRange)
        
    # Don't allow the user to create an empty hazard grid
    if not numpy.any(mask):
        hazardUtils.statusBarMsg(
            "NO EDIT AREA SELECTED:  \n Select area from map or load edit area in GFE!", 
            "S", "GHG Status")
        return False    

    # Create the temporary hazard grid
    hazardUtils._addHazard(weName, timeRange, hazardKey, mask)
    
    # convert timeRange to Python for logging
    timeRange = TimeRange.TimeRange(timeRange)
    # log the creation of the temp hazard
    LogStream.logEvent("Set: " + weName + " " + 
          hazardUtils._printTime(timeRange.startTime().unixTime()) + " " + 
          hazardUtils._printTime(timeRange.endTime().unixTime()) + " " + 
          hazardKey + " " + str(zones))
    return True

##
# 
def ensureSeparated(dbss):
    hazardUtils = HazardUtils(dbss, None)
    rtnCode = hazardUtils._separateHazardGrids()
    separated = rtnCode in [SUCCESS, FAIL_REDUNDANT]
    return separated
