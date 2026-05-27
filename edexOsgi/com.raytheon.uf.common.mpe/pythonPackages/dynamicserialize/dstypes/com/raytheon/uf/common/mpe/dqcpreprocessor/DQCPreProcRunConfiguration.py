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

# File auto-generated against equivalent DynamicSerialize Java class
# 
#      SOFTWARE HISTORY
# 
#     Date            Ticket#       Engineer       Description
#     ------------    ----------    -----------    --------------------------
#     Jan 28, 2019      7699         smanoj         Generated

class DQCPreProcRunConfiguration(object):

    def __init__(self):
        self.runDate = None
        self.numDays = None
        self.setZero = None
        self.areas = None

    def getRunDate(self):
        return self.runDate

    def setRunDate(self, runDate):
        self.runDate = runDate

    def getNumDays(self):
        return self.numDays

    def setNumDays(self, numDays):
        self.numDays = numDays

    def getSetZero(self):
        return self.setZero

    def setSetZero(self, setZero):
        self.setZero = setZero

    def getAreas(self):
        return self.areas

    def setAreas(self, areas):
        self.areas = areas

