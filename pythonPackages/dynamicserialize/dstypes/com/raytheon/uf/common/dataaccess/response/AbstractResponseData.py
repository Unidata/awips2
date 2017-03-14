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

import abc


class AbstractResponseData(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __init__(self):
        self.time = None
        self.level = None
        self.locationName = None
        self.attributes = None

    def getTime(self):
        return self.time

    def setTime(self, time):
        self.time = time

    def getLevel(self):
        return self.level

    def setLevel(self, level):
        self.level = level

    def getLocationName(self):
        return self.locationName

    def setLocationName(self, locationName):
        self.locationName = locationName

    def getAttributes(self):
        return self.attributes

    def setAttributes(self, attributes):
        self.attributes = attributes

