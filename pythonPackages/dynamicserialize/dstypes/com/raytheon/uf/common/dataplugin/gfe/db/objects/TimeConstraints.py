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



HOUR = 3600;
DAY = 24 * HOUR;

class TimeConstraints(object):

    def __init__(self, duration=0, repeatInterval=0, startTime=0):
        duration = int(duration)
        repeatInterval = int(repeatInterval)
        startTime = int(startTime)
        
        if duration != 0 or repeatInterval != 0 or startTime != 0:
            if (repeatInterval <= 0 or repeatInterval > DAY) or \
               (DAY % repeatInterval != 0 or repeatInterval < duration) or \
               (startTime < 0 or startTime > DAY) or \
               (duration < 0 or duration > DAY):
                raise ValueError("Bad init values for TimeConstraints.") 
        
        self.duration = duration
        self.repeatInterval = repeatInterval
        self.startTime = startTime
        
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        if not self.anyConstraints():
            return "<NoConstraints>"
        else:
            return "[s=" + str(self.startTime / HOUR) + "h,i=" + \
                   str(self.repeatInterval / HOUR) + "h,d=" + \
                   str(self.duration / 3600) + "h]"
                   
    def __eq__(self, other):
        if not isinstance(other, TimeConstraints):
            return False
        if self.isValid() != other.isValid():
            return False
        if self.duration != other.duration:
            return False
        if self.repeatInterval != other.repeatInterval:
            return False
        return (self.startTime == other.startTime)
    
    def __ne__(self, other):
        return (not self.__eq__(other))
    
    def anyConstraints(self):
        return (self.duration != 0)
    
    def isValid(self):
        if self.duration != 0 or self.repeatInterval != 0 or self.startTime != 0:
            if (self.repeatInterval <= 0 or self.repeatInterval > DAY) or \
               (DAY % self.repeatInterval != 0 or self.repeatInterval < self.duration) or \
               (self.startTime < 0 or self.startTime > DAY) or \
               (self.duration < 0 or self.duration > DAY):
                return False
        return True

    def getDuration(self):
        return self.duration

    def setDuration(self, duration):
        self.duration = duration

    def getRepeatInterval(self):
        return self.repeatInterval

    def setRepeatInterval(self, repeatInterval):
        self.repeatInterval = repeatInterval

    def getStartTime(self):
        return self.startTime

    def setStartTime(self, startTime):
        self.startTime = startTime

