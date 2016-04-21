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
# ConfigurableIssuance.py
# Methods for setting up configurable issuance.
#
# Author: hansen
# ----------------------------------------------------------------------------

import TimeRangeUtils
import string, types, time
import TimeRange, AbsTime

class ConfigurableIssuance(TimeRangeUtils.TimeRangeUtils):
    def __init__(self):
        TimeRangeUtils.TimeRangeUtils.__init__(self)

    def getIssuanceInfo(self, productIssuance, issuanceList, creationTime=None):
        # Create a NarrativeDef for a "narrative" type of product
        # from an issuanceList and selected item

        (currentLocalTime, self._shift) = self.determineTimeShift()
        if creationTime is None:
            day = currentLocalTime.day
            month = currentLocalTime.month
            year = currentLocalTime.year
            hour = currentLocalTime.hour
            minutes = currentLocalTime.minute
        else:
            localTime = time.localtime(creationTime)
            year = localTime[0]
            month = localTime[1]
            day = localTime[2]
            hour = localTime[3]
            minutes = localTime[4]      
            
        # Determine "issuanceHour"
        startTime = AbsTime.absTimeYMD(year,month,day,hour)
        
        # find the entry for our selection
        #print productIssuance, issuanceList
        entry = self.getEntry(productIssuance, issuanceList)
        desc, startHour, endHour, expireHour, p1Label, \
              lateNightPhrase, lateDayPhrase, todayFlag, narrativeDef = entry
        period1Label = p1Label
        period1LateDayPhrase = lateDayPhrase
        period1LateNightPhrase = lateNightPhrase

        # Take care of "issuanceHour" variable
        startHour = self.convertIssuanceHour(startHour, hour, minutes)
        endHour = self.convertIssuanceHour(endHour, hour, minutes)
        expireHour = self.convertIssuanceHour(expireHour, hour, minutes)

        # Determine startTime and period1
        startTime = AbsTime.absTimeYMD(year, month, day, 0)
        startTime = startTime + startHour * 3600
        endTime = AbsTime.absTimeYMD(year, month, day, 0)
        endTime = endTime + endHour * 3600
        period1 = (endTime.unixTime() - startTime.unixTime())/3600

        # Set "period1" if it appears in narrativeDef
        newNarrativeDef = []
        totalHours = 0
        firstPeriod = 1
        for component, period in narrativeDef:
            # Handle custom components - added in OB8.2.
            # "Custom" components are intended to replace "priorPeriod" which is removed.
            # "Custom" component entries in a narrative definition are of the form:
            #     ("Custom", (componentName, timeRange))
            # where timeRange can be (start_hours, end_hours) or an AFPS.TimeRange.
            # Start_hours and end_hours are relative to midnight local time
            # of the product creation date.
            
            if component == "Custom":
                newNarrativeDef.append((component, period))
                continue
                        
            if firstPeriod:
                if period == "period1":
                    period = period1
                else:
                    period1 = period
                firstPeriod = 0
            totalHours = totalHours + period
            newNarrativeDef.append((component, period))

        # Convert to GMT time before making time range
        startTime = startTime - self._shift
        tr = TimeRange.TimeRange(startTime, startTime + (totalHours * 3600))
        timeRange = tr
        period1TimeRange = TimeRange.TimeRange(
            tr.startTime(), tr.startTime() + period1*3600)
        narrativeDef = newNarrativeDef
        # Expiration time -- convert to GMT
        expireStartTime = AbsTime.absTimeYMD(year, month, day, 0) - self._shift
        expireStartTime = expireStartTime + expireHour * 3600
        expireTime = expireStartTime
        issueTime = AbsTime.current()
        #issueTime = self.getCurrentTime(
        #    None, "%l%M %p %Z %a %b %e %Y", stripLeading=1)
        #expireTimeRange = AFPS.TimeRange(expireStartTime, expireStartTime + 3600)
        #expireTime = string.upper(self.timeDisplay(expireTimeRange, "", "", "%d%H%M", ""))
        return Issuance(entry, timeRange, expireTime, issueTime, narrativeDef,
                 period1TimeRange, period1LateDayPhrase, period1LateNightPhrase,
                 period1Label, todayFlag)
    
    def convertIssuanceHour(self, issuanceHour, currentHour, currentMinutes):
        if type(issuanceHour) == types.StringType:
            if currentMinutes > self.issuanceHour_minutesPastHour():
                currentHour = currentHour + 1
                # Don't cross to the next day
                if currentHour == 24:
                    currentHour = 23
            issuanceHour = string.replace(issuanceHour, "issuanceHour", `currentHour`)
            exec "resultHour = " + issuanceHour
            return resultHour            
        else:
            return issuanceHour
        
    def getEntry(self, productIssuance, issuanceList):            
        found =0
        for entry in issuanceList:
            issuanceDescription = entry[0]
            if productIssuance == issuanceDescription:
                found = 1
                break
        if found == 0:
            return None
        else:
            return entry

    def issuanceHour_minutesPastHour(self):
        # Minutes past the hour after which "issuanceHour" will jump to the next hour
        # The exception is Hour 23 which will always be truncated i.e. we won't jump
        # to the next day.
        #
        # Default is to truncate the hour so that we always get the hazards
        # reported for that hour.
        return 65

class Issuance:
    def __init__(self, entry, timeRange, expireTime, issueTime, narrativeDef,
                 period1TimeRange, period1LateDayPhrase, period1LateNightPhrase,
                 period1Label, todayFlag):
        self.__entry = entry
        self.__timeRange = timeRange
        self.__expireTime = expireTime
        self.__issueTime = issueTime
        self.__narrativeDef = narrativeDef
        self.__period1TimeRange = period1TimeRange
        self.__period1LateDayPhrase =  period1LateDayPhrase
        self.__period1LateNightPhrase = period1LateNightPhrase
        self.__period1Label = period1Label
        self.__todayFlag = todayFlag
    def entry(self):
        return self.__entry        
    def timeRange(self):
        return self.__timeRange   
    def expireTime(self):
        return self.__expireTime  
    def issueTime(self):
        return self.__issueTime  
    def narrativeDef(self):
        return self.__narrativeDef
    def period1TimeRange(self):
        return self.__period1TimeRange       
    def period1LateDayPhrase(self):
        return self.__period1LateDayPhrase
    def period1LateNightPhrase(self):
        return self.__period1LateNightPhrase
    def period1Label(self):
        return self.__period1Label
    def todayFlag(self):
        return self.__todayFlag

         

