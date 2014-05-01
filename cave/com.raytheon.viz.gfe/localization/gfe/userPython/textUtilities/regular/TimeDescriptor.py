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
# TimeDescriptor.py
# Methods for producing time descriptors.
#
# Author: hansen
# ----------------------------------------------------------------------------

import types
import time, string
import TimeRangeUtils
import Interfaces
import Holidays
import AbsTime
import TimeRange

class TimeDescriptor(TimeRangeUtils.TimeRangeUtils, Interfaces.Interfaces):
    def __init__(self):
        Interfaces.Interfaces.__init__(self)
        TimeRangeUtils.TimeRangeUtils.__init__(self)

##    def getHolidayLabel(self, tr):
##        return "HOLIDAY"

    def getCurrentTime(self, argDict=None, format="%I%M %p %Z %a %b %d %Y",
                        shiftToLocal=1, upperCase=0, stripLeading=1):
        # Return a text string of the current time in the given format
        if argDict is not None and argDict.has_key("creationTime"):
            ctime = argDict['creationTime']
        else:
            ctime = time.time()
        if shiftToLocal == 1:
            curTime = time.localtime(ctime)
        else:
            curTime = time.gmtime(ctime)
            localTime = time.localtime(ctime)
            zoneName = time.strftime("%Z",localTime)
        timeStr = time.strftime(format, curTime)
        if shiftToLocal == 0:
            timeStr = string.replace(timeStr, zoneName, "GMT")
        if stripLeading==1 and (timeStr[0] == "0" or timeStr[0] == " "):
            timeStr = timeStr[1:]
        if argDict is None:
            language = "english"
        else:
            language = argDict["language"]
        timeStr = self.translateExpr(timeStr, language)
        if upperCase == 1:
            timeStr = string.upper(timeStr)
        timeStr = string.replace(timeStr, "  ", " ")
        return timeStr

    def getIssueTime(self, argDict, upperCase=0):
        # Return the issue time formatted and translated
        # issueRange is set up as the first time range of the
        # product.
        timeRange = argDict["issueRange"]
        timeStr = timeRange.startTime().string()
        timeStr = self.translateExpr(timeStr,argDict["language"])
        if upperCase == 1:
            timeStr = string.upper(timeStr)
        return timeStr

    def getWeekday(self, timeRange, holidays=0, shiftToLocal=0,
                    labelType="Worded", today=0, tomorrow=0, holidayModule="Holidays",
                   nextDay24HourLabel=0, splitDay24HourLabel=0):
        # Check for long time ranges
        if timeRange.duration() > 24 * 3600:
            # If splitDay24HourLabel is not set, we need to make the
            # timeRanges 24 hours long so that getWeekday_descriptor
            # will honor the splitDay24HourLabel setting and leave
            # weekdays as simply the weekday name e.g.
            #  SATURDAY instead of SATURDAY AND SATURDAY NIGHT
            if splitDay24HourLabel:
                # Make 12-hour start and end timeRanges
                durHours1 = 12
                durHours2 = 0
            else:
                # Make 24-hour start and end timeRanges
                durHours1 = 24
                durHours2 = 12
            startTR = TimeRange.TimeRange(timeRange.startTime(),
                                     timeRange.startTime() + (durHours1 * 3600))
            endTR = TimeRange.TimeRange(timeRange.endTime() - (12 * 3600),
                                   timeRange.endTime() + (durHours2*3600))
            startDay = self.getWeekday_descriptor(startTR, holidays, shiftToLocal,
                    "Combo", today, tomorrow, holidayModule, nextDay24HourLabel)
            endDay = self.getWeekday_descriptor(endTR, holidays, shiftToLocal,
                    "Capital", today, tomorrow, holidayModule, nextDay24HourLabel)
            return startDay + " through " + endDay
        else:  # do the normal thing
            return self.getWeekday_descriptor(timeRange, holidays, shiftToLocal,
                                              labelType, today, tomorrow, holidayModule,
                                              nextDay24HourLabel, splitDay24HourLabel)
        
    def getWeekday_descriptor(self, timeRange, holidays=0, shiftToLocal=0,
                              labelType="Worded", today=0, tomorrow=0,
                              holidayModule="Holidays", nextDay24HourLabel=0,
                              splitDay24HourLabel=0):
        # Return a weekday text string
        # Arguments:
        #  timeRange
        #  holidays : if 1, the holiday file will be consulted
        #  shiftToLocal : if 1, will shift the given time range to local time
        #  labelType : See Labels dictionary below for types
        #  today : if 1, Today, Tonight,
        #    will be returned instead of corresponding weekday name
        #  tomorrow: if 1, Tomorrow, Tomorrow Night
        #    will be returned instead of corresponding weekday name
        #  holidayModule: file containing holiday dates
        #  nextDay24HourLabel: if 1, a 24-hour time period starting
        #    after 1600, will be labeled as the next day.
        #    This is to accomodate 24 extended periods that go from
        #    6pm-6pm.
        #  splitDay24HourLabel: if 0, a 24-hour period will be labeled with
        #    simply the weekday name (e.g. SATURDAY)
        #    instead of including the day and night periods
        #    (e.g. SATURDAY AND SATURDAY NIGHT)
        #
        # If the time range is for today AND is less than 12 hours,
        # we must accurately describe the period.
        # At some point, this may need to be coordinated with TextRules
        # timePeriod descriptors.

        currentLocalTime, shift = self.determineTimeShift()
        if shiftToLocal == 1:
            timeRange = TimeRange.TimeRange(timeRange.startTime() + shift,
                                        timeRange.endTime() + shift)
        labels = self.Labels()[labelType]
        pre = labels["PrePunctuation"]
        post = labels["PostPunctuation"]
        todayFlag = currentLocalTime.day == timeRange.startTime().day     
        try:
            if self._productIssuance == "Next Day":                       
                todayFlag = currentLocalTime.day + 1 == timeRange.startTime().day
        except:
            pass
        startHour = timeRange.startTime().hour
        dayNight = self.getPeriod(timeRange)
        durationHours = timeRange.duration() / 3600
        if nextDay24HourLabel:
            nextDay24Hour = durationHours >= 24 and timeRange.startTime().hour > 16
        else:
            nextDay24Hour = 0
        splitDay24Hour = splitDay24HourLabel and durationHours == 24
        # Do not say "Night" if:
        #    startHour is between midnight self.DAY (e.g. 6 am)
        nightTimeFlag = durationHours <= 12 and dayNight == self.NIGHTTIME() \
                            and startHour > self.DAY()
         
        # Check for holiday
        if not splitDay24Hour and not todayFlag and holidays == 1 and \
           (dayNight == self.DAYTIME() or dayNight == self.DAYNIGHT()):

            if nextDay24Hour == 1:
                label = Holidays.getHolidayLabel(timeRange.endTime())
            else:
                label = Holidays.getHolidayLabel(timeRange.startTime())

            if label != "":
                return pre + label + post

        # Check for today or tonight
        if today == 1:
            if todayFlag:
                if dayNight == self.DAYNIGHT():
                    # Key off of end time
                    keyRange = TimeRange.TimeRange(
                        timeRange.endTime()-3600, timeRange.endTime())
                    dayNight = self.getPeriod(keyRange)
                #if durationHours == 1:
                #    label =  labels["Now"]
                if durationHours < 12 and durationHours > 1:
                    if dayNight == self.DAYTIME():
                        label = labels["Rest of Today"]
                    else:
                        label = labels["Rest of Tonight"]
                elif dayNight == self.NIGHTTIME():
                    label = labels["Tonight"]
                else:
                    label = labels["Today"]
                return pre + label + post

        # Check for tomorrow or tomorrow night
        if tomorrow == 1:
            startTime = timeRange.startTime() - 24*3600
            if startTime.day == currentLocalTime.day:
                if durationHours == 1:
                    label = timeRange.startTime().string() + ": "
                elif nightTimeFlag:
                    label = labels["Tomorrow"] + " " + labels["Night"]
                else:
                    label = labels["Tomorrow"]
                try:
                    if self._productIssuance == "Next Day":       
                        label = "TONIGHT"
                except:
                    pass
                return pre + label + post

        # Week day
        weekdayName = labels["Weekday"][timeRange.startTime().weekday()]
        if durationHours == 1:
            label = self.timeDisplay(timeRange, "Zulu", "", "", "%I %p")
            if label[0] == "0":
                label = label[1:]
            label = label + " " + weekdayName
        # Check for Night and Evening
        elif nightTimeFlag:
            if durationHours <= 6:
                label = weekdayName + " " + labels["Evening"]
            else:
                label = weekdayName + " " + labels["Night"]
        elif nextDay24Hour == 1:
            # If we have a 24 hour time period starting late in the day,
            # use the next day as the label.
            label = labels["Weekday"][timeRange.endTime().weekday()]
        elif splitDay24Hour:
            # See if we are starting with a night or day period
            weekNight = weekdayName + " " + labels["Night"]
            if startHour < self.NIGHT():
                # Monday and Monday Night OR
                # Labor Day and Monday Night
                weekDayHoliday = self.getHolidayLabel(timeRange.startTime(), holidays)
                if weekDayHoliday != "":
                    weekdayName = weekDayHoliday
                label = weekdayName + " and " + weekNight
            else:
                # Sunday Night and Monday OR
                # Sunday Night and Labor Day
                nextWeekdayName = self.getHolidayLabel(timeRange.endTime(), holidays)
                if nextWeekdayName == "":
                    nextWeekdayName = labels["Weekday"][timeRange.endTime().weekday()]
                label = weekNight + " and " + nextWeekdayName                  
        else:
            label = weekdayName
        # Check for Evening

        return pre + label + post        

    def getHolidayLabel(self, absTime, holidays):
        if holidays:
            return Holidays.getHolidayLabel(absTime)
        else:
            return ""

    def getPeriodLabel(self, range, currentLocalTime, shift, labelType,
                        holidays=0):
        # Do a worded label, e.g. Now, Today, Tonight, Tuesday... etc.
        # Compare range (in GMT time) to current local time

        label = self.getWeekday(range, holidays, 1, labelType)
        return label

    def getLocalWeekday(self, timeRange):
        return self.getWeekday(timeRange, holidays=1, shiftToLocal=1,
                                labelType="Capital", today=1, tomorrow=0)
    
    def getLocalWeekdayName(self, timeRange):
        return self.getWeekday(timeRange, holidays=1, shiftToLocal=1,
                                labelType="Capital")

    def Labels(self):
        return {
            "SimpleWorded": {
                "PrePunctuation": "",
                "PostPunctuation": "",
                "Weekday" : {
                    6 : "Sunday",
                    0 : "Monday",
                    1 : "Tuesday",
                    2 : "Wednesday",
                    3 : "Thursday",
                    4 : "Friday",
                    5 : "Saturday"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
            },
            "Worded": {
                "PrePunctuation": "",
                "PostPunctuation": ": ",
                "Weekday" : {
                    6 : "Sunday",
                    0 : "Monday",
                    1 : "Tuesday",
                    2 : "Wednesday",
                    3 : "Thursday",
                    4 : "Friday",
                    5 : "Saturday"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
            },
            "Capital": {
                "PrePunctuation": "",
                "PostPunctuation": "...",
                "Weekday" : {
                    6 : "Sunday",
                    0 : "Monday",
                    1 : "Tuesday",
                    2 : "Wednesday",
                    3 : "Thursday",
                    4 : "Friday",
                    5 : "Saturday"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
            },
            "CapitalWithPeriod": {
                "PrePunctuation": ".",
                "PostPunctuation": "...",
                "Weekday" : {
                    6 : "Sunday",
                    0 : "Monday",
                    1 : "Tuesday",
                    2 : "Wednesday",
                    3 : "Thursday",
                    4 : "Friday",
                    5 : "Saturday"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
            },
            "Abbreviated": {
                "PrePunctuation": "",
                "PostPunctuation": "",
                "Weekday" : {
                    6 : "Sun",
                    0 : "Mon",
                    1 : "Tue",
                    2 : "Wed",
                    3 : "Thu",
                    4 : "Fri",
                    5 : "Sat"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
            },
            "CapsAbbreviated": {
                "PrePunctuation": "",
                "PostPunctuation": "",
                "Weekday" : {
                    6 : "SUN",
                    0 : "MON",
                    1 : "TUE",
                    2 : "WED",
                    3 : "THU",
                    4 : "FRI",
                    5 : "SAT"
                },
                "Now": "NOW",
                "Today":"TODAY",
                "Tonight": "TONIGHT",
                "Rest of Today":"REST OF TODAY",
                "Rest of Tonight": "REST OF TONIGHT",
                "Night": "NIGHT",
                "Evening": "EVENING",
                "Afternoon": "THIS AFTERNOON",
            },
            "Combo": {
                "PrePunctuation": ".",
                "PostPunctuation": "",
                "Weekday" : {
                    6 : "Sunday",
                    0 : "Monday",
                    1 : "Tuesday",
                    2 : "Wednesday",
                    3 : "Thursday",
                    4 : "Friday",
                    5 : "Saturday"
                },
                "Now": "Now",
                "Today":"Today",
                "Tonight": "Tonight",
                "Rest of Today":"Rest of Today",
                "Rest of Tonight": "Rest of Tonight",
                "Night": "Night",
                "Evening": "Evening",
                "Afternoon": "This Afternoon",
                }
        }

    def timePeriod_descriptor_list(self, tree, node):
        # Contains definition for localtime start/end times and phrase
        # Tuples, 0=startHrLT, 1=endHrLT, 2=phrase
        day = self.DAY()
        return [
                (day, (day+3)%24, "early in the morning"),    # 6a-9a
                (day, (day+6)%24, "in the morning"),          # 6a-noon
                (day, (day+9)%24, "until late afternoon"),    # 6a-3p
                (day, (day+12)%24, ""),                       # 6a-6p
                (day, (day+15)%24, "until early evening"),    # 6a-9p
                (day, (day+18)%24, "through the evening"),    # 6a-midnite

                ((day+2)%24, (day+3)%24, "early in the morning"),  # 8a-9a

                ((day+3)%24, (day+6)%24, "late in the morning"), # 9a-noon
                ((day+3)%24, (day+9)%24, "in the late morning and early afternoon"), # 9a-3p
                ((day+3)%24, (day+12)%24, "in the late morning and afternoon"),      # 9a-6p
                ((day+3)%24, (day+15)%24, "until early evening"),      # 9a-9p
                ((day+3)%24, (day+18)%24, "through the evening"),      # 9a-midnite

                ((day+5)%24, (day+6)%24, "late in the morning"),      # 11a-noon

                ((day+6)%24, (day+9)%24,  "early in the afternoon"),      # noon-3p
                ((day+6)%24, (day+12)%24, "in the afternoon"),            # noon-6p
                ((day+6)%24, (day+15)%24, "in the afternoon and evening"),# noon-9p
                ((day+6)%24, (day+18)%24, "in the afternoon and evening"),# noon-midnite

                ((day+8)%24, (day+9)%24, "early in the afternoon"),      # 2pm-3pm

                ((day+9)%24, (day+12)%24, self.lateDay_descriptor),   # 3p-6p
                ((day+9)%24, (day+15)%24, "early in the evening"),    # 3p-9p
                ((day+9)%24, (day+18)%24, "in the evening"),          # 3p-midnite
                ((day+9)%24, (day+21)%24, "until early morning"),     # 3p-3a
                ((day+9)%24,  day, ""),                               # 3p-6a

                ((day+11)%24, (day+12)%24, self.lateDay_descriptor), # 5p-6p

                ((day+12)%24, (day+15)%24, "early in the evening"),   # 6p-9p
                ((day+12)%24, (day+18)%24, "in the evening"),         # 6p-midnite
                ((day+12)%24, (day+21)%24, "until early morning"),    # 6p-3a
                ((day+12)%24, day, ""),                               # 6p-6a

                ((day+14)%24, (day+15)%24, "early in the evening"), # 8p-9p

                ((day+15)%24, (day+18)%24, "late in the evening"),                  # 9p-midnite
                ((day+15)%24, (day+21)%24, "in the late evening and early morning"),# 9p-3a
                ((day+15)%24, day, "in the late evening and overnight"),            # 9p-6a

                ((day+17)%24, (day+18)%24, "late in the evening"), # 11p-midnight

                ((day+18)%24, (day+21)%24, "after midnight"),               # midnite-3a
                ((day+18)%24, day, "after midnight"),                       # midnite-6a
                ((day+18)%24, (day+6)%24, ""),                              # midnite-noon

                ((day+20)%24, (day+21)%24, "after midnight"), # 2a-3a

                ((day+21)%24, day, self.lateNight_descriptor),              # 3a-6a
                ((day+21)%24, (day+3)%24, "early in the morning"),          # 3a-9a
                ((day+21)%24, (day+6)%24, "early in the morning"),          # 3a-noon
                ((day+21)%24, (day+9)%24, "until afternoon"),               # 3a-3p
                ((day+21)%24, (day+12)%24, ""),                             # 3a-6p

                ((day+23)%24, (day)%24, self.lateNight_descriptor), # 5a-6a

                ]

    def lateDay_descriptor(self, tree, node, timeRange):
        return "late in the afternoon"

    def lateNight_descriptor(self, tree, node, timeRange):
        return "early in the morning"

    def include_timePeriod_descriptor_flag(
        self, tree, node, statsByRange, index, element):
        # For "range" phrases, tells whether or not to include the time_descriptor
        #  i.e "North winds 10-15 knots becoming 20-30 knots in the afternoon."
        # "index" tells which sub-phrase of the phrase we're qualifying, and the default
        # is to qualify every other sub-phrase.
        #
        # Default only applies to Wind and reports "every other" sub-phrase
        if element != "Wind":
            return 1
        flag = 0
        odd = len(statsByRange)%2
        # If there is an odd number of sub-phrases,
        # report on 1st sub-phrase and then every other one
        if odd == 1:
            if index%2 == 0:
                flag = 1
        # If there is an even number of sub-phrases,
        # report on 2nd sub-phrase and then every other one
        elif index%2 == 1:
            flag = 1
        return flag

    # Time period Table Access
    def timePeriod_descriptor(self, tree, node, timeRange):
        # Returns a descriptor phrase for the time range.
        # Assumes the timeRange is in GMT and converts it to Local time.

        # more than 12 hours, return empty string
        if timeRange.duration() > 12*3600:
            return ""
        if timeRange == node.getTimeRange():
            return ""

        # determine the local time
        localTime, shift = self.determineTimeShift()
        periodStart = timeRange.startTime() + shift
        periodEnd = timeRange.endTime() + shift
        startHour = periodStart.hour
        endHour = periodEnd.hour

        # get the table
        table = self.timePeriod_descriptor_list(tree, node)

        # look for the best match entry, start with the startTime match
        bestIndexes = []
        bestTime = 9999
        for i in xrange(len(table)):
            diff = self.hourDiff(startHour, table[i][0])
            if diff < bestTime:
                bestTime = diff
        for i in xrange(len(table)):
            diff = self.hourDiff(startHour, table[i][0])
            if diff == bestTime:
               bestIndexes.append(table[i])

        # if nothing found, return "" string
        if len(bestIndexes) == 0:
            return ""

        # now find the best match for the ending time, from the ones earlier
        bestTime = 9999
        returnValue = ''
        for i in xrange(len(bestIndexes)):
            diff = self.hourDiff(endHour, bestIndexes[i][1])
            if diff < bestTime:
                returnValue = bestIndexes[i][2]
                bestTime = diff

        if type(returnValue) is types.MethodType:
            return returnValue(tree, node, timeRange)
        else:
            return returnValue

    def hourDiff(self, h1, h2):
        # returns the number of hours difference
        h = abs(h1-h2)
        if h > 12:
            h = 24 - h
        return h

    def dayDict(self):
        return {0:"SUN",1:"MON",2:"TUE",3:"WED",4:"THU",5:"FRI",6:"SAT"}
    def monthDict(self):
        return {1:"JAN",2:"FEB",3:"MAR",4:"APR",5:"MAY",6:"JUN",
                          7:"JUL", 8:"AUG",9:"SEP",10:"OCT",11:"NOV",12:"DEC"}
    def monthDict1(self):
        return {1:"Jan",2:"Feb",3:"Mar",4:"Apr",5:"May",6:"Jun",7:"Jul",
                8:"Aug",9:"Sep",10:"Oct",11:"Nov",12:"Dec"}

    #********************
    # Methods for formatting TimeRange labels

    def timeRangeLabel(self, timeRange):
        # Return the label for the given time range
        start = timeRange.startTime()
        end = timeRange.endTime()
        return start.string() + " - " + end.string()

    def hourAmPm(self, hour):
        # Given a military time hour, return
        # Non-military time plus AM or PM
        if hour == 0:
            hour = 12
            ampm = " AM"
        elif hour == 12:
            ampm = " PM"
        elif hour < 12:
            ampm = " AM"
        else:
            hour = hour - 12
            ampm = " PM"
        return hour, ampm

    def periodLabel(self, timeRange):
        # Return the label for the given time range
        start = timeRange.startTime()
        hourStr = `start.hour`+"Z/"+`start.day`
        return hourStr

    def localTimeRangeLabel(self, timeRange):
        # Return the label for the given time range
        localTime, shift = self.determineTimeShift()
        localStart = timeRange.startTime() + shift
        localEnd = timeRange.endTime() + shift
        localRange = TimeRange.TimeRange(localStart, localEnd)

        # Determine name of time zone (e.g. MST,  MDT...)
        localTime = time.localtime(time.time())
        zoneName = time.strftime("%Z",localTime)

        # Create label
        localStr = self.timeRangeLabel(localRange)
        localStr = string.replace(localStr, "GMT", zoneName)
        return localStr

    def localOneHourTRLabel(self, timeRange):
        # Return label in form: Month Day, Year hour - hour
        # E.g.  Sept 30, 2000 1 AM - 2 AM
        localTime, shift = self.determineTimeShift()
        start = timeRange.startTime() + shift
        localTime = time.localtime(time.time())
        zoneName = time.strftime("%Z",localTime)
        monthDict = self.monthDict()
        str =  monthDict[start.month] + " " + `start.day` + ", " + `start.year`
        hour1, ampm1 = self.hourAmPm(start.hour)
        hour2, ampm2 = self.hourAmPm(start.hour+1)
        return str + " " + `hour1` + ampm1 + " " + zoneName + " - " + \
            `hour2` + ampm2 + " " + zoneName

    def localTRLabel(self, timeRange):
        # Return label in form
        # E.g.  Sept 30, 1 AM MST TO Oct 1, 2 AM MST
        localTime, shift = self.determineTimeShift()
        start = timeRange.startTime() + shift
        end = timeRange.endTime() + shift
        localTime = time.localtime(time.time())
        zoneName = time.strftime("%Z",localTime)
        hour1, ampm1 = self.hourAmPm(start.hour)
        hour2, ampm2 = self.hourAmPm(end.hour)
        monthDict1 = self.monthDict1()
        str1 =  `hour1` + ampm1 + " " + zoneName + " " + monthDict1[start.month] + " " + `start.day`
        str2 =  `hour2` + ampm2 + " " + zoneName + " " + monthDict1[end.month] + " " + `end.day`
        return str1 + " TO " + str2

    def localPeriodLabel(self, timeRange):
        # Return the label for the given time range

        # Adjust time from GMT to local
        localTime, shift = self.determineTimeShift()
        start = timeRange.startTime()  +  shift

        # Determine name of time zone (e.g. MST,  MDT...)
        localTime = time.localtime(time.time())
        zoneName = time.strftime("%Z",localTime)

        # Create label
        hourStr = `start.hour`+ " " + zoneName + "/"+`start.day`
        return hourStr

    def localTimeLabel(self, timeRange):
        # Return a label of length 6 the form: hour AM/PM
        # E.g. 5 PM
        return self.localHourLabel(timeRange.startTime(),6)

    def localRangeLabel(self, timeRange):
        # Return label of range: 5AM-6PM
        label1 = self.localHourLabel(timeRange.startTime(),4)
        label2 = self.localHourLabel(timeRange.endTime(),4)
        str = label1 + "-" + label2
        return string.replace(str," ","")

    def localHourLabel(self, absTime, length=6):
        # Convert from gmt to local time
        localTime, shift = self.determineTimeShift()
        start = absTime + shift
        hour = start.hour
        hour, ampm = self.hourAmPm(hour)
        label = `hour` + ampm
        return string.rjust(label,length)

    def strToGMT(self, timeStr):
        # Convert a time str in the form of local time
        # hours and minutes to a GMT AbsTime for the current day.
        # The time string must be 6 or 7 characters long
        # E.g.  "1030 AM"
        #       "800 PM"

        # Get the hour and minutes
        length = len(timeStr)
        amPm = timeStr[length-2:length]
        minutes = timeStr[length-5:length-3]
        minutes = int(minutes)
        if length > 6:
            hour = timeStr[0:2]
        else:
            hour = timeStr[0]
        hour = int(hour)
        if hour == 12:
            if amPm == "AM":
                hour = 0
        else:
            if amPm == "PM":
                hour = hour + 12
        # Make a local AbsTime
        curLocalTime, shift = self.determineTimeShift()
        newTime = AbsTime.absTimeYMD(curLocalTime.year, curLocalTime.month,
                                  curLocalTime.day, hour, minutes)
        # Convert to GMT
        return newTime - shift
