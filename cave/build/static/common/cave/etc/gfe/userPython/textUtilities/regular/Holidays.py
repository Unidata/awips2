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
import time

def MonthStart(monthStartStr):
    # Given a string of the form 'Jan 1 2003', determines the day of the
    # week and the day of the year of the start of the month.

    monthStartTuple = time.strptime(monthStartStr, '%b %d %Y')
    monthStartDOW = int(time.strftime('%u', monthStartTuple))
    monthStartDOY = monthStartTuple[7]
    return monthStartDOW, monthStartDOY

def ThirdMonday(monthStartDOW, monthStartDOY):
    # Determines the day of the year of the two holidays that fall on the
    # third Monday of a month.
    if monthStartDOW == 1:
        holidayDOY = monthStartDOY + 14

    else:
        holidayDOY = monthStartDOY + (8 - monthStartDOW) + 14

    return holidayDOY

def calculateHolidayList(yearStr):

    # Define holiday strings
    NewYearsDay = 'NEW YEARS DAY'
    MartinLutherDay = 'MARTIN LUTHER KING JR DAY'
    WashingtonsBirthday = 'WASHINGTONS BIRTHDAY'
    MemorialDay = 'MEMORIAL DAY'
    IndependenceDay = 'INDEPENDENCE DAY'
    LaborDay = 'LABOR DAY'
    ColumbusDay = 'COLUMBUS DAY'
    VeteransDay = 'VETERANS DAY'
    ThanksgivingDay = 'THANKSGIVING DAY'
    ChristmasDay = 'CHRISTMAS DAY'

    # Initialize Holidays list
    Holidays = []

    # Add New Years Day, Independence Day, Veterans Day, and Christmas Day.
    Holidays.append( ( 1, 1, NewYearsDay ) )
    Holidays.append( ( 7, 4, IndependenceDay ) )
    Holidays.append( ( 11, 11, VeteransDay ) )
    Holidays.append( ( 12, 25, ChristmasDay ) )

#    yearStr = time.strftime( '%Y' )

    # Calculate Martin Luther King Jr Day, the 3rd Monday in January.
    monthStartDOW, monthStartDOY = MonthStart( 'Jan 1 %s' % yearStr )
    holidayDOY = ThirdMonday( monthStartDOW, monthStartDOY )
    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 1, holidayTuple[2], MartinLutherDay ) )

    # Calculate Washington's Birthday, the third Monday in February.
    monthStartDOW, monthStartDOY = MonthStart( 'Feb 1 %s' % yearStr )
    holidayDOY = ThirdMonday(monthStartDOW, monthStartDOY)
    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 2, holidayTuple[2], WashingtonsBirthday ) )

    # Calculate Memorial Day, the last Monday in May.
    monthStartDOW, monthStartDOY = MonthStart( 'May 1 %s' % yearStr )

    if monthStartDOW == 1:
        holidayDOY = monthStartDOY + 28

    elif monthStartDOW == 6 or monthStartDOW == 7:
        holidayDOY = monthStartDOY + ( 8 - monthStartDOW ) + 28

    else:
        holidayDOY = monthStartDOY + ( 8 - monthStartDOW ) + 21

    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 5, holidayTuple[2], MemorialDay ) )

    # Calculate Labor Day, 1st Monday in September.
    monthStartDOW, monthStartDOY = MonthStart( 'Sep 1 %s' % yearStr )

    if monthStartDOW == 1:
        holidayDOY = monthStartDOY

    else:
        holidayDOY = monthStartDOY + ( 8 - monthStartDOW )

    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 9, holidayTuple[2], LaborDay ) )

    # Calculate Columbus Day, 2nd Monday in October.
    monthStartDOW, monthStartDOY = MonthStart( 'Oct 1 %s' % yearStr )

    if monthStartDOW == 1:
        holidayDOY = monthStartDOY + 7

    else:
        holidayDOY = monthStartDOY + ( 8 - monthStartDOW ) + 7

    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 10, holidayTuple[2], ColumbusDay ) )


    # Calculate Thanksgiving Day, 4th Thursday of November.

    monthStartDOW, monthStartDOY = MonthStart('Nov 1 %s' % yearStr)
    if monthStartDOW >=1 and monthStartDOW <=4:
        holidayDOY = monthStartDOY + ( 4 - monthStartDOW ) + 21

    else:
        holidayDOY = monthStartDOY + 28 - ( monthStartDOW - 4 )

    holidayTuple = time.strptime( '%d %s' % ( holidayDOY, yearStr ), '%j %Y' )
    Holidays.append( ( 11, holidayTuple[2], ThanksgivingDay ) )

    Holidays.sort()
    return Holidays

# static member that will serve as a cache
_holidayList = []

# Given a timeRange, this method will return a holiday label that
# corresponds to the startTime of the specified timeRange.
def getHolidayLabel(startTime):
    global _holidayList  # make the holiday list global for caching
    # if the holiday list has not yet been made, make it.
    if _holidayList == []:
        year = startTime.year
        yearStr = str(year)
        _holidayList = calculateHolidayList(yearStr)

    # get the day and month and see if today is a holiday
    trDay = startTime.day
    trMonth = startTime.month
    for month, day, label in _holidayList:
        if month == trMonth and day == trDay:
            return label

    # If we made to here, it's not a holiday so return empty string
    return ""
