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


#
# iscTime - contains time functions used to determine how a new incoming
# grid should be reconsiled with the an existing grid inventory.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/06/09        1995          bphillip       Initial Creation.
#    
# 
#


# leap year routine
def leapYear(year):
    return (year % 4 == 0 and (year+100) % 400 != 0)

# days in month routine
def daysInMonth(month, year):
    days = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

    if month != 2:
        return days[month-1]

    # special February handling for leap years
    if leapYear(year):
        return days[1]+1
    else:
        return days[1]

# convert to time from components
# 0-year,1-month,2-day,3-hour,4-min,5-sec
def timeFromComponents(timeTuple):
    epochDays = 0
    pyear = 1970
    pmonth = 1  # startTime

    while pyear != timeTuple[0]:
        epochDays = epochDays + 365   # days in year
        if leapYear(pyear):
           epochDays = epochDays + 1    # account for leap year
        pyear = pyear + 1

    while pmonth != timeTuple[1]:
        epochDays = epochDays + daysInMonth(pmonth, timeTuple[0])
        pmonth = pmonth + 1

    epochDays = epochDays + timeTuple[2] - 1;   # but not this day

    epochTime = epochDays*86400 + \
      timeTuple[3]*3600 + timeTuple[4]*60 + timeTuple[5]

    return int(epochTime)


# time range routines
def containsT(tr, t):
    return (t >= tr[0] and t < tr[1])

def overlaps(tr1, tr2):
    if containsT(tr2, tr1[0]) or containsT(tr1, tr2[0]):
        return 1
    return 0

def intersection(tr1, tr2):
    if tr1[0] < tr2[0]:
        startTime = tr2[0]
    else:
        startTime = tr1[0]
    if tr1[1] > tr2[1]:
        endTime = tr2[1]
    else:
        endTime = tr1[1]
    if startTime >= endTime:
        return None   # no intersection
    else:
        return (startTime, endTime)


# sort function for output of mergeTR. Sorts in ascending time order
# based on start time.
def sortFunc(a, b):
    atr = a[1]
    btr = b[1]
    if atr[0] < btr[0]:
        return -1
    elif atr[0] == btr[0]:
        return 0
    else:
        return 1

# grid time comparision, left and partially overlaps
def leftPartialOverlap(baseTR, invTR):
    return invTR[0] < baseTR[0] and invTR[1] > baseTR[0] \
          and invTR[1] < baseTR[1]

# grid time comparision, left and overlaps
def leftOverlap(baseTR, invTR):
    return invTR[0] == baseTR[0] and invTR[1] < baseTR[1]

# grid time comparision, inside grid
def insideOverlap(baseTR, invTR):
    return invTR[0] > baseTR[0] and invTR[1] < baseTR[1]

# grid time comparision, right and overlaps
def rightOverlap(baseTR, invTR):
    return invTR[0] > baseTR[0] and invTR[1] == baseTR[1]

# grid time comparision, right and partially overlaps
def rightPartialOverlap(baseTR, invTR):
    return invTR[0] > baseTR[0] and invTR[0] < baseTR[1] \
          and invTR[1] > baseTR[1]

# grid time comparision, bigger than base grid (spans)
def spans(baseTR, invTR):
    return invTR[0] < baseTR[0] and invTR[1] > baseTR[1]

# grid time comparision, bigger than base grid on left side (spans)
def spansLeft(baseTR, invTR):
    return invTR[0] < baseTR[0] and invTR[1] == baseTR[1]

# grid time comparision, bigger than base grid on right side(spans)
def spansRight(baseTR, invTR):
    return invTR[0] == baseTR[0] and invTR[1] > baseTR[1]

# grid time comparision, exact match
def exactMatch(baseTR, invTR):
    return baseTR == invTR

# mergeTR - takes an incoming time range, the current inventory of
# grids as time ranges, and returns a list of tuples containing the 
# original associated tr (None for new grid), and the new
# time range for the grids, and whether the input grid overlaps.
# NOTE: functions have been inlined for performance
def mergeTR(inputTR, currInv):
    outList = []
    for invTR in currInv:
        # exact match case
        #if exactMatch(inputTR, invTR):
        if inputTR == invTR:
            outList.append((invTR, invTR, 1))

        # grid is to left and partially overlaps
        #elif leftPartialOverlap(inputTR, invTR):
        elif invTR[0] < inputTR[0] and invTR[1] > inputTR[0] \
          and invTR[1] < inputTR[1]:
            outList.append((invTR, (invTR[0], inputTR[0]), 0))
            outList.append((invTR, (inputTR[0], invTR[1]), 1))

        # grid is to left and overlaps
        #elif leftOverlap(inputTR, invTR):
        elif invTR[0] == inputTR[0] and invTR[1] < inputTR[1]:
            outList.append((invTR, (invTR[0], invTR[1]), 1))

        # grid is inside input grid
        #elif insideOverlap(inputTR, invTR):
        elif invTR[0] > inputTR[0] and invTR[1] < inputTR[1]:
            outList.append((invTR, (invTR[0], invTR[1]), 1))

        # grid is to right and overlaps
        #elif rightOverlap(inputTR, invTR):
        elif invTR[0] > inputTR[0] and invTR[1] == inputTR[1]:
            outList.append((invTR, (invTR[0], inputTR[1]), 1))

        # grid is to the right and partially overlaps
        #elif rightPartialOverlap(inputTR, invTR):
        elif invTR[0] > inputTR[0] and invTR[0] < inputTR[1] \
          and invTR[1] > inputTR[1]:
            outList.append((invTR, (invTR[0], inputTR[1]), 1))
            outList.append((invTR, (inputTR[1], invTR[1]), 0))

        # grid is bigger than input grid on both sides
        #elif spans(inputTR, invTR):
        elif invTR[0] < inputTR[0] and invTR[1] > inputTR[1]:
            outList.append((invTR, (invTR[0], inputTR[0]), 0))
            outList.append((invTR, (inputTR[0], inputTR[1]), 1))
            outList.append((invTR, (inputTR[1], invTR[1]), 0))

        # spans left
        #elif spansLeft(inputTR, invTR):
        elif invTR[0] < inputTR[0] and invTR[1] == inputTR[1]:
            outList.append((invTR, (invTR[0], inputTR[0]), 0))
            outList.append((invTR, (inputTR[0], inputTR[1]), 1))
            
        # spans right
        #elif spansRight(inputTR, invTR):
        elif invTR[0] == inputTR[0] and invTR[1] > inputTR[1]:
            outList.append((invTR, (inputTR[0], inputTR[1]), 1))
            outList.append((invTR, (inputTR[1], invTR[1]), 0))
            



    # now determine what parts of inputTR are not covered by any of the
    # splits from the inventory.
    newList = []
    if len(outList) == 0:  #no overlap at all
        newList.append((None, inputTR, 0))
    else:
        modinputTR = inputTR
        for o in outList:   #gTR=grid,modinputTR=input grid
            gTR = o[1]
            if overlaps(inputTR, gTR):
                if gTR[0] != modinputTR[0]:
                    newList.append((None,(modinputTR[0],gTR[0]),0))
                modinputTR = (gTR[1], modinputTR[1])
        if modinputTR[0] != modinputTR[1]:
            newList.append((None, modinputTR, 0))

    # merge the two lists
    for i in newList:
        outList.append(i)
    outList.sort(sortFunc)
    return outList 

