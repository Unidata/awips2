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

import string, time
from com.raytheon.uf.common.status import UFStatus
from com.raytheon.uf.common.status import UFStatus_Priority as Priority


### New class for fetching ETN from TCM header for tropical hazards
class TCMDecoder:
    def __init__(self):
        self.pos = 0
                            # key words in TCM products from NCEP
        self.keyWordDict = {"NATIONAL HURRICANE CENTER" : self.decodeAltFilename,
                            }

        self.fcstList = []  # a place to store all of the forecasts

        self.text = []  #  the text product

        self.currentFcst = {}  # the current forecast we are docoding

        self.baseProductTime = 0

        self.altFilename = ""
        
        self._handler = UFStatus.getHandler("GFE", 'GFE') 

    def stripText(self):
        endStr = chr(13) + chr(13) + chr(10)
        for i in range(len(self.text)):
            self.text[i] = string.replace(self.text[i], endStr, "")
        return

    def getFcstList(self):
        return self.fcstList

    def getBaseProductTime(self):
        return self.baseProductTime

    def getAltInfoFilename(self):
        return self.altFilename

    def currentLine(self):
        return self.text[self.pos]

    def nextLine(self):
        self.pos = self.pos + 1
        if self.pos < len(self.text):
            return self.text[self.pos]
        else:
            return ""

    def monthNum(self, monthStr):
        monthList = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]

        try:
            return monthList.index(monthStr) + 1
        except ValueError:
            return 0

    def convertBaseTime(self, timeStr):
        # timeStr format: "HHMM UTC DAY MON DD YYYY"
        try:
            baseTime = time.strptime(timeStr, "%H%M UTC %a %b %d %Y")
            print "baseTime is", baseTime
            return baseTime
        except ValueError:
            return
            

        # extract time parts from the str
#        strList = string.split(timeStr)
#        if len(strList) != 6:
#            print "Invalid time string:", timeStr
#            print "Format should be of the form HHMM UTC DAY MON DD YYYY"
#            return
#
#        hour = int(timeStr[0:2])
#        minute = int(timeStr[2:4])
#        monthStr = strList[3]
#        month = self.monthNum(monthStr)
#        day = int(strList[4])
#        year = int(strList[5])
#
#        # time.mktime returns time in seconds but in local time
#        baseTime = time.mktime((year, month, day, hour, minute, 0, 0, 0, 0))
#        print "month is", month
#        print "baseTime is", baseTime

##        # Adjust to UTC
##        diffTime = time.mktime(time.gmtime()) - time.mktime(time.localtime())
##        print "diffTime is", diffTime

##        # subtract timeZone and round to the nearest hour
##        roundedTime = int((baseTime - diffTime) / 3600) * 3600
##
#        return baseTime

    def convert_ddhhmm(self, ddhhmmStr, baseTime):

        # remove the slash if present
        ddhhmmStr = string.replace(ddhhmmStr, "/", "")

        if baseTime == 0:
            baseTime = time.time()

        # extract the time parts
        dayStr = ddhhmmStr[0:2]
        hourStr = ddhhmmStr[2:4]
        minStr = ddhhmmStr[4:6]
        day = int(dayStr)
        hour = int(hourStr)
        minute = int(minStr)
        tupleTime = time.gmtime(baseTime)
        year = tupleTime[0]
        month = tupleTime[1]
        # see if we crossed over to a new month
        if tupleTime[2] > day:
            month = month + 1
            if month > 12:
                month = 1
                year = year + 1

        newTuple = (year, month, day, hour, minute, tupleTime[5],
                    tupleTime[6], tupleTime[7], tupleTime[8])

        secondsTime = time.mktime(newTuple)
        # Adjustment to UTC
        diffTime = time.mktime(time.gmtime()) - time.mktime(time.localtime())
        return secondsTime - diffTime  # subtract timeZone

    def decodeProductTime(self):
        # Time of the product found on the next line
        timeStr = self.nextLine()
        print "the time string is:", timeStr 

        # sanity check for the time string
        hhmm = timeStr[0:4]
        for c in hhmm:
            if not c in string.digits:
                return

        baseTime = self.convertBaseTime(timeStr)
        self.baseProductTime = baseTime

        return

    def decodeAltFilename(self):
        nameStr = self.currentLine()
        parts = string.split(nameStr)

        self.altFilename = parts[-1]  # grab the last string token
        return

    def decodeTCMProduct(self, TCMProduct):
        self.text = TCMProduct
        self.pos = 0
        self.fcstList = []
##        self.defaultEyeDiameter = eyeDiameter

        self.stripText()
        while self.pos < len(TCMProduct):
            line = self.currentLine()
            for k in self.keyWordDict.keys():
                if string.find(line, k) > -1:
                    self.keyWordDict[k]()
                    break
            self.pos = self.pos + 1

        # store the last forecast in the list of forecasts
        if self.currentFcst != {}:
            self.fcstList.append(self.currentFcst)
            self.currentFcst = {}  # reset

        return
## End TCM decoder class

### New methods to pull ETN from TCM if grid is not initialized with ETN
def tcmETNforTrop(tcmProduct):
    tcmProd = tcmProduct
#        print "chosen TCM is", tcmProd
    tcmDecoder = TCMDecoder()
    TCMProduct = getTextProductFromDB(tcmProd)
    if len(TCMProduct) < 3:
        msg = tcmProd + " could not be retrieved from the text database."
        statusBarMsg(msg, "S")
        return None   # Just return if no TCM is found.  Something's really wrong
    else:
        tcmDecoder.decodeTCMProduct(TCMProduct)
        altFileName = tcmDecoder.getAltInfoFilename()
        stormNum = altFileName[2:4]
##            print "storm number is", stormNum
        nationalBase = "10"
        tropicalETN = nationalBase + stormNum
##            print "Tropical ETN is: ", tropicalETN
##            print "lenth of tropical ETN is:", len(tropicalETN)
    return tropicalETN

def getTextProductFromDB(productID):
    from com.raytheon.viz.gfe import Activator
    from com.raytheon.viz.gfe.core import DataManager
    from com.raytheon.viz.gfe.product import TextDBUtil
        
    prefStore = Activator.getDefault().getPreferenceStore()
    if prefStore.contains("TestVTECDecode"):
        testVtec = prefStore.getBoolean("TestVTECDecode")
    else:
        testVtec = False
    gfeMode = (DataManager.getCurrentInstance().getOpMode().name() == "OPERATIONAL")
    opMode = testVtec or gfeMode
    fullText = TextDBUtil.retrieveProduct(productID, opMode)
    textList =  fullText.splitlines(True)
    return textList

def statusBarMsg(message, status, category="GFE"):
    from com.raytheon.uf.common.status import UFStatus
    from com.raytheon.uf.common.status import UFStatus_Priority as Priority
    from com.raytheon.viz.gfe import Activator
    from com.raytheon.viz.gfe.constants import StatusConstants
    
    if "A" == status:
        importance = Priority.PROBLEM
    elif "R" == status:
        importance = Priority.EVENTA
    elif "U" == status:
        importance = Priority.CRITICAL
    else:
        importance = Priority.SIGNIFICANT
            
    self._handler.handle(importance,message)