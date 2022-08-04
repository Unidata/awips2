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
# This program decodes a product's UGC and VTEC strings
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer       Description
# ------------- -------- -------------- --------------------------
#                                       Initial creation
# Feb 19, 2013  1636     rferrel        Use TimeTools to get file timestamp.
# May 07, 2013  1973     rferrel        Adjust Issue and Purge times to be
#                                       relative to start time.
# Jun 24, 2013  16317    D. Friedman    If no storm line, parse storm motion
#                                       from event text.
# Aug 21, 2013  16501    mgamazaychikov Adjusted calculation of Purge time in
#                                       NoVTECWarningDecoder.
# Sep 12, 2013  2249     rferrel        When incoming file from warngen adjust
#                                       start time from file's timestamp.
# Oct 03, 2013  2402     bsteffen       Make PythonDecoder more extendable.
# May 15, 2014  2536     bclement       moved WMO time parsing to WMOTimeParser
# May 15, 2014  3157     dgilling       Update location of WclInfo class.
# Jun 10, 2014  3268     dgilling       Update location of WclInfo class.
# Dec 17, 2014  4953     randerso       Fixed decoding of non-VTEC from command line
# May 14, 2015  4027     randerso       Changed regex used to decode date line to accept mixed case
# Mar 24, 2015  4320     dgilling       Fix NullPointerException in StdWarningDecoder.__init__()
#                                       when decoding product not from a file.
# Mar 26, 2015  4324     dgilling       Improve handling of all 0s time values in HVTEC strings.
# Sep 23, 2015  4848     nabowle        Handle UGC-like lines in the text segment.
# Nov 10, 2015  17068    ryu            Improve handling of lines starting with a UGC code
#                                       but do not really begin new segments
# Jul 18, 2016  5749     randerso       Remove replacement of commas with ellipses in segment text
# Oct 10, 2017  6328     randerso       Fix ugclist in non-vtec products, code cleanup
# Apr 10, 2018  20632    ryu            Fix RE for the MND date line to match
#                                       a mixed case timezone designation (ChST).
# Jul  5, 2019  7879     tgurney        Python 3 fixes
# Jan 09, 2020  7997     rblum          Added FA.A and FL.Y to PHENSIGS_IGNORE_HVTEC.
# Feb 27, 2020  21887    bstough        Modified check for DTG in UGC to account for DTG by itself
#                                        on the last line of the UGC
# Jun 17, 2020  20996    smoorthy       Fixed polygon processing in flood warning products
#
# @author rferrel
##



import calendar
import sys, os, time, re, getopt

from ufpy import TimeUtil

from com.raytheon.uf.common.wmo import WMOTimeParser
from com.raytheon.uf.edex.decodertools.time import TimeTools
import LogStream


ACCURATE_CITIES_PILS = ['CFW', 'FFA', 'NPW', 'RFW', 'WSW']

REGIONS = {
             "AK":"ALASKA", "AL":"SOUTHERN", "AR":"SOUTHERN",
             "AS":"PACIFIC", "AZ":"WESTERN", "CA":"WESTERN",
             "CO":"CENTRAL", "CT":"EASTERN", "DC":"EASTERN",
             "DE":"EASTERN", "FL":"SOUTHERN", "GA":"SOUTHERN",
             "GU":"PACIFIC", "HI":"PACIFIC", "IA":"CENTRAL",
             "ID":"WESTERN", "IL":"CENTRAL", "IN":"CENTRAL",
             "KS":"CENTRAL", "KY":"CENTRAL", "LA":"SOUTHERN",
             "MA":"EASTERN", "MD":"EASTERN", "ME":"EASTERN",
             "MI":"CENTRAL", "MN":"CENTRAL", "MO":"CENTRAL",
             "MS":"SOUTHERN", "MT":"WESTERN", "NC":"EASTERN",
             "ND":"CENTRAL", "NE":"CENTRAL", "NH":"EASTERN",
             "NJ":"EASTERN", "NM":"SOUTHERN", "NV":"WESTERN",
             "NY":"EASTERN", "OH":"EASTERN", "OK":"SOUTHERN",
             "OR":"WESTERN", "PA":"EASTERN", "PR":"SOUTHERN",
             "RI":"EASTERN", "SC":"EASTERN", "SD":"CENTRAL",
             "TN":"SOUTHERN", "TX": "SOUTHERN", "UT":"WESTERN",
             "VA":"EASTERN", "VI":"SOUTHERN", "VT":"EASTERN",
             "WA":"WESTERN", "WI":"CENTRAL", "WV":"EASTERN",
             "WY":"CENTRAL", "GM":"SOUTHERN" }

BEARINGS = {
            "NORTH": 0, "NORTHEAST": 45, "EAST": 90, "SOUTHEAST": 135,
            "SOUTH": 180, "SOUTHWEST": 225, "WEST": 270, "NORTWEST": 315 }

SPEED_CONVERSION = {
                    "MPH": 0.86898,
                    "KNOTS": 1,
                    "KTS": 1,
                    "KPH": 0.53996
                    }

PHENSIGS_IGNORE_HVTEC = ["FL.A", "FF.A", "FA.Y", "FA.W", "FF.W", "FA.A", "FL.Y"]


class StdWarningDecoder():

    def __init__(self, text=None, filePath=None, command=None):
        self._hasDTG = None
        self._officeFromWMO = None

        #to ensure time calls are based on Zulu
        os.environ['TZ'] = "GMT0"

        self._deleteAfterProcessing = 0
        if filePath is None:
            self._incomingFilename = None
        else:
            self._incomingFilename = filePath
        self._notifyGFE = True
        self._makeBackups = 1  #make backups after each "update"
        self._wmoid = None

        self._command = command
        self._timeOffset = 0

        #decode the command line
        if command is not None:
            self._decodeCommandLine()
            self._rawMessage = None
            checkForWmo = True
        else:
            self._rawMessage = text
            checkForWmo = True

        # default time is just present time
        self._time = time.time() + self._timeOffset

        if self._incomingFilename:
            # base time for decoder
            warningTimestamp = TimeTools.getWarningTimestamp(self._incomingFilename)
            if warningTimestamp is not None:
                self._time = int(warningTimestamp)
            else:
                if WMOTimeParser.allowArchive():
                    try:
                        yyyymmddhh = WMOTimeParser.getTimestamp(self._incomingFilename)
                        if len(yyyymmddhh) < 10:
                            timeTuple = time.strptime(yyyymmddhh, "%Y%m%d")
                        else :
                            timeTuple = time.strptime(yyyymmddhh, "%Y%m%d%H")
                        self._time = time.mktime(timeTuple)
                    except :
                        LogStream.logProblem('Unable to get timestamp from filename: "%s"' % (self._incomingFilename))

        os.umask(0)  #ensure proper permissions

        self._mappedPils = {}

        #get product
        self._rawMessage, self._lines = self._getProduct(text, checkForWmo)
        if len(self._lines) < 2:
            raise Exception("Empty Product -- ABORTING")

        #Delete incoming product if requested.
        if self._deleteAfterProcessing:
            os.remove(self._incomingFilename)

        #set up vtec regular expressions
        self._vtecRE = r'/[OTEX]\.([A-Z]{3})\.([A-Z]{4})\.([A-Z]{2})\.' + \
          r'([WAYSOFN])\.([0-9]{4})\.([0-9]{6})T([0-9]{4})Z\-' + \
          r'([0-9]{6})T([0-9]{4})Z/'
        self._hVtecRE = r'/([0-9A-Z]{5})\.([0-3NU])\.([A-Z]{2})\.' + \
          r'([0-9]{6})T([0-9]{4})Z\.' + \
          r'([0-9]{6})T([0-9]{4})Z\.([0-9]{6})T([0-9]{4})Z\.([A-Z][A-Z])/'
        self._badVtecRE = r'^\s?/.*/\s?$'
        self._endSegmentRE = r'^\$\$'
        self._dlineRE = r"^1?[0-9]{3} [AP]M [A-Z][A-Za-z]?[DS]T.*[A-Z][A-Z,a-z]{2} " + \
          r"[0123]?[0-9] 2[0-9]{3}.*$"
        self._ugcRE = r'^[A-Z][A-Z][CZ][0-9]{3}[->]'
        #02272020 Make the first dash optional to account for rare instances where
        #DTG exists by itself on last line
        self._endTimeRE = r'-?[0-3][0-9][0-2][0-9][0-5][0-9]-$'

        #maximum future time (used for until further notice)
        self._maxFutureTime = float(2 ** 31 - 1)  #max signed int

    def decode(self):
        #get pil and date-time group
        self._productPil, self._issueTime, linePos, \
          self._completeProductPil = self._getPilAndDTG()

         # If this is a WCL - don't go any further. Run WCL procedure and exit.
        if self._productPil[0:3] == "WCL":
            endpoint = "WCLWatch"
            # build a Java object for the warning
            from com.raytheon.edex.plugin.gfe.watch import WclInfo
            import JUtil
            lines = JUtil.pyValToJavaObj(self._lines)
            warning = WclInfo(int(self._issueTime * 1000),
                              self._completeProductPil, lines, self._notifyGFE)
            from com.raytheon.uf.edex.core import EDEXUtil
            EDEXUtil.getMessageProducer().sendAsync(endpoint, warning)
            LogStream.logEvent("%s forwarded to WCLWatch" % self._productPil)
            return []

        # Determine if this is a segmented product
        segmented = self._determineSegmented(linePos)

        # Get overview text
        if segmented == 1:
            
            self._overviewText, linePos = self._getOverviewText(linePos)
        else:
            self._overviewText = ''
        LogStream.logDebug("OverviewText: ", self._overviewText)

        #find all UGCs, VTEC strings, and segment text
        ugcVTECList = self._getUGCAndVTECStrings(linePos)

        self._polygon = self._getPolygon(linePos)
        self._storm = self._getStorm(linePos)

        #convert UGC strings into UGC list
        ugcVTECSegText = []
        segCount = 1
        for ugcString, vtecStrings, segText, cities in ugcVTECList:
            purgeTime = None
            self._checkForDTG(ugcString)
            if self._hasDTG:
                purgeTime = ugcString[-7:-1]
            else:
                purgeTime = self._getPurgeTimeStrFromVTEC(vtecStrings)

            polygon = None
            if not (self._polygon is None or len(self._polygon) != len(ugcVTECList)):
               polygon = self._polygon[segCount -1]

            vtecList = self._expandVTEC(ugcString, vtecStrings, segCount,
              segText, cities, purgeTime, polygon)  
            segCount = segCount + 1
            for r in vtecList:
                ugcVTECSegText.append(r)
        if len(ugcVTECSegText) == 0:
            LogStream.logVerbose("No VTEC Found in product")
            return

        return ugcVTECSegText

    def _checkForDTG(self, ugcString):
        if re.search(r'.*[0-3][0-9][0-2][0-9][0-5][0-9]', ugcString):
            self._hasDTG = True
        else:
            self._hasDTG = False

    def _getPurgeTimeStrFromVTEC(self, vtecStrings):
        for vtecS, hvtec in vtecStrings:
            search = re.search(self._vtecRE, vtecS)
            timeStr = str(search.group(8)[-2:]) + str(search.group(9))
            return timeStr
        return None

    def _usage(self):
        #Prints out usage information if started without sufficient command
        #line arguments.
        s = """
usage: VTECDecoder -f productfilename -d -a activeTableName
-f productfilename:  location of the file to be decoded
-d:                  delete input file after processing flag
-g:                  Notify GFE when configured items arrive
-w: wmoid            WMOid for product, (optional, for info only)
-n:                  Do not make backups (optional)
-z: drtInfo          Run in DRT mode
"""
        print(s)
        LogStream.logProblem(s)

    def _decodeCommandLine(self):
        #Routine to decode the command line.
        if self._command is None:
            self.command = sys.argv

        if len(self._command) < 2:
            self._usage()
            sys.exit(1)

        LogStream.logVerbose("Command line: ", self._command[1:])

        try:
            optionlist, arglist = getopt.getopt(self._command[1:], 'f:da:gw:nz:')
        except getopt.error as val:
            LogStream.logProblem(val)
            self._usage()
            sys.exit(1)

        self._notifyGFE = False

        for each_option in optionlist:
            if each_option[0] == '-f':
                self._incomingFilename = each_option[1]
            elif each_option[0] == '-w':
                self._wmoid = each_option[1]
            elif each_option[0] == '-d':
                self._deleteAfterProcessing = 1
            elif each_option[0] == '-n':
                self._makeBackups = 0
            elif each_option[0] == '-g':
                self._notifyGFE = True
            elif each_option[0] == '-z':
                self._timeOffset = TimeUtil.determineDrtOffset(each_option[1])[0]
                #import offsetTime
                #offsetTime.setDrtOffset(each_option[1])

        if self._incomingFilename is None:
            LogStream.logProblem("Invalid command line specified", self._command[1:])
            self._usage()
            sys.exit(1)


        LogStream.logVerbose("WMOID: ", self._wmoid)

    def _getProduct(self, text=None, checkForWmo=False):
        #Opens, reads the product. Splits into lines, strips whitespace,
        if text is None:
            # Open in binary mode to avoid text mode newline handling
            # (would not correctly handle \r\r\n line endings)
            fd = open(self._incomingFilename, 'rb')
            if fd == -1:
                s = "Unable to open incoming file: " + self._incomingFilename
                LogStream.logProblem(s)
                raise Exception(s)
            buf = fd.read().decode()
            fd.close()
        else:
            buf = text

        raw = buf

        if checkForWmo:
            wmore = r'(\w{6} (\w{4}) (\d{6})(?: \w{3})?)'
            wmosearch = re.search(wmore, buf)
            if wmosearch:
                self._officeFromWMO = wmosearch.group(2)
                self._wmoid = wmosearch.group(1)

        #eliminate junk characters and change some
        if self._wmoid is not None:
            index = buf.find(self._wmoid)
            if index != -1:
                buf = buf[index:]  #remove garbage before start of real prod
#        buf = re.sub(r",", r"...", buf)   #eliminate commas, replace with ...
        buf = re.sub(r"\.\.\. r", "...", buf)  #'... ' becomes '...'
        buf = re.sub(r"\r", r"", buf)  #eliminate carriage returns

        #LogStream.logVerbose("Product:\n", buf)

        #process the file into lines, eliminate trailing whitespace
        lines = [line.rstrip() for line in buf.split('\n')]

        return raw, lines

    def _determineSegmented(self, startLine):
        #
        # Determine if this is a segmented product or not
        #
        count = startLine
        dlineFlag = 0
        while count < 12 and count < len(self._lines):
            if re.search(self._ugcRE,
               self._lines[count]):
                if dlineFlag == 0:
                    return 0

            if re.search(self._dlineRE, self._lines[count]):
                dlineFlag = 1

            count += 1

        return 1



    def _getOverviewText(self, startLine):
        #searches through the product from the startLine to the date-time
        #group, then until the first UGC line.  Extracts out the text for
        #the overview text (which is after the MND header.  Returns the
        #overviewText.
        count = startLine

        #search for the MND header date line
        while 1:
            dline_search = re.search(self._dlineRE, self._lines[count])
            count = count + 1
            if dline_search:
                break
            if count >= len(self._lines) - 1:
                raise Exception("No MND date line to start overview text")
        startOverviewLine = count  #next line after MND date line

        #search for the 1st UGC line
        while 1:
            if self.checkForBeginSegment(count):
                stopOverviewLine = count - 1
                break
            count = count + 1
            if count >= len(self._lines) - 1:
                raise Exception("No UGC line to end overview text")

        #now eliminate any blank lines between the start/stop overview line
        while startOverviewLine <= stopOverviewLine:
            if len(self._lines[startOverviewLine].strip()) != 0:
                break
            startOverviewLine = startOverviewLine + 1
        while startOverviewLine <= stopOverviewLine:
            if len(self._lines[stopOverviewLine].strip()) != 0:
                break
            stopOverviewLine = stopOverviewLine - 1

        LogStream.logDebug("start/stop overview: ", startOverviewLine,
          stopOverviewLine)

        #put together the text
        if startOverviewLine <= stopOverviewLine:
            overviewLines = self._lines[startOverviewLine:stopOverviewLine + 1]
            overviewText = '\n'.join(overviewLines)
            return (overviewText, stopOverviewLine)
        else:
            return ("", startLine)


    def _getPilAndDTG(self):
        #searches through the product (lines) and extracts out the product
        #pil and date-time group. Returns (pil, issueTime, lineEnd, fullpil).
        # The line end is how far the processing got for the PIL line.
        count = 0
        while 1:
            dtg_search = re.search(r' ([0123][0-9][012][0-9][0-5][0-9])',
              self._lines[count])
            pil_search = re.search(r'^([A-Z]{3})(\w{3}|\w{2}|\w{1})',
              self._lines[count + 1])

            if dtg_search and pil_search:
                LogStream.logVerbose("Dtg=", dtg_search.group(0))
                LogStream.logVerbose("Pil=", pil_search.group(0))
                return (self._lines[count + 1][0:3],
                  self._dtgFromDDHHMM(dtg_search.group(1)), count + 2,
                    pil_search.group(0))
            count = count + 1
            if count >= len(self._lines) - 1:
                LogStream.logProblem("Did not find either the product DTG" + \
                  " or the pil:\n", '\n'.join(self._lines),
                  LogStream.exc())
                raise Exception("Product DTG or Pil missing")

    def _dtgFromDDHHMM(self, dtgString, baseTime=None):
        if dtgString is None:
            return 0.0
        if baseTime is None :
            baseTime = self._time
        #utility function taking a ddhhmm string
        #group1=day, group2=hour, group3=minute
        #returns a time object
        wmo_day = int(dtgString[0:2])
        wmo_hour = int(dtgString[2:4])
        wmo_min = int(dtgString[4:6])

        gmtuple = time.gmtime(baseTime)
        wmo_year = gmtuple.tm_year
        wmo_month = gmtuple.tm_mon
        current_day = gmtuple.tm_mday
        if current_day - wmo_day > 15:
            # next  month
            wmo_month = wmo_month + 1
            if wmo_month > 12:
                wmo_month = 1
                wmo_year = wmo_year + 1
        elif current_day - wmo_day < -15:
            # previous month
            wmo_month = wmo_month - 1
            if wmo_month < 1:
                wmo_month = 12
                wmo_year = wmo_year - 1

        s = "%i%02i%02i%02i%02iUTC" % (wmo_year, wmo_month, wmo_day, wmo_hour, wmo_min)
        timeTuple = time.strptime(s, "%Y%m%d%H%M%Z")
        wmoTime = time.mktime(timeTuple)  #TZ is GMT0, so this mktime works

        LogStream.logVerbose("DTG=", dtgString, "IssueTime=",
          time.asctime(time.gmtime(wmoTime)))

        return wmoTime


    def _getUGCAndVTECStrings(self, lineStart):
        #goes through the product, extracts UGC and VTEC strings and the
        #segment text, returns a list of (UGC keys, VTEC strings, segText).
        #Segment number is determined by order in the list.
        ugcList = []
        count = lineStart  #start on line following PIL
        while 1:
            #look for the first UGC line
            if self.checkForBeginSegment(count):
                LogStream.logDebug("First line of UGC found on line: ", count,
                  '[' + self._lines[count] + ']')

                #print("ugc found on line ", count)

                #find the line with the terminating ugc (dtg), might be
                #the same one. Terminating line has -mmddhh
                #combine all of the UGC lines that are split across
                nxt = 0  #number of lines from the first UGC line
                ugc = ""  #final UGC codes
                while count + nxt < len(self._lines):
                    if re.search(self._endTimeRE, self._lines[count + nxt]):
                        LogStream.logDebug("Last line of UGC found on line: ",
                          count + nxt, '[' + self._lines[count + nxt] + ']')
                        ugc = "".join(self._lines[count:count + nxt + 1])
                        break

                    nxt = nxt + 1

                    # if we hit the end, break out and let the len(ugc) check fail
                    if count + nxt == len(self._lines):
                        break

                    #after incrementing check if the next line is not a
                    #continuation of the ugc because it is a vtec string
                    #print("checking for non-ugc")
                    #print(self._lines[count+nxt])
                    if re.search(self._vtecRE, self._lines[count + nxt]):
                        LogStream.logDebug("Last line of UCG was previous line: ",
                                           count + nxt,
                                           '[' + self._lines[count + nxt - 1] + ']')
                        nxt = nxt - 1
                        ugc = "".join(self._lines[count:count + nxt + 1])
                        break

                if len(ugc) == 0:
                    s = "Did not find end of UGC line which started on line %i" % count
                    LogStream.logProblem(s)
                    raise Exception("Aborting due to bad UGC lines")


                #find the VTEC codes following the ugc line(s)
                nxt = nxt + 1  #go the 1st line after ugc
                vtectext = []
                while count + nxt < len(self._lines):
                    if re.search(self._vtecRE, self._lines[count + nxt]):
                        hvtec = None
                        if re.search(self._hVtecRE, self._lines[count + nxt + 1]):
                            hvtec = self._lines[count + nxt + 1]
                        vtectext.append((self._lines[count + nxt], hvtec))
                        LogStream.logDebug("VTEC found on line: ",
                          count + nxt, self._lines[count + nxt])
                    elif (re.search(self._badVtecRE, self._lines[count + nxt]) \
                      and not re.search(self._hVtecRE, self._lines[count + nxt])):
                        LogStream.logProblem("Bad VTEC line detected on line#",
                          count + nxt, '[' + self._lines[count + nxt] + ']',
                          'UGC=', ugc)
                        raise Exception("Aborting due to bad VTEC line")
                    else:
                        break  #no more VTEC lines for this ugc
                    nxt = nxt + 1  #go to next line

                # for capturing the city names
                cityFirst = count + nxt
                cityLast = cityFirst - 1

                #capture the text from dtg to the $$ at the beginning of
                #the line.  Just in case there isn't a $$, we also look
                #for a new VTEC or UGC line, or the end of file.
                textFirst = count + nxt
                dtgFound = 0
                segmentText = ""
                while count + nxt < len(self._lines):

                    # Date-TimeGroup
                    if dtgFound == 0 and re.search(self._dlineRE,
                      self._lines[count + nxt]):
                        cityLast = count + nxt - 1
                        textFirst = count + nxt + 2  #first text line
                        dtgFound = 1

                    # found the $$ line
                    elif re.search(self._endSegmentRE, self._lines[count + nxt]):
                        segmentText = self._prepSegmentText(\
                          self._lines[textFirst:count + nxt])
                        break

                    # found a probable UGC line, terminate the segment
                    # if a DDMMHH or VTEC can be found
                    elif self.checkForBeginSegment(count + nxt):
                        segmentText = self._prepSegmentText(\
                            self._lines[textFirst:count + nxt])
                        nxt = nxt - 1  #back up one line to redo UGC outer loop
                        break

                    # end of file, terminate the segment
                    elif count + nxt + 1 == len(self._lines):
                        segmentText = self._prepSegmentText(\
                          self._lines[textFirst:count + nxt + 1])
                        break

                    nxt = nxt + 1  #next line

                # capture cities
                cityText = ''
                for i in range(cityFirst, cityLast + 1):
                    line = self._lines[i].rstrip()

                    if line.startswith("INCLUDING THE"):
                        cityText = line
                    elif cityText != '':
                        cityText += line

                cities = []
                if cityText != '':
                    cities = cityText.split('...')[1:]

                #add the ugc and vtec text to the list
                ugcList.append((ugc, vtectext, segmentText, cities))

                count = count + nxt

            count = count + 1
            if count >= len(self._lines):
                break

        for e in ugcList:
            LogStream.logVerbose("UGC/VTEC found: ", e[0], e[1])
        return ugcList

    def checkForBeginSegment(self, start):
        if re.search(self._ugcRE, self._lines[start]) and \
                not self.checkForFalseUGCLine(start):
            return True
        return False

    def checkForFalseUGCLine(self, toCheck):
        # tries to determine if an apparent UGC line encountered in a text
        # segment is an actual UGC line, or is a UGC-like line in the free-text
        # such as:
        #THE AFFECTED AREAS WERE...
        #GMZ074-GMZ054-
        #STRAITS OF FLORIDA FROM WEST END OF SEVEN MILE BRIDGE TO HALFMOON
        #SHOAL OUT 60 NM...
        falsePositive = True
        while toCheck < len(self._lines):
            # look for the end date or vtec line before "$$" or the end of the
            # file
            if re.search(self._endTimeRE,
                         self._lines[toCheck]) or \
                         re.search(self._vtecRE, self._lines[toCheck]):
                falsePositive = False
                break

            # blank line
            if not self._lines[toCheck]:
                break

            if re.search(self._endSegmentRE, self._lines[toCheck]):
                break

            toCheck = toCheck + 1

        return falsePositive

    def _expandUGC(self, ugcString):
        #expand a UGC string into its individual UGC codes, returns the list.
        ugc_list = []  #returned list of ugc codes
        ugc_line = None
        if (self._hasDTG):
            ugc_line = ugcString[0:-8]  #eliminate dtg at end of ugc string
        else:
            ugc_line = ugcString

        #print("ugc_line is ", ugc_line)
        working_ugc_list = ugc_line.split('-')  #individual parts
        state = ''
        code_type = ''

        for ugc in working_ugc_list:
            try:
                # Case One (i.e., WIZ023)...
                if len(ugc) == 6:
                    state, code_type, decUGCs = self._ugcCaseOne(ugc)
                    for d in decUGCs:
                        ugc_list.append(d)

                # Case Two (i.e., 023)...
                elif len(ugc) == 3:
                    decUGCs = self._ugcCaseTwo(ugc, state, code_type)
                    for d in decUGCs:
                        ugc_list.append(d)

                # Case Three (ie. 044>067)
                elif len(ugc) == 7:
                    decUGCs = self._ugcCaseThree(ugc, state, code_type)
                    for d in decUGCs:
                        ugc_list.append(d)

                # Case Four (ie. WIZ044>067)
                elif len(ugc) == 10:
                    state, code_type, decUGCs = self._ugcCaseFour(ugc)
                    for d in decUGCs:
                        ugc_list.append(d)

                # Problem - malformed UGC
                else:
                    raise Exception("Malformed UGC Found")

            except:
                LogStream.logProblem("Failure to decode UGC [" + ugc + \
                  "] in [" + ugc_line + "]\n", "\n".join(self._lines), LogStream.exc())
                raise Exception("Failure to decode UGC")

        #print("ugc_list is ", ugc_list)
        return ugc_list

    def _ugcCaseOne(self, ugc):
        #Decodes the WIZ023 case. Returns state, code type, and list of
        #decoded UGC codes.  Returns None on failure.
        subtype_search = re.search(r'[A-Z][A-Z][CZ][0-9][0-9][0-9]', ugc)
        if not subtype_search:
            return None
        state = ugc[0:2]
        code_type = ugc[2]
        code = ugc[3:6]
        decodedUgcs = [(state + code_type + code)]
        return (state, code_type, decodedUgcs)



    def _ugcCaseTwo(self, ugc, state, code_type):
        #Decodes the 034 case. Current state and code_type are provided in
        #order to generate ugc code. Returns list of decoded UGC codes.
        #Returns None on failure.
        subtype_search = re.search(r'[0-9][0-9][0-9]', ugc)
        if not subtype_search:
            return None
        decodedUgcs = [(state + code_type + ugc)]
        return decodedUgcs

    def _ugcCaseThree(self, ugc, state, code_type):
        #Decodes the 044>067 case. Current state and code_type are provided
        #in order to generate ugc code.  Returns list of decoded UGC codes.
        #Returns None on failure.
        subtype_search = re.search(r'[0-9][0-9][0-9]>[0-9][0-9][0-9]', ugc)
        if not subtype_search:
            return None

        start_code = int(ugc[0:3])
        end_code = int(ugc[4:7])
        ugcList = []
        for code in range(start_code, end_code + 1):
            codeString = "%03i" % code
            ugcList.append(state + code_type + codeString)
        return ugcList


    def _ugcCaseFour(self, ugc):
        #Decodes the WIZ023>056 case. Returns state, code type, and list of
        #decoded UGC codes.  Returns None on failure.
        searchString = r'[A-Z][A-Z][CZ][0-9][0-9][0-9]>[0-9][0-9][0-9]'
        subtype_search = re.search(searchString, ugc)
        if not subtype_search:
            return None

        state = ugc[0:2]
        code_type = ugc[2]
        ugcList = self._ugcCaseThree(ugc[3:10], state, code_type)
        return (state, code_type, ugcList)

    def _getPolygon(self, linePos):

        polygons = []
        poly = r'LAT\.\.\.LON [\d\s]*'
        dec = r'[\d\s]+'
        count = linePos
        latlon = None
        while count < len(self._lines):
            if re.search(poly, self._lines[count]):
                nxt = 0  #number of lines from the first latlon line
                while count + nxt + 1 < len(self._lines):
                    if re.match(dec, self._lines[count + nxt + 1]):
                        nxt = nxt + 1
                    else:
                        break
                n = 0
                latlon = ""
                while n <= nxt:
                    latlon += self._lines[count + n] + " "
                    n += 1
                latlon = latlon.rstrip()
                latlon = latlon[10:]

                polygons.append(latlon) 

            count += 1
        return polygons 

    def _getStorm(self, linePos):
        stormre = r'TIME\.\.\.MOT\.\.\.LOC \d+Z (\d+)DEG (\d+)KT ((?:\d{4,5}\s?)*)'
        nextline = r' *((?:\d{4,5}\s?)+)'
        storm = None
        count = linePos
        while count < len(self._lines):
            search = re.search(stormre, self._lines[count])
            if search:
                coordString = search.group(3)
                # check the next line
                if re.search(nextline, self._lines[count + 1]):
                    coordString = coordString + " " + self._lines[count + 1].lstrip()
                counter = 0
                coords = coordString.split(" ")
                buf = "MULTIPOINT("
                tempbuf = ""
                for coord in coords:
                    counter += 1
                    if counter % 2 == 0:
                        buf = buf + str(float(coord) / -100) + " " + tempbuf + ", "
                        tempbuf = ""
                    else:
                        tempbuf = str(float(coord) / 100)

                buf = buf[0:-2]
                buf = buf + ")"
                storm = (int(search.group(1)), int(search.group(2)), buf)
                break
            count = count + 1
        if storm is None:
            storm = self._getStormFromEvent()

        LogStream.logEvent('returning storm=%s' % repr(storm))
        return storm

    def _getStormFromEvent(self):
        event_pattern = re.compile(r'^(?:\* )?AT (?:NOON|MIDNIGHT|\d+)(.*?)\n[ \r\t\f\v]*\n',
                                   re.M | re.DOTALL)
        m = event_pattern.search(self._rawMessage)
        if m is None:
            return None

        event_text = m.group(1)

        motion_pattern = re.compile(r'\bMOVING\s*(\w+)\s*AT\s*(\d+(?:\.\d*)?)\s*(\w+)')
        m = motion_pattern.search(event_text)
        if m is not None:
            bearing = BEARINGS.get(m.group(1).upper())
            if bearing is not None:
                # Bearing is stored as the direction the storm is coming FROM
                bearing -= 180
                if bearing < 0:
                    bearing += 360
            else:
                LogStream.logProblem("could not parse bearing '%s' in '%s'" %
                                     (m.group(1), m.group(0)))

            conversion_factor = SPEED_CONVERSION.get(m.group(3))
            converted_speed = None
            if conversion_factor is not None:
                try:
                    converted_speed = int(round(float(m.group(2)) * conversion_factor))
                except Exception as e:
                    LogStream.logProblem("error processing speed in '%s': %s" %
                                         (m.group(0), e))
            else:
                LogStream.logProblem("unknown unit '%s' in '%s'" %
                                     (m.group(3), m.group(0)))
        else:
            # find "STATIONARY in the last sentence
            motion_pattern = re.compile(r'\bSTATIONARY\s*.\s*')
            if motion_pattern.search(event_text) is not None:
                bearing = converted_speed = 0
            else:
                bearing = converted_speed = None

        # TODO: Storm location: This require parsing (potentially) multiple
        # references to cities (or marine locations) with distance/bearing
        # offsets.  Need access to geospatial information to get the actual
        # lat/lons.
        if bearing is not None and converted_speed is not None:
            return (bearing, converted_speed, None)
        else:
            return None

    def _calcTime(self, yymmdd, hhmm, allZeroValue):
        #Returns tuple of time, and allZeroFlag. Time is based on the
        #two time strings.  If all zeros, then return allZeroValue
        if yymmdd == "000000" and hhmm == "0000":
            return (allZeroValue, True)
        else:
            timeString = yymmdd + hhmm
            timeTuple = time.strptime(timeString, "%y%m%d%H%M")
            return (int(calendar.timegm(timeTuple) * 1000), False)

    def _expandVTEC(self, ugcstring, vtecStrings, segment, segmentText, cities,
                    purgeTime, polygon):

        #Routine takes a list of vtec strings and expands them out into
        #the format of the active table.
        #Returns the records.
        ugcs = self._expandUGC(ugcstring)
        records = []
        for vtecS, hvtec in vtecStrings:

            search = re.search(self._vtecRE, vtecS)

            #construct the active table entries, without the geography
            template = {}
            template['vtecstr'] = search.group(0)
            template['etn'] = search.group(5)
            template['sig'] = search.group(4)
            template['phen'] = search.group(3)
            template['segText'] = segmentText
            template['overviewText'] = self._overviewText
            template['phensig'] = template['phen'] + '.' + template['sig']
            template['act'] = search.group(1)
            template['seg'] = segment
            startTime, zeros = self._calcTime(search.group(6),
              search.group(7), self._issueTime * 1000)
            endTime, ufn = self._calcTime(search.group(8),
              search.group(9), self._maxFutureTime * 1000)
            template['startTime'] = int(startTime)
            template['endTime'] = int(endTime)
            template['ufn'] = ufn
            template['officeid'] = search.group(2)
            template['purgeTime'] = int(self._dtgFromDDHHMM(purgeTime, self._issueTime) * 1000)
            template['issueTime'] = int(self._issueTime * 1000)
            template['state'] = "Decoded"
            template['xxxid'] = self._completeProductPil[3:]
            if (self._hasDTG):
                template['countyheader'] = ugcstring[:-8]
            else:
                template['countyheader'] = ugcstring
            if self._productPil[:3] in ACCURATE_CITIES_PILS:
                template['cities'] = cities

            #remap pil if in mappedPils table to relate events that are
            #issued in one product, and updated in another product
            template['pil'] = self._remapPil(template['phen'],
              template['sig'], self._productPil)

            template['ugcZoneList'] = ", ".join(ugcs)
            state = ugcstring[0:2]
            if state in REGIONS:
                template['region'] = REGIONS[state]
            else:
                template['region'] = 'DEFAULT'
            template['wmoid'] = self._wmoid
            template['productClass'] = template['vtecstr'][1]
            template['geometry'] = polygon

            try:
                from com.raytheon.uf.common.time import DataTime, TimeRange
                valid = TimeRange(int(startTime), int(endTime))
                template['dataTime'] = DataTime(int(startTime), valid)
            except:
                template['dataTime'] = int(endTime)

            template['rawmessage'] = self._rawMessage
            if self._storm is not None:
                template['motdir'] = self._storm[0]
                template['motspd'] = self._storm[1]
                template['loc'] = self._storm[2]

            if hvtec is not None:
                hsearch = re.search(self._hVtecRE, hvtec)
                template['locationID'] = hsearch.group(1)
                template['floodSeverity'] = hsearch.group(2)
                template['immediateCause'] = hsearch.group(3)
                template['floodRecordStatus'] = hsearch.group(10)

                if template['phensig'] in PHENSIGS_IGNORE_HVTEC:
                    fakeBeginTime = None
                    fakeEndTime = None
                    fakeCrestTime = None
                else:
                    fakeBeginTime = template['issueTime']
                    fakeEndTime = int(self._maxFutureTime * 1000)
                    fakeCrestTime = fakeEndTime
                template['floodBegin'] = self._calcTime(hsearch.group(4), hsearch.group(5), fakeBeginTime)[0]
                template['floodCrest'] = self._calcTime(hsearch.group(6), hsearch.group(7), fakeCrestTime)[0]
                template['floodEnd'] = self._calcTime(hsearch.group(8), hsearch.group(9), fakeEndTime)[0]

            records.append(template)

        return records

    def _remapPil(self, phen, sig, pil):
        # remaps the product pil for certain phen/sig/pils.  The VTECDecoder
        # needs to relate hazards through all states from the same pil. Some
        # short-fused hazards issue in one pil and followup/cancel in
        # another pil.
        key = (phen, sig, pil)
        rPil = self._mappedPils.get(key, pil)
        if rPil != pil:
            LogStream.logEvent("Remapped Pil", key, "->", rPil)
        return rPil


    def _prepSegmentText(self, lines):
        # eliminate leading and trailing blank lines from the set of
        # lines, the joins the lines and returns one long string.

        # eliminate leading blank lines
        while len(lines) and len(lines[0]) == 0:
            del lines[0]
        # eliminate trailing blank lines
        while len(lines) and len(lines[-1]) == 0:
            del lines[-1]
        # assemble into long string
        return '\n'.join(lines)

    # time contains, if time range (tr) contains time (t), return 1
    def __containsT(self, tr, t):
        return (t >= tr[0] and t < tr[1])

    # time overlaps, if tr1 overlaps tr2 (adjacent is not an overlap)
    def __overlaps(self, tr1, tr2):
        if self.__containsT(tr2, tr1[0]) or self.__containsT(tr1, tr2[0]):
            return 1
        return 0

class NoVTECWarningDecoder(StdWarningDecoder):

    #this is a special case, we do not want to make changes to this class that will break parsing
    #warnings that do have vtec strings.  If needed all non-vtec specific code should be split out
    #into new classes
    def _makeRecordWithoutVTEC(self, ugcstring, segment, segmentText, cities, purgeTime, ugcs, polygon):
        records = []

        #construct the active table entries, without the geography
        template = {}
        template['segText'] = segmentText
        template['overviewText'] = self._overviewText
        template['seg'] = segment
        startTime = self._issueTime * 1000
        ddhhmmz = ugcstring[-7:-1]
        endTime = self._dtgFromDDHHMM(ddhhmmz) * 1000
        template['startTime'] = int(startTime)
        template['endTime'] = int(endTime)
        template['ufn'] = True  # is this correct?
        if self._officeFromWMO:
            template['officeid'] = self._officeFromWMO

        template['purgeTime'] = int(self._dtgFromDDHHMM(purgeTime, self._issueTime) * 1000)
        template['issueTime'] = int(self._issueTime * 1000)
        template['state'] = "Decoded"
        template['xxxid'] = self._completeProductPil[3:]
        template['countyheader'] = ugcstring[:-8]
        if self._productPil[:3] in ACCURATE_CITIES_PILS:
            template['cities'] = cities
        template['pil'] = self._productPil
        template['ugcZoneList'] = ", ".join(ugcs)
        state = ugcstring[0:2]
        if state in REGIONS:
            template['region'] = REGIONS[state]
        else:
            template['region'] = 'DEFAULT'
        template['wmoid'] = self._wmoid
        template['geometry'] = polygon
        try:
            from com.raytheon.uf.common.time import DataTime, TimeRange
            valid = TimeRange(int(startTime), int(endTime))
            template['dataTime'] = DataTime(int(startTime), valid)
        except:
            template['dataTime'] = int(endTime)

        template['rawmessage'] = self._rawMessage
        if self._storm is not None:
            template['motdir'] = self._storm[0]
            template['motspd'] = self._storm[1]
            template['loc'] = self._storm[2]

        records.append(template)

        return records

    def _expandVTEC(self, ugcstring, vtecStrings, segment, segmentText, cities,
                    purgeTime, polygon):
        #Routine takes a list of vtec strings and expands them out into
        #the format of the active table.
        #Returns the records.

        ugcs = self._expandUGC(ugcstring)
        return self._makeRecordWithoutVTEC(ugcstring, segment, segmentText, cities, purgeTime, ugcs, polygon)

class WarningDecoder():

    def __init__(self, text=None, filePath=None, command=None):
        #hold on to text and filePath
        self.text = text
        self.filePath = filePath

        self._vtecRE = r'/[OTEX]\.([A-Z]{3})\.([A-Z]{4})\.([A-Z]{2})\.' + \
          r'([WAYSOFN])\.([0-9]{4})\.([0-9]{6})T([0-9]{4})Z\-' + \
          r'([0-9]{6})T([0-9]{4})Z/'
        self._hVtecRE = r'/([0-9A-Z]{5})\.([0-3NU])\.([A-Z]{2})\.' + \
          r'([0-9]{6})T([0-9]{4})Z\.' + \
          r'([0-9]{6})T([0-9]{4})Z\.([0-9]{6})T([0-9]{4})Z\.([A-Z][A-Z])/'
        self._badVtecRE = r'^\s?/.*/\s?$'

        self._stdWarningDecode = None
        if str == type(command):
            command = command.split()
        if command and str == type(command[0]) and command[0].startswith('-'):
            self._command = [''] + command
        else:
            self._command = command

    def decode(self):
        #discover which type of warning to decode
        self._checkForVTEC()

        #create appropraite class and decode
        if (self._stdWarningDecode):
            #print("using standard warning decoder reason: ", self._stdWarningDecode)
            decoder = StdWarningDecoder(self.text, self.filePath, self._command)
            return decoder.decode()
        else:
            #print("using no vtec warning decoder")
            decoder = NoVTECWarningDecoder(self.text, self.filePath, self._command)
            return decoder.decode()

    def _checkForVTEC(self):
        contents = ""
        if (self.text):
            contents = self.text
        elif (self.filePath):
            fd = open(self.filePath, 'r')
            contents = fd.read()
            fd.close()
        else:
            #define self._stdWarningDecode as something so the std decoder is used
            self._stdWarningDecode = 'no file or text'
            return

        #search through contents to find a vtec string
        lines = contents.split('\n')

        for line in lines:
            if re.search(self._vtecRE, line):
                self._stdWarningDecode = 'found a vtec string'
                return
            elif (re.search(self._badVtecRE, line) \
                  and not re.search(self._hVtecRE, line)):
                self._stdWarningDecode = 'found bad vtec string'
                return

