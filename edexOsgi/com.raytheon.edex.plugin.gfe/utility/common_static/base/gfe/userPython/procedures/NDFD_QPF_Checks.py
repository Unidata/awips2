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
# NDFD_QPF_Checks (was SnowAmtQPFPoPWxCheck)
#
# Author: Jay Smith, WFO Fairbanks, jay.smith@noaa.gov, 907-458-3721
# SnowAmtQPFPoPWxCheck Incarnation
# Version: 1.0.0, 09/14/2006 - Initial version
#          1.0.1, 10/12/2006 - Added PoP/QPF check at request of DSPAC
#          1.0.2, 10/18/2006 - Changed PoP/QPF check to treat the PoP as
#                 floating. Instead of checking each individual PoP grid
#                 against its corresponding QPF grid, the max of all the
#                 PoP grids overlapping a QPF grid will be checked.
#          1.1.0, 01/25/2007 - Added options to choose which checks to run.
#                 Reorganized code so that each check is its own method.
#                 Added a check for QPF and Wx. Added highlighting for the
#                 created temporary grids.
#          1.1.1, 02/01/2007 - Changed the SnowAmt/Wx check to return
#                 consistent results for SnowAmt > 0 and Wx grid containing
#                 S, SW, or IP regardless of whether the frozen precip is
#                 mixed with freezing and/or liquid precip.
#          1.2.0, 02/13/2007 - Added a configuration option to provide a CWA
#                 edit area to run the procedure over. A bad edit area or no
#                 edit area will result in running over the whole domain.
#                 Modified the SnowAmt/Wx and QPF/Wx checks to handle two
#                 cases. Case 1: The SnowAmt/QPF grid is 6-hr long and starts
#                 at 00, 06, 12, or 18 UTC. Then only one of the corresponding
#                 Wx grids has to meet the consistency rule. Case 2: The
#                 SnowAmt/QPF grid does not meet the case 1 definition. Then
#                 all of the corresponding Wx grids must meet the consistency
#                 rule.
# NDFD_QPF_Checks Incarnation
# Version: 1.0.0, 04/04/2007 - The program now requires the presence of the
#                 following 3 grids: QPF6hr, SnowAmt6hr, and PoP12hr. The 2
#                 6-hr grids are expected to start at 00, 06, 12, or 18 UTC
#                 and be exactly 6 hours long. The PoP12hr grids are expected
#                 to start at 00 or 12 UTC and be exactly 12 hours long.
#                 The procedure still needs the PoP and Wx grids. With these
#                 grid changes, the procedure can now finally perform the last
#                 of the required NDFD checks. This also means there is no
#                 longer the need for two check cases in the two "Wx" checks.
#                 Now, Wx is consistent if any of its grids have the appropriate
#                 type. The procedure can now be run in a "quiet" mode if
#                 called from another procedure. Any messages generated in
#                 "quiet" mode will be of "R" severity so no pop-up messages
#                 are generated but the information is still available from the
#                 GFE status bar area. All error messages are templated in a
#                 dictionary in a separate method. This allows me to put all
#                 the triple-quoted strings, which ignore indentation, in one
#                 location. The code for checking for locked grids was also
#                 templated in its own method, which chopped about 15 lines of
#                 code off the front of each check method. There were a couple
#                 of places where I was applying the "tolerance" values
#                 incorrectly, which have been fixed. I dropped the
#                 "Inconsistent" label from all temporary grid names. I was
#                 making those grid names so long, they didn't actually fit
#                 in the spatial editor window when the grid manager was on
#                 the left. Temporary grid names now are just a concatenation
#                 of the two grids used in the check.
# Version: 1.1.0, The logic for the handling of inconsistencies in the
#                 SnowAmt6hr/Wx, QPF6hr/Wx, and PoP12hr/QPF6hr checks could
#                 result in false positive inconsistencies. This is because
#                 I was checking for inconsistencies on each of these grids
#                 individually when I needed to be checking the cumulative
#                 inconsistencies of these grids. With the new logic, if any
#                 of these checks has inconsistencies, the resulting temporary
#                 grid will have the same time range as the "controlling" grid,
#                 which is the first grid listed in each check name. Also, I
#                 have enforced the precision of the SnowAmt6hr and QPF6hr
#                 grids in the methods where they are used. SmartScript has a
#                 method called "around" that does this. I ran into an issue
#                 when this procedure was called from NDFD_QPF_Suite. I am
#                 unable to figure out how to uncache the QPF6hr, SnowAmt6hr,
#                 and PoP12hr grids after they are generated by the
#                 Collaborate_PoP_SnowAmt_QPF procedure. For the QPF6hr and
#                 SnowAmt6hr grids, this means getGrids is getting them from
#                 the cache, which means these float parameters have machine
#                 precision. This is utterly unacceptable. I have to have
#                 ifpServer precision. Now, I think I've ensured this.
#          1.1.1, 05/02/2007 - There have been instances of unexpected procedure
#                 performance if the NDFD QPF parameters are not visible in the
#                 grid manager when the procedure is run. The procedure will
#                 now require the existence of a weather element group which
#                 contains just the NDFP QPF parameters.
#          1.2.0, 05/03/2007 - Upon further review, the unexpected procedure
#                 performance arises when some of the NDFD QPF parameters are
#                 not present in the grid manager. However, I do not need to
#                 load a weather element group to make the parameters present.
#                 I can use the loadParm command on each element instead. Given
#                 this, the "weGroup" configuration has been removed. Also,
#                 some people believe the lock checking is overly stringent.
#                 To some extent, I agree. For the purposes of this procedure,
#                 other GFE users can have any of the NDFD QPF parmeteres
#                 locked. The user running the procedure, however, cannot have
#                 any of the parameters locked; i.e., that person must save
#                 those elements before running the procedure.
# The procedure performs the following checks:
# 1. If SnowAmt6hr present and >= 0.5 inches, then corresponding QPF6hr grids
#    must be >= 0.01 inches.
# 2. If SnowAmt6hr >= 0.1 inches then at least one of the corresponding Wx
#    grids must have S, SW, or IP.
# 3. If QPF6hr > 0.0, then at least one of the corresponding PoP grids must
#    be > 0
# 4. If QPF6hr > 0.0 then at least one of the corresponding Wx grids must have
#    R, RW, S, SW, RS, IP, L, ZR, ZL.
# 5. If PoP12hr >= 50%, then at least one of the corresponding QPF6hr grids
#    must be >= 0.0.
# For all of the checks above, if the initial threshold is not exceeded, then
# the two grids are consistent by definition. In other words:
# 1. If SnowAmt6hr < 0.5, then SnowAmt6hr and QPF6hr are always consistent.
# 2. If SnowAmt6hr < 0.1, then SnowAmt6hr and Wx are always consistent.
# 3. If QPF6hr = 0.0, then QPF6hr and PoP are always consistent.
# 4. If QPF6hr = 0.0, then QPF6hr and Wx are always consistent.
# 5. If PoP12hr < 50%, then PoP12hr and QPF6hr are always consistent.
# For the Wx checks above, only the Wx type is considered.
#
# ****** NOTE NOTE NOTE NOTE ******
# The program checks the PoP12hr, QPF6hr, and SnowAmt6hr grids to make sure
# their time constraints are met. For any grid where the time constraint is
# violated, those grids are not checked. To reiterate the time constraints:
# PoP12hr: starts at either 00 or 12 UTC and is exactly 12 hours duration
# QPF6hr: starts at 00, 06, 12, or 18 UTC and is exactly 6 hours duration
# SnowAmt6hr: starts at 00, 06, 12 or 18 UTC and is exactly 6 hours duration
# ****** NOTE NOTE NOTE NOTE ******
#
# If discrepancies are found, then the "bad" grids will be highlighted.
# Temporary grids showing where the discrepancies occur will be created and
# also highlighted.
#
# I've written this code such that it's optimized to minimize memory usage
# (at least I think I've done that). As a result, it's not particularly
# optimized for ifpServer database access. In fact, I retrieve the various
# grids from the ifpServer database many times during the procedure's run.
# This will have an impact on how fast the procedure runs (it'll run slower
# than if I had optimized for ifpServer database access). The choice to favor
# memory optimization comes from my belief that there are still "memory leak"
# problems in the GFE and that the consequences of those problems will be most
# manifest when this procedure is most likely to be run (near the end of the
# shift). Funky memory problems are a prime cause of funky application
# behavior like application crashes or spontaneous logouts. So, this procedure
# basically reads a grid into memory, keeps it as long as it's needed, and
# then discards it.
#
# Finally, this procedure is also intended to provide an example to other
# developers of how to write and document code. I have reservations as to how
# well I've succeeded at that task. The code is heavily documented, probably
# excessively so. Also, it's not as well as organized as it could be. As you
# look through the various methods, it should become quickly apparent that
# there is a lot of repeated code. I've consciously left some the code this
# way in the hopes that it will be easier to understand by more novice
# programmers and because the code hasn't quite grown to the point where
# updating the repeating code is onerous or overly error-prone. It would be
# better to capture the repeating code in separate methods, but keeping track
# of the where you are in the code becomes harder the more you have to jump
# around from method to method. Anyone who has ever worked with the text
# formatters can sympathize with that. As with all things, there are trade-
# offs involved. UPDATE: 4/3/2007 - Starting with the first NDFD_QPF_Checks
# version, I consolidated quite a bit of the repeating code into separate
# methods. So, there's some improvement on that front.
#
# Acknowledgement:
# Many of the Python "tricks" I use in this procedure I learned from
# reading/perusing the following book: Python Cookbook, Alex Martelli &
# David Ascher, eds., 2002, O'Reilly and Associates
# ----------------------------------------------------------------------------

MenuItems = ["Consistency"]

VariableList = [
    ('Check_Cleanup', 'Check', 'radio', ['Check', 'Cleanup']),
    ('Run SnowAmt6hr/QPF6hr Check?', ['Yes'], 'check', ['Yes']),
    ('Run SnowAmt6hr/Wx Check?', ['Yes'], 'check', ['Yes']),
    ('Run QPF6hr/PoP Check?', ['Yes'], 'check', ['Yes']),
    ('Run QPF6hr/Wx Check?', ['Yes'], 'check', ['Yes']),
    ('Run PoP12hr/QPF6hr Check?', ['Yes'], 'check', ['Yes']),
    ('If "Cleanup" is selected, then only cleanup actions will run.\nNo checks will be made, regardless of the above settings.', '', 'label'),
    ]

#### Config section
# Both the QPF and SnowAmt grids have values which are floating point
# numbers. This means comparisons must use a tolerance value. In other
# words, 0.5 may be represented in machine numbers as 0.49999999999 or
# 0.500000000001. By specifying a tolerance value, we account for the
# vagaries of machine representation of floating point numbers while
# keeping the precision of the comparisons to acceptable levels. Depending
# on the comparison being done, the tolerance value will be added to or
# subtracted from the comparison value to allow for machine error in the
# floating point number representation.
# By default in the GFE, QPF precision is to the nearest one-hundredth while
# SnowAmt precision is to the nearest tenth.
qpfTol = 0.00001  # 1/100,000 tolerance vs 1/100 precision
snowAmtTol = 0.0001  # 1/10,000 tolerance vs 1/10 precision
# Inconsistent grid highlight color. One size fits all. To turn off
# highlighting, set the variable to the empty string, ''.
inconGridColor = 'red'
# Temporary grid highlight color. One size fits all. To turn off highlighting,
# set the variable to the empty string, ''.
tempGridColor = 'orange'
# Name of CWA edit area to use instead of running the procedure over the
# whole domain. Set to the empty string, '', if you want the procedure to
# always run over the whole domain. If the procedure has a problem with the
# edit area you provide, it will run over the whole domain. You should probably
# choose an edit area that is slightly larger than your entire CWA. It's
# possible that when mapping your GFE grids to NDFD grids that the NDFD thinks
# some GFE grid cells are in your CWA that the GFE does not think are in your
# CWA. Using an edit area slightly larger than the CWA, like the ISC_Send_Area
# which is the mask used when sending grids to the NDFD, should eliminate the
# possibibilty of the NDFD intermittently flagging CWA border "points" as
# inconsistent. Note: running the procedure over a subset of the entire GFE
# domain does not really provide any performance gains. Given the way the
# underlying array data structure works, calculations are almost always made
# at every single grid point first and then a mask is applied to limit the
# meaningful results to the edit area. For the purposes of this procedure, the
# values outside the edit area are set to the appropriate "consistent" result.
# The real benefit of this option is it limits the inconsistent results to the
# areas the forecaster really cares about, which should lessen the workload of
# using this procedure. Marine Offices: Make sure the edit area provided
# includes your marine zones.
cwaEditArea = 'ISC_Send_Area'
#### Config section end
        
import SmartScript
from numpy import *

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def __cleanup(self, timeRange):
        # Remove any temporary grids created previously. Turn off any
        # previous highlighting.
        for element in (
            'SnowAmt6hrQPF6hr', 'SnowAmt6hrWx', 'QPF6hrPoP', 'QPF6hrWx',
            'PoP12hrQPF6hr'):
            try:
                # From SmartScript
                self.unloadWE('Fcst', element, 'SFC')
            except:
                # A failure is almost certainly no grids to unload.
                pass
        # Turn off any highlights. From SmartScript
        self.highlightGrids(
            'Fcst', 'SnowAmt6hr', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids(
            'Fcst', 'QPF6hr', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids(
            'Fcst', 'Wx', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids(
            'Fcst', 'PoP', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids(
            'Fcst', 'PoP12hr', 'SFC', timeRange, inconGridColor, on=0)
        return

    def __checkConfigValueTypes(self):
        # Make sure the values provided in the configuration section are the
        # correct type.
        # Later in the code are two methods devoted to creating error
        # messages. The error messages here could pop up in quite a large
        # number of different combinations, which makes capturing them in the
        # later methods very complex. Rather than do that and considering
        # that these error messages should never appear once the procedure
        # is correctly set up, I decided to leave them here. There always has
        # to be an exception. :-)
        import types
        message = ''
        badValues = False
        if not type(qpfTol) is types.FloatType:
            message = '%sThe "qpfTol" variable is not defined as a floating point value. Please contact your IFPS focal point to fix this.\n' % message
            badValues = True
        if not type(snowAmtTol) is types.FloatType:
            message = '%sThe "snowAmtTol" variable is not defined as a floating point value. Please contact your IFPS focal point to fix this.\n' % message
            badValues = True
        if not type(inconGridColor) is types.StringType:
            message = '%sThe "inconGridColor" variable is not defined as a string value. Please contact your IFPS focal point to fix this.\n' % message
            badValues = True
        if not type(tempGridColor) is types.StringType:
            message = '%sThe "tempGridColor" variable is not defined as a string value. Please contact your IFPS focal point to fix this.\n' % message
            badValues = True
        if not type(cwaEditArea) is types.StringType:
            message = '%sThe "cwaEditArea" variable is not defined as a string value. Please contact your IFPS focal point to fix this.\n' % message
            badValues = True
        if badValues:
            message = '%sYou will not be able to run the procedure until the problem is corrected.' % message
            # The next two commands are from SmartScript
            self.statusBarMsg(message, 'U')
            self.cancel()
        return

    def __checkLockedStatus(self, elementList):
        # There can be a significant difference between the values stored
        # in memory and the values returned from the database. This is because
        # when values are saved, the element's precision (as defined in
        # serverConfig.py/localConfig.py) is enforced. Values in memory do not
        # have the element's precision enforced; in fact, they have the
        # machine precision of the underlying data type.
        # At the beginning of each check method, a call to this method is
        # made to make sure the grids are saved. A check method will not run
        # if the grids it's to check are not saved. This method will return
        # a list of boolean values indicating if the elements are locked by me
        # and then if the elements are locked by other.
        # The lockedByMe and lockedByOther methods are from SmartScript
        lockedByMe = []
        lockedByOther = []
        for element in elementList:
            if self.lockedByMe(element, 'SFC'):
                lockedByMe.append(True)
            else:
                lockedByMe.append(False)
##            if self.lockedByOther(element, 'SFC'):
##                lockedByOther.append(True)
##            else:
##                lockedByOther.append(False)
            lockedByOther.append(False)
        return lockedByMe + lockedByOther

    def __getMsgSeverity(self, severity):
        # For calls to self.statusBarMsg where I intended the severity to be
        # something other than 'R', this method is now called to determine
        # what the severity should be. This procedure can be called from
        # another procedure in such a way as to suppress the pop-up type of
        # status bar messages. This is done by passing in a varDict with a
        # 'Quiet' key which evaluates to 'True'. For those situations, the
        # procedure defers to the calling program and turns any non 'R'
        # severities into 'R' severity. This allows the message to be
        # communicated still to the GFE session, but only via the 'Status'
        # line area of the GFE. When run interactively from the GFE, the
        # severity this procedure assigns to a message will be used.
        # This method is actually invoked in the call to statusBarMsg in
        # place of the severity string. As long as the entry for the severity
        # in statusBarMsg evaluates to a string type, statusBarMsg will be
        # 'happy'.
        if self._quiet:
            return 'R'
        return severity

    def __checkTC(self, element, gridTR):
        # The QPF6hr, SnowAmt6hr, and PoP12hr grids have specific time
        # constraints that the respective grids must adhere to. In other
        # words, it's not acceptable to this procedure for the QPF6hr grid,
        # for example, to be stretched to 12 hours long. This method makes
        # sure each of the grids exactly conforms to the time constraint
        # defintion. The method returns True if good, False if bad. If, for
        # some reason, the method gets called with some other element, the
        # method will return True.
        if element == 'QPF6hr' or element == 'SnowAmt6hr':
            startHourTup = (0, 6, 12, 18)
            goodDuration = 6 * 3600
        elif element == 'PoP12hr':
            startHourTup = (0, 12)
            goodDuration = 12 * 3600
        else:
            return True
        
        if gridTR.startTime().hour in startHourTup and \
           gridTR.duration() == goodDuration:
            return True
        return False

    def _runSnowAmt6hrQPF6hrCheck(self, timeRange):
        # This method implements the check that if SnowAmt6hr >= 0.5, then
        # QPF6hr must be >= 0.01.

        # If there are locks, post urgent messages and return from the method.
        snowLockMe, qpfLockMe, snowLockOther, qpfLockOther = \
                    self.__checkLockedStatus(['SnowAmt6hr', 'QPF6hr'])
        if snowLockMe or qpfLockMe or snowLockOther or qpfLockOther:
            # Something's locked, create messages.
            self._makeLockMsgs(
                snowLockMe, qpfLockMe, snowLockOther, qpfLockOther,
                'SnowAmt6hr', 'QPF6hr', 'snowLockMe', 'qpfLockMe',
                'snowLockOther', 'qpfLockOther', 'SnowAmt6hr/QPF6hr')
            return

        # Make sure there are actually SnowAmt6hr grids in the time range.
        # The self.getGridInfo command will return an empty list if there
        # are no grids in the time range. This is more efficient than using
        # self.getGrids with mode='First' and noDataError=0.
        # The getGridInfo method is from SmartScript
        snowAmtInfoList = self.getGridInfo(
            'Fcst', 'SnowAmt6hr', 'SFC', timeRange)
        if snowAmtInfoList == []:
            message = self._getMsg(
                'noGrids', element='SnowAmt6hr', timeRange=timeRange,
                method='SnowAmt6hr/QPF6hr')
            # The statusBarMsg method is from SmartScript
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        # One might ask why I don't just return the result of self.getGrids
        # to a variable and iterate over that. I'm trying to minimize the
        # memory footprint of the procedure. Reading all the grids into a
        # variable could be a fairly large memory hit. I believe the construct
        # below only reads one SnowAmt6hr grid at a time into memory, the one
        # that's being checked. (But I can't find the reference that supports
        # my belief.) By using the cache=0 switch on all the self.getGrids
        # command, I prevent the GFE from saving the grids into memory for me.
        # (At least, that's what I think the cache=0 switch does. The
        # SmartScript documentation is a little vague on this point.)
        # The Python builtin command enumerate loops over an iterable object
        # and returns a 2-tuple containing the current index of the
        # iteration and the object at that index. In cases where I need
        # both the index and the object, I think this construct is more
        # elegant than:
        # for i in xrange(len(iterableObject)):
        #     object = iterableObject[i]
        for snowAmtIndex, snowAmtGrid in enumerate(self.getGrids(
            'Fcst', 'SnowAmt6hr', 'SFC', timeRange, mode='List', cache=0)):
            gridTR = snowAmtInfoList[snowAmtIndex].gridTime()
            # Check to make sure the grid meets it's time constraints.
            if not self.__checkTC('SnowAmt6hr', gridTR):
                message = self._getMsg(
                    'badTC', element='SnowAmt6hr', timeRange=gridTR)
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # around is from SmartScript
            snowAmtGrid = around(snowAmtGrid, 1)
            # The greater_equal method is from Numeric. For the given array
            # and threshold, a new array of the same dimensions as the input
            # array is returned. The new array has the value 1 where the
            # input array was greater than or equal to the threshold and
            # has the value 0 elsewhere.
            # The getGridInfo method is from SmartScript
            halfInchMask = greater_equal(snowAmtGrid, 0.5 - snowAmtTol)
            qpfInfoList = self.getGridInfo('Fcst', 'QPF6hr', 'SFC', gridTR)
            # There should always be more QPF6hr grids than SnowAmt6hr grids,
            # so if qpfInfoList is empty, then there are missing QPF6hr
            # grids. Otherwise, qpfInfoList will have length 1 because
            # SnowAmt6hr and QPF6hr have the same time constrain. However,
            # the QPF6hr grid that overlaps the SnowAmt6hr grid will still
            # need to be checked to make sure it hasn't been stretched.
            if qpfInfoList == []:
                message = self._getMsg(
                    'noGrids', element='QPF6hr', timeRange=gridTR,
                    method='SnowAmt6hr/QPF6hr')
                # The statusBarMsg is from SmartScript
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # I don't need the noDataError=0 in the self.getGrids call
            # because if there were no grids in the gridTR, the previous
            # if block would have caught that.
            # The getGrids method is from SmartScript
            qpfGrid = self.getGrids(
                'Fcst', 'QPF6hr', 'SFC', gridTR, mode='First', cache=0)
            if not self.__checkTC('QPF6hr', gridTR):
                message = self._getMsg(
                    'badTC', element='QPF6hr', timeRange=gridTR)
                # The statusBarMsg method is from SmartScrtipt
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # If we get here, then we have a SnowAmt6hr grid and a QPF6hr
            # grid which meet their time constraints and are ready to be
            # compared.
            # around is from SmartScript
            qpfGrid = around(qpfGrid, 2)
            # The less method is from Numeric. It behaves analogously to
            # the greater_equal method described above using less than for
            # the comparison.
            qpfMask = less(qpfGrid, 0.01 - qpfTol)
            # The following is the "truth" table for the logical
            # comparison.
            # SnowAmt6hr >= 0.5, 1; SnowAmt6hr < 0.5, 0
            # QPF6hr < 0.01, 1; QPF6hr >= 0.01, 0
            # SnowAmt6hr >= 0.5 (1) and QPF6hr < 0.01 (1) = 1 (Bad result)
            # SnowAmt6hr >= 0.5 (1) and QPF6hr >= 0.01 (0) = 0 (Good result)
            # SnowAmt6hr < 0.5 (0) and QPF6hr < 0.01 (1) = 0 (Good result)
            # SnowAmt6hr < 0.5 (0) and QPF6hr >= 0.01 (0) = 0 (Good result)
            # The logical_and method is from Numeric. A logical and comparison
            # results in a "True" value if both compared elements are "True".
            # Otherwise, the result is "False".
            consistMask = logical_and(halfInchMask, qpfMask)
            # Now, apply the CWA mask. There's an assumption here that
            # all offices will use a mask and provide a valid one, which
            # means this step does something meaningful. If that assumption
            # does not hold, then the next statement doesn't actually
            # change anything, even though each and every grid point has a
            # comparison check made.
            # where is from Numeric. The first argument is a mask.
            # The second argument is/are the value/values to use at the
            # array points where the mask is one. The third argument
            # is/are the value/values to use at the array points
            # where the mask is zero. For this comparison, I want
            # the values of consistMask where self.cwaMask is one and
            # I want the "good result", which is zero, where
            # self.cwaMask is zero.
            consistMask = where(self.cwaMask, consistMask, 0)
            # The ravel and sometrue methods are from Numeric.
            if sometrue(ravel(consistMask)):
                # The ravel method reduces the rank of the array by one.
                # Since we had a 2-d array, the ravel function creates a
                # 1-d array (a vector) such that reading the 2-d array from
                # left-to-right, top-to-bottom returns the same values as
                # reading the 1-d array from left-to-right. The sometrue
                # method performs a logical or on subsequent element pairs
                # in the 1-d array and returns the final result. If
                # there are inconsistencies, the result will be 1.
                # The highlightGrids method is from SmartScript.
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt6hr', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'QPF6hr', 'SFC', gridTR, inconGridColor)
                # The createGrid method is from SmartScript
                self.createGrid(
                    'Fcst', 'SnowAmt6hrQPF6hr', 'SCALAR',
                    consistMask, gridTR,
                    descriptiveName='SnowAmt6hrQPF6hrInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt6hrQPF6hr', 'SFC', gridTR,
                        tempGridColor)
                self.inconsistent = True
        # While not required, I like to terminate my methods with a return
        # statement to make it clear this is where the method ends.
        return

    def _runSnowAmt6hrWxCheck(self, timeRange):
        # This implements the check that if SnowAmt6hr >= 0.1, then the Wx grid
        # must contain S, SW, or IP, regardless of whether or not there is
        # any freezing or liquid types. Finally, the check does not look at
        # anything other than the Wx type. In other words, the check will be
        # okay if SnowAmt != 0 and Wx has Chc:S:- or Def:SW:-- or Lkly:S:+.

        # If there are locks, post urgent messages and return from the method.
        snowLockMe, wxLockMe, snowLockOther, wxLockOther = \
                    self.__checkLockedStatus(['SnowAmt6hr', 'Wx'])
        if snowLockMe or wxLockMe or snowLockOther or wxLockOther:
            # Something's locked, create messages.
            self._makeLockMsgs(
                snowLockMe, wxLockMe, snowLockOther, wxLockOther,
                'SnowAmt6hr', 'Wx', 'snowLockMe', 'wxLockMe',
                'snowLockOther', 'wxLockOther', 'SnowAmt6hr/Wx')
            return

        # Make sure there are actually SnowAmt6hr grids in the time range.
        # The getGridInfo method is from SmartScript.
        snowAmtInfoList = self.getGridInfo(
            'Fcst', 'SnowAmt6hr', 'SFC', timeRange)
        if snowAmtInfoList == []:
            message = self._getMsg(
                'noGrids', element='SnowAmt6hr', timeRange=timeRange,
                method='SnowAmt6hr/Wx')
            # The statusBarMsg method is from SmartScript.
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        for snowAmtIndex, snowAmtGrid in enumerate(self.getGrids(
            'Fcst', 'SnowAmt6hr', 'SFC', timeRange, mode='List', cache=0)):
            gridTR = snowAmtInfoList[snowAmtIndex].gridTime()
            # Make sure the snowAmtGrid meets the time constraints.
            if not self.__checkTC('SnowAmt6hr', gridTR):
                message = self._getMsg(
                    'badTC', element='SnowAmt6hr', timeRange=gridTR)
                # The statusBarMsg method is from SmartScript.
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # around is from SmartScript
            snowAmtGrid = around(snowAmtGrid, 1)
            # The greater_equal method is from Numeric.
            # The getGridInfo method is from SmartScript.
            nonZeroMask = greater_equal(snowAmtGrid, 0.1 - snowAmtTol)
            wxInfoList = self.getGridInfo('Fcst', 'Wx', 'SFC', gridTR)
            # Check for Wx grid in gridTR
            if wxInfoList == []:
                message = self._getMsg(
                    'noGrids', element='Wx', timeRange=gridTR,
                    method='SnowAmt6hr/Wx')
                # The statusBarMsg method is from SmartScript
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # Now check the overlapping Wx grids. Initialize a totally
            # inconsistent grid.
            # ones is from Numeric
            inconsistGrid = ones(nonZeroMask.shape, int)
            for wxIndex, wxGrid in enumerate(self.getGrids(
                'Fcst', 'Wx', 'SFC', gridTR, mode='List', cache=0)):
                # The wxMask method is from SmartScript
                sMask = self.wxMask(wxGrid, ':S:')
                swMask = self.wxMask(wxGrid, ':SW:')
                ipMask = self.wxMask(wxGrid, ':IP:')
                # The logical_or method is from Numeric. For the two input
                # arrays, if both values are "False", then the result is
                # "False". Otherwise, the result is "True".
                snowMask = logical_or(logical_or(sMask, swMask), ipMask)
                # I don't need these arrays any longer. Delete them to free
                # up the memory they use.
                del (sMask, swMask, ipMask)
                # The where method is from Numeric
                wxMask = where(snowMask, 0, 1)
                # "Truth" table for the logical comparison follows
                # SnowAmt6hr >= 0.1, 1; SnowAmt6hr < 0.1, 0
                # Wx has S, SW, or IP, 0; Wx doesn't have S, SW, or IP, 1
                # SnowAmt6hr >= 0.1 (1) and Wx has (0) = 0 (Good result)
                # SnowAmt6hr >= 0.1 (1) and Wx doesn't have (1) = 1 (Bad result)
                # SnowAmt6hr < 0.1 (0) and Wx has (0) = 0 (Good result)
                # SnowAmt6hr < 0.1 (0) and Wx doesn't have (1) = 0 (Good result)
                #
                # The logical_and, where, sometrue, and ravel methods are all
                # from Numeric.
                consistMask = logical_and(nonZeroMask, wxMask)
                consistMask = where(self.cwaMask, consistMask, 0)
                # Update inconsistGrid to be the current state of the
                # inconsistencies.
                inconsistGrid = logical_and(inconsistGrid, consistMask)
                if not sometrue(ravel(inconsistGrid)):
                    # There were no longer any inconsistencies between
                    # SnowAmt6hr and Wx.
                    break
            else:
                # This block will only execute if the for loop runs to
                # completion, i.e., the break statement is not executed.
                # So, if we get here, we have inconsistencies and need to
                # highlight the appropriate grids.
                # The highlightGrids method is from SmartScript.
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt6hr', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'Wx', 'SFC', gridTR, inconGridColor)
                # The createGrid method is from SmartScript
                self.createGrid(
                    'Fcst', 'SnowAmt6hrWx', 'SCALAR', inconsistGrid, gridTR,
                    descriptiveName='SnowAmt6hrWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt6hrWx', 'SFC', gridTR,
                        tempGridColor)
                self.inconsistent = True            
        return

    def _runQPF6hrPoPCheck(self, timeRange):
        # This method implements the check that if any QPF6hr grid is non zero
        # then one of the corresponding floating PoP grids must also be non
        # zero.

        # If there are locks, post urgent messages and return from the method.
        qpfLockMe, popLockMe, qpfLockOther, popLockOther = \
                    self.__checkLockedStatus(['QPF6hr', 'PoP'])
        if qpfLockMe or popLockMe or qpfLockOther or popLockOther:
            # Something's locked, create messages.
            self._makeLockMsgs(
                qpfLockMe, popLockMe, qpfLockOther, popLockOther,
                'QPF6hr', 'PoP', 'qpfLockMe', 'popLockMe',
                'qpfLockOther', 'popLockOther', 'QPF6hr/PoP')
            return

        # The getGridInfo method is from SmartScript.
        qpfInfoList = self.getGridInfo('Fcst', 'QPF6hr', 'SFC', timeRange)
        # Make sure there are actually QPF6hr grids in the time range.
        if qpfInfoList == []:
            message = self._getMsg(
                'noGrids', element='QPF6hr', timeRange=timeRange,
                method='QPF6hr/PoP')
            # The statusBarMsg method is from SmartScript.
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        for qpfIndex, qpfGrid in enumerate(self.getGrids(
            'Fcst', 'QPF6hr', 'SFC', timeRange, mode='List', cache=0)):
            gridTR = qpfInfoList[qpfIndex].gridTime()
            # Check the QPF6hr grid time constraints
            if not self.__checkTC('QPF6hr', gridTR):
                message = self._getMsg(
                    'badTC', element='QPF6hr', timeRange=gridTR)
                # The statusBarMsg method is from SmartScript.
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # around is from SmartScript
            qpfGrid = around(qpfGrid, 2)
            # The greater_equal method is from Numeric. The getGrids method
            # is from SmartScript.
            qpfNonZeroMask = greater_equal(qpfGrid, 0.01 - qpfTol)
            popGrid = self.getGrids(
                'Fcst', 'PoP', 'SFC', gridTR, mode='Max', noDataError=0,
                cache=0)
            # Since I don't need to loop over the PoP grids, just get their
            # max, I don't need to call getGridInfo like in other methods.
            # With noDataError=0 in the getGrids call, if there are no grids,
            # then the special Python value None will be returned. So, I can
            # just check that to see if all the PoP grids are missing. If
            # there were a gap in the PoP grids, that would not be caught.
            # But, no one's PoP grids should ever have a gap in them, right?
            if popGrid == None:
                message = self._getMsg(
                    'noGrids', element='PoP', timeRange=gridTR,
                    method='QPF6hr/PoP')
                # The statusBarMsg method is from SmartScript.
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # The equal method is from Numeric.
            popZeroMask = equal(popGrid, 0)
            # popZeroMask = 1 if PoP = 0; popZeroMask = 0 if PoP != 0
            # qpfNonZeroMask = 1 if QPF6hr > 0; qpfNonZeroMask = 0 if QPF6hr = 0
            # PoP = 0 (1) and QPF6hr = 0 (0) => 0 (Good result)
            # PoP != 0 (0) and QPF6hr = 0 (0) => 0 (Good result)
            # PoP != 0 (0) and QPF6hr > 0 (1) => 0 (Good result)
            # PoP = 0 (1) and QPF6hr > 0 (1) => 1 (Bad result)
            #
            # The logical_and, where, sometrue, and ravel methods are all
            # from Numeric.
            consistMask = logical_and(qpfNonZeroMask, popZeroMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            if sometrue(ravel(consistMask)):
                # The good result is if the logical_and returns zeros
                # for every grid point, that is "none true". So, if
                # the sometrue method evaluates True, there are
                # inconsistencies.
                # The createGrid and highlightGrids methods are from
                # SmartScript.
                self.createGrid(
                    'Fcst', 'QPF6hrPoP', 'SCALAR',
                    consistMask, gridTR,
                    descriptiveName='QPF6hrPoPInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF6hrPoP', 'SFC', gridTR,
                        tempGridColor)
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF6hr', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'PoP', 'SFC', gridTR, inconGridColor)
                self.inconsistent = True
        return

    def _runQPF6hrWxCheck(self, timeRange):
        # This method implements the check that if QPF6hr non zero, then the
        # corresponding Wx grids must contain a precipitable Wx type. Note:
        # the method only checks the Wx type, no cov/prob, no inten, etc.

        # If there are locks, post urgent messages and return from the method.
        qpfLockMe, wxLockMe, qpfLockOther, wxLockOther = \
                    self.__checkLockedStatus(['QPF6hr', 'Wx'])
        if qpfLockMe or wxLockMe or qpfLockOther or wxLockOther:
            # Something's locked, create messages.
            self._makeLockMsgs(
                qpfLockMe, wxLockMe, qpfLockOther, wxLockOther,
                'QPF6hr', 'Wx', 'qpfLockMe', 'wxLockMe',
                'qpfLockOther', 'wxLockOther', 'QPF6hr/Wx')
            return

        # The getGridInfo method is from SmartScript.
        qpfInfoList = self.getGridInfo('Fcst', 'QPF6hr', 'SFC', timeRange)
        # Make sure there are actually QPF6hr grids in the time range.
        if qpfInfoList == []:
            message = self._getMsg(
                'noGrids', element='QPF6hr', timeRange=timeRange,
                method='QPF6hr/Wx')
            # The statusBarMsg method is from SmartScript.
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        for qpfIndex, qpfGrid in enumerate(self.getGrids(
            'Fcst', 'QPF6hr', 'SFC', timeRange, mode='List', noDataError=0,
            cache=0)):
            gridTR = qpfInfoList[qpfIndex].gridTime()
            # Make sure the QPF6hr grid meets the time constraints
            if not self.__checkTC('QPF6hr', gridTR):
                message = self._getMsg(
                    'badTC', element='QPF6hr', timeRange=gridTR)
                # The statusBarMsg method is from SmartScript.
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # around is from SmartScript
            qpfGrid = around(qpfGrid, 2)
            # The greater_equal method is from Numeric.
            qpfNonZeroMask = greater_equal(qpfGrid, 0.01 - qpfTol)
            # The getGridInfo method is from SmartScript.
            wxInfoList = self.getGridInfo('Fcst', 'Wx', 'SFC', gridTR)
            # Make sure there are Wx grids overlapping the QPF6hr grid
            if wxInfoList == []:
                message = self._getMsg(
                    'noGrids', element='Wx', timeRange=gridTR,
                    method='QPF6hr/Wx')
                # The statusBarMsg method is from SmartScript.
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # Initialize a totally inconsistent grid and loop over the
            # overlapping Wx grids.
            inconsistGrid = ones(qpfNonZeroMask.shape, int)
            for wxIndex, wxGrid in enumerate(self.getGrids(
                'Fcst', 'Wx', 'SFC', gridTR, mode='List', noDataError=0,
                cache=0)):
                # The wxMask method is from SmartScript.
                sMask = self.wxMask(wxGrid, ':S:')
                swMask = self.wxMask(wxGrid, ':SW:')
                ipMask = self.wxMask(wxGrid, ':IP:')
                # The logical_or method is from Numeric.
                snowMask = logical_or(logical_or(sMask, swMask), ipMask)
                # I don't need these three grids any longer, so delete them
                # and free up their memory.
                del (sMask, swMask, ipMask)
                rMask = self.wxMask(wxGrid, ':R:')
                rwMask = self.wxMask(wxGrid, ':RW:')
                lMask = self.wxMask(wxGrid, ':L:')
                zlMask = self.wxMask(wxGrid, ':ZL:')
                zrMask = self.wxMask(wxGrid, ':ZR:')
                # The logical_or method is from Numeric.
                rainMask = logical_or(
                    rMask, logical_or(
                        rwMask, logical_or(
                            lMask, logical_or(zlMask, zrMask))))
                # Again, I don't need these grids any longer, so delete them
                # and free up their memory.
                del (rMask, rwMask, lMask, zlMask, zrMask)
                precipMask = logical_or(snowMask, rainMask)
                del (snowMask, rainMask)
                wxMask = where(precipMask, 0, 1)
                # QPF6hr >= 0.01, 1; QPF6hr < 0.01, 0
                # Wx has precip, 0; Wx doesn't have precip, 1
                # QPF6hr >= 0.01 (1) and Wx has (0) = 0 (Good result)
                # QPF6hr >= 0.01 (1) and Wx doesn't have (1) = 1 (Bad result)
                # QPF6hr < 0.01 (0) and Wx has (0) = 0 (Good result)
                # QPF6hr < 0.01 (0) and Wx doesn't have (1) = 0 (Good result)
                #
                # The logical_and, where, sometrue, and ravel methods are all
                # from Numeric.
                consistMask = logical_and(qpfNonZeroMask, wxMask)
                consistMask = where(self.cwaMask, consistMask, 0)
                # Update the inconsistGrid to the current state of the
                # inconsistencies.
                inconsistGrid = logical_and(inconsistGrid, consistMask)
                if not sometrue(ravel(inconsistGrid)):
                    # There were no longer any inconsistencies between the Wx
                    # grids and the QPF6hr grid.
                    break
            else:
                # This block will only execute if the for loop runs to
                # completion, i.e., the break statement is not executed.
                # So, if we get here, we have inconsistencies and need to
                # highlight the appropriate grids.
                # The highlightGrids method is from SmartScript.
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF6hr', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'Wx', 'SFC', gridTR, inconGridColor)

                # The createGrid method is from SmartScript.
                self.createGrid(
                    'Fcst', 'QPF6hrWx', 'SCALAR', inconsistGrid, gridTR,
                    descriptiveName='QPF6hrWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF6hrWx', 'SFC', gridTR,
                        tempGridColor)
                self.inconsistent = True            
        return

    def _runPoP12hrQPF6hrCheck(self, timeRange):
        # This method implements the check that if any PoP12hr grid
        # is >= 50%, then at least one of the two corresponding QPF6hr grids
        # must be non zero.

        # If there are locks, post urgent messages and return from the method.
        qpfLockMe, popLockMe, qpfLockOther, popLockOther = \
                    self.__checkLockedStatus(['QPF6hr', 'PoP12hr'])
        if qpfLockMe or popLockMe or qpfLockOther or popLockOther:
            # Something's locked, create messages.
            self._makeLockMsgs(
                qpfLockMe, popLockMe, qpfLockOther, popLockOther,
                'QPF6hr', 'PoP12hr', 'qpfLockMe', 'popLockMe',
                'qpfLockOther', 'popLockOther', 'PoP12hr/QPF6hr')
            return

        # The getGridInfo method is from SmartScript.
        # Make sure there are actually PoP12hr grids in the time range
        popInfoList = self.getGridInfo('Fcst', 'PoP12hr', 'SFC', timeRange)
        if popInfoList == []:
            message = self._getMsg(
                'noGrids', element='PoP12hr', timeRange=timeRange,
                method='PoP12hr/QPF6hr')
            # The statusBarMsg method is from SmartScript.
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        # This is the one check where it will almost always be the case that
        # the "controlling" element (PoP12hr) will extend later in time than
        # the "checked" element (QPF6hr). We don't want a lot of annoying
        # pop-up messages for missing QPF6hr grids for that case. I will
        # determine the end time of the last QPF6hr grid, adjust it back to the
        # nearest 00 or 12 UTC time to align it with the PoP12hr grid, and then
        # check the end time of the last PoP12hr grid. If the end time for
        # the QPF6hr grid is earlier, I will adjust the timeRange variable
        # inside this method to end with the QPF6hr grids.
        # The getGridInfo method is from SmartScript.
        qpfInfoList = self.getGridInfo('Fcst', 'QPF6hr', 'SFC', timeRange)
        # Make sure there are actually QPF6hr grids in the time range
        if qpfInfoList == []:
            message = self._getMsg(
                'noGrids', element='QPF6hr', timeRange=timeRange,
                method='PoP12hr/QPF6hr')
            self.statusBarMsg(message, self.__getMsgSeverity('U'))
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        lastQPFTR = qpfInfoList[-1].gridTime()
        qpfEndTime = lastQPFTR.endTime().unixTime()
        qpfEndHr = lastQPFTR.endTime().hour
        qpfEndTime -= ((qpfEndHr % 12) * 3600)
        popEndTime = popInfoList[-1].gridTime().endTime().unixTime()
        if popEndTime > qpfEndTime:
            # Adjust time range to QPF6hr time range
            qpfStartTime = qpfInfoList[0].gridTime().startTime().unixTime()
            qpfDuration = (qpfEndTime - qpfStartTime) / 3600
            offset = (qpfStartTime - \
                      self.timeRange0_1.startTime().unixTime()) / 3600
            timeRange = self.createTimeRange(
                offset, offset+qpfDuration, 'Zulu')
            message = self._getMsg(
                'changeTR', method='PoP12hr/QPF6hr', timeRange=timeRange)
            self.statusBarMsg(message, 'R')
            # Because the timeRange has changed, popInfoList needs to be
            # updated. qpfInfoList will be updated later.
            # The getGridInfo method is from SmartScript.
            popInfoList = self.getGridInfo('Fcst', 'PoP12hr', 'SFC', timeRange)
            # Now, it's possible there were gaps in the PoP12hr grids and the
            # new time range spans a gap. So, we have to check for grid
            # existence again.
            if popInfoList == []:
                message = self._getMsg(
                    'noGrids', element='PoP12hr', timeRange=timeRange,
                    method='PoP12hr/QPF6hr')
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                return
        for popIndex, popGrid in enumerate(self.getGrids(
            'Fcst', 'PoP12hr', 'SFC', timeRange, mode='List', cache=0)):
            gridTR = popInfoList[popIndex].gridTime()
            qpfInfoList = self.getGridInfo('Fcst', 'QPF6hr', 'SFC', gridTR)
            # Check for existence of QPF6hr grids in the time range.
            if qpfInfoList == []:
                message = self._getMsg(
                    'noGrids', element='QPF6hr', timeRange=gridTR,
                    method='PoP12hr/QPF6hr')
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # Check the PoP12hr time constraints
            if not self.__checkTC('PoP12hr', gridTR):
                message = self._getMsg(
                    'badTC', element='PoP12hr', timeRange=gridTR)
                self.statusBarMsg(message, self.__getMsgSeverity('U'))
                continue
            # The greater_equal method is from Numeric.
            pop50Mask = greater_equal(popGrid, 50)
            # Initialize a totally inconsistent grid.
            inconsistGrid = ones(pop50Mask.shape, int)
            for qpfIndex, qpfGrid in enumerate(self.getGrids(
                'Fcst', 'QPF6hr', 'SFC', gridTR, mode='List', cache=0)):
                qpfGridTR = qpfInfoList[qpfIndex].gridTime()
                # Check the QPF6hr time contraints
                if not self.__checkTC('QPF6hr', qpfGridTR):
                    message = self._getMsg(
                        'badTC', element='QPF6hr', timeRange=qpfGridTR)
                    self.statusBarMsg(message, self.__getMsgSeverity('U'))
                    # If one of the QPF6hr grids has a bad time constraint,
                    # then I only have one QPF6hr grid. Time to break.
                    break
                # around is from SmartScript
                qpfGrid = around(qpfGrid, 2)
                # The less method is from Numeric.
                qpfMask = less(qpfGrid, 0.01 - qpfTol)
                # The following is the "truth" table for the logical
                # comparison.
                # PoP12hr >= 50, 1; PoP12hr < 50, 0
                # QPF6hr < 0.01, 1; QPF6hr >= 0.01, 0
                # PoP12hr >= 50 (1) and QPF6hr < 0.01 (1) = 1 (Bad result)
                # PoP12hr >= 50 (1) and QPF6hr >= 0.01 (0) = 0 (Good result)
                # PoP12hr < 50 (0) and QPF6hr < 0.01 (1) = 0 (Good result)
                # PoP12hr < 50 (0) and QPF6hr >= 0.01 (0) = 0 (Good result)
                # logical_and is from Numeric
                # The logical_and, where, sometrue, and ravel methods are all
                # from Numeric.
                consistMask = logical_and(pop50Mask, qpfMask)
                consistMask = where(self.cwaMask, consistMask, 0)
                # Update the inconsistentGrid with the state of the
                # inconsistencies.
                inconsistGrid = logical_and(inconsistGrid, consistMask)
                # ravel and sometrue are from Numeric
                if not sometrue(ravel(inconsistGrid)):
                    # There were no longer any inconsistencies between the
                    # QPF6hr grids and PoP12hr grid.
                    break
            else:
                # This else block will only execute if the for loop exits
                # "naturally", i.e., the above break statement didn't execute.
                # This means there were inconsistencies.
                # The highlightGrids method is from SmartScript.
##                lin_index = nonzero(ravel(inconsistGrid))
##                sh = list(shape(inconsistGrid))
##                sh.reverse()
##                new_index = zeros((len(lin_index), len(sh)))
##                mod = zeros(len(lin_index))
##                for j in arange(len(lin_index)):
##                    count = len(sh)
##                    for i in sh:
##                        lin_index[j], mod[j] = divmod(lin_index[j], i)
##                        count = count - 1
##                        new_index[j, count] = mod[j]
##                print new_index
##                print popGrid[0,0], qpfGrid[0,0]
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'PoP12hr', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'QPF6hr', 'SFC', gridTR, inconGridColor)
                self.createGrid(
                    'Fcst', 'PoP12hrQPF6hr', 'SCALAR', inconsistGrid, gridTR,
                    descriptiveName='PoP12hrQPF6hrInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'PoP12hrQPF6hr', 'SFC', gridTR,
                        tempGridColor)
                self.inconsistent = True
        return

    def _makeLockMsgs(
        self, lockMe1, lockMe2, lockOther1, lockOther2, element1, element2,
        lockMeKey1, lockMeKey2, lockOtherKey1, lockOtherKey2, method):
        # As I went through the five check methods, I noted that this code
        # was basically being repeated over and over again. So, here's a
        # case where I took the repeated code, made it abstract, and turned
        # it into a callable method. Now where each of the five methods had
        # about 20 lines of code to do this, they now have only 5, which is
        # a fairly substantial decrease. The trade-off is in somewhat
        # lessened code readability because of having to jump from method
        # to method to track the code.
        # Below, I assign the call to _getMsg to a temporary variable
        # called message for readability. Embedding the call to _getMsg
        # in the call to statusBarMsg makes those calls much harder to
        # follow.
        # The statusBarMsg method is from SmartScript.
        if lockMe1:
            message = self._getMsg(
                lockMeKey1, method=method, element=element1)
            self.statusBarMsg(
                message, self.__getMsgSeverity('U'))
        if lockMe2:
            message = self._getMsg(
                lockMeKey2, method=method, element=element2)
            self.statusBarMsg(
                message, self.__getMsgSeverity('U'))
        if lockOther1:
            message = self._getMsg(
                lockOtherKey1, method=method, element=element1)
            self.statusBarMsg(
                message, self.__getMsgSeverity('U'))
        if lockOther2:
            message = self._getMsg(
                lockOtherKey2, method=method, element=element2)
            self.statusBarMsg(
                message, self.__getMsgSeverity('U'))
        return

    def _msgDict(self):
        # Since I seem to be incapable of writing concise error messages,
        # I decided to capture the error messages in a separate method.
        # Because I tend to be verbose, I like to use triple quoted strings
        # for messages since this helps minimize (not eliminate) characters
        # which extend beyond the right margin of the editor window. But triple
        # quoted strings disrupt the indentation patterns of the code, making
        # the code harder to read. By capturing all the triple quoted strings
        # in a separate method, the indentation issue is mitigated. Doing this
        # does add some complexity to the code, but it's a fair trade-off in my
        # mind. This method is just a dictionary of all the error messages,
        # which may contain string formatting code place holders. Another
        # method, _getMsg, will look up the message boiler plates here and have
        # the logic to correctly pass the needed variables to the string
        # formatting codes.
        return {
'complete': 'NDFD_QPF_Checks complete.',
'0_240':
'''The NDFD_QPF_Checks procedure did not run over the 0 to 240 hour time period,
it ran over %s. This may be what you desired.''',
'cwaMask':
'''The procedure was not able to use the CWA edit area, %s,
provided in the configuration. You should inform the person responsible for procedures of
this problem. The procedure ran over the whole domain.''',
'incon': 'NDFD_QPF_Checks complete. Inconsistencies found!',
'snowLockMe':
'''You have the %s grid locked. Please save the %s grid. The %s
check was not run.''',
'qpfLockMe':
'''You have the %s grid locked. Please save the %s grid. The %s check was
not run.''',
'snowLockOther':
'''Another user has the %s grid locked. Please have that user save the %s grid. The
%s check was not run.''',
'qpfLockOther':
'''Another user has the %s grid locked. Please have that user save the %s grid. The
%s check was not run.''',
'wxLockMe':
'''You have the %s grid locked. Please save the %s grid. The %s check was not run.''',
'wxLockOther':
'''Another user has the %s grid locked. Please have that user save the %s grid. The %s
check was not run.''',
'popLockMe':
'''You have the %s grid locked. Please save the %s grid. The %s check was
not run.''',
'popLockOther':
'''Another user has the %s grid locked. Please have that user save the %s grid. The
%s check was not run.''',
'noGrids':
'''There are no %s grids in the time range, %s.
The %s Check skipped the time range.''',
'changeTR':
'''The time range of the %s check was changed to ensure the PoP12hr grid is not checked
beyond the time of the last QPF6hr grid. The time range used was %s.''',
'badTC':
'''A %s grid has the following time range: %s,
which does not adhere to the time constraint requirement. This %s grid has not been consistency checked at all.
Please fix the grid and re-run the procedure.''',
}

    def _getMsg(self, key, timeRange=None, method=None, element=None):
        # This method looks up the needed error message by passing key to
        # _msgDict. The other parameters, if provided, are used to expand
        # the embedded string formatting codes. The resulting message is
        # passed back to the caller.
        message = self._msgDict().get(key, '')
        if key == '0_240':
            message = message % str(timeRange)
            return message
        if key == 'cwaMask':
            message = message % cwaEditArea
            return message
        if key == 'incon':
            if inconGridColor and tempGridColor:
                message = '%s Inconsistent grids highlighted %s.\nTemporary grids highlighted %s.' % (message, inconGridColor, tempGridColor)
                return message
            if inconGridColor:
                message = '%s Inconsistent grids highlighted %s.' % (
                    message, inconGridColor)
                return message
            if tempGridColor:
                message = '%s Temporary grids highlighted %s.' % (
                    message, tempGridColor)
                return message
            return message
        if key == 'snowLockMe' or key == 'qpfLockMe' or key == 'wxLockMe' or \
           key == 'snowLockOther' or key == 'qpfLockOther' or \
           key == 'wxLockOther' or key == 'popLockMe' or \
           key == 'popLockOther':
            message = message % (element, element, method)
            return message
        if key == 'noGrids':
            message = message % (element, str(timeRange), method)
            return message
        if key == 'changeTR':
            message = message % (method, str(timeRange))
            return message
        if key == 'badTC':
            message = message % (element, str(timeRange), element)
            return message
        # If for some reason the key look-up failed, then the message
        # variable will be the empty string, '', and this will be returned.
        # Since the calling method expects a string to be returned, I must
        # ensure that this happens.
        return message

    def execute(self, timeRange, varDict):
        # Are we in quiet mode? The variableList above does NOT have a
        # variable for 'Quiet', by design. When run interactively, some error
        # conditions will generate pop-up messages. If an office decides to
        # run this procedure as part of an over-arching check procedure, they
        # can choose to turn the pop-up messages into routine messages by
        # passing in a varDict with a 'Quiet' key that evaluates to 'True'.
        # In Python, you can use the 'get' method on a dictionary to test
        # for the existence of a key. If the key exists, then 'get' returns
        # the value. If the key doesn't exist, then the second argument of
        # the 'get' call is returned. If you don't provide a second argument
        # to the 'get' call and the key is not found, then None is returned.
        # As you can see below, my call to 'get' will return False if the key
        # 'Quiet' is not in varDict. The 'get' method allows you to avoid
        # constructs like:
        # try:
        #     self._quiet = varDict['Quiet']
        # except KeyError:
        #     self._quiet = False
        # I don't know for sure, but I'd be willing to bet that the 'get'
        # method is just a wrapper to the 'try/except' construct very
        # similar to the one demonstrated.
        self._quiet = varDict.get('Quiet', False)
##        self._quiet = True

        # Make sure the configuration values are the correct types.
        self.__checkConfigValueTypes()
        # createTimeRange is from SmartScript
        timeRange0_240 = self.createTimeRange(0, 240, 'Zulu')
        checkCleanup = varDict.get('Check_Cleanup', 'Check')
        self.__cleanup(timeRange0_240)
        if checkCleanup == 'Cleanup':
            self.statusBarMsg(self._getMsg('complete'), 'R')
            self.cancel()
        elementList = (
            'PoP', 'QPF', 'Wx', 'SnowAmt', 'QPF6hr', 'SnowAmt6hr', 'PoP12hr')
        for element in elementList:
            self.loadParm('Fcst', element, 'SFC')
        if timeRange.endTime().unixTime() - timeRange.startTime().unixTime() < \
           3600: # No time range selected, use 0 to 240 hour range
            timeRange = timeRange0_240

        # If the user has a time range swept out, send an informational
        # message.
        if (timeRange.startTime().unixTime() != \
            timeRange0_240.startTime().unixTime()) or \
           (timeRange.endTime().unixTime() != \
            timeRange0_240.endTime().unixTime()) or \
           (timeRange.duration() != timeRange0_240.duration()):
            # What the incredibly dense expression in the above if statement
            # does is compare the start and end of the time range to the start
            # and end of the 0-240 hour time range. If either are different,
            # then a time range was swept out.
            self.statusBarMsg(
                self._getMsg(
                    '0_240', timeRange=timeRange), self.__getMsgSeverity('S'))

        # I'll need to know the unix time of 00Z so I can determine the
        # start time of temporary grids later. I'll need this in more than
        # one of the methods called later, so this will become an instance
        # variable, i.e., prefixed with "self." I also need an instance
        # variable that flags whether or not there were inconsistent grids.
        self.timeRange0_1 = self.createTimeRange(0, 1, 'Zulu')
        self.inconsistent = False
        
        # A CWA edit area can be provided in the configuration section.
        # Attempt to encode that edit area as a Numeric Python mask so that
        # the later checks are limited to the edit area. The GFE is not very
        # friendly if the encoding fails. The GFE will send a nasty message
        # to the user, but continue executing the procedure. No trappable
        # error is thrown. As of this writing, the GFE appears to create an
        # array of shape (0, 0) if the encoding cannot be done, so I will
        # check for that and, if I find it, then set the edit area to the
        # domain.
        # encodeEditArea comes from SmartScript. For the points that are in
        # the edit area, a value of one is assigned. Otherwise, a value of
        # zero is assigned.
        if cwaEditArea:
            self.cwaMask = self.encodeEditArea(cwaEditArea)
            if self.cwaMask.shape == (0, 0):
                # Use the getGridInfo command to get information about the
                # SnowAmt grid. From this, the grid size can be extracted. I
                # could use getGridInfo on any valid GFE grid.
                # getGridInfo is from SmartScript
                snowAmtInfoList = self.getGridInfo(
                    'Fcst', 'SnowAmt6hr', 'SFC', timeRange)
                # I painfully discovered that the array shape is (y, x)
                gridSize = (snowAmtInfoList[0].gridLocation().gridSize().y,
                            snowAmtInfoList[0].gridLocation().gridSize().x)
                # ones is from Numeric. It creates an array of the given size
                # and data type where all values are one.
                self.cwaMask = ones(gridSize, int)
                self.statusBarMsg(
                    self._getMsg('cwaMask'), self.__getMsgSeverity('S'))
        else:
            snowAmtInfoList = self.getGridInfo(
                'Fcst', 'SnowAmt6hr', 'SFC', timeRange)
            gridSize = (snowAmtInfoList[0].gridLocation().gridSize().y,
                        snowAmtInfoList[0].gridLocation().gridSize().x)
            self.cwaMask = ones(gridSize, int)
                
        # Based on the user's input, run the appropriate checks.
        # By making each of these options a checkbox with only one option in
        # the VariableList above, if an option is unchecked then an empty
        # list, [], will be what's in varDict. If an option is checked then a
        # list with the value "Yes", ["Yes"], will be what's in varDict. In
        # Python, a conditional expression can be whether or not a data
        # structure is empty. In these cases, an empty data structure,
        # e.g., an empty list, an empty tuple, an empty dictionary,
        # conditionally test to False while non empty data structures
        # conditionally test to True. In the if statements below, every varDict
        # lookup returns a list: either [] or ["Yes"]. I think the constructs
        # below or more elegant and easier to understand.
        if varDict['Run SnowAmt6hr/QPF6hr Check?']:
            # Call the SnowAmt6hr/QPF6hr check method
            self._runSnowAmt6hrQPF6hrCheck(timeRange)
        if varDict['Run SnowAmt6hr/Wx Check?']:
            # Call the SnowAmt6hr/Wx check method
            self._runSnowAmt6hrWxCheck(timeRange)
        if varDict['Run QPF6hr/PoP Check?']:
            # Call the QPF6hr/PoP check method
            self._runQPF6hrPoPCheck(timeRange)
        if varDict['Run QPF6hr/Wx Check?']:
            # Call the QPF6hr/Wx check method
            self._runQPF6hrWxCheck(timeRange)
        if varDict['Run PoP12hr/QPF6hr Check?']:
            # Call the PoP12hr/QPF6hr check method
            self._runPoP12hrQPF6hrCheck(timeRange)
        if self.inconsistent:
            self.statusBarMsg(self._getMsg('incon'), self.__getMsgSeverity('S'))
        else:
            self.statusBarMsg(self._getMsg('complete'), 'R')
