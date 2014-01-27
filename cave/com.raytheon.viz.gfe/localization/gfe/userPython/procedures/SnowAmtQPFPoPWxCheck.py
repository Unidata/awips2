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
#----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# SnowAmtQPFPoPWxCheck
#
# Author: Jay Smith, WFO Fairbanks, jay.smith@noaa.gov, 907-458-3721
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
# The procedure performs the following checks:
# 1. If SnowAmt present and >= 0.5 inches, then corresponding QPF grids
#    must add up to 0.01 inches.
# 2. If SnowAmt >= 0.1 inches, then there are two cases:
#    a. If the SnowAmt grid is exactly 6 hours long and starts at 00, 06, 12,
#       or 18 UTC, then at least one of the corresponding Wx grids must have
#       S, SW, or IP.
#    b. If the SnowAmt grid does not adhere to the time constraints listed in
#       in the previous paragraph, then all of the corresponding Wx grids
#       must have S, SW, or IP. This more stringent test is required because
#       with grids offset from the NDFD time constraints, it's possible for
#       the GFE to evaluate the grids as consistent using an "any"
#       criteria but have the NDFD flag those same grids as inconsistent.
# 3. If QPF > 0, then at least one of the corresponding PoP grids must be > 0
# 4. If QPF > 0, then there are two cases:
#    a. If the QPF grid is exactly 6 hours long and starts at 00, 06, 12, or 18
#       UTC, then at least one of the corresponding Wx grids must have R, RW,
#       S, SW, RS, IP, L, ZR, ZL.
#    b. If the QPF grid does not adhere to the time constraints listed in the
#       previous paragraph, then all corresponding Wx grids must contain a
#       precipitating weather type. This more stringent test is required
#       because with grids offset from the NDFD time constraints, it's
#       possible for the GFE to evaluate grids as consistent using an "any"
#       criteria but have the NDFD flag those same grids as inconsistent.
# For all of the checks above, if the initial threshold is not exceeded, then
# the two grids are consistent by definition. In other words:
# 1. If SnowAmt < 0.5, then SnowAmt and QPF are always consistent.
# 2. If SnowAmt < 0.1, then SnowAmt and Wx are always consistent.
# 3. If QPF = 0, then QPF and PoP are always consistent.
# 4. If QPF = 0, then QPF and Wx are always consistent.
# For the Wx checks above, only the Wx type is considered.
#
# ****** NOTE NOTE NOTE NOTE ******
# At this time, the check for two 6-hour QPF grids vs. one 12-hr PoP grid
# is not implemented because neither of those grid definitions is implemented
# in the GFE baseline. I don't know how to do a check on grids that don't
# exist.
# ****** NOTE NOTE NOTE NOTE ******
#
# If discrepancies are found, then the "bad" grids will be highlighted.
# Temporary grids showing where the discrepancies occur will be created and
# also highlighted.
#
# Dealing with QPF and SnowAmt is always a pain, because they are "cumulative"
# elements. This procedure will account for the possibility that the SnowAmt and
# QPF grids are not the same duration. It will also account for the possibilty
# that the SnowAmt and QPF grids are not aligned on either or both ends.
# The only sane way to handle either situation is to believe that the QPF
# accumulation happens uniformally across the grid's duration and to use
# the proportional amount of the QPF that corresponds the SnowAmt grid's
# duration. Some examples:
# 1. The QPF grid is 3 hours long and there are 3, 1-hour, SnowAmt grids.
# Each SnowAmt grid will be compared to 1/3 the value of the QPF grid.
# 2. The last two hours of a 3-hour QPF grid overlaps a 2-hour SnowAmt grid.
# The SnowAmt grid will be compared to 2/3 the value of the QPF grid.
# 3. Two 3-hour QPF grids align with one 6-hour SnowAmt grid. The first QPF
# grid will be compared to the SnowAmt grid. If the consistency check passes
# on that comparison, the program will continue. If the consistency check
# fails, then the sum of the two QPF grids will be compared to the SnowAmt
# grid.
# 4. The last four hours of a 6-hour QPF grid and the first two hours of a
# 3-hour QPF grid overlap a 6-hour SnowAmt grid. The SnowAmt grid will be
# compared to 2/3 of the first QPF grid. If the consistency check passes,
# the program will continue. If the consistency check fails, then 2/3 of the
# first QPF grid will be added to 2/3 of the second QPF grid and that QPF
# sum will be compared against the SnowAmt grid.
#
# Confused yet? Of course, all of these gyrations can be avoided if the
# QPF and SnowAmt grids are aligned and of the same duration.
#
# Unfortunately, the GFE does not provide a way to deal with proportional
# amounts of the accumulative grids, so I have done this.
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
# there is a lot of repeated code. I've consciously left the code this way in
# the hopes that it will be easier to understand by more novice programmers
# and because the code hasn't quite grown to the point where updating the
# repeating code is onerous or overly error-prone. It would be better to
# capture the repeating code in separate methods, but keeping track of the
# where you are in the code becomes harder the more you have to jump around
# from method to method. As with all things, there are trade-offs involved.
# ----------------------------------------------------------------------------

MenuItems = ["Consistency"]

VariableList = []
VariableList.append(('Check_Cleanup', 'Check', 'radio', ['Check', 'Cleanup']))
VariableList.append(('Run SnowAmt/QPF Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run SnowAmt/Wx Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run QPF/PoP Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('Run QPF/Wx Check?', ['Yes'], 'check', ['Yes']))
VariableList.append(('If "Cleanup" is selected, then only cleanup actions will run.\nNo checks will be made, regardless of the above settings.', '', 'label'))

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
        # Remove any temporary grids created previously.
        for element in (
            'SnowAmtQPFInconsistent', 'SnowAmtWxInconsistent',
            'QPFPoPInconsistent', 'QPFWxInconsistent'):
            try:
                # From SmartScript
                self.unloadWE('Fcst', element, 'SFC')
            except:
                # A failure is almost certainly no grids to unload.
                pass
        # Turn off any highlights. From SmartScript
        self.highlightGrids('Fcst', 'SnowAmt', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids('Fcst', 'QPF', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids('Fcst', 'Wx', 'SFC', timeRange, inconGridColor, on=0)
        self.highlightGrids('Fcst', 'PoP', 'SFC', timeRange, inconGridColor, on=0)
        return

    def __checkConfigValueTypes(self):
        import types
        message = ''
        badValues = False
        if not type(inconGridColor) is types.StringType:
            message = '%sThe "inconGridColor" variable is not defined as a string value. Please contact your IFPS focal point to fix this problem.\n' % message
            badValues = True
        if not type(tempGridColor) is types.StringType:
            message = '%sThe "tempGridColor" variable is not defined as a string value. Please contact your IFPS focal point to fix this problem.\n' % message
            badValues = True
        if not type(cwaEditArea) is types.StringType:
            message = '%sThe "cwaEditArea" variable is not defined as a string value. Please contact your IFPS focal point to fix this problem.\n' % message
            badValues = True
        if badValues:
            message = '%sYou will not be able to run the procedure until the problem is corrected.' % message
            # The next two commands are from SmartScript
            self.statusBarMsg(message, 'U')
            self.cancel()
        return

    def _runSnowAmtQPFCheck(self, timeRange):
        # This method implements the check that if SnowAmt >= 0.5, then
        # QPF must be >= 0.01.

        # There can be a significant difference between the values stored
        # in memory and the values returned from the database. This is because
        # when values are saved, the element's precision (as defined in
        # serverConfig.py/localConfig.py) is enforced. Values in memory do not
        # have the element's precision enforced; in fact, they have the
        # machine precision of the underlying data type.
        # If there are locks, post an urgent message and return from the method.
        message = ''
        # lockedByMe is from SmartScript
        if self.lockedByMe('QPF', 'SFC'):
            message = '%sYou have the QPF grid locked. Please save the QPF grid.\n' % message
        if self.lockedByMe('SnowAmt', 'SFC'):
            message = '%sYou have the SnowAmt grid locked. Please save the SnowAmt grid.\n' % message
        # lockedByOther is from SmartScript
        if self.lockedByOther('QPF', 'SFC'):
            message = '%sThe QPF grid is locked by someone else. Please have that person save the QPF grid.\n' % message
        if self.lockedByOther('SnowAmt', 'SFC'):
            message = '%sThe SnowAmt grid is locked by someone else. Please have that person save the SnowAmt grid.\n' % message
        if message:
            message = '%sThe SnowAmt/QPF Check was not run.' % message
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have locked grid problems.
            return

        # Make sure there are actually SnowAmt grids in the time range.
        # The self.getGrids command will return None if there are no grids
        # in the time range for mode='First' and noDataError=0. The None
        # variable cannot be iterated over. Rather than trap in a try/except,
        # I'll just check for the condititon. This may not be the most
        # Pythonic way of doing things, but it allows me to avoid having
        # a bunch of code indented beneath a try statement. If no SnowAmt
        # grids are found, post an urgent message and return from the method.
        # getGrids is from SmartScript
        snowAmtInfoList = self.getGridInfo('Fcst', 'SnowAmt', 'SFC', timeRange)
        if [] == snowAmtInfoList:
            message = 'There are no SnowAmt grids in the time range you selected.\nThe SnowAmt/QPF Check did not run.'
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return

        # getGridInfo is from SmartScript
        # One might ask why I don't just return the result of self.getGrids
        # to a variable and iterate over that. I'm trying to minimize the
        # memory footprint of the procedure. Reading all the grids into a
        # variable could be a fairly large memory hit. The construct below
        # only reads one SnowAmt grid at a time into memory, the one that's
        # being checked. By using the cache=0 switch on all the self.getGrids
        # command, I prevent the GFE from saving the grids into memory for me.
        # The Python builtin command enumerate loops over an iterable object
        # and returns a 2-tuple containing the current index of the
        # iteration and the object at that index. In cases where I need
        # both the index and the object, I think this construct is more
        # elegant than:
        # for i in xrange(len(iterableObject)):
        #     object = iterableObject[i]
        snowAmtGrids = self.getGrids('Fcst', 'SnowAmt', 'SFC', 
            timeRange, mode='List', noDataError=0,cache=0)
        for snowAmtIndex, snowAmtGrid in enumerate(snowAmtGrids):
            # greater_equal is from Numeric. For the given array and
            # threshold, a new array of the same dimensions as the input
            # array is returned. The new array has the value 1 where the
            # input array was greater than or equal to the threshold and
            # has the value 0 elsewhere.
            halfInchMask = greater_equal(snowAmtGrid, 0.5 - snowAmtTol)
            gridTR = snowAmtInfoList[snowAmtIndex].gridTime()
            # zeros is from Numeric. It creates an array of all zeros for
            # the given dimensions and numeric type.
            qpfSum = zeros(snowAmtGrid.shape, float32)
            qpfGrids = self.getGrids(
                'Fcst', 'QPF', 'SFC', gridTR, mode='List', noDataError=0,
                cache=0)
            if qpfGrids is None:
                message = '''There are no QPF grids in time range %s.
The SnowAmt/QPF Check skipped the time range.''' % gridTR
                self.statusBarMsg(message, 'U')
                continue
            qpfInfoList = self.getGridInfo('Fcst', 'QPF', 'SFC', gridTR)
            for qpfIndex, qpfGrid in enumerate(qpfGrids):
                snowAmtGridStartTime = gridTR.startTime().unixTime()
                qpfGridTR = qpfInfoList[qpfIndex].gridTime()
                qpfGridStartTime = qpfGridTR.startTime().unixTime()
                fraction = 1.0
                if qpfGridStartTime < snowAmtGridStartTime:
                    diff = snowAmtGridStartTime - qpfGridStartTime
                    fraction -= (float(diff) / qpfGridTR.duration())
                snowAmtGridEndTime = gridTR.endTime().unixTime()
                qpfGridEndTime = qpfGridTR.endTime().unixTime()
                if qpfGridEndTime > snowAmtGridEndTime:
                    diff = qpfGridEndTime - snowAmtGridEndTime
                    fraction -= (float(diff) / qpfGridTR.duration())
                # For some reason, the construct:
                # qpfSum = qpfSum + (qpfGrid * fraction)
                # doesn't assign the expression evaluation back to qpfSum.
                # Thus, I use a temporary variable.
                qpfTemp = qpfSum + (qpfGrid * fraction)
                qpfSum = qpfTemp
                del qpfTemp
                # less is from Numeric. It behaves analogously to greater_equal,
                # described above.
                qpfMask = less(qpfSum, 0.01 + qpfTol)
                # The following is the "truth" table for the logical
                # comparison.
                # SnowAmt >= 0.5, 1; SnowAmt < 0.5, 0
                # QPF < 0.01, 1; QPF >= 0.01, 0
                # SnowAmt >= 0.5 (1) and QPF < 0.01 (1) = 1 (Bad result)
                # SnowAmt >= 0.5 (1) and QPF >= 0.01 (0) = 0 (Good result)
                # SnowAmt < 0.5 (0) and QPF < 0.01 (1) = 0 (Good result)
                # SnowAmt < 0.5 (0) and QPF >= 0.01 (0) = 0 (Good result)
                # logical_and is from Numeric
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
                # ravel and sometrue are from Numeric.
                if not sometrue(ravel(consistMask)):
                    # This is the good result, even though it may not be
                    # intuitive. The ravel function reduces the rank of the
                    # array by one. Since we had a 2-d array, the ravel
                    # function creates a 1-d array (a vector) such that
                    # reading the 2-d array from left-to-right, top-to-
                    # bottom returns the same values as reading the 1-d
                    # array from left-to-right. The sometrue function
                    # performs a logical or on subsequent element pairs
                    # in the 1-d array and returns the final result. If
                    # there's no inconsistency, the result will be 0.
                    # Thus, negating the sometrue result gives us the
                    # positive outcome. Phew.
                    # Since QPF is an accumulative element, we don't need
                    # to continue the loop once the QPF sum meets the
                    # threshold.
                    break
            else:
                # This block will only execute if the for loop runs to
                # completion, i.e., the break statement is not executed.
                # So, if we get here, we have an inconsistency and need to
                # highlight the appropriate grids.
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'QPF', 'SFC', gridTR, inconGridColor)
                # createGrid is from SmartScript
                # Since this block of code only executes if the for loop
                # runs to completion, then the value of consistMask from
                # the for loop will contain all of the inconsistencies.
                self.createGrid(
                    'Fcst', 'SnowAmtQPFInconsistent', 'SCALAR', consistMask,
                    gridTR, descriptiveName='SnowAmtQPFInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmtQPFInconsistent', 'SFC', gridTR,
                        tempGridColor)
                self.inconsistent = True
        # While not required, I like to terminate my methods with a return
        # statement to make it clear this is where the method ends.
        return

    def _runSnowAmtWxCheck(self, timeRange):
        # This implements the check that if SnowAmt >= 0.1, then the Wx grid
        # must contain S, SW, or IP, regardless of whether or not there is
        # any freezing or liquid types. Finally, the check does not look at
        # anything other than the Wx type. In other words, the check will be
        # okay if SnowAmt != 0 and Wx has Chc:S:- or Def:SW:-- or Lkly:S:+.

        # There can be a significant difference between the values stored
        # in memory and the values returned from the database. This is because
        # when values are saved, the element's precision (as defined in
        # serverConfig.py/localConfig.py) is enforced. Values in memory do not
        # have the element's precision enforced; in fact, they have the
        # machine precision of the underlying data type.
        # If there are locks, post an urgent message and return from the method.
        message = ''
        # lockedByMe is from SmartScript
        if self.lockedByMe('Wx', 'SFC'):
            message = '%sYou have the Wx grid locked. Please save the Wx grid.\n' % message
        if self.lockedByMe('SnowAmt', 'SFC'):
            message = '%sYou have the SnowAmt grid locked. Please save the SnowAmt grid.\n' % message
        # lockedByOther is from SmartScript
        if self.lockedByOther('Wx', 'SFC'):
            message = '%sThe Wx grid is locked by someone else. Please have that person save the Wx grid.\n' % message
        if self.lockedByOther('SnowAmt', 'SFC'):
            message = '%sThe SnowAmt grid is locked by someone else. Please have that person save the SnowAmt grid.\n' % message
        if message:
            message = '%sThe SnowAmt/Wx Check was not run.' % message
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have locked grid problems.
            return

        # Make sure there are actually SnowAmt grids in the time range.
        # The self.getGrids command will return None if there are no grids
        # in the time range for noDataError=0. The None
        # variable cannot be iterated over. Rather than trap in a try/except,
        # I'll just check for the condititon. This may not be the most
        # Pythonic way of doing things, but it allows me to avoid having
        # a bunch of code indented beneath a try statement. If no SnowAmt
        # grids are found, post an urgent message and return from the method.
        # getGrids is from SmartScript
        snowAmtInfoList = self.getGridInfo('Fcst', 'SnowAmt', 'SFC', timeRange)
        if [] == snowAmtInfoList:
            message = 'There are no SnowAmt grids in the time range you selected.\nThe SnowAmt/Wx Check did not run.'
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return

        snowAmtGrids = self.getGrids(
            'Fcst', 'SnowAmt', 'SFC', timeRange, mode='List', noDataError=0,
            cache=0)
        for snowAmtIndex, snowAmtGrid in enumerate(snowAmtGrids):
            nonZeroMask = greater_equal(snowAmtGrid, 0.1 - snowAmtTol)
            gridTR = snowAmtInfoList[snowAmtIndex].gridTime()
            
            wxInfoList = self.getGridInfo('Fcst', 'Wx', 'SFC', gridTR)
            if [] == wxInfoList:
                message = '''There are no Wx grids in time range %s.
The SnowAmt/Wx Check skipped the time range.''' % gridTR
                self.statusBarMsg(message, 'U')
                continue
            # There are two cases, which I'll capture in individual methods
            # If the SnowAmt grid is exactly 6 hours long and starts at
            # 00, 06, 12, or 18 UTC, then only one overlapping Wx grid needs
            # to match. Otherwise, all overlapping Wx grids need to match.
            if gridTR.duration() / 3600 == 6 and \
               gridTR.startTime().hour in (0, 6, 12, 18):
                self._snowAmtWxCheckLocked(nonZeroMask, gridTR, wxInfoList)
            else:
                self._snowAmtWxCheckUnlocked(nonZeroMask, gridTR, wxInfoList)
        return

    def _snowAmtWxCheckLocked(self, nonZeroMask, gridTR, wxInfoList):
        # The "Locked" comes from the idea that if the SnowAmt grid meets
        # the duration and start time constraints, then it's been "locked".
        # I need to capture the consistency masks for each individual Wx grid
        # just in case I end up with inconsistencies.
        consistMaskList = []
        for wxIndex, wxGrid in enumerate(self.getGrids(
            'Fcst', 'Wx', 'SFC', gridTR, mode='List', noDataError=0,
            cache=0)):
            # wxMask is from SmartScript
            sMask = self.wxMask(wxGrid, ':S:')
            swMask = self.wxMask(wxGrid, ':SW:')
            ipMask = self.wxMask(wxGrid, ':IP:')
            snowMask = logical_or(logical_or(sMask, swMask), ipMask)
            del (sMask, swMask, ipMask)
            wxMask = where(snowMask, 0, 1)
            # "Truth" table for the logical comparison follows
            # SnowAmt >= 0.1, 1; SnowAmt < 0.1, 0
            # Wx has S, SW, or IP, 0; Wx doesn't have S, SW, or IP, 1
            # SnowAmt >= 0.1 (1) and Wx has (0) = 0 (Good result)
            # SnowAmt >= 0.1 (1) and Wx doesn't have (1) = 1 (Bad result)
            # SnowAmt < 0.1 (0) and Wx has (0) = 0 (Good result)
            # SnowAmt < 0.1 (0) and Wx doesn't have (1) = 0 (Good result)
            #
            consistMask = logical_and(nonZeroMask, wxMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            consistMaskList.append(consistMask)
            if not sometrue(ravel(consistMask)):
                # There were no inconsistencies with this Wx grid. Since only
                # one needs to be consistent, we don't need to do any more
                # checks.
                break
        else:
            # This block will only execute if the for loop runs to
            # completion, i.e., the break statement is not executed.
            # So, if we get here, we have an inconsistency and need to
            # highlight the appropriate grids.
            if inconGridColor:
                self.highlightGrids(
                    'Fcst', 'SnowAmt', 'SFC', gridTR, inconGridColor)
                self.highlightGrids(
                    'Fcst', 'Wx', 'SFC', gridTR, inconGridColor)
            # createGrid is from SmartScript
            for index in xrange(len(wxInfoList)):
                # Create temporary grids for each Wx grid. Limit the start and
                # end times of the temporary grids so that they don't extend
                # beyond the start and end times of the corresponding SnowAmt
                # grid.
                wxGridTR = wxInfoList[index].gridTime()
                tempGridStartTime = wxGridTR.startTime().unixTime()
                if tempGridStartTime < gridTR.startTime().unixTime():
                    tempGridStartTime = gridTR.startTime().unixTime()
                tempGridEndTime = wxGridTR.endTime().unixTime()
                if tempGridEndTime > gridTR.endTime().unixTime():
                    tempGridEndTime = gridTR.endTime().unixTime()
                tempGridDur = (tempGridEndTime - tempGridStartTime) / 3600
                offset = (tempGridStartTime - \
                          self.timeRange0_1.startTime().unixTime()) / 3600
                # Because the time range may be different for the temporary
                # grid, I need to create and use that time range when
                # creating the temporary grid.
                tempGridTR = self.createTimeRange(
                    offset, offset+tempGridDur, 'Zulu')
                self.createGrid(
                    'Fcst', 'SnowAmtWxInconsistent', 'SCALAR',
                    consistMaskList[index], tempGridTR,
                    descriptiveName='SnowAmtWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmtWxInconsistent', 'SFC', gridTR,
                        tempGridColor)
            self.inconsistent = True            
        return

    def _snowAmtWxCheckUnlocked(self, nonZeroMask, gridTR, wxInfoList):
        # The "Unlocked" comes from the idea that if the SnowAmt grid does
        # not meet the duration and start time constraints, then it's been
        # left "unlocked".
        for wxIndex, wxGrid in enumerate(self.getGrids(
            'Fcst', 'Wx', 'SFC', gridTR, mode='List', noDataError=0,
            cache=0)):
            # wxMask is from SmartScript
            sMask = self.wxMask(wxGrid, ':S:')
            swMask = self.wxMask(wxGrid, ':SW:')
            ipMask = self.wxMask(wxGrid, ':IP:')
            snowMask = logical_or(logical_or(sMask, swMask), ipMask)
            del (sMask, swMask, ipMask)
            wxMask = where(snowMask, 0, 1)
            # "Truth" table for the logical comparison follows
            # SnowAmt >= 0.1, 1; SnowAmt < 0.1, 0
            # Wx has S, SW, or IP, 0; Wx doesn't have S, SW, or IP, 1
            # SnowAmt >= 0.1 (1) and Wx has (0) = 0 (Good result)
            # SnowAmt >= 0.1 (1) and Wx doesn't have (1) = 1 (Bad result)
            # SnowAmt < 0.1 (0) and Wx has (0) = 0 (Good result)
            # SnowAmt < 0.1 (0) and Wx doesn't have (1) = 0 (Good result)
            #
            # All Wx grids overlapping the SnowAmt grid must be consistent.
            consistMask = logical_and(nonZeroMask, wxMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            if sometrue(ravel(consistMask)):
                # I'll highlight the SnowAmt grids and Wx grids in
                # gridTR as I did with QPF. However, I'll make
                # temporary grids here using the Wx grid's time
                # range but, the temporary grid cannot start before
                # the start of the corresponding SnowAmt grid nor can
                # it end after the end of the corresponding SnowAmt grid.
                wxGridTR = wxInfoList[wxIndex].gridTime()
                tempGridStartTime = wxGridTR.startTime().unixTime()
                if tempGridStartTime < gridTR.startTime().unixTime():
                    # Clip to start of SnowAmt grid
                    tempGridStartTime = gridTR.startTime().unixTime()
                tempGridEndTime = wxGridTR.endTime().unixTime()
                if tempGridEndTime > gridTR.endTime().unixTime():
                    # Clip to end of SnowAmtGrid
                    tempGridEndTime = gridTR.endTime().unixTime()
                tempGridDur = (tempGridEndTime - tempGridStartTime) / 3600
                offset = (tempGridStartTime - \
                          self.timeRange0_1.startTime().unixTime()) / 3600
                # Since either the front or end of the Wx grid's
                # time range may have been clipped, create a time
                # range using those values.
                tempGridTR = self.createTimeRange(
                    offset, offset+tempGridDur, 'Zulu')
                self.createGrid(
                    'Fcst', 'SnowAmtWxInconsistent', 'SCALAR', consistMask,
                    tempGridTR, descriptiveName='SnowAmtWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmtWxInconsistent', 'SFC', gridTR,
                        tempGridColor)
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'SnowAmt', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'Wx', 'SFC', wxGridTR, inconGridColor)
                self.inconsistent = True
        return

    def _runQPFPoPCheck(self, timeRange):
        # This method implements the check that if any QPF grid is non zero
        # then one of the corresponding floating PoP grids must also be non
        # zero.

        # There can be a significant difference between the values stored
        # in memory and the values returned from the database. This is because
        # when values are saved, the element's precision (as defined in
        # serverConfig.py/localConfig.py) is enforced. Values in memory do not
        # have the element's precision enforced; in fact, they have the
        # machine precision of the underlying data type.
        # If there are locks, post an urgent message and return from the method.
        message = ''
        # lockedByMe is from SmartScript
        if self.lockedByMe('QPF', 'SFC'):
            message = '%sYou have the QPF grid locked. Please save the QPF grid.\n' % message
        if self.lockedByMe('PoP', 'SFC'):
            message = '%sYou have the PoP grid locked. Please save the PoP grid.\n' % message
        # lockedByOther is from SmartScript
        if self.lockedByOther('QPF', 'SFC'):
            message = '%sThe QPF grid is locked by someone else. Please have that person save the QPF grid.\n' % message
        if self.lockedByOther('PoP', 'SFC'):
            message = '%sThe PoP grid is locked by someone else. Please have that person save the PoP grid.\n' % message
        if message:
            message = '%sThe QPF/PoP Check was not run.' % message
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have locked grid problems.
            return

        # Make sure there are actually QPF grids in the time range.
        # The self.getGrids command will return None if there are no grids
        # in the time range for mode='First' and noDataError=0. The None
        # variable cannot be iterated over. Rather than trap in a try/except,
        # I'll just check for the condititon. This may not be the most
        # Pythonic way of doing things, but it allows me to avoid having
        # a bunch of code indented beneath a try statement. If no SnowAmt
        # grids are found, post an urgent message and return from the method.
        # getGrids is from SmartScript
        qpfInfoList = self.getGridInfo('Fcst', 'QPF', 'SFC', timeRange)
        if [] == qpfInfoList:
            message = 'There are no QPF grids in the time range you selected.\nThe QPF/PoP Check did not run.'
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        qpfGrids = self.getGrids(
            'Fcst', 'QPF', 'SFC', timeRange, mode='List', noDataError=0,
            cache=0) 
        for qpfIndex, qpfGrid in enumerate(qpfGrids):
            gridTR = qpfInfoList[qpfIndex].gridTime()
            
            popGrid = self.getGrids(
                'Fcst', 'PoP', 'SFC', gridTR, mode='Max', noDataError=0,
                cache=0)
            if popGrid is None:
                message = '''There are no PoP grids in time range %s.
The QPF/PoP Check skipped the time range.''' % gridTR
                self.statusBarMsg(message, 'U')
                continue
            qpfNonZeroMask = greater(qpfGrid, qpfTol)
            popZeroMask = equal(popGrid, 0)
            # popZeroMask = 1 if PoP = 0; popZeroMask = 0 if PoP != 0
            # qpfNonZeroMask = 1 if QPF > 0; qpfNonZeroMask = 0 if QPF = 0
            # PoP = 0 (1) and QPF = 0 (0) => 0 (Good result)
            # PoP != 0 (0) and QPF = 0 (0) => 0 (Good result)
            # PoP != 0 (0) and QPF > 0 (1) => 0 (Good result)
            # PoP = 0 (1) and QPF > 0 (1) => 1 (Bad result)
            consistMask = logical_and(qpfNonZeroMask, popZeroMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            if sometrue(ravel(consistMask)):
                # The good result is if the logical_and returns zeros
                # for every grid point, that is "none true". So, if
                # the sometrue method evaluates True, there are
                # inconsistencies.
                self.createGrid(
                    'Fcst', 'QPFPoPInconsistent', 'SCALAR', consistMask, gridTR,
                    descriptiveName='QPFPoPInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPFPoPInconsistent', 'SFC', gridTR,
                        tempGridColor)
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'PoP', 'SFC', gridTR, inconGridColor)
                self.inconsistent = True

            ##### Edited by Rob Radzanowski (WFO-CTP) 03-16-2009 to add missing NDFD check for QPF=0 & PoP > 50
            ##### which is causing unexplained yellow banners due to lack of checking for this error.
            qpfZeroMask = equal(qpfGrid, 0)
            popGrid = self.getGrids(
                'Fcst', 'PoP', 'SFC', gridTR, mode='Max', noDataError=0, cache=0)
            popGreater50Mask = greater(popGrid, 50)
            # popGreater50Mask = 1 if PoP > 50; popGreater50Mask = 0 if PoP <= 50
            # qpfZeroMask = 0 if QPF > 0; qpfZeroMask = 1 if QPF = 0
            # PoP > 50 (1) and QPF > 0 (0) => 0 (Good result)
            # PoP > 50 (1) and QPF = 0 (1) => 1 (Bad result)
            # PoP <= 50 (0) and QPF > 0 (0) => 0 (Good/Irrelevant result)
            # PoP <= 50 (0) and QPF = 0 (1) => 0 (Good result)

            consistMask2 = logical_and(qpfZeroMask, popGreater50Mask)
            consistMask2 = where(self.cwaMask, consistMask2, 0)
            if sometrue(ravel(consistMask2)):
                # The good result is if the logical_and returns zeros
                # for every grid point, that is "none true". So, if
                # the sometrue method evaluates True, there are
                # inconsistencies.
                self.createGrid(
                    'Fcst', 'QPFPoPInconsistent', 'SCALAR', consistMask2, gridTR,
                    descriptiveName='QPFPoPInconsistent', 
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')

                if tempGridColor:
                    self.highlightGrids('Fcst', 'QPFPoPInconsistent', 'SFC', gridTR, tempGridColor)
                if inconGridColor:
                    self.highlightGrids('Fcst', 'QPF', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids('Fcst', 'PoP', 'SFC', gridTR, inconGridColor)
                self.inconsistent = True         
        return

    def _runQPFWxCheck(self, timeRange):
        # This method implements the check that if QPF non zero, then the
        # corresponding Wx grids must contain a precipitable Wx type. Note:
        # the method only checks the Wx type, no cov/prob, no inten, etc.

        # There can be a significant difference between the values stored
        # in memory and the values returned from the database. This is because
        # when values are saved, the element's precision (as defined in
        # serverConfig.py/localConfig.py) is enforced. Values in memory do not
        # have the element's precision enforced; in fact, they have the
        # machine precision of the underlying data type.
        # If there are locks, post an urgent message and return from the method.
        message = ''
        # lockedByMe is from SmartScript
        if self.lockedByMe('QPF', 'SFC'):
            message = '%sYou have the QPF grid locked. Please save the QPF grid.\n' % message
        if self.lockedByMe('Wx', 'SFC'):
            message = '%sYou have the Wx grid locked. Please save the Wx grid.\n' % message
        # lockedByOther is from SmartScript
        if self.lockedByOther('QPF', 'SFC'):
            message = '%sThe QPF grid is locked by someone else. Please have that person save the QPF grid.\n' % message
        if self.lockedByOther('Wx', 'SFC'):
            message = '%sThe Wx grid is locked by someone else. Please have that person save the Wx grid.\n' % message
        if message:
            message = '%sThe QPF/Wx Check was not run.' % message
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have locked grid problems.
            return

        # Make sure there are actually QPF grids in the time range.
        # I'll just check for the condititon. If no SnowAmt
        # grids are found, post an urgent message and return from the method.
        qpfInfoList = self.getGridInfo('Fcst', 'QPF', 'SFC', timeRange)
        if [] == qpfInfoList:
            message = 'There are no QPF grids in the time range you selected.\nThe QPF/PoP Check did not run.'
            self.statusBarMsg(message, 'U')
            # I return instead of aborting because the user may have asked for
            # other tests that do not have missing grid problems.
            return
        for qpfIndex, qpfGrid in enumerate(self.getGrids(
            'Fcst', 'QPF', 'SFC', timeRange, mode='List', noDataError=0,
            cache=0)):
            qpfNonZeroMask = greater(qpfGrid, qpfTol)
            gridTR = qpfInfoList[qpfIndex].gridTime()
            wxInfoList = self.getGridInfo('Fcst', 'Wx', 'SFC', gridTR)
            if [] == wxInfoList:
                message = '''There are no Wx grids in time range %s.
The QPF/Wx Check skipped the time range.''' % gridTR
                self.statusBarMsg(message, 'U')
                continue
            # There are two cases. If the QPF grid is exactly 6 hours long and
            # starts at 00, 06, 12, or 18 UTC, then only one of the
            # corresponding Wx grids needs to be consistent. Otherwise, all the
            # corresponding Wx grids need to be consistent.
            if gridTR.duration() / 3600 == 6 and gridTR.startTime().hour in (0, 6, 12, 18):
                self._qpfWxCheckLocked(qpfNonZeroMask, gridTR, wxInfoList)
            else:
                self._qpfWxCheckUnlocked(qpfNonZeroMask, gridTR, wxInfoList)
        return

    def _qpfWxCheckLocked(self, qpfNonZeroMask, gridTR, wxInfoList):
        # The "Locked" comes from the idea that if the QPF grid is
        # exactly 6 hours long and starts at 00, 06, 12, or 18 UTC, then it
        # is "locked".
        consistMaskList = []
        for wxIndex, wxGrid in enumerate(self.getGrids(
            'Fcst', 'Wx', 'SFC', gridTR, mode='List', noDataError=0,
            cache=0)):
            # wxMask is from SmartScript
            sMask = self.wxMask(wxGrid, ':S:')
            swMask = self.wxMask(wxGrid, ':SW:')
            ipMask = self.wxMask(wxGrid, ':IP:')
            snowMask = logical_or(logical_or(sMask, swMask), ipMask)
            del (sMask, swMask, ipMask)
            rMask = self.wxMask(wxGrid, ':R:')
            rwMask = self.wxMask(wxGrid, ':RW:')
            lMask = self.wxMask(wxGrid, ':L:')
            zlMask = self.wxMask(wxGrid, ':ZL:')
            zrMask = self.wxMask(wxGrid, ':ZR:')
            # logical_or is from Numeric
            rainMask = logical_or(
                rMask, logical_or(
                    rwMask, logical_or(
                        lMask, logical_or(zlMask, zrMask))))
            del (rMask, rwMask, lMask, zlMask, zrMask)
            precipMask = logical_or(snowMask, rainMask)
            del (snowMask, rainMask)
            wxMask = where(precipMask, 0, 1)
            # QPF >= 0.01, 1; QPF < 0.01, 0
            # Wx has precip, 0; Wx doesn't have precip, 1
            # QPF >= 0.01 (1) and Wx has (0) = 0 (Good result)
            # QPF >= 0.01 (1) and Wx doesn't have (1) = 1 (Bad result)
            # QPF < 0.01 (0) and Wx has (0) = 0 (Good result)
            # QPF < 0.01 (0) and Wx doesn't have (1) = 0 (Good result)
            consistMask = logical_and(qpfNonZeroMask, wxMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            consistMaskList.append(consistMask)
            if not sometrue(ravel(consistMask)):
                # There were no inconsistencies with this Wx grid. Since only
                # one needs to be consistent, we don't need to do any more
                # checks.
                break
        else:
            # This block will only execute if the for loop runs to
            # completion, i.e., the break statement is not executed.
            # So, if we get here, we have an inconsistency and need to
            # highlight the appropriate grids.
            if inconGridColor:
                self.highlightGrids(
                    'Fcst', 'QPF', 'SFC', gridTR, inconGridColor)
                self.highlightGrids(
                    'Fcst', 'Wx', 'SFC', gridTR, inconGridColor)
            # createGrid is from SmartScript
            for index in xrange(len(wxInfoList)):
                # Create temporary grids for each Wx grid. Limit the time
                # range of the temporary grid so that it doesn't start any
                # earlier or any later than the corresponding QPF grid.
                wxGridTR = wxInfoList[index].gridTime()
                tempGridStartTime = wxGridTR.startTime().unixTime()
                if tempGridStartTime < gridTR.startTime().unixTime():
                    tempGridStartTime = gridTR.startTime().unixTime()
                tempGridEndTime = wxGridTR.endTime().unixTime()
                if tempGridEndTime > gridTR.endTime().unixTime():
                    tempGridEndTime = gridTR.endTime().unixTime()
                tempGridDur = (tempGridEndTime - tempGridStartTime) / 3600
                offset = (tempGridStartTime - \
                          self.timeRange0_1.startTime().unixTime()) / 3600
                # Since the temporary grid could have a different time range
                # than the Wx grid, I need to create and use that time range
                # when creating the temporary grid.
                tempGridTR = self.createTimeRange(
                    offset, offset+tempGridDur, 'Zulu')
                self.createGrid(
                    'Fcst', 'QPFWxInconsistent', 'SCALAR',
                    consistMaskList[index], tempGridTR,
                    descriptiveName='QPFWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPFWxInconsistent', 'SFC', gridTR,
                        tempGridColor)
            self.inconsistent = True            
        return

    def _qpfWxCheckUnlocked(self, qpfNonZeroMask, gridTR, wxInfoList):
        # The "Unlocked" comes from the idea that if the QPF grid is not
        # exactly 6 hours long and starting at 00, 06, 12, or 18 UTC, then it
        # is "unlocked".
        for wxIndex, wxGrid in enumerate(self.getGrids(
            'Fcst', 'Wx', 'SFC', gridTR, mode='List', noDataError=0,
            cache=0)):
            # wxMask is from SmartScript
            sMask = self.wxMask(wxGrid, ':S:')
            swMask = self.wxMask(wxGrid, ':SW:')
            ipMask = self.wxMask(wxGrid, ':IP:')
            snowMask = logical_or(logical_or(sMask, swMask), ipMask)
            del (sMask, swMask, ipMask)
            rMask = self.wxMask(wxGrid, ':R:')
            rwMask = self.wxMask(wxGrid, ':RW:')
            lMask = self.wxMask(wxGrid, ':L:')
            zlMask = self.wxMask(wxGrid, ':ZL:')
            zrMask = self.wxMask(wxGrid, ':ZR:')
            # logical_or is from Numeric
            rainMask = logical_or(
                rMask, logical_or(
                    rwMask, logical_or(
                        lMask, logical_or(zlMask, zrMask))))
            del (rMask, rwMask, lMask, zlMask, zrMask)
            precipMask = logical_or(snowMask, rainMask)
            del (snowMask, rainMask)
            wxMask = where(precipMask, 0, 1)
            # QPF >= 0.01, 1; QPF < 0.01, 0
            # Wx has precip, 0; Wx doesn't have precip, 1
            # QPF >= 0.01 (1) and Wx has (0) = 0 (Good result)
            # QPF >= 0.01 (1) and Wx doesn't have (1) = 1 (Bad result)
            # QPF < 0.01 (0) and Wx has (0) = 0 (Good result)
            # QPF < 0.01 (0) and Wx doesn't have (1) = 0 (Good result)
            #
            # All Wx grids overlapping the SnowAmt grid must be consistent.
            consistMask = logical_and(qpfNonZeroMask, wxMask)
            consistMask = where(self.cwaMask, consistMask, 0)
            if sometrue(ravel(consistMask)):
                wxGridTR = wxInfoList[wxIndex].gridTime()
                tempGridStartTime = wxGridTR.startTime().unixTime()
                if tempGridStartTime < gridTR.startTime().unixTime():
                    # Clip to start of QPF grid
                    tempGridStartTime = gridTR.startTime().unixTime()
                tempGridEndTime = wxGridTR.endTime().unixTime()
                if tempGridEndTime > gridTR.endTime().unixTime():
                    # Clip to end of QPF Grid
                    tempGridEndTime = gridTR.endTime().unixTime()
                tempGridDur = (tempGridEndTime - tempGridStartTime) / 3600
                offset = (tempGridStartTime - \
                          self.timeRange0_1.startTime().unixTime()) / 3600
                # Since either the front or end of the Wx grid's
                # time range may have been clipped, create a time
                # range using those values.
                tempGridTR = self.createTimeRange(
                    offset, offset+tempGridDur, 'Zulu')
                self.createGrid(
                    'Fcst', 'QPFWxInconsistent', 'SCALAR', consistMask,
                    tempGridTR, descriptiveName='QPFWxInconsistent',
                    minAllowedValue=0, maxAllowedValue=1, units='Good/Bad')
                if tempGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPFWxInconsistent', 'SFC', gridTR,
                        tempGridColor)
                if inconGridColor:
                    self.highlightGrids(
                        'Fcst', 'QPF', 'SFC', gridTR, inconGridColor)
                    self.highlightGrids(
                        'Fcst', 'Wx', 'SFC', wxGridTR, inconGridColor)
                self.inconsistent = True
        return

    def _calcTolerance(self, gridInfo):
        precision = gridInfo.gridParmInfo.getPrecision()
        return pow(10, -precision)
    
    def execute(self, timeRange, varDict):
        # Make sure the configuration values are the correct types.
        self.__checkConfigValueTypes()
        # createTimeRange is from SmartScript
        timeRange0_240 = self.createTimeRange(0, 241, 'Zulu')
        checkCleanup = varDict.get('Check_Cleanup', 'Check')
        self.__cleanup(timeRange0_240)
        if checkCleanup == 'Cleanup':
            message = 'SnowQPFPoPWxCheck complete.'
            self.statusBarMsg(message, 'R')
            self.cancel()
        if timeRange.endTime().unixTime() - timeRange.startTime().unixTime() < \
           3600: # No time range selected, use create a 0 to 240 hour range
            timeRange = timeRange0_240

        # If the user has a time range swept out, send an informational
        # message.
        if (timeRange.startTime().unixTime() != timeRange0_240.startTime().unixTime()) or \
           (timeRange.endTime().unixTime() != timeRange0_240.endTime().unixTime()) or \
           (timeRange.duration() != timeRange0_240.duration()):
            message = 'The SnowAmtQPFPoPWxCheck procedure did not run over the 0 to 240 hour time period,\nit ran over %s. This may be what you desired.' % str(timeRange)
            self.statusBarMsg(message, 'S')

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
                    'Fcst', 'SnowAmt', 'SFC', timeRange)
                # I painfully discovered that the array shape is (y, x)
                gridSize = (snowAmtInfoList[0].gridLocation().gridSize().y,
                            snowAmtInfoList[0].gridLocation().gridSize().x)
                # ones is from Numeric. It creates an array of the given size
                # and data type where all values are one.
                self.cwaMask = ones(gridSize, Int)
                message = \
'''The procedure was not able to use the CWA edit area, %s, provided
in the configuration. You should inform the person responsible for procedures
of this problem. The procedure ran over the whole domain.''' % cwaEditArea
                self.statusBarMsg(message, 'S')
        else:
            snowAmtInfoList = self.getGridInfo(
                'Fcst', 'SnowAmt', 'SFC', timeRange)
            gridSize = (snowAmtInfoList[0].gridLocation().gridSize().y,
                        snowAmtInfoList[0].gridLocation().gridSize().x)
            self.cwaMask = ones(gridSize, Int)
                
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
        if varDict['Run SnowAmt/QPF Check?']:
            # Call the SnowAmt/QPF check method
            self._runSnowAmtQPFCheck(timeRange)
        if varDict['Run SnowAmt/Wx Check?']:
            # Call the SnowAmt/Wx check method
            self._runSnowAmtWxCheck(timeRange)
        if varDict['Run QPF/PoP Check?']:
            # Call the QPF/PoP check method
            self._runQPFPoPCheck(timeRange)
        if varDict['Run QPF/Wx Check?']:
            # Call the QPF/Wx check method
            self._runQPFWxCheck(timeRange)
        message = 'SnowAmtQPFPoPWxCheck complete.'
        if self.inconsistent:
            message = '%s Inconsistencies found! Grids highlighted %s and %s.' % (
                message, inconGridColor, tempGridColor)
            self.statusBarMsg(message, 'S')
        else:
            self.statusBarMsg(message, 'R')

