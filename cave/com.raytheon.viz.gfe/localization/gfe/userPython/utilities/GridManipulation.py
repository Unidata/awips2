# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# GridManipulation - Version 2.1
#
# Author:  Matthew H. Belk                  Created: 10/02/2010
#          WFO Taunton MA             Last Modified: 12/03/2015
# ----------------------------------------------------------------------------

import SmartScript
import types, re
import LogStream

import numpy as np
import TimeRange, AbsTime

class GridManipulation(SmartScript.SmartScript):

    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    ############################################################################
    #  (originally from CheckTandTd by Tom LeFebvre).
    
    ##
    # Get the list of time ranges at the grid whose element name is WEName
    # contains grids. The model and level of the weather element are assumed
    # to be "Fcst" and "SFC" respectively, but can be identified differently.
    # 
    # @param WEName: Name of a weather element
    # @type WEName: string
    # @param model: Name of model to inventory (default = "Fcst")
    # @type model: string
    # @param level: Level of a weather element to inventory (default = "SFC")
    # @type level: string
    # @return: time ranges at which WEName has data.
    # @rtype: Python list of Python TimeRange objects
    def GM_getWEInventory(self, WEName, dbase="Fcst", level="SFC", 
                          timeRange=TimeRange.allTimes()):
        """Return a list of time ranges with available data for a field from
        a specific database and level.
        Args:
            string WEName: name of field to inventory
            string dbase: name of database to search (default = 'Fcst')
            string level: level of data to inventory (default = 'SFC')
        Returns:
            Python list of Python time range objects
        """

#        print "Getting inventory of -> '%s' from '%s' at '%s'" % \
#        (WEName, dbase, level)

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        #  so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(dbase, WEName, level, timeRange)
        except:
            return trList

        trList = [g.gridTime() for g in gridInfo
                  if timeRange.overlaps(g.gridTime())]
        return trList


    ##
    # Get time ranges locked by other workstations for the weather element named
    # weName. The model for weName is taken from this object's mutableID() method;
    # the level defaults to "SFC", but can be identified for a different level.
    # @param weName: Name of a weather element.
    # @type weName: string
    # @param level: Level of a weather element to inventory (default = "SFC")
    # @type level: string
    # @return: time ranges locked by others
    # @rtype: Python list of TimeRanges; if asJava is True, these are Java 
    # TimeRanges, otherwise they are Python TimeRanges. 
    def GM_getParmLocksByOthers(self, weName, level="SFC"):
        """Return a list of time ranges of locked data within the current 
        mutable database (typically 'Fcst').
        Args:
            string WEName: name of field to inventory
            string level: level of data to inventory (default = 'SFC')
        Returns:
            Python list of Python time range objects
        """

        # returns list of time ranges locked by others for this weather element
        parm = self.getParm(self.mutableID(), weName, level)
        if parm is None:
            return []

        lockTable = parm.getLockTable()
        locksByOthers = lockTable.lockedByOther()
        trList = []

        for lock in locksByOthers.toArray():
            print lock
            
            start = lock.getStart().getTime() / 1000
            end = lock.getEnd().getTime() / 1000
            tr = self.GM_makeTimeRange(start, end)
            
            trList.append(tr)
        
        return trList


    ##
    # Filter trList, returning only the time ranges that overlap timeRange.
    # @param timeRange: the time range to test against
    # @type timeRange: a Python TimeRange
    # @param trList: the list of time ranges to filter
    # @type trList: Python list of Python TimeRanges
    # @param closest: toggle to find time ranges closest to selected time range (False = overlap only)  
    # @type closest: integer
    # @return: The time ranges in trList that overlap timeRange.
    # @rtype: a Python list of Python time ranges
    def GM_overlappingTRs(self, timeRange, trList, closest=False):
        """Return a list of time ranges of locked data within the current 
        mutable database (typically 'Fcst').
        Args:
            TimeRange timeRange: a Python time range object
            list trList: list of Python time range objects
            boolean closest: if True, force new time range list to start and 
                             end with the times closest to the start and end of 
                             initial selected time range.  If False (default),
                             only include times which overlap the initial 
                             selected time range.
        Returns:
            Python list of Python time range objects
        """
    
        #  Get ready to return updated list of times 
        newTRList = []
        
        #  Get ready to track certain times
        beforeTime = None   #  start time closest to selected time range start 
        afterTime = None    #  time range closest to selected time range start
        beforeTR = None     #  start time closest to selected time range end
        afterTR = None      #  time range closest to selected time range end
 
        #  Get start and end time of selected time range
        selectStartTime = timeRange.startTime()     
        selectEndTime = timeRange.endTime()
 
       #=======================================================================
       #  Examine each time range in the list

        for tr in trList:

            #  If this time range overlaps the selected range
            if timeRange.overlaps(tr):

                #  Add it to the list
                newTRList.append(tr)

            #  Otherwise, if we should find the closest time ranges
            elif closest:

                #  Get the start time of this time range
                startTime = tr.startTime()

                #  Compute the difference between the start of this grid and 
                #  the start and end times of our selected time range
                diffStartTime = (startTime - selectStartTime)
                diffEndTime = (startTime - selectEndTime)
#                print "\t", diffStartTime, diffEndTime
     
                #  If start time of this grid is the closest to start time of
                #  selected time range, or it's the first one
                if beforeTime is None or \
                   ((diffStartTime < 0 and diffStartTime >= beforeTime) or
                    (diffStartTime >= 0 and diffStartTime < beforeTime)):
     
                    #  Mark this grid as the closest to the selected start time 
                    beforeTime = diffStartTime
                    beforeTR = tr
     
#                    print "beforeTime =", beforeTime, beforeTR
     
                #  If start time of this grid is the closest to end time of
                #  selected time range, or it's the first one
                if afterTime is None or \
                   (diffEndTime >= 0 and diffEndTime <= abs(afterTime)):
     
                    #  Mark this grid as the closest to the selected end time 
                    afterTime = diffEndTime
                    afterTR = tr
     
#                    print "afterTime =", afterTime, afterTR
    
#                print "newTRList = ", newTRList, beforeTR, afterTR
 
        #  If we don't have any grids in the list and we should determine the
        #  closest grid time ranges to the selected time range
        if len(newTRList) == 0 and closest:
            
            #  Add closest start and end time ranges to selected time range 
            newTRList = [beforeTR, afterTR]
 
        #  Ensure time ranges are sorted when we return them
        newTRList.sort(self.GM_trSortMethod)

        #  Finally, return our completed list
        return newTRList


    ##
    # method so that timeRanges will be sorted earliest to latest
    # @param first: The first time range to compare
    # @type first: Python TimeRange
    # @param last: The second time range to compare
    # @type last: Python TimeRange
    # @return: -1 if first starts before last, 1 if first starts after last,
    #          and 0 if first and last start at the same time.
    # @rtype: integer
    def GM_trSortMethod(self, first, last):
        """Sorts Python time ranges into ascending order.
        Args:
            TimeRange first: a Python time range object
            TimeRange last: a Python time range object
        Returns:
            An integer indicating the ascending order of the compared time 
            range objects. 
        """
        if first.startTime() < last.startTime():
            return -1
        elif first.startTime() == last.startTime():
            return 0
        else:
            return 1


    ##
    # Concatenate TRList1 and TRList2 and sort by starting times.
    # @param TRList1: time ranges of the minT grid
    # @type TRList1: Python list of Python TimeRange objects.
    # @param TRList2: time ranges of the maxT grid
    # @type TRList2: Python list of Python TimeRange objects.
    # @return: The combined and sorted collection.
    # @rtype: Python list of Python TimeRange objects
    def GM_mergeTRLists(self, TRList1, TRList2):
        """Merges and sorts Python time range lists into ascending order.
        Args:
            TimeRange TRList1: a Python time range object
            TimeRange TRList2: a Python time range object
        Returns:
            A merged and sorted list of Python time range objects. 
        """
       
        #  Merge the lists
        combined = set(TRList1) | set(TRList2)

        #  Sort the resulting time range list in ascending order
        newList = sorted(combined, self.GM_trSortMethod)
        
        #  Return the merged and sorted list
        return newList

    #
    ############################################################################
 
    ############################################################################
    #  Other utility methods originally provided by Tom LeFebvre (GSD)
    
    ##
    # Gets the maximum possible time range.
    # @return: The maximum possible Python time range.
    def GM_makeMaxTimeRange(self):
        return TimeRange.allTimes()


    ##
    # Insert a message into the log files.
    # @param string: message to insert into log file
    # @type string: string
    # @return: nothing
    # @rtype: nothing
    def GM_logToolUse(self, string):
        """Inserts an entry into the log files. 
        Args:
            string string: message to be inserted into the log files
        Returns:
            Nothing 
        """

        gtime = self._gmtime().timetuple()
        ts="%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d"%(gtime[0], gtime[1], gtime[2],
                                                  gtime[3], gtime[4], gtime[5])
        
        #  Insert this message into the logs
        LogStream.logEvent("%s| %s" % (ts, string))


    ##
    # Creates a time range
    # @param start: start of time range in seconds since the epoch began
    # @type start: double
    # @param end: end of time range in seconds since the epoch began
    # @type end: double
    # @return: time range
    # @rtype: time range object appropriate for AWIPS version
    def GM_makeTimeRange(self, start, end):
        """Creates a time range. 
        Args:
            double start - start of time range in seconds since the epoch began
            double end - end of time range in seconds since the epoch began
        Returns:
            Time range appropriate for AWIPS version
        """

        startTime = AbsTime.AbsTime(start)
        endTime = AbsTime.AbsTime(end)

        return TimeRange.TimeRange(startTime, endTime)


    ##
    # Creates a list time range objects to process
    # @param executeTR: time range to use in creating list of time steps
    # @type executeTR: time range object appropriate to AWIPS version 
    # @param interpHours: time step to use in hours (default = 1)
    # @type end: integer
    # @return: time range objects
    # @rtype: Python list of time range objects
    def GM_makeTimeRangeList(self, executeTR, interpHours=1):
        """Creates a list of time range objects from specified time range. 
        Args:
            executeTR - time range object appropriate to AWIPS version
            integer interpHours - number of hours between each time step
                                  (default = 1)
        Returns:
            Python list of time range appropriate for AWIPS version
        """

        start = executeTR.startTime().unixTime()
        end = executeTR.endTime().unixTime()

        trList = []
        for t in range(start, end, 3600*interpHours):
          
            tr = self.GM_makeTimeRange(t, t + 3600)
            trList.append(tr)
        return trList


    ##
    # Searches a grid inventory for the first available time ranges before and
    # after the target time range and returns those objects  
    # @param modelInventory: list of available data times for a particular model
    # @type modelInventory: Python list
    # @param targetTR: time range to use as basis for search
    # @type targetTR: time range object
    # @return: time ranges of available data before and after target time range
    # @rtype: time range objects appropriate for AWIPS version or None for missing data 
    def GM_getPrevNextModelTimes(self, modelInventory, targetTR):
        """Creates a time range. 
        Args:
            list modelInventory - list of available data times for a model
            time range targetTR - time range to use as basis for search
        Returns:
            Previous and next time range objects appropriate for AWIPS version,
            or None for missing data
        """

        #  If we have a model inventory 
        if len(modelInventory) == 0:
            print "Model Inventory is empty"
            return None, None
        
        #  Convert target time range object into number of seconds since epoch
        targetTRsecs = targetTR.startTime().unixTime()

        #-----------------------------------------------------------------------
        #  Make sure we're in range
        
        #  Target time range is before all available model data
        if targetTRsecs < modelInventory[0].startTime().unixTime():
            return None, None

        #  Target time range is after all available model data
        if  targetTRsecs > modelInventory[-1].startTime().unixTime():
            return None, None
        
        #-----------------------------------------------------------------------
        #  Search the model inventory
        
        for i in range(len(modelInventory)):
            
            #  If we found the first availble model time ranges on both sides
            #  of the target time range
            if modelInventory[i].startTime().unixTime() < targetTRsecs and \
               modelInventory[i + 1].startTime().unixTime() > targetTRsecs:
                
                #  Return these time range objects
                return modelInventory[i], modelInventory[i+1]

        #  If we made it this far, indicate we could not find appropriate
        #  time range objects 
        return None, None


    ##
    # Interpolates a sounding at the specified time range, if needed.  
    # Otherwise, will use a cached sounding if appropriate. 
    # within the target time range and returns those objects  
    # @param model: model to use to grab cube
    # @type model: string
    # @param weName: weather element name to get cube data for
    # @type weName: string
    # @param levels: list of levels to use in constructing cube 
    # @type levels: Python list 
    # @param timeRange: time range to use as basis for search
    # @type timeRange: time range object
    # @param modelInventory: list of available data times for a particular model
    # @type modelInventory: Python list
    # @return: cube of geopotential height and cube of specified field
    # @rtype: Python tuple of numpy cube data 
    def GM_interpolateSounding(self, model, weName, levels, timeRange,
                               modelInventory):
                
        prevTR, nextTR = self.GM_getPrevNextModelTimes(modelInventory, 
                                                       timeRange)
        if prevTR is None or nextTR is None:
             return None

        prevGHCube, prevCube = self.makeNumericSounding(model, weName, levels,
                                                        prevTR, noDataError=0)
        nextGHCube, nextCube = self.makeNumericSounding(model, weName, levels,
                                                        nextTR, noDataError=0)
        # calculate weights for a time-weighted average
        t1 = timeRange.startTime().unixTime() - prevTR.startTime().unixTime()
        t2 = nextTR.startTime().unixTime() - timeRange.startTime().unixTime()
        prevWt = float(t2) / float(t1 + t2)
        nextWt = float(t1) / float(t1 + t2)
        
        interpGHCube = (prevGHCube * prevWt) + (nextGHCube * nextWt)
        
        #  If this is a cube of scalars
        if re.search("(?i)wind", weName) is None:
            interpCube = (prevCube * prevWt) + (nextCube * nextWt)
        else:

            #  Break up the wind into u and v components
            (prevU, prevV) = self.MagDirToUV(prevCube[0], prevCube[1])
            (nextU, nextV) = self.MagDirToUV(nextCube[0], nextCube[1])

            #  Interpolate the wind components 
            interpU = (prevU * prevWt) + (nextU * nextWt)
            interpV = (prevV * prevWt) + (nextV * nextWt)
 
            #  Now compute the final wind magnitude and direction 
            interpCube = self.UVToMagDir(interpU, interpV)
       
        return interpGHCube, interpCube


    ##
    # Interpolates a grid field at the specified time range, if needed.  
    # Otherwise, will use a cached sounding if appropriate. 
    # within the target time range and returns those objects  
    # @param model: model to use to grab field
    # @type model: string
    # @param weName: weather element name to get cube data for
    # @type weName: string
    # @param level: level of data to interpolate 
    # @type level: string 
    # @param timeRange: time range to use as basis for search
    # @type timeRange: time range object
    # @param modelInventory: list of available data times for a particular model
    # @type modelInventory: Python list
    # @return: grid of specified field
    # @rtype: numpy grid data 
    def GM_interpolateGrid(self, model, weName, level, timeRange, 
                           modelInventory):
        prevTR, nextTR = self.GM_getPrevNextModelTimes(modelInventory,
                                                       timeRange)
                 
        if prevTR is None or nextTR is None:
             return None
         
        prevGrid = self.getGrids(model, weName, level, prevTR, noDataError=0)
        nextGrid = self.getGrids(model, weName, level, nextTR, noDataError=0)

        # calculate weights for a time-weighted average
        t1 = timeRange.startTime().unixTime() - prevTR.startTime().unixTime()
        t2 = nextTR.startTime().unixTime() - timeRange.startTime().unixTime()
        prevWt = t2 / float(t1 + t2)
        nextWt = t1 / float(t1 + t2)

        #  If this is a grid of scalars
        if re.search("(?i)wind", weName) is None:
            finalGrid = (prevGrid * prevWt) + (nextGrid * nextWt)
        else:
 
            #  Break up the wind into u and v components
            (prevU, prevV) = self.MagDirToUV(prevGrid[0], prevGrid[1])
            (nextU, nextV) = self.MagDirToUV(nextGrid[0], nextGrid[1])

            #  Interpolate the wind components 
            interpU = (prevU * prevWt) + (nextU * nextWt)
            interpV = (prevV * prevWt) + (nextV * nextWt)
 
            #  Now compute the final wind magnitude and direction 
            finalGrid = self.UVToMagDir(interpU, interpV)
       
        return finalGrid

    #
    ############################################################################
 
 
    ############################################################################
    #  Define a method to manipulate grid times
    ############################################################################

    ##
    # Produces a list of Python time ranges
    # @param dataDict: time ranges of available data 
    # @type dataDict: Python dictionary keyed by database 
    # @param dataLocks: time ranges which are locked by others  
    # @type dataLocks: Python list of time ranges which are locked by others
    # @param interpHours: requested time step in hours 
    # @type interpHours:  integer
    # @return: list of Python time range objects
    # @rtype: Python list
    def GM_makeNewTRlist(self, dataDict, dataLocks, interpHours=3): 
        """Produces a list of Python time ranges. 
        Args:
            dataDict: Python dictionary of time ranges of available data keyed by database
            dataLocks: Python list of time ranges which are locked by others
            interpHours: requested time step in hours
        Returns:
            Python list of Python time range objects 
        """

        #=======================================================================
        #  Make a new list of time ranges to iterate over

        newTRlist = []

        #-----------------------------------------------------------------------
        #  Look at all the models we have data for

        for model in dataDict.keys():
            #-------------------------------------------------------------------
            #  Start with all time steps from this model

            for tr in dataDict[model].keys():
                #print "TR:", dir(tr)
                
                pyStart = self._gmtime(tr.startTime().unixTime())
                startHour = pyStart.tm_hour
                
#                print "HOUR:", startHour
                #---------------------------------------------------------------
                #  If this time range is not already locked by someone else, and
                #  it is one we would want to have but do not have yet, and it
                #  is one we have data for from this model

#                print "newTRlist:", newTRlist, "type:", type(newTRlist)
#                print "dataLocks:", dataLocks, "type:", type(dataLocks)

                if tr not in newTRlist and tr not in dataLocks and \
                   (startHour % interpHours) == 0 and \
                   dataDict[model][tr] is not None:

                    #  Add this time range to the new time range list
                    newTRlist.append(tr)
        
        #-----------------------------------------------------------------------
        #  Sort new model time range list by time

        newTRlist.sort(self.GM_trSortMethod)

        #-----------------------------------------------------------------------
        #  Return completed consolidated time range list

        return newTRlist


    ############################################################################
    #  Define a method to adjust time range which will be deleted - this is so
    #  only grids for which we have data from selected model will be deleted
    ############################################################################

    ##
    # Produces a new time range which can be used to delete data
    # @param timeRange: initial selected time range 
    # @type timeRange: Python time range object 
    # @param TRList: time ranges with available data  
    # @type TRlist: Python list of time ranges
    # @param adjustTR: number of hours to delete before and after available data to make for easier interpolation 
    # @type adjustTR:  integer
    # @return: list of Python time range objects
    # @rtype: Python list
    def GM_adjustDeleteTimeRange(self, timeRange, TRlist, adjustTR=0):
        """Adjusts a time range for purposes of deleting grids.  The intent is
        to make it easier to interpolate between old and new data. 
        Args:
            timeRange: Python time range object representing selected time 
                    ranage
            TRlist: Python list of Python time range objects where data is 
                    available
            integer adjustTR: number of hours to delete on either side of 
                    available data to make for easier interpolation 
        Returns:
            a TimeRange object spanning adjusted time range 
        """

        #-----------------------------------------------------------------------
        #  Get ready to set new limits of the time range

        newStart = None
        newEnd = None

        #-----------------------------------------------------------------------
        #  Look through the time ranges we have for model data

        for tr in TRlist:

            #  If this grid is in the selected time range
            if timeRange.overlaps(tr):

                #  If we have not yet determined a start time
                if newStart is None:

                    #  Define the new start time
                    newStart = tr.startTime().unixTime() - adjustTR*3600.0

                #  If we have not yet determined an end time
                if tr.endTime().unixTime() > newEnd:

                    #  Define the new end time
                    newEnd = tr.endTime().unixTime() + adjustTR*3600.0

##        print '+'*90
##        print newStart, newEnd
##        print TimeRange.TimeRange(AbsTime.AbsTime(newStart), AbsTime.AbsTime(newEnd))

        #-----------------------------------------------------------------------
        #  Return adjusted time range - if we did adjust it

        if newStart is not None and newEnd is not None:
            
            return TimeRange.TimeRange(AbsTime.AbsTime(newStart), 
                                       AbsTime.AbsTime(newEnd))

        #  Otherwise, return the original time range
        else:
            return timeRange

        
    ############################################################################
    #  Define a method to linearly interpolate data 
    ############################################################################

    ##
    # Produces an updated Python dictionary with interpolated data where needed
    # @param dataDict: data for a specific time, can be mixed (e.g. gh, t, p) 
    # @type dataDict: Python dictionary keyed by Python TimeRange object
    # @param TRList: list of times for  
    # @type TRlist: Python list of time ranges
    # @param adjustTR: number of hours to delete before and after available data to make for easier interpolation 
    # @type adjustTR:  integer
    # @return: list of Python time range objects
    # @rtype: Python list
    def GM_interpolateData(self, dataDict, TRlist, interpHours=3, 
                            vector=[], singleLevel=[]):

        #-----------------------------------------------------------------------
        #  Determine the structure (i.e. how many fields are present) of the
        #  data dictionary

        try:
            numFields = len(dataDict[TRlist[0]])
        except:
            print "No data to interpolate!"
            return dataDict

        #-----------------------------------------------------------------------
        #  Cycle through each time period we already have

        for index in range(len(TRlist) - 1):

#            print "\tindex = ", index

            #-------------------------------------------------------------------
            #  Define a list to hold the times we need to create soundings for

            makeList = []
            
            #-------------------------------------------------------------------
            #  Get the time range of the current and next soundings we have

            current = TRlist[index]
            next = TRlist[index + 1]
#            print '*'*80
#            print current, next

            #-------------------------------------------------------------------
            #  Get the starting times of each sounding time range

            currentStart = current.startTime().unixTime()
            nextStart = next.startTime().unixTime()
            
            #-------------------------------------------------------------------
            #  See how far apart these soundings are in time (hours)

            diffTime = nextStart - currentStart
#            print diffTime, interpHours*3600 

            #-------------------------------------------------------------------
            #  If gap between data time steps are more than what we need

            if int(diffTime) > interpHours*3600:

                #--------------------------------------------------------------
                #  Keep track of seconds we are between data time steps

                curTime = float(interpHours*3600)
                
                #---------------------------------------------------------------
                #  Make a new time range every three hours
#                print '\t', int(currentStart + curTime), int(nextStart)

                while int(currentStart + curTime) < int(nextStart):

                    #-----------------------------------------------------------
                    #  Compute linear interpolation weight

                    weight = curTime / diffTime
#                    print "weight = ", weight
                    
                    #-----------------------------------------------------------
                    #  Make a new TimeRange object for this new time step

                    newTR = TimeRange.TimeRange(
                                 AbsTime.AbsTime(currentStart + curTime),
                                 AbsTime.AbsTime(currentStart + curTime + 3600)
                                )

                    #-----------------------------------------------------------
                    #  Define an empty string to hold all interpolated data
                    #  which should be placed within the final data structure
                    #  for this time

                    finalData = ""

                    #===========================================================
                    #  Interpolate data for each field at this time step

                    for field in range(numFields):

                        #  Create a final data structure for interpolated data
                        exec "data%d = []" % (field)

                        #  If this field is a vector, make component data 
                        #  structures             
                        if field in vector:
                            exec "data%dU = []" % (field)
                            exec "data%dV = []" % (field)

                        #-------------------------------------------------------
                        #  Get data from the current and next time steps we have

                        try:
                            curData = dataDict[current][field]
                        except:
                            #  No point in continuing with this time step
                            msg = "Could not get 'current' data -> %s" % \
                                  (repr(current))
                            self.statusBarMsg(msg, "R")
                            continue                    # move on
                        
                        try:
                            nextData = dataDict[next][field]
                        except:
                            #  No point in continuing with this time step
                            msg = "Could not get 'next' data -> %s" % \
                                  (repr(next))
                            self.statusBarMsg(msg, "R")
                            continue                    # move on

                        #-------------------------------------------------------
                        #  If this field is a vector, separate it into its'
                        #  u and v components

                        if field in vector:

                            (curU, curV) = self.MagDirToUV(curData[0],
                                                           curData[1])

                            (nextU, nextV) = self.MagDirToUV(nextData[0],
                                                             nextData[1])

                        #=======================================================
                        #  If this field is a single level

                        if field in singleLevel:

                            if not vector:
                                data = (curData + (nextData - curData) * weight)                                                               
                            else:
                                u = (curU + (nextU - curU) * weight)                        
                                v = (curV + (nextV - curV) * weight)
                            
                            #---------------------------------------------------
                            #  Get the newly interpolated grids 

                            if not vector:

                                if type(data) == types.ListType:
                                    dataGrid = data[0]                           
                                else:
                                    dataGrid = data

                            else:
                                if type(u) == types.ListType:
                                    uGrid = u[0]                           
                                else:
                                    uGrid = u

                                if type(v) == types.ListType:
                                    vGrid = v[0]                           
                                else:
                                    vGrid = v
                                
                            #---------------------------------------------------
                            #  Add current level into the new data structure

                            if not vector:
                                exec "data%d = array(dataGrid)" % (field)
                            else:
                                exec "data%dU = array(uGrid)" % (field)
                                exec "data%dV = array(vGrid)" % (field)

                        #=======================================================
                        #  Otherwise, cycle through each level in the sounding

                        else:

                            for level in xrange(curData.shape[0]):
                    
                                #-----------------------------------------------
                                #  Construct sounding values for this level

                                if not vector:
                                    data = (curData[level] +
                                        (nextData[level] - curData[level]) *
                                        weight)
                                else:
                                    u = (curU[level] +
                                         (nextU[level] - curU[level]) * weight)
                                    
                                    v = (curV[level] +
                                         (nextV[level] - curV[level]) * weight)
      
                                #-----------------------------------------------
                                #  Get the newly interpolated grids 

                                if not vector:

                                    if type(data) == types.ListType:
                                        dataGrid = data[0]                           
                                    else:
                                        dataGrid = data

                                else:
                                    if type(u) == types.ListType:
                                        uGrid = u[0]                           
                                    else:
                                        uGrid = u

                                    if type(v) == types.ListType:
                                        vGrid = v[0]                           
                                    else:
                                        vGrid = v
                                    
                                #-----------------------------------------------
                                #  Add current level into the new sounding

                                if not vector:
                                    exec "data%d = data%d + [dataGrid]" % \
                                         (field, field)
                                else:
                                    exec "data%dU = data%dU + [uGrid]" % \
                                         (field, field)
                                    exec "data%dV = data%dV + [vGrid]" % \
                                         (field, field)

                            #---------------------------------------------------
                            #  Finish off the new cube for this time

                            if not vector:
                                exec "data%d = array(data%d)" % (field, field)
                            else:                        
                                exec "data%dU = array(data%dU)" % (field, field)
                                exec "data%dV = array(data%dV)" % (field, field)

                        #=======================================================
                        #  If this is a vector field, reconstruct vector from
                        #  the components

                        if vector:
                            exec "data%d = self.UVToMagDir(data%dU, data%dV)" %\
                                 (field, field, field)

                        #=======================================================
                        #  Add current interpolated data for this time step to
                        #  the final data structure

                        exec "finalData += 'data%d'" % (field)

                        if field < (numFields - 1):
                            finalData += ", "

                    #-----------------------------------------------------------
                    #  Add this interpolated data to data structure

                    exec "dataDict[newTR] = (%s)" % (finalData)

                    msg = "Created data for -> %s" % (repr(newTR))
                    self.statusBarMsg(msg, "R")
                    
                    #-----------------------------------------------------------
                    #  Move on to next desired time step

                    curTime += float(interpHours)*3600.0

        #-----------------------------------------------------------------------
        #  Return the completed data dictionary

        return dataDict


    ############################################################################
    #  Define a method to smooth data 
    ############################################################################

    ##
    # Produces a smoother version of a numpy grid.
    # @param grid: numpy grid to be smoothed 
    # @type grid: numpy array
    # @param factor: factor to control level of smoothing 
    # @type factor: Python integer
    # @return: smoother grid object
    # @rtype: numpy array
    def GM_smoothGrid(self, grid, factor=3):
        # This code is essentially the NumericSmooth example
        # smart tool customized for our purposes.
        # factors of less than 3 are useless or dangerous
        if factor < 3:
            return grid

        half = int(factor)/ 2
        sg = np.zeros(grid.shape, np.float64)
        count = np.zeros(grid.shape, np.float64)
        for y in range(-half, half + 1):
            for x in range(-half, half + 1):
                if y < 0:
                    yTargetSlice = slice(-y, None, None)
                    ySrcSlice = slice(0, y, None)
                if y == 0:
                    yTargetSlice = slice(0, None, None)
                    ySrcSlice = slice(0, None, None)
                if y > 0:
                    yTargetSlice = slice(0, -y, None)
                    ySrcSlice = slice(y, None, None)
                if x < 0:
                    xTargetSlice = slice(-x, None, None)
                    xSrcSlice = slice(0, x, None)
                if x == 0:
                    xTargetSlice = slice(0, None, None)
                    xSrcSlice = slice(0, None, None)
                if x > 0:
                    xTargetSlice = slice(0, -x, None)
                    xSrcSlice = slice(x, None, None)

                target = [yTargetSlice, xTargetSlice]
                src = [ySrcSlice, xSrcSlice]
                sg[target] = sg[target] + grid[src]
                count[target] += 1
        return sg / count
