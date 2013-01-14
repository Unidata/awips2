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
########################################################################
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
#    SmartScript -- library of methods for Smart Tools and Procedures
# History
# Time        Ticket#      Developer    Comments
# ----------------------------------------------------------------------
# 01/09/2012  DR15626      J. Zeng      Add methods 
#                                       enableISCsend
#                                       clientISCSendStatus
#                                       manualSendISC_autoMode
#                                       manualSendISC_manualMode
#
# Author: hansen
# ----------------------------------------------------------------------------
########################################################################
import types, string, time, sys
from math import *
from numpy import *
import os
import numpy
import re
import jep
import BaseTool, Exceptions
import DatabaseID, TimeRange, AbsTime, ParmID
import GridInfo
import JUtil

from java.util import ArrayList
from java.util import Date
from java.nio import FloatBuffer

from com.raytheon.uf.common.time import SimulatedTime
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DByte
from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DFloat
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKey
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteKeyDef
from com.raytheon.uf.common.dataplugin.gfe.discrete import DiscreteDefinition
from com.raytheon.uf.common.dataplugin.gfe.weather import WeatherKey
from com.raytheon.uf.common.dataplugin.gfe.db.objects import TimeConstraints
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GridParmInfo
from com.raytheon.uf.common.dataplugin.gfe.server.request import SendISCRequest

class SmartScript(BaseTool.BaseTool):
    
    def __init__(self, dataMgr):    
        BaseTool.BaseTool.__init__(self)
        self.__dataMgr = dataMgr
        self.__parmMgr = self.__dataMgr.getParmManager()
        self.__refSetMgr = self.__dataMgr.getRefManager()
        self.__mutableID = DatabaseID.DatabaseID(self.__parmMgr.getMutableDatabase())
        self.__cycler = self.__dataMgr.getGridCycler()
        self.__parmOp = self.__dataMgr.getParmOp()
        # A cache of grids accessed by the derived class
        #self.__pythonGrids = []
        self.__accessTime = 0
        self.__gridLoc = None
        self.__topoGrid = None        
        self.__toolType = "numeric"
        self._empty = zeros(self.getGridShape(), float32)
        self._minus = self._empty - 1
        self._handlers = dict() 

    
    ##
    ## Call ProcessVariableList to obtain values from the user
    ##
    ## @param VariableList: list() of tuples describing the widgets to display
    ##
    ## @return dict() of values gathered from the widgets
    ##     
    def getVariableListInputs(self, VariableList):
        import ProcessVariableList
        return ProcessVariableList.buildWidgetList(VariableList)


    def mutableID(self):
        # Returns the mutable database ID
        return self.__mutableID

    def getGridLoc(self):
        if self.__gridLoc is None:
            parmIDs = self.__parmMgr.getAvailableParms(self.__mutableID.toJavaObj())
            if len(parmIDs) > 0:
                tempParmID = parmIDs[0]
                info = self.__dataMgr.getClient().getGridParmInfo(tempParmID)
                self.__gridLoc = info.getGridLoc()
        return self.__gridLoc

    def setToolType(self, toolType):
        # Tool type is "point-based", "numeric", "parm-based"
        # It is set when SmartScript is instantiated.
        # For Procedures, it is set to the default of "point-based"
        # So a procedure can override this by using this method.
        self.__toolType = toolType

    def editAreaList(self):
        # Returns list of all known edit areas, as a list of strings.
        eans = self.__refSetMgr.getAvailableSets()
        eaList = []
        size = eans.size()
        for i in range(size):
            eaList.append(eans.get(i).getName())
        return eaList

    def getSite4ID(self, id3):
        # Returns 4-letter site id, based on 3-letter site id
        if id3 in ['SJU']:
            return "TJSJ"
        elif id3 in ['AFG', 'AJK', 'HFO', 'GUM']:
            return "P" + id3
        elif id3 in ['AER', 'ALU']:
            return "PAFC"
        else:
            return "K" + id3


    def loadedParms(self):
        # Returns a list of tuples that are weather elements that are 
        # loaded.  The tuples are (element, level, model).  element and
        # level are strings.  model is a DatabaseID.
        allParms = self.__parmMgr.getAllParms()
        retList = []
        for p in allParms:
            pid = p.getParmID()
            dbid = DatabaseID.DatabaseID(pid.getDbId())
            retList.append((pid.getParmName(), pid.getParmLevel(), dbid))
        return retList

    def availableParms(self):
        # Returns a list of tuples that are weather elements that are 
        # available.  The tuples are (element, level, model).  element and
        # level are strings.  model is a DatabaseID.
        retList = []
        dbs = self.__parmMgr.getAvailableDbs()
        for i in range(dbs.size()):
            d = dbs.get(i);
            parms = self.__parmMgr.getAvailableParms(d)
            for pid in parms:
                dbid = DatabaseID.DatabaseID(pid.getDbId())
                retList.append((pid.getParmName(), pid.getParmLevel(), dbid))
        return retList

    def selectedParms(self):
        # Returns a list of tuples that are weather elements that are
        # currently selected.  The tuples are (element, level, model).
        # Element and level are string. model is a DatabaseID.
        retList = []
        parms = self.__parmMgr.getSelectedParms()
        for p in parms:
            parmid = p.getParmID()
            javaDbId = parmid.getDbId()
            dbid = None
            if javaDbId is not None:
                dbid = DatabaseID.DatabaseID(javaDbId)
            retList.append((parmid.getParmName(), parmid.getParmLevel(),
              dbid))

        return retList
    
    def loadParm(self, model, element, level, mostRecent=0):
        # loads a parm and makes it visible.
        parm = self.getParm(model, element, level, timeRange=None,
          mostRecent=mostRecent)
        if parm is not None:
            self.__parmMgr.setParmDisplayable(parm, 1)
        else:
            raise TypeError("SmartScript loadParm: " + \
              "couldn't load " + `model` + ' ' + `element` + ' ' + `level` + \
              ' ' + mostRecent)
    ##
    # Get the list of timeranges locked by me in this weather element.
    #
    # @param weName: Weather element to look for locks on
    # @type weName: String
    # @param level: The level of the element to look for locks on
    # @type level: String
    # @return: The time ranges 
    # @rtype: Python list of Python TimeRanges
    def lockedByMe(self, weName, level):
        # returns list of time ranges locked by me in this weather element
        # Uses the mutable database
        parm = self.getParm(self.mutableID(), weName, level)
        if parm is None:
            return []
        lt = parm.getLockTable()
        jlbm = lt.lockedByMe()
        # jlbm is a Java list of Java TimeRanges. Convert it to Python.
        jlbmIter = jlbm.iterator()
        lbm = []
        while (jlbmIter.hasNext()):
            jtr = jlbmIter.next()
            tr = TimeRange.TimeRange(jtr)
            lbm.append(tr)
        return lbm
    
    ##
    # Get the list of timeranges locked by other users in this weather element.
    #
    # @param weName: Weather element to look for locks on
    # @type weName: String
    # @param level: The level of the element to look for locks on
    # @type level: String
    # @return: The time ranges 
    # @rtype: Python list of Python TimeRanges
    def lockedByOther(self, weName, level):
        # returns list of time ranges locked by others in this weather element
        # Uses the mutable database
        parm = self.getParm(self.mutableID(), weName, level)
        if parm is None:
            return []
        lt = parm.getLockTable()
        jlbo = lt.lockedByOther()
        # jlbo is a Java list of Java TimeRanges. Convert it to Python.
        jlboIter = jlbo.iterator()
        lbo = []
        while (jlboIter.hasNext()):
            jtr = jlboIter.next()
            tr = TimeRange.TimeRange(jtr)
            lbo.append(tr)
        return lbo

    def forceLock(self, weName, level, startT, endT):
        # forces locks in the given time range (startT to endT).
        # startT, endT can either be ints/floats, or should be AbsTimes
        # Returns 0 if not successful, 1 for okay.
        if (type(startT) is types.IntType or type(startT) is types.FloatType) \
          and (type(endT) is types.IntType or type(endT) is types.FloatType):
            t1 = AbsTime.AbsTime(int(startT))
            t2 = AbsTime.AbsTime(int(endT))
            tr = TimeRange.TimeRange(t1, t2)
        else:
            tr = TimeRange.TimeRange(startT, endT)   #AbsTime
        parm = self.getParm(self.mutableID(), weName, level)
        if parm is None:
            return 0
        else:
            return parm.forceLockTR(tr.toJavaObj())
            

    def vtecActiveTable(self):
        #returns the VTEC active table (or specified table)
        import ActiveTableVtec
        entries = self.__dataMgr.getClient().getVTECActiveTable(self.__dataMgr.getSiteID())
        return ActiveTableVtec.transformActiveTableToPython(entries)


    def gfeOperatingMode(self):
        #returns the current operating mode of the GFE.
        #Standard, PRACTICE, TEST
        return self.__dataMgr.getOpMode().name()
   
#------------------------------------------------------------------------
# ISC control functions
#------------------------------------------------------------------------

    def enableISCsend(self, state):
        #sets the overall isc send state.  If the send state is false, then
        #no ISC grids can be transmitted.  To change the behavior
        #when these programs (e.g., procedures) are run from the command line,
        #you can enable/disable the send capability upon saving.  This
        #command does not send grids, but sets the system state.  When
        #saving grids and SendISCOnSave is set, or the manual Send ISC Dialog
        #is used, then the grids will be sent.
        self.__dataMgr.enableISCsend(state)

    def clientISCSendStatus(self):
        #returns the current state for sending isc from this program.  This
        #depicts the state of whether this client has been enabled to send
        #ISC via the SendISCOnSave or manual Send ISC Dialog.  The ifpServer
        #still needs to be properly configured for sending to occur.
        return self.__dataMgr.clientISCSendStatus()

    def manualSendISC_autoMode(self):
        #Simulates the use of the SendISCDialog.  Note if the ifpServer's
        #SendISCOnSave is enabled, then this routine will fail as grids are
        #sent when saved and the manual operation is not allowed.  The
        #overall isc send state must also be True for this command to work.
        req = []
        parms = self.__parmMgr.getAllAvailableParms();
        for parm in parms:
            pid = parm.getParmID()
            tr = parm.getParmTimeRange()
            req.append(SendISCRequest(pid,tr)) 
        self.__parmOp.sendISC(req)

    def manualSendISC_manualMode(self, requests):
        #simulates the use of the SendISCDialog.  Note if the ifpServers's
        #SendISCOnSave is enabled, then this routine will fail as grids are
        #sent when saved and the manual operation is not allowed.
        #The requests are tuples of (parmName, parmLevel, timeRange). The
        #TimeRange is an AFPS.TimeRange() instance.  The overall isc
        #send state must also be True for this command to work.
        req = []
        for parmName, parmLevel, tr in requests:
            pid = ParmID.ParmID(parmName, self.mutableID(),
              parmLevel).toJavaObj()
            req.append(SendISCRequest(pid, tr))
        self.__parmOp.sendISC(req)


#########################################################################
## Smart Tool methods                                                  ##
#########################################################################

        # Arguments
        #   The following arguments are used throughout the
        #   SmartScript Library methods
        #
        # self: When you call a method, use the "self" prefix (see
        #       examples below)
        # model: There are various ways to specify the database model
        #       from which you want the values:
        #    -- Simply "Fcst" or "Official" OR
        #    -- siteID_type_model_modeltime
        #       where the "type" is an empty string for Standard GFE data
        #       and is "D2D" for D2D data.
        #       Examples:
        #         BOU__NAM12_Mar2912  :gets March 29 12Z NAM12 run created by GFE.
        #         BOU_D2D_NAM12_Mar2912  :gets March 29 12Z original NAM12 run from D2D.
        #       If you omit the "modeltime", the most recent model run will
        #       be selected. For example:
        #         BOU__NAM12 : gets the most recent NAM12 run created by GFE.
        #         BOU_D2D_NAM12 : gets the most recent original NAM12 run from D2D.
        #    -- the result of soliciting a model from the user using the
        #       "model" or "D2D_model" type of VariableList entry. (See
        #       examples above.)
        #    -- you may also use a DatabaseID (see getDatabase, below)
        #    -- simple string with no special characters (this will be
        #       assumed to be a model created "on-the-fly"
        # element: The element name in quotes:
        #       e.g.  "QPF", "rh", "tp"
        # level: The level in quotes:
        #       e.g. "SFC", "MB350", "BL030"
        # x, y: integer coordinates
        # timeRange: Must be a special time range object such as
        #   that passed in the argument list as GridTimeRange
        # mode: specifies how to handle the situation if multiple grids
        #   are found within the given time range:
        #   "TimeWtAverage": return time-weighted Average value
        #   "Average" : return Average values
        #   "Max" : return Max values
        #   "Min" : return Min values
        #   "Sum" : return Summed values
        #   "First" : return values from grid with earliest time range
        #   "List" : return list of grids (or values for getValue)
        # noDataError: If 1, and there is no data, the Smart Tool will abort.
        #   Otherwise, return None. None is a special variable in Python
        #   which can be tested as follows:
        #     PoP = self.getGrid("Fcst", "PoP", "SFC", GridTimeRange,
        #           noDataError=0)
        #     if PoP is None:
        #         print "No data found for PoP"
        # mostRecentModel: Applies only to model data. Will get the
        #   most recent model and ignore any times (if included) in the
        #   model argument.  (Note that if a time is not included in the
        #   model argument, you will automatically get the most recent
        #   model no matter how this argument is set.)

    ###########################
    ## Grid Access methods

    def getGrids(self, model, element, level, timeRange,
                 mode="TimeWtAverage",
                 noDataError=1, mostRecentModel=0,
                 cache=1):
        # Get the value(s) for the given model, element, and level
        #   at the x, y coordinate and over the given timeRange.
        #
        # The resulting grid values can be accessed as follows:
        #   PoPGrid = self.getGrids("Fcst","PoP","SFC", GridTimeRange)
        #   popValue = PoPGrid[x][y]
        #
        #  where x and y are integer grid coordinates.
        #
        # The argument descriptions are given above

        if isinstance(model, DatabaseID.DatabaseID):
            model = model.modelIdentifier()
            
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()

#        if cache:
#            for cModel, cElement, cLevel, cMostRecent, cRange, cMode, cResult in \
#                    self.__pythonGrids:
#                if cModel == model and cElement == element and \
#                       cLevel == level and cRange == timeRange \
#                       and cMode == mode and cMostRecent == mostRecentModel:
#                    return cResult

        # Get the parm from parmMgr, find the corresponding result
        exprName = self.getExprName(model, element, level, mostRecentModel)
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        if parm is None:
            if noDataError == 1:
                raise Exceptions.EditActionError(
                    "NoData", "No Weather Element for " + exprName)
            else:
                return None
        result = self.__cycler.getCorrespondingResult(parm, timeRange, mode)        
        retVal = None        
        if result is not None:
            if len(result) == 0:
                retVal = None
            elif "List" == mode:
                xlated = []
                for rgrid in result:
                    xlgrid = rgrid.getGridSlice()
                    xlgrid = xlgrid.__numpy__
                    if len(xlgrid) == 1:
                        xlgrid = xlgrid[0];
                    elif len(xlgrid) == 2 and isinstance(xlgrid[1], str):
                        xlgrid[1] = eval(xlgrid[1])
                    xlated.append(xlgrid)
                retVal = xlated
            else:
                result = result[0];
                result = result.getGridSlice()
                result = result.__numpy__
                if len(result) == 1:
                    retVal = result[0]
                elif len(result) == 2 and isinstance(result[1], str):
                    retVal = (result[0], eval(result[1]))
                else:
                    retVal = (result[0], result[1])
            
        if retVal is None or retVal == []:
            if noDataError == 1:
                msg = "No corresponding grids for " + exprName + " " + str(timeRange)
                raise UserWarning(msg)
#        else:
#            self.__pythonGrids.append((model, element, level, mostRecentModel,
#                                       timeRange, mode, retVal))
        return retVal


    # Returns history info for the specified model, element, level and
    # timerange.  ISC grids force this to be a list of lists [[]].
    def getGridHistory(self, model, element, level, timeRange):
        if isinstance(model, DatabaseID.DatabaseID):
            model = model.modelIdentifier()
        exprName = self.getExprName(model, element, level)
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        if parm is None:
            raise Exceptions.EditActionError(
                    "NoData", "getGridInfo: No Weather Element " + exprName)
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        grids = parm.getGridInventory(timeRange)
        if len(grids) == 0:
            return []
        historyList = []
        for grid in grids:
            history = grid.getHistory()
            histList = []
            for h in history:
                histList.append((str(h.getOrigin()), 
                                 ParmID.ParmID(jParmId=h.getOriginParm()), 
                                 TimeRange.TimeRange(h.getOriginTimeRange()),
                                 AbsTime.AbsTime(h.getTimeModified()), 
                                 str(h.getWhoModified()), 
                                 AbsTime.AbsTime(h.getUpdateTime()),
                                 AbsTime.AbsTime(h.getPublishTime())))

            historyList.append(histList)

        return historyList

    def taperGrid(self, editArea, taperFactor=5):
        # Returns a 2-D Grid of values between 0-1 about the
        # given edit area.
        # These values can be applied by smart tools to taper results.
        # Argument:
        #   editArea : must be of type AFPS.ReferenceData or None
        #              (use editArea tool argument)
        #   taperFactor: If set to zero, will do Full Taper
        # Example:
        #  def preProcessTool(self, editArea):
        #     self._tGrid = self.taperGrid(editArea, 5)
        #  def execute(self, variableElement):
        #     return = variableElement + self._tGrid * 10.0
        #
        taperGrid = self.__refSetMgr.taperGrid(editArea, taperFactor)
        taperGrid = taperGrid.__numpy__
        taperGrid = taperGrid[0]
        return taperGrid

    def directionTaperGrid(self, editArea, direction):
        # Returns a 2-D Grid of values between 0-1 within the
        # given edit area.
        # E.g. if the Dir is W and x,y is half-way along the
        #  W to E vector within the given edit area, the value of
        #  directionTaperGrid at x,y will be .5
        # These values can be applied by smart tools to show
        #  spatial progress across an edit area.
        # Argument:
        #   editArea : must be of type AFPS.ReferenceData or None
        #              (use editArea tool argument)
        #   direction : 16 point text direction e.g. "NNW", "NW", etc.
        # Example:
        #  def preProcessTool(self, editArea):
        #      self._spaceProgress = self.directionTaperGrid(editArea, "NW")
        #  def execute(self, variableElement):
        #      return variableElement * self._spaceProgress
        #
        taperGrid = self.__refSetMgr.directionTaperGrid(editArea, direction)
        taperGrid = taperGrid.__numpy__
        taperGrid = taperGrid[0]
        return taperGrid


    def getComposite(self, WEname, GridTimeRange, exactMatch=1, onlyISC=0):
        # Returns a composite grid consisting of the primary grid and any
        # corresponding ISC grid, blended together based on the mask information
        # derived from the Grid Data History. Primary grid must exist. Returns
        # the set of points that are valid in the output grid. (Note the output
        # grid consists of the primary grid and isc grid. Any "invalid" points,
        # indicate those areas that have no isc data and are outside the home
        # site's region.  The returned grid will have the primary data in
        # the site's region.)
        #
        # A Python tuple is returned.
        # For Scalar elements, the tuple contains:
        #   a numeric grid of 1's and 0's where 1 indicates a valid point
        #   a numeric grid of scalar values
        # For Vector elements, the tuple contains:
        #   a numeric grid of 1's and 0's where 1 indicates a valid point
        #   a numeric grid of scalar values representing magnitude
        #   a numeric grid of scalar values representing direction
        # For Weather elements, the tuple contains:
        #   a numeric grid of 1's and 0's where 1 indicates a valid point
        #   a numeric grid of byte values representing the weather value
        #   list of keys corresponding to the weather values
        #
        # For example:
        #    isc = self.getComposite(WEname, GridTimeRange)
        #    if isc is None:
        #      self.noData()
        #    # See if we are working with a Scalar or Vector element
        #    wxType = variableElement_GridInfo.type()
        #    if wxType == 0: # SCALAR
        #         bits, values = isc
        #    elif wxType == 1: # VECTOR
        #         bits, mag, dir = isc


        if onlyISC == 0:
            exprName = self.getExprName("Fcst", WEname, "SFC")
        else:
            exprName = self.getExprName("ISC", WEname, "SFC")
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        if parm is None:
            return None
        seTime = AbsTime.AbsTime(self.__dataMgr.getSpatialDisplayManager().getSpatialEditorTime())
        if GridTimeRange.contains(seTime):
            gridTime = seTime
        else:
            gridTime = GridTimeRange.startTime()
        from com.raytheon.viz.gfe.edittool import GridID
        gid = GridID(parm, gridTime.javaDate())
        from com.raytheon.uf.common.dataplugin.gfe.db.objects import GFERecord_GridType as GridType
        
        wxType = self.__dataMgr.getClient().getGridParmInfo(parm.getParmID()).getGridType()
        if GridType.SCALAR.equals(wxType):
            from com.raytheon.uf.common.dataplugin.gfe.slice import ScalarGridSlice
            slice = ScalarGridSlice()
            bits = self.__dataMgr.getIscDataAccess().getCompositeGrid(gid, exactMatch, slice)
            args = (bits.__numpy__[0], slice.getScalarGrid().__numpy__[0])
        elif GridType.VECTOR.equals(wxType):
            from com.raytheon.uf.common.dataplugin.gfe.slice import VectorGridSlice
            slice = VectorGridSlice()
            bits = self.__dataMgr.getIscDataAccess().getVectorCompositeGrid(gid, exactMatch, slice)
            args = (bits.__numpy__[0], slice.getMagGrid().__numpy__[0], slice.getDirGrid().__numpy__[0])
        elif GridType.WEATHER.equals(wxType):
            from com.raytheon.uf.common.dataplugin.gfe.slice import WeatherGridSlice
            slice = WeatherGridSlice()
            bits = self.__dataMgr.getIscDataAccess().getCompositeGrid(gid, exactMatch, slice)
            keys = []
            for k in slice.getKeys():
                keys.append(str(k))
            args = (bits.__numpy__[0], slice.getWeatherGrid().__numpy__[0], keys)
        elif GridType.DISCRETE.equals(wxType):
            from com.raytheon.uf.common.dataplugin.gfe.slice import DiscreteGridSlice
            slice = DiscreteGridSlice()
            bits = self.__dataMgr.getIscDataAccess().getCompositeGrid(gid, exactMatch, slice)
            keys = []
            for k in slice.getKey():
                keys.append(str(k))
            args = (bits.__numpy__[0], slice.getDiscreteGrid().__numpy__[0], keys)
        return args

    ##
    # Return the GridInfo object for the given weather element and timeRange
    # Example:
    #    timeRange = self.getTimeRange("Today")
    #    infoList = self.getGridInfo("Fcst", "T", "SFC", timeRange)
    #    for info in infoList:
    #        print "grid", info.gridTime()
    #
    # @param model: The model for which grid info is requested.
    # @type model: DatabaseId or String
    # @param element: The element for which grid info is requested.
    # @type element: String
    # @param level: The level for which grid info is requested.
    # @type level: String
    # @param timeRange: A time range over which grid info is requested.
    # @type timeRange: com.raytheon.uf.common.time.TimeRange or TimeRange
    # @param mostRecentModel: whether to use current time in request expr.
    # @type mostRecentModel: integer or boolean
    # @return: Java GridParmInfo object 
    def getGridInfo(self, model, element, level, timeRange,
                    mostRecentModel=0):
        if isinstance(model, DatabaseID.DatabaseID):
            model = model.modelIdentifier()
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        parm = self.getParm(model, element, level, mostRecentModel)
        if parm is None:
            exprName = self.getExprName(model, element, level, mostRecentModel)
            raise Exceptions.EditActionError(
                    "NoData", "getGridInfo: No Weather Element " + exprName)
        grids = parm.getGridInventory(timeRange)
        if len(grids) == 0:
            return []
        gridParmInfo = parm.getGridInfo()
        gridInfos = []
        for grid in grids:
            timeRange = grid.getGridTime()
            gridInfo = GridInfo.GridInfo(gridParmInfo=gridParmInfo,
                                         gridTime=timeRange)
            gridInfos.append(gridInfo)
        return gridInfos

    ###########################
    ## Sounding methods

    # Numeric only
    def makeNumericSounding(self, model, element, levels, timeRange,
                     noDataError=1, mostRecentModel=0):
        # Make a numeric sounding for the given model, element, and levels

        # Example:
        #  levels = ["MB850","MB800","MB750","MB700","MB650","MB600"]
        #  gh_Cube, rh_Cube = self.makeNumericSounding(
        #                         model, "rh", levels, GridTimeRange)
        #
        # Arguments:
        #
        # The "levels" argument is a Python list of levels INCREASING
        #  in height.
        # This method returns two numeric cubes:
        #   ghCube of geopotential heights for the given levels
        #   valueCube of values for the given levels

        ghCube = []
        valueCube = []
        magCube = []
        dirCube = []
        for level in levels:

            ghGrids = self.getGrids(model, "gh", level, timeRange,
                                    noDataError=noDataError,
                                    mostRecentModel=mostRecentModel)
            if ghGrids is None:
                return None

            valueGrids = self.getGrids(model, element, level, timeRange,
                                       noDataError=noDataError,
                                       mostRecentModel=mostRecentModel)
            if valueGrids is None:
                return None

            if type(ghGrids) == types.ListType:
                ghGrid = ghGrids[0]
            else:
                ghGrid = ghGrids

            if type(valueGrids) == types.ListType:
                valueGrid = valueGrids[0]
            else:
                valueGrid = valueGrids

            #jdynina ghCube = ghCube + [ghGrid]
            ghCube.append(ghGrid)

            if type(valueGrid) == types.TupleType:
                magCube = magCube + [valueGrid[0]]
                dirCube = dirCube + [valueGrid[1]]
            else:
                valueCube = valueCube + [valueGrid]

        ghCube = array(ghCube)
        if len(magCube) > 0:
            magCube = array(magCube)
            dirCube = array(dirCube)
            valueCube = (magCube, dirCube)
        else:
            valueCube = array(valueCube)
        return (ghCube, valueCube)

    # numeric only
    def getNumericMeanValue(self, model, element, levels, timeRange,
                     noDataError=1):
        # Return a numeric array of mean values for the given element
        #  between and including the given levels
        if len(levels) < 1:
            return self.errorReturn(
                noDataError,
                "SmartScript.getNumericMeanValue:: No Levels for Mean Value.")
        elementType = "Scalar"
        empty = self.getTopo() * 0.0
        totalValue = empty
        uSum = empty
        vSum = empty
        for level in levels:
            value = self.getGrids(model, element, level, timeRange,
                             noDataError=noDataError)
            if type(value) == types.TupleType:
                elementType = "Vector"
                uw, vw = self.MagDirToUV(value[0], value[1])
                uSum = uSum + uw
                vSum = vSum + vw
            else:
                totalValue = totalValue + value
        # Compute the average
        totCount = float(len(levels))
        if elementType == "Scalar":
            return totalValue / totCount
        else:
            u = uSum / totCount
            v = vSum / totCount
            mag, dir = self.UVToMagDir(u, v)
            mag = int(mag + 0.5)
            dir = int(dir + 0.5)
            return (mag, dir)


    ###########################
    ## Conversion methods

    def UVToMagDir(self, u, v):
        RAD_TO_DEG = 57.29577951308232
        # Sign change to make math to meteor. coords work
        u = - u
        v = - v
        speed = numpy.sqrt(u * u + v * v)
        dir = numpy.arctan2(u, v) * RAD_TO_DEG
        dir = numpy.where(numpy.greater_equal(dir, 360), dir - 360, dir)
        dir = numpy.where(numpy.less(dir, 0), dir + 360, dir)
        return (speed, dir)

    def MagDirToUV(self, mag, dir):
        DEG_TO_RAD = 0.017453292519943295
        # Note sign change for components so math to meteor. coords works
        uw = - sin(dir * DEG_TO_RAD) * mag
        vw = - cos(dir * DEG_TO_RAD) * mag
        return (uw, vw)
    
    def convertMsecToKts(self, value_Msec):
        # Convert from meters/sec to Kts
        return value_Msec * 1.944
    
    def convertKtoF(self, t_K):
        # Convert the temperature from Kelvin to Fahrenheit
        # Degrees Fahrenheit = (Degrees Kelvin - 273.15) / (5/9) + 32
        t_F = (t_K - 273.15) * 9.0 / 5.0 + 32.0
        return t_F

    def KtoF(self, t_K):
        return self.convertKtoF(t_K)

    def convertFtoK(self, t_F):
        # Convert the temperature from Kelvin to Fahrenheit
        # Degrees Kelvin = (Degrees Fahrenheit - 32) * (5 / 9) + 273.15
        t_K = (t_F - 32.0) * (5.0 / 9.0) + 273.15;
        return t_K

    def FtoK(self, t_F):
        return self.convertFtoK(t_F)

    def convertFtToM(self, value_Ft):
        # Convert the value in Feet to Meters
        return value_Ft/3.28084
    
#########################################################################
## Error Handling                                                      ##
#########################################################################

    def abort(self, info):
        # This call will send the info to the GFE status bar,
        #  put up a dialog with the given info, and abort the
        #  smart tool or procedure.
        # Example:
        #  self.abort("Error processing my tool")
        #
        raise TypeError, info
    
    def noData(self, info="Insufficient Data to run Tool"):
        # Raise the NoData exception error
        raise Exceptions.EditActionError("NoData", info)
    
    def cancel(self):
        # Cancels a smart tool without displaying an error message
        raise Exceptions.EditActionError("Cancel", "Cancel")

    def errorReturn(self, noDataError, message):
        if noDataError == 1:
            self.abort(message)
        else:
            return None

    ##
    # Sends the text message to the GFE status bar with the
    #  given status code: "R" (regular), "S" (significant), "U" (urgent),
    #  or "A" (alert)
    # Example:
    #  self.statusBarMsg("Running Smart Tool", "R")
    # 
    # @param message: The message to send.
    # @type message: string
    # @param status: Importance of message. "A"=Alert, "R"=Regular, "U"=Urgent;
    #                anything else=Significant
    # @type status: string
    # @param category: The message category. Defaults to "GFE".
    # @type category: string
    # @return: None
    def statusBarMsg(self, message, status, category="GFE"):
        from com.raytheon.uf.common.status import UFStatus
        from com.raytheon.uf.common.status import UFStatus_Priority as Priority
        
        if "A" == status:
            importance = Priority.PROBLEM
        elif "R" == status:
            importance = Priority.EVENTA
        elif "U" == status:
            importance = Priority.CRITICAL
        else:
            importance = Priority.SIGNIFICANT
        
        if category not in self._handlers:
            self._handlers[category] = UFStatus.getHandler("GFE", category, 'GFE')
        
        self._handlers[category].handle(importance, message);

   #########################
    ##  Smart Commands
    ##
    ## These commands take some similar arguments:
    ##   editArea : must be of type AFPS.ReferenceData or None
    ##              (See getEditArea)
    ##              If you specify None, the system will supply
    ##              the active edit area from the GFE or from
    ##              the editArea argument for runProcedure.
    ##   timeRange: must be of type AFPS.TimeRange or None
    ##              (See getTimeRange and createTimeRange)
    ##              If you specify None, the system will supply
    ##              the selected Time Range from the GFE or from
    ##              the timeRange argument for runProcedure.
    ##   varDict  : If you supply a varDict in this call, the
    ##              variable list dialog will not be displayed
    ##              when the tool is run.
    ##              If you supply a varDict from a Procedure,
    ##              make sure that the variables
    ##              for all the tools called by the Procedure are
    ##              supplied in your varDict.
    ##   missingDataMode: Can be "Stop", "Skip", or "Create". If not
    ##              included, will be set to the current GFE default.
    ##   modal:     If 0, VariableList dialogs will appear with the
    ##              non-modal "Run" and "Run/Dismiss" buttons.
    ##              Otherwise, they will appear with the "Ok" button.
    ##
    ##  If editValues is true, the grid values are changed.
    ##  FOR POINT-BASED TOOLS ONLY:
    ##     If calcArea is true, a reference area is created and saved which
    ##       shows discrepancies greater than the DiscrepancyValue between the current
    ##       value and new value.
    ##     If calcGrid is true, a scalar grid is created which shows the discrepancy
    ##       amount between the current value and new value. (Not implemented.)
    ##
    ## These commands all return an error which will be None if no
    ##   errors occurred.  Otherwise, the errorType and errorInfo
    ##   can be accessed e.g. error.errorType() and error.errorInfo()
    ## If "noData" has been called, the errorType will be "NoData" and
    ##   can be tested by the calling tool or script.
    
    
    def callSmartTool(self, toolName, elementName, editArea=None,
                      timeRange=None, varDict=None,
                      editValues=1, calcArea=0, calcGrid=0,
                      passErrors=[],
                      missingDataMode="",
                      modal=1):
        # passErrors:  a list of errors to ignore and pass back to the
        #  calling program.  Some errors that can be ignored are:
        #    NoData
        #    NoElementToEdit
        #    ExecuteOrClassError
        #    LockedGridError
        #
        # For example:
        #  In the Procedure:
        #     error = self.callSmartTool(
        #        "MyTool", "MixHgt", editArea, timeRange, varDict,
        #        passErrors= ["NoData"])
        #     if error is not None:
        #        print "No Data available to run tool"
        #
        #  In the Smart Tool:
        #     mixHgt = self.getGrids(model, "MixHgt", "SFC", timeRange)
        #     if mixHgt is None:
        #        self.noData()

        if editArea is None or not editArea.getGrid().isAnyBitsSet():
            editArea = self.__refSetMgr.fullRefSet()
            emptyEditAreaFlag = True
        else:
            emptyEditAreaFlag = False
        if varDict is not None:
            varDict = str(varDict)
        
        parm = self.getParm(self.__mutableID, elementName, "SFC")
        if timeRange is None:
            from com.raytheon.viz.gfe.core.parm import ParmState
            timeRange = parm.getParmState().getSelectedTimeRange()
        else:
            timeRange = timeRange.toJavaObj()
        
        from com.raytheon.viz.gfe.smarttool import SmartUtil
        
        result = SmartUtil.callFromSmartScript(self.__dataMgr, toolName, elementName, editArea, 
                                            timeRange, varDict, emptyEditAreaFlag, 
                                            JUtil.pylistToJavaStringList(passErrors), 
                                            missingDataMode, parm)
        
        if result:
            raise Exceptions.EditActionError(errorType="Error", errorInfo=str(result))   
        return None
    
    def callProcedure(self, name, editArea=None, timeRange=None, varDict=None,
                      missingDataMode="Stop",
                      modal=1):
        if editArea is None:
            from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
            editArea = ReferenceData()
        if timeRange is None:
            from com.raytheon.uf.common.time import TimeRange as JavaTimeRange
            timeRange = JavaTimeRange()
        else:
            timeRange = timeRange.toJavaObj()
            
        from com.raytheon.viz.gfe.procedures import ProcedureUtil
        if varDict:
            varDict = str(varDict)

        result = ProcedureUtil.callFromSmartScript(self.__dataMgr, name, editArea, timeRange, varDict)
        
        # callSmartTool raises the exception put here it is returned.
        if result:
           return Exceptions.EditActionError(errorType="Error", errorInfo=str(result))   
        return None        
    

    ###########################
    ## Creating On-the-fly elements

    def createGrid(self, model, element, elementType, numericGrid, timeRange,
                   descriptiveName=None, timeConstraints=None,
                   precision=None, minAllowedValue=None,
                   maxAllowedValue=None, units=None, rateParm=0,
                   discreteKeys=None, discreteOverlap=None,
                   discreteAuxDataLength=None, defaultColorTable=None):


        # Creates a grid for the given model and element.
        # If the model and element do not already exist, creates them on-the-fly
        #
        # The descriptiveName, timeConstraints, precision, minAllowedValue,
        # maxAllowedValue, units, rateParm, discreteKeys, discreteOverlap, 
        # and discreteAuxDataLength only need to be
        # specified for the first grid being created.  These 
        # values are ignored for subsequent calls to createGrid() for 
        # the same weather element.

        # For new parms, the defaultColorTable is the one to be used for 
        # display.  If not specified and not in the gfe configuration file,
        # a DEFAULT color table will be used.

        # DISCRETE elements require a definition for discreteKeys,
        # discreteAuxDataLength,  and discreteOverlap. For DISCRETE, the 
        # precision, minAllowedValue, maxAllowedValue, and rateParm 
        # are ignored.

        # Note that this works for numeric grids only.
        # The arguments exampleModel, exampleElement, and exampleLevel can be
        # supplied so that the new element will have the same characteristics
        # (units, precision, etc.) as the example element.
        #
        # model -- If you are creating an "on-the-fly" element (i.e. not
        #          in the server), this should be a simple string with
        #          with no special characters.  The site ID and other
        #          information will be added for you.
        #          If you are creating a grid for a model that exists
        #          in the server, follow the guidelines for the model
        #          argument described for the "getValue" command.
        # element -- This should be a simple string with no special
        #          characters.
        # elementType -- "SCALAR", "VECTOR", "WEATHER", or "DISCRETE"
        # numericGrid -- a Numeric Python grid
        # timeRange -- valid time range for the grid.  You may want
        #          to use the "createTimeRange" command
        #
        # The descriptiveName, timeConstraints, precision, minAllowedValue,
        # precision, minAllowedValue, maxAllowedValue, and units can be
        # used to define the GridParmInfo needed. Note that timeConstraints
        # is not the C++ version, but a (startSec, repeatSec, durSec).
        #
        # Example:
        #    self.createGrid("ISCDisc", WEname+"Disc", "SCALAR", maxDisc,
        #                   GridTimeRange, descriptiveName=WEname+"Disc")
        #
        if string.find(element, "_") >= 0:
            message = "SmartScript:createGrid --" + \
                              "Illegal element name contains underscore. " + \
                              "No special characters allowed. "
            self.abort(message)
        parm = self.getParm(model, element, "SFC")
        if parm is None:
            # Create a parm on-the-fly
            # Parm ID
            siteID = self.__dataMgr.getSiteID()
            if model == "Fcst":
                dbi = self.__mutableID
            else:
                dbi = DatabaseID.databaseID(siteID + "_GRID__" + model + "_00000000_0000")
            pid = ParmID.ParmID(element, dbid=dbi).toJavaObj()
            # Grid Parm Info set up to use a default at first
            if elementType == "VECTOR":
                example = self.getParm("Fcst", "Wind", "SFC")
            elif elementType == "WEATHER":
                example = self.getParm("Fcst", "Wx", "SFC")
            elif elementType == "SCALAR":
                example = self.getParm("Fcst", "T", "SFC")
            elif elementType == "DISCRETE":
                example = self.getParm("Fcst", "Hazards", "SFC")
            else:
                message = "SmartScript:createGrid -- illegal element type"
                self.abort(message)

            exampleGPI = example.getGridInfo()
            #exampleGPI = example.parmInfo()

            #look for overrides
            if descriptiveName is None:
                descriptiveName = element
            if timeConstraints is None:
                tc = exampleGPI.getTimeConstraints()
            elif isinstance(timeConstraints, types.TupleType):
                # TC constructor (dur, repeat, start)
                # TC tuple (start, repeat, dur)
                tc = TimeConstraints(timeConstraints[2], timeConstraints[1],
                                     timeConstraints[0])
            else:
                # Assume Java TimeConstraints or compatible
                tc = TimeConstraints(
                  timeConstraints.getDuration(), timeConstraints.getRepeatInterval(),
                  timeConstraints.getStartTime())
            if precision is None:
                precision = exampleGPI.getPrecision()
            if maxAllowedValue is None:                
                maxAllowedValue = exampleGPI.getMaxValue()
            if minAllowedValue is None:                
                minAllowedValue = exampleGPI.getMinValue()
            if units is None:
                units = exampleGPI.getUnitString()

            if tc.anyConstraints() == 0:
                timeIndependentParm = 1
                timeRange = TimeRange.TimeRange.allTimes().toJavaObj()
            else:
                timeIndependentParm = 0

            #create the new GridParmInfo
            minAllowedValue = float(minAllowedValue)
            maxAllowedValue = float(maxAllowedValue)
            gpi = GridParmInfo(pid,
                exampleGPI.getGridLoc(), exampleGPI.getGridType(), units,
                descriptiveName, minAllowedValue, maxAllowedValue,
                precision, timeIndependentParm, tc, rateParm)

            # if DISCRETE, deal with the key definitions
            if elementType == "DISCRETE":
                if discreteKeys is None or discreteOverlap is None or \
                  discreteAuxDataLength is None:
                    message = "SmartScript:createGrid --" + \
                              "Discrete elements require discretekeys, " + \
                              "discreteAuxDataLength, " + \
                              "and discreteOverlap defined. "
                    self.abort(message)
                currDef = DiscreteKey.discreteDefinition(siteID)
                keys = ArrayList()
                for h in discreteKeys:
                    if type(h) is types.TupleType:
                        kname, kdesc = h
                    elif type(h) is types.StringType:
                        kname = h
                        kdesc = h
                    keys.add(DiscreteKeyDef(kname, kdesc))
                currDef.addDefinition(pid.getCompositeName(), discreteOverlap,
                                             discreteAuxDataLength, keys)
                DiscreteKey.setDiscreteDefinition(siteID, currDef)

            #set a default color table if specified
            if defaultColorTable is not None:
                from com.raytheon.viz.gfe import Activator
                prefName = element + "_defaultColorTable"
                Activator.getDefault().getPreferenceStore().setValue(prefName, defaultColorTable)
 
            #create the parm
            parm = self.__parmMgr.createVirtualParm(pid, gpi, None, 1, 1)

        # Create Java objects from numericGrid.
        # Do this here because, while numericGrid can be sent straight to Java,
        # the keys of discrete grids arrive as a single string, which must then
        # be parsed. It's easier to create Java objects of the proper types here.
        javaGrid = None
        auxJavaGrid = None
        javaOldKeys = None
        if elementType == "DISCRETE" or elementType == "WEATHER":
            ngZero = numericGrid[0]
            dimx = ngZero.shape[1]
            dimy = ngZero.shape[0]
            # Use createGrid() to get around Jep problems with 3-arg ctor.
            ngZero = ngZero.astype('int8')
            javaGrid = Grid2DByte.createGrid(dimx, dimy, ngZero)
            oldKeys = numericGrid[1]
            javaOldKeys = ArrayList()
            for oldKey in oldKeys:
                # it seems stupid that we break apart tuples for discrete keys
                # when modifying the DiscreteDefinition, but not here when
                # creating the actual grid. It actually prevents the grid from
                # being created because the string representation of the tuple
                # won't match what we added to the DiscreteDefinition.
                # However, this is exactly what AWIPS1 does...
                # SEE GridCycler.C, line 1131
                # FIXME: add oldKey[0] to the ArrayList for tuple types
                javaOldKeys.add(str(oldKey))
        elif elementType == "SCALAR":
            if numericGrid.dtype.name != 'float32':
                numericGrid = numericGrid.astype('float32')
            javaGrid = Grid2DFloat.createGrid(numericGrid.shape[1], numericGrid.shape[0], numericGrid)
        elif elementType == "VECTOR":
            ngZero = numericGrid[0].astype(float32)
            ngOne = numericGrid[1].astype(float32)
            javaGrid = Grid2DFloat.createGrid(ngZero.shape[1], ngZero.shape[0], ngZero)
            auxJavaGrid = Grid2DFloat.createGrid(ngOne.shape[1], ngOne.shape[0], ngOne)            
        else:
            raise ValueError, "Unknown elementType: %s" % elementType

        # Make sure we pass a java TimeRange to Java methods
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        gridData = self.__cycler.makeGridDataFromNumeric(parm, timeRange, javaGrid, auxJavaGrid, javaOldKeys)
        parm.replaceGriddedData(timeRange, gridData)

    ##
    # 
    # @param model: Model name
    # @type model: string
    # @param element: Element name
    # @type element: string
    # @param level: Level name
    # @type level: string
    # @param timeRange: Time range of grid
    # @type timeRange: Python or Java TimeRange
    # @return: True if grids were deleted
    def deleteGrid(self, model, element, level, timeRange):
        # Deletes any grids for the given model and element
        # completely contained in the given timeRange.
        # If the model and element do not exist or if there are no existing grids,
        #   no action is taken.
        #
        parm = self.getParm(model, element, level)
        if parm is None:
            returnVal = False
        else:
            if isinstance(timeRange, TimeRange.TimeRange):
                timeRange = timeRange.toJavaObj()
            returnVal = parm.deleteTR(timeRange)
        return returnVal

    def highlightGrids(self, model, element, level, timeRange, color, on=1):
        # Highlight the grids in the given time range using designated
        # color.  If "on" is 0, turn off the highlight.
        parm = self.getParm(model, element, level)
        from com.raytheon.viz.gfe.core.msgs import HighlightMsg
        from com.raytheon.uf.common.time import TimeRange as javaTimeRange
        
        trs = jep.jarray(1, javaTimeRange)
        trs[0] = timeRange.toJavaObj()
        HighlightMsg(parm, trs, on, color).send()

    def makeHeadlineGrid(self, headlineTable, fcstGrid, headlineGrid=None):
        # This method defines a headline grid based on the specified data.
        # The headlineTable parameter must be a list of tuples each containing
        # the threshold for each headline category and headline label
        # Example:
        #    headlineTable =[(15.0, 'SW.Y'),
        #                    (21.0, 'SC.Y'),
        #                    (34.0, 'GL.W'),
        #                    (47.0, 'SR.W'),
        #                    (67.0, 'HF.W'),
        #                    ]
        # "fsctGrid" is the grid that defines what headline category should
        # be assigned. "headlineGrid" is the grid you wish to combine with
        # the calculated grid.  This forces a combine even if the GFE is not
        # in combine mode.  Omitting "headlineGrid" will cause the calculated
        # grid to replace whatever is in the GFE, no matter what the GFE's
        # combine mode. Note that a side effect of omitting the headline grid
        # is that the GFE will end up in replace mode after the tool completes.
        noneKey = "<None>"  # define the <None> key
        # set the mode to replace so the tool always behaves the same

        if headlineGrid is None: # make new headline grid components
            headValues = zeros(fcstGrid.shape, 'int8')
            headKeys = [noneKey]
            self.setCombineMode("Replace") # force a replace in GFE
        else:
            headValues, headKeys = headlineGrid

        # make sure the headlineTable is not empty
        if len(headlineTable) <= 0:
            self.statusBarMsg("HeadlineTable is empty", "S")
            return headlineGrid

        # make a list of (mask, key) for the new headlines
        newHeadlines = []
        for value, headline in headlineTable:
            mask = greater_equal(fcstGrid, value)
            if sometrue(mask):
                newHeadlines.append((mask, headline))
        # make the same list for old headlines
        oldHeadlines = []
        for i in range(len(headKeys)):
            mask = equal(headValues, i)
            if sometrue(mask):
                oldHeadlines.append((mask, headKeys[i]))

        # make combinations at every intersection
        for newMask, newKey in newHeadlines:
            for oldMask, oldKey in oldHeadlines:
                overlap = logical_and(newMask, oldMask) # intersection
                if sometrue(overlap): #  combined key needed
                    if oldKey == newKey:
                        continue
                    if oldKey == noneKey:
                        combinedKey = newKey
                    else:
                        combinedKey = oldKey + "^" + newKey
                    # make sure the key is on the list
                    if combinedKey not in headKeys:
                        headKeys.append(combinedKey)
                    index = self.getIndex(combinedKey, headKeys)
                    headValues = where(overlap, index, headValues)

        # return the new headlines grid
        return (headValues, headKeys)


    ######################
    ##  Utility Commands

    def findDatabase(self, databaseName, version=0):
        # Return an AFPS.DatabaseID object.
        #  databaseName can have the appended type. E.g. "NAM12" or "D2D_NAM12"
        #  version is 0 (most recent), -1 (previous), -2, etc.
        # E.g.
        #    databaseID = self.findDatabase("NAM12",0)
        # returns most recent NAM12 model
        result = self.__parmMgr.findDatabase(databaseName, version)
        if result is not None:
            result = DatabaseID.DatabaseID(result)             
        return result

    def getDatabase(self, databaseString):
        # Return an AFPS.DatabaseID object.
        #  databaseString is the result of a VariableList entry of type
        #   "model" or "D2D_model"
        dbID = DatabaseID.databaseID(databaseString)
        return dbID

    def getTimeRange(self, timeRangeName):
        # Returns an AFPS.TimeRange object given a time range name
        # as defined in the GFE
        # E.g.
        #   timeRange = self.getTimeRange("Today")
        tr = self.__dataMgr.getSelectTimeRangeManager().getRange(timeRangeName).toTimeRange();
        return TimeRange.TimeRange(tr)

    def createTimeRange(self, startHour, endHour, mode="LT", dbID=None):
        # Returns an AFPS.TimeRange object given by:
        #    startHour, endHour
        #       (range is startHour up to and not including endHour)
        #       startHour and endHour are relative to midnight of the
        #          current day either in Local or Zulu time (see below)
        #    mode can be:
        #    "LT" : the startHour and endHour are relative to local time
        #    "Zulu": relative to Zulu time,
        #    "Database": relative to a database (e.g. model time.
        #      In this case, the databaseID for the model must
        #      be supplied (see findDatabase)
        #
        # E.g.
        #    timeRange = self.createTimeRange(0,121,"Zulu")
        #    databaseID = self.findDatabase("NAM12")
        #    timeRange = self.createTimeRange(120,241,"Database",databaseID)
        
        if mode == "Database" and dbID is None:
            raise TypeError("SmartScript createTimeRange: " + \
                      "Must specify a database ID for mode=Database")

        if mode == "LT":
            localTime = time.mktime(time.localtime())
            gmTime = time.mktime(time.gmtime())
            localAbsTime = AbsTime.AbsTime(localTime)
            delta = localTime - gmTime
            
            todayMidnight = AbsTime.absTimeYMD(localAbsTime.year, localAbsTime.month,
                                               localAbsTime.day)
            start = todayMidnight + (startHour * 3600) - delta
            end = todayMidnight + (endHour * 3600) - delta
            return TimeRange.TimeRange(start, end)                    
        elif mode == "Database" and dbID.toJavaObj().getModelTime() != "00000000_0000":
            start = dbID.modelTime() + (startHour * 3600)
            end = dbID.modelTime() + (endHour * 3600)
            return TimeRange.TimeRange(start, end)
        else:
            currentTime = time.gmtime()
            today = AbsTime.absTimeYMD(currentTime.tm_year, currentTime.tm_mon,
                                       currentTime.tm_mday)
            start = today + (startHour * 3600)
            end = today + (endHour * 3600)
            return TimeRange.TimeRange(start, end)

    def getSamplePoints(self, sampleSetName=None):
        # Return a list of x,y tuples representing sample points
        # sampleSet is the name of a saved sample set
        # if sampleSet is None, the sample points will be
        #   those currently displayed on the GFE
        points = []
        sampleSet = self.__dataMgr.getSampleSetManager()
        if sampleSetName is None:
            locations = sampleSet.getLocations()
        else:
            locations = sampleSet.sampleSetLocations(sampleSetName)
        for i in range(locations.size()):
            xy = self.getGridLoc().gridCoordinate(locations.get(i))
            points.append((xy.x, xy.y))
        return points

    def _timeDisplay(self, timeRange, LTorZulu, durFmt, startFmt, endFmt):
        # Return a string display for the given timeRange, assumed to be
        #  in GMT.
        # If LTorZulu == "LT", the timeRange will be converted from GMT
        #  to local time.
        # durationFmt, startFmt, endFmt are format strings for the
        #  timeRange duration, the start time and end time respectively.
        # See Text Product User Guide to see possible formats.
        #
        # Example:
        #   self._timeDisplay(timeRange, "LT", "%H hours ",
        #                     "%a %b %d, %Y %I:%M %p",
        #                    " to %a %b %d, %Y %I:%M %p %Z")
        #
        #   yields a string such as:
        #
        #  12 hours Mon Apr 23, 2001 06:00 AM to Mon Apr 23, 2001 06:00 PM MDT.
        if LTorZulu == "LT":
            # Convert to local time
            timeRange = self._shiftedTimeRange(timeRange)
        display = ""
        if durFmt != "":
            duration = timeRange.duration()
            durHours = duration / 3600
            durMinutes = duration / 3600 / 60
            durStr = string.replace(durFmt, "%H", `durHours`)
            durStr = string.replace(durStr, "%M", `durMinutes`)
            display = display + durStr
        if startFmt != "":
            display = display + timeRange.startTime().stringFmt(startFmt)
        if endFmt != "":
            display = display + timeRange.endTime().stringFmt(endFmt)
        if LTorZulu == "LT":
            # Adjust time zone to local time
            localTime = time.localtime(time.time())
            zoneName = time.strftime("%Z", localTime)
            display = string.replace(display, "GMT", zoneName)
        return display
        
    def _shiftedTimeRange(self, timeRange):
        localTime, shift = self._determineTimeShift()
        return TimeRange.TimeRange(timeRange.startTime() + shift,
                              timeRange.endTime() + shift)

    def _determineTimeShift(self):
        ''' Get the current Simulated UTC time and convert it to the
        Site Time Zone as AbsTime return this and the number of seconds the
        Simulated UTC time was shifted to get local time
        '''
        ldt = self._localtime()
        shift = int(ldt.utcoffset().total_seconds())
        currentTime = AbsTime.absTimeYMD(ldt.year, ldt.month, ldt.day, ldt.hour, ldt.minute)
        return currentTime, shift

    def _localtime(self, date=None, tz=None):
        ''' Assumes date (default is current Simulate Time) is a UTC time to convert
            to the time zone tz (default is Site Time Zone).
            returns datetime
            
            This should be used instead of time.localtime()
        '''
        from pytz import timezone
        
        if tz is None:
            tzname = self.__dataMgr.getClient().getSiteTimeZone()
            tz = timezone(tzname)

        utczone = timezone('UTC')
        gmdt = utczone.localize(self._gmtime(date))
        tzdt = gmdt.astimezone(tz)
        return tzdt

    def _gmtime(self, date=None):
        ''' This takes date (default current Simulated Time) and converts it to AbsTime
        
            This should be used instead of time.gmtime()
        '''
        if date is None:
            date = SimulatedTime.getSystemTime().getTime()
        return AbsTime.AbsTime(date)
    
    def dayTime(self, timeRange, startHour=6, endHour=18):
        # Return 1 if start of timeRange is between the
        #  startHour and endHour, Return 0 otherwise.
        # Assume timeRange is GMT and convert to local time.
        shift = self.determineTimeShift()
        startTime = timeRange.startTime() + shift
        localStartHour = startTime.hour
        if localStartHour >= startHour and localStartHour < endHour:
            return 1
        else:
            return 0
        
    def determineTimeShift(self):
        loctime, shift = self._determineTimeShift()
        return shift

    def getEditArea(self, editAreaName):
        # Returns an AFPS.ReferenceData object given an edit area name
        # as defined in the GFE

        # Example:
        #    myArea = self.getEditArea("BOU")
        #    self.callSmartTool("MyTool", "T", editArea=myArea, timeRange)
        #
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
        refID = ReferenceID(editAreaName)
        return self.__dataMgr.getRefManager().loadRefSet(refID)

    def saveEditArea(self, editAreaName, refData):
        # Saves the AFPS.ReferenceData object with the given name
        
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
        refID = ReferenceID(editAreaName)
        refData = ReferenceData(refData.getGloc(), refID, refData.getGrid())
        self.__dataMgr.getRefManager().saveRefSet(refData)

    def setActiveEditArea(self, area):
        # Set the AFPS.ReferenceData area to be the active one in the GFE
        # Note: This will not take effect until AFTER the smart tool or
        # procedure is finished executing.
        self.__dataMgr.getRefManager().setActiveRefSet(area)

    def getActiveEditArea(self):
        # Get the AFPS.ReferenceData area for the active one in the GFE
        return self.__dataMgr.getRefManager().getActiveRefSet()
    
    def clearActiveEditArea(self):
        # Clear the active edit area in the GFE
        #area = AFPS.ReferenceData_default()
        #self.__dataMgr.referenceSetMgr().setActiveRefSet(area)
        self.__dataMgr.getRefManager().clearRefSet()
    
    def setActiveElement(self, model, element, level, timeRange,
                         colorTable=None, minMax=None, fitToData=0):
        # Set the given element to the active one in the GFE
        # A colorTable name may be given.
        # A min/max range for the colorTable may be given.
        # If fitToData = 1, the color table is fit to the data
        #
        # Example:
        #    self.setActiveElement("ISCDisc", WEname+"Disc", "SFC", GridTimeRange,
        #                          colorTable="Discrepancy", minMax=(-20,+20),
        #                          fitToData=1)
        #
        parm = self.getParm(model, element, level)
        spatialMgr = self.__dataMgr.getSpatialDisplayManager()
        if minMax or colorTable:
            rsc = spatialMgr.getResourcePair(parm).getResource()
            from com.raytheon.uf.viz.core.rsc.capabilities import ColorMapCapability
            params = rsc.getCapability(ColorMapCapability).getColorMapParameters()
            if colorTable:
                from com.raytheon.uf.viz.core.drawables import ColorMapLoader
                colorMap = ColorMapLoader.loadColorMap(colorTable)
                elemType = parm.getGridInfo().getGridType().toString()
                if ('DISCRETE' == elemType):
                    from com.raytheon.viz.gfe.rsc import DiscreteDisplayUtil
                    DiscreteDisplayUtil.deleteParmColorMap(parm)
                params.setColorMap(colorMap)
                params.setColorMapName(colorTable)
                rsc.issueRefresh()
            if minMax:
                minVal, maxVal = minMax
                if (minVal != maxVal):
                    params.setColorMapMax(maxVal)
                    params.setColorMapMin(minVal)
            parm.getListeners().fireColorTableModified(parm)
        if fitToData:
            from com.raytheon.viz.gfe.rsc.colorbar import FitToData
            fitter = FitToData(self.__dataMgr, parm)
            fitter.fitToData()
        spatialMgr.activateParm(parm)
        spatialMgr.setSpatialEditorTime(timeRange.startTime().javaDate())
                

    def getActiveElement(self):
        return self.__dataMgr.getSpatialDisplayManager().getActivatedParm()

    def getGridCellSwath(self, editArea, cells):
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceID
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
        # Returns an AFPS.ReferenceData swath of the given
        # number of cells around the given an edit area.
        # The edit area must not be a query.
        if type(editArea) is types.StringType:
            editArea = self.getEditArea(editArea)
        grid2DB = None
        multipolygon = editArea.getPolygons(CoordinateType.valueOf("GRID"))
        numPolygons = multipolygon.getNumGeometries()
        for n in range(numPolygons):
            polygon = multipolygon.getGeometryN(n)
            grid2DBit = self.getGridLoc().gridCellSwath(
                polygon.getCoordinates(), float(cells), False)
            if grid2DB is not None:
                grid2DB = grid2DB | grid2DBit
            else:
                grid2DB = grid2DBit
        return ReferenceData(self.getGridLoc(), ReferenceID("test"), grid2DB)

    def getLatLon(self, x, y):
        # Get the latitude/longitude values for the given grid point
        from com.vividsolutions.jts.geom import Coordinate        
        coords = Coordinate(float(x), float(y))
        cc2D = self.getGridLoc().latLonCenter(coords)
        return cc2D.y, cc2D.x
    
    def getLatLonGrids(self):
        gridLoc = self.getGridLoc()
        latLonGrid = gridLoc.getLatLonGrid().__numpy__[0];
        latLonGrid = numpy.reshape(latLonGrid, (2,gridLoc.getNy().intValue(),gridLoc.getNx().intValue()), order='F')
        return latLonGrid[1], latLonGrid[0]


    def getGridCell(self, lat, lon):
        # Get the corresponding x,y values for the given lat/lon
        # Return None, None if the lat/lon is outside the grid domain
        cc2D = self.getGridLoc().gridCell(lat, lon)
        gridSize = self.getGridLoc().gridSize()
        if cc2D.x < 0 or cc2D.x >= gridSize.x or \
           cc2D.y < 0 or cc2D.y >= gridSize.y:
            return None, None
        else:
            return cc2D.x, cc2D.y

    def getGrid2DBit(self, editArea):
        # Returns a Grid of on/off values indicating whether
        # or not the grid point is in the given edit area.
        # This could be used as follows in a Smart Tool:
        #  def preProcessGrid(self):
        #     editArea = self.getEditArea("Area1")
        #     self.__area1Bits = self.getGrid2DBit(editArea)
        #     editArea = self.getEditArea("Area2")
        #     self.__area2Bits = self.getGrid2DBit(editArea)
        #
        #  def execute(self, x, y):
        #     if self.__area1Bits.get(x,y) == 1:
        #        <process a point in Area1>
        #     elif self.__area2Bits.get(x,y) == 1:
        #        <process a point in Area2>
        #
        return editArea.getGrid()

    def getGridTimes(self, model, element, level, startTime, hours):
        # Return the timeRange and gridTimes for the number of hours
        # FOLLOWING the given startTime
        timeRange = TimeRange.TimeRange(startTime, startTime + hours * 3600)
        parm = self.getParm(model, element, level, timeRange)
        gridTimes = parm.getGridInfo().getTimeConstraints().constraintTimes(timeRange.toJavaObj())
        pyList = []
        for t in gridTimes:
            pyList.append(TimeRange.TimeRange(t))
        return timeRange, pyList

    def getExprName(self, model, element, level="SFC", mostRecent=0):
        # Return an expressionName for the element
        # This method is complicated because it is handling all the
        # variations for the "model" argument.  For a description
        # of the variations, see the "getValue" documentation above.

        siteID = self.__mutableID.siteID()
        if type(model) is types.StringType:            
            modelStr = model
        else:
            # Must be a databaseID, so get model string            
            modelStr = model.modelName()
        if element == "Topo" or modelStr == self.__mutableID.modelName():
            exprName = element
        elif modelStr == "Official":
            dbType = self.__mutableID.type()
            modelName = "Official"
            exprName = element + "_" + level + "_" + siteID + "_" + dbType + "_" + modelName
        else:
            if type(model) is types.StringType:
                if string.count(model, "_") == 5:
                    # String as databaseID
                    dbID = DatabaseID.databaseID(model)
                elif string.find(model, "_") < 0:
                    # Assume "on-the-fly" so need to prepend site
                    exprName = element + "_" + level + "_" + siteID + "__" + model
                    dbID = DatabaseID.databaseID_default()
                else:
                    # Assume model is site_type_modelName
                    exprName = element + "_" + level + "_" + model
                    dbID = DatabaseID.databaseID_default()
            else:
                # Assume it is already a dbID
                dbID = model
            if dbID.siteID() is not None and dbID.siteID() != "":
                if str(dbID) == str(self.__mutableID):
                    exprName = element
                else:
                    exprName = element + "_" + level + "_" + dbID.siteID() + "_" + \
                               dbID.type() + "_" + dbID.modelName()
                    if mostRecent == 0:
                        if dbID.toJavaObj().getModelDate() is None:
                            exprName = exprName + "_00000000_0000"
                        else:
                            exprName = exprName + "_" + dbID.modelTime().stringFmt(
                                "%b%d%H")
        return exprName

    def getSiteID(self):
        return self.__dataMgr.getSiteID()

    def getModelName(self, databaseString):
        # Return the model name.
        #  databaseString is the result of a VariableList entry of type
        #   "model" or "D2D_model"
        dbID = DatabaseID.databaseID(databaseString)
        return dbID.modelName()

    def getD2Dmodel(self, model):
        # Given a GFE Surface model, return the corresponding D2D model
        if isinstance(model, DatabaseID.DatabaseID):
            model = model.modelIdentifier()
        d2d = string.replace(model, "__", "_D2D_")
        return d2d

    def getParm(self, model, element, level, timeRange=None, mostRecent=0):
        # Returns the parm object for the given model, element, and level
        exprName = self.getExprName(model, element, level, mostRecent)
        #print "Expression Name", exprName
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        return parm
    
    def getParmByExpr(self, exprName):
        #print "Expression Name", exprName
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        return parm

    ##
    # @param elementNames: ignored
    #
    # @deprecated: Cacheing is controlled by the system. 
    def cacheElements(self, elementNames):
        pass

    ##
    # Cacheing is controlled by the system. Users may still call this method 
    # to delete temporary parms in the parm manager.
    # 
    # @param elementNames: ignored
    def unCacheElements(self, elementNames):
        self.__parmMgr.deleteTemporaryParms()

    def loadWEGroup(self, groupName):
        parmArray = self.__parmMgr.getAllAvailableParms();
        parmIDs = self.__dataMgr.getWEGroupManager().getParmIDs(
              groupName, parmArray)
        # Load the group
        self.__parmMgr.setDisplayedParms(parmIDs)

    ##
    # @param model: Database model name
    # @type model: String
    # @param element: Element name, i.e., "Hazards"
    # @type element: String
    # @param level: Parm level, i.e., "SFC"
    # @type level: String
    # @return: None
    def unloadWE(self, model, element, level, mostRecent=0):
        # unloads the WE from the GFE
        exprName = self.getExprName(model, element, level, mostRecent)
        parm = self.__parmMgr.getParmInExpr(exprName, 1)
        if parm is None:
            return
        parmJA = jep.jarray(1, parm)
        parmJA[0] = parm
        self.__parmMgr.deleteParm(parmJA)

    def saveElements(self, elementList):
        # Save the given Fcst elements to the server
        # Example:
        #    self.saveElements(["T","Td"])
        for element in elementList:
            parm = self.getParm("Fcst", element, "SFC")
            parm.saveParameter(True)

    def publishElements(self, elementList, timeRange):
        # Publish the given Fcst elements to the server
        # over the given time range.
        # NOTE: This method is design to run from a procedure
        # NOT a SmartTool!!!
        # Example:
        #    self.publishElements(["T","Td"], timeRange)
        from com.raytheon.uf.common.dataplugin.gfe.server.request import CommitGridRequest
        requests = ArrayList()
        for element in elementList:
            # get the inventory for this element from the server
            parm = self.getParm("Fcst", element, "SFC")
            recList = self.__dataMgr.getClient().getGridInventory(parm.getParmID())
            publishTimeRange = timeRange
            if recList is not None:
                recSize = recList.size()                
                for x in range(recSize):
                    tr = TimeRange.TimeRange(recList.get(x))
                    if tr.overlaps(timeRange):                    
                        publishTimeRange = publishTimeRange.combineWith(tr)
                                
            cgr = CommitGridRequest(parm.getParmID(), publishTimeRange.toJavaObj())
            requests.add(cgr)
        resp = self.__dataMgr.getClient().commitGrid(requests)
        r = resp.getPayload()
        size = r.size()
        for x in range(size):
            notify = r.get(x)
            pid = notify.getParmId()
            p = self.__parmMgr.getParm(pid)
            if not p:
                p = self.__parmMgr.addParm(pid, False, False)
            p.inventoryArrived(notify.getReplacementTimeRange(), notify.getHistories())                                                                   

    def combineMode(self):
        from com.raytheon.viz.gfe.core.parm import ParmState
        from com.raytheon.viz.gfe.core.parm import ParmState_CombineMode as CombineMode
        mode = ParmState.getCurrentCombineMode()
        if mode.equals(CombineMode.valueOf("COMBINE")):
            return True
        else:
            return False

    def setCombineMode(self, mode):
        from com.raytheon.viz.gfe.core.parm import ParmState_CombineMode as CombineMode
        if mode == "Combine":
            self.__parmOp.setCombineMode(CombineMode.valueOf("COMBINE"))
        elif mode == "Replace":
            self.__parmOp.setCombineMode(CombineMode.valueOf("REPLACE"))
        else:
            self.statusBarMsg("Invalid Weather Combine mode.", "S")
            return None

    def getVectorEditMode(self):
        # Returns Vector Edit mode in the GFE
        # mode:
        #    "Magnitude Only"
        #    "Direction Only"
        #    "Both"
        from com.raytheon.viz.gfe.core.parm import ParmState
        from com.raytheon.viz.gfe.core.parm import ParmState_VectorMode as VectorMode
        mode = ParmState.getCurrentVectorMode()
        if mode.equals(VectorMode.valueOf("MAGNITUDE")):
            return "Magnitude Only"
        elif mode.equals(VectorMode.valueOf("DIRECTION")):
            return "Direction Only"
        elif mode.equals(VectorMode.valueOf("BOTH")):
                return "Both"
        return "None"

    def setVectorEditMode(self, mode):
        # Sets the Vector Edit mode in the GFE
        # mode:
        #    "Magnitude only"
        #    "Direction only"
        #    "Both"
        from com.raytheon.viz.gfe.core.parm import ParmState_VectorMode as VectorMode
        if mode == "Magnitude Only":
            self.__parmOp.setVectorMode(VectorMode.valueOf("MAGNITUDE"))
        elif mode == "Direction Only":
            self.__parmOp.setVectorMode(VectorMode.valueOf("DIRECTION"))
        else:
            self.__parmOp.setVectorMode(VectorMode.valueOf("BOTH"))

    def getConfigItem(self, itemName, default=None):
        # Return the configuration file value for "itemName"
        #  If not found, return the default given
        from com.raytheon.viz.gfe import Activator
        prefs = Activator.getDefault().getPreferenceStore()
        if prefs.contains(itemName):
            if prefs.isString(itemName):
                return str(prefs.getString(itemName))            
            elif prefs.isInt(itemName):
                return prefs.getInt(itemName)
            elif prefs.isFloat(itemName):
                return prefs.getFloat(itemName)
            elif prefs.isDouble(itemName):
                return prefs.getDouble(itemName)
            elif prefs.isLong(itemName):
                return prefs.getLong(itemName)
            elif prefs.isBoolean(itemName):
                return prefs.getBoolean(itemName)
            elif prefs.isStringArray(itemName):
                pa = []
                jsa = prefs.getStringArray(itemName)
                for i in jsa:
                    pa.append(str(i))
                return pa
            elif prefs.isFloatArray(itemName):
                pa = []
                jsa = prefs.getFloatArray(itemName)
                for i in jsa:
                    pa.append(i.floatValue())
                return pa
            elif prefs.isIntArray(itemName):
                pa = []
                jsa = prefs.getIntArray(itemName)
                for i in jsa:
                    pa.append(i.intValue())
                return pa
            else:
                return default
        else:
            return default

    def esat(self, temp):
        return exp(26.660820 - 0.0091379024 * temp - 6106.3960 / temp)

    ##
    # Get the discrete keys for elementName.
    #
    # @param elementName: Name of an element.
    # @type elementName: string
    # @return: the keys for the element
    # @rtype: list of strings
    def getDiscreteKeys(self, elementName):
        parm = self.getParm("Fcst", elementName, "SFC")
        keyList = parm.getGridInfo().getDiscreteKeys()
        keyList = JUtil.javaStringListToPylist(keyList)
        return keyList

#########################################################################
## Numeric Python methods                                              ##
#########################################################################

    def getTopo(self):
        # Return the numeric topo grid
        if self.__topoGrid is None:
            topo = self.__parmMgr.getParmInExpr("Topo", True)            
            self.__topoGrid = self.__cycler.getCorrespondingResult(
                                topo, TimeRange.allTimes().toJavaObj(), "TimeWtAverage")[0]
            self.__topoGrid = self.__topoGrid.getGridSlice().__numpy__[0]
        return self.__topoGrid

    def wxMask(self, wx, query, isreg=0):
        # Returns a numeric mask i.e. a grid of 0's and 1's
        #  where the value is 1 if the given query succeeds
        # Arguments:
        #  wx -- a 2-tuple:
        #    wxValues : numerical grid of byte values
        #    keys : list of "ugly strings" where the index of
        #      the ugly string corresponds to the byte value in
        #      the wxValues grid.
        #  query -- a text string representing a query
        #  isreg -- if 1, the query is treated as a regular expression
        #           otherwise as a literal string
        # Examples:
        #  # Here we want to treat the query as a regular expression
        #  PoP = where(self.wxMask(wxTuple, "^Chc:", 1), maximum(40, PoP), PoP)
        #  # Here we want to treat the query as a literal
        #  PoP = where(self.wxMask(wxTuple, ":L:") maximum(5, PoP), PoP)
        #
        rv = zeros(wx[0].shape)
        if not isreg:
            for i in xrange(len(wx[1])):
                #if fnmatch.fnmatchcase(wx[1][i], query):
                if string.find(wx[1][i], query) >= 0:
                    rv = logical_or(rv, equal(wx[0], i))
        else:
            r = re.compile(query)
            for i in xrange(len(wx[1])):
                m = r.search(wx[1][i])
                if m is not None:
                    rv = logical_or(rv, equal(wx[0], i))
        return rv

        # Returns a numeric mask i.e. a grid of 0's and 1's
        #  where the value is 1 if the given query succeeds
        # Arguments:
        #  Discrete -- a 2-tuple:
        #    grid : numerical grid of byte values
        #    keys : list of "ugly strings" where the index of
        #      the ugly string corresponds to the byte value in
        #      the wxValues grid.
        #  query -- a text string representing a query
        #  isreg -- if 1, the query is treated as a regular expression
        #           otherwise as a literal string
        # Examples:
        #  # Here we want to treat the query as a regular expression
        #  PoP = where(self.wxMask(wxTuple, "^Chc:", 1), maximum(40, PoP), PoP)
        #  # Here we want to treat the query as a literal
        #  PoP = where(self.wxMask(wxTuple, ":L:") maximum(5, PoP), PoP)
    discreteMask = wxMask

    ##
    # Sort the subkeys of uglyStr alphabetically.
    #
    # @param uglyStr: A key with "^"s separating subkeys
    # @type uglyStr: string
    # @return: uglyStr with alpha sorted subkeys.
    # @rtype: string
    def sortUglyStr(self, uglyStr):
        parts = uglyStr.split("^")
        if len(parts) < 2:
            return uglyStr

        # do the sort
        parts.sort()

        sortedStr = "^".join(parts)

        return sortedStr

    ##
    # Get the index of uglyStr within keys.
    # This routine compares normalized (sorted) versions of uglyStr and
    # keys to be sure that equivalent hazards are assigned the same grid 
    # index.
    # When a matching key is not in keys, uglyStr will be added to keys
    # and the index of the new entry will be returned.
    #
    # @param uglyStr: A hazard key
    # @type uglyStr: string
    # @param keys: Existing hazard keys
    # @type keys: list
    # @return: The index of a key equivalent to uglyStr in keys
    # @rtype: int 
    def getIndex(self, uglyStr, keys):
        # Returns the byte value that corresponds to the
        #   given ugly string. It will add a new key if a new ugly
        #   string is requested.
        # Arguments:
        #   uglyStr: a string representing a weather value
        #   keys: a list of ugly strings.
        #     A Wx argument represents a 2-tuple:
        #       wxValues : numerical grid of byte values
        #       keys : list of "ugly strings" where the index of
        #        the ugly string corresponds to the byte value in the wxValues grid.
        #     For example, if our keys are:
        #       "Sct:RW:-:<NoVis>:"
        #       "Chc:T:-:<NoVis>:"
        #       "Chc:SW:-:<NoVis>:"
        #    Then, the wxValues grid will have byte values of 0 where
        #    there is "Sct:RW:-:<NoVis>:", 1 where there is "Chc:T:-:<NoVis>:"
        #    and 2 where there is "Chc:SW:-:<NoVis>:"
        #
        #  The ugly strings are also used by DISCRETE.  The keys are
        #  separated by '^' for the subkeys.
        #  18 Nov 2005 - tl
        #  Added sorting to ugly strings to prevent duplicate keys 
        #  Duplicate keys causes a bug when generating hazards grids.

        sortedUglyStr = self.sortUglyStr(uglyStr)
        for str in keys:
            if sortedUglyStr == self.sortUglyStr(str):
                return keys.index(str)
        keys.append(uglyStr)
        return len(keys) - 1

    ##
    # Returns a Numeric Python mask for the edit area
    # "editArea" can be a named area or a ReferenceData object
    # @param editArea: An edit area to obtain a mask for
    # @type editArea: String or referenceArea
    # @return: grid for the edit area
    # @rtype: numpy array of int8
    def encodeEditArea(self, editArea):
        # Returns a Numeric Python mask for the edit area
        # "editArea" can be a named area or a referenceData object
        if type(editArea) is types.StringType:
            editArea = self.getEditArea(editArea)
        return editArea.getGrid().__numpy__[0]

    def decodeEditArea(self, mask):
        # Returns a refData object for the given mask
        from com.raytheon.uf.common.dataplugin.gfe.grid import Grid2DBit
        from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData, ReferenceID
        gridLoc = self.getGridLoc()
        nx = gridLoc.getNx().intValue()
        ny = gridLoc.getNy().intValue()
        bytes = mask.astype('int8')
        grid = Grid2DBit.createBitGrid(nx, ny, bytes)
        return ReferenceData(gridLoc, ReferenceID("test"), grid)
        

    def getindicies(self, o, l):
        if o > 0:
            a = slice(o, l); b = slice(0, l - o)
        elif o < 0:
            a = slice(0, l + o); b = slice(- o, l)
        else:
            a = slice(0, l); b = slice(0, l)
        return a, b

    def offset(self, a, x, y):
        # Gives an offset grid for array, a, by x and y points
        sy1, sy2 = self.getindicies(y, a.shape[0])
        sx1, sx2 = self.getindicies(x, a.shape[1])
        b = zeros(a.shape, a.dtype)
        b[sy1, sx1] = a[sy2, sx2]
        return b

    def agradient(self, a):
        # Gives offset grids in the "forward" x and "up" y directions
        dx = a - self.offset(a, 1, 0)
        dy = a - self.offset(a, 0, - 1)
        return dx, dy

    def diff2(self, x, n=1, axis= - 1):
        """diff2(x,n=1,axis=-1) calculates the first-order, discrete
        center difference approximation to the derivative along the axis
        specified. array edges are padded with adjacent values.
        """
        a = asarray(x)
        nd = len(a.shape)
        slice1 = [slice(None)] * nd
        slice2 = [slice(None)] * nd
        slice1[axis] = slice(2, None)
        slice2[axis] = slice(None, - 2)
        tmp = a[slice1] - a[slice2]
        rval = zeros(a.shape)
        slice3 = [slice(None)] * nd
        slice3[axis] = slice(1, - 1)
        rval[slice3] = tmp
        slice4 = [slice(None)] * nd
        slice4[axis] = slice(0, 1)
        rval[slice4] = tmp[slice4]
        slice5 = [slice(None)] * nd
        slice5[axis] = slice(- 1, None)
        rval[slice5] = tmp[slice5]
        if n > 1:
            return diff2(rval, n - 1)
        else:
            return rval

    ##
    # Get the grid shape from the GridLocation stored in the parm manager.
    #
    # @return: The number of data points in the X and Y directions.
    # @rtype: 2-tuple of int
    def getGridShape(self):
        gridLoc = self.__parmMgr.compositeGridLocation()
        gridShape = (gridLoc.getNy().intValue(), gridLoc.getNx().intValue())
        return gridShape
    
#########################################################################
## Procedure methods                                                   ##
#########################################################################

    # These commands always apply to the mutable model only.
    # NOTE:  Most of these commands are duplicated with "old" and
    #  "recommended" versions which end in "Cmd".  For example, "copy"
    #  is the "old" version and will eventually not be supported
    #  while the recommended version is "copyCmd".

    # Command Arguments:
    # name1, name2, name3     is a list of the weather element names
    # startHour     is the starting hour for the command offset from modelbase
    # endHour       is the ending hour for the command offset from modelbase.
    #               The ending hour is NOT included in the processing of the
    #               command.
    # modelbase     is the name of the model to be used to determine base times
    #               Note that if this is "", then 0000z from today will be
    #               used for the base time.
    # modelsource   is the name of the model to be used in the copy command
    # copyOnly      is 0 for move and 1 for copy only in the time shift command
    # hoursToShift  is the number of hours to shift the data in time
    #                shift command
    # databaseID    must be of type AFPS.DatabaseID
    #            Can be obtained in various ways:
    #            --By calling findDatabase (see below)
    #            --By calling getDatabase (see below) with the result
    #              of a VariableList entry of type "model" or "D2D_model"
    # timeRange    must be of type AFPS.TimeRange.
    #            Can be obtained in various ways:
    #            --As an argument passed into Smart Tool or Procedure,
    #            --By calling getTimeRange (see below)
    #            --By calling createTimeRange (see below)

    # List of available commands:
    # copyCmd(['name1', 'name2', 'name3'], databaseID, timeRange)
    #    Copies all grids for each weather element from the given database
    #    into the weather element in the mutable database that overlaps
    #    the time range.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.copyCmd(['T', 'Wind'], databaseID, timeRange)
    #    will copy the Temperature and Wind fields analysis through 48 hours
    #    from the latest NAM12 and place them into the forecast.
    # copyToCmd([('srcName1', 'dstName1'),
    #            ('srcName2', 'dstName2')], databaseID, timeRange)
    #    Copies all grids for each weather element from the given database
    #    into the weather element in the mutable database that overlaps
    #    the time range.  The source name and destination name are both
    #    supplied.  This allows for copying data with different names
    #    (The units must match).
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.copyToCmd([('MaxT', 'T'), ('T', 'MinT')], databaseID, 
    #         timeRange)
    #    will copy the Max Temperature into T and T into MinT.
    #    from the latest NAM12 and place them into the forecast.
    #
    # deleteCmd(['name1', 'name2', 'name3'], timeRange)
    #    Deletes all grids that overlap the input time range for element
    #    in the mutable database.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.deleteCmd(['T', 'Wind'], timeRange)
    #    will delete the Temperature and Wind fields analysis up to
    #    but not including 48 hours relative to the start time of
    #    the latest NAM12 model.
    #
    # zeroCmd(['name1', 'name2', 'name3'], timeRange)
    #    Assigns the minimum possible value for scalar and vector, and "<NoWx>"
    #    for weather for the parameter in the mutable database for all grids
    #    that overlap the specified time range.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.zeroCmd(['T', 'Wind'], databaseID, timeRange)
    #    will zero the Temperature and Wind grids through 48 hours
    #    relative to the start time of the latest NAM12 model.
    #
    # interpolateCmd(['name1', 'name2', 'name3'], timeRange,
    #       interpMode="GAPS", interpState="SYNC", interval=0, duration=0)
    #    Interpolates data in the forecast for the named weather elements
    #    for the given timeRange.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.interpolateCmd(['T', 'Wind'], timeRange, "GAPS","SYNC")
    #    will interpolate the Temperature and Wind grids up to but
    #    but not including 48 hours relative to the start time of
    #the latest NAM12 model.
    #    The interpolation will run in SYNC mode i.e. completing before
    #    continuing with the procedure.
    #
    # createFromScratchCmd(['name1', 'name2'], timeRange, repeat, duration)
    #    Creates one or more grids from scratch over the given timeRange
    #    and assigns the default (minimum possible value for scalar
    #    and vector, "<NoWx>" for weather).
    #    The repeat interval and duration (both specified in hours) are
    #    used to control the number of grids created.  If 0 is specified for
    #    either one, than only 1 grid is created for the given time range.  If
    #    valid numbers for duration and repeat are given, then grids will
    #    be created every "repeat" hours and they will have a duration
    #    of "duration" hours.  If there is not enough room remaining to create
    #    a grid with the full duration, then no grid will be created in the space
    #    remaining.  If you don't get the desired results, be sure that your input
    #    time range starts on a valid time constraint for the element.  If the
    #    element's time constraints (not the values supplied in this routine) contains
    #    gaps (i.e., duration != repeatInterval), then the repeat interval and
    #    duration will be ignored and grids will be created for each possible
    #    constraint time.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.createFromScratchCmd(['T', 'Wind'], timeRange, 3, 1)
    #    will create the 1-hour Temperature grids through 48 hours at
    #    3 hour intervals relative to the start time of the latest NAM12 model.
    #
    # timeShiftCmd(['name1', 'name2'], copyOnly, shiftAmount, timeRange)
    #    Performs a time shift by the shiftAmount for all elements that
    #    overlap the time range.
    #    Example:
    #       databaseID = self.findDatabase("NAM12") # Most recent NAM12 model
    #       timeRange = self.createTimeRange(0, 49, "Database", databaseID)
    #       self.timeShiftCmd(['T', 'Wind'], 1, 3, timeRange)
    #
    # splitCmd(elements, timeRange)
    #    Splits any grid that falls on the start time or ending time of the
    #    specified time range for the given parameter in the mutable database.
    #
    # fragmentCmd(elements, timeRange)
    #    Fragments any grids that overlap the input time range for the parm
    #    identified in the mutable database.
    #
    # assignValueCmd(elements, timeRange, value)
    #    Assigns the specified value to all grids points for the grids that
    #    overlap the specified time range, for the weather element in the mutable
    #    database specified.
    #   value is:
    #    an Integer or Float for SCALAR
    #    a magnitude-direction tuple for VECTOR:  e.g. (55,120)
    #    a text string for Weather which can be obtained via the
    #      WxMethods WxString method
    #  Example:
    #    # Scalar
    #    value = 60
    #    self.assignValue(["T","Td"], 0, 12, 'NAM12', value)
    #    # Vector
    #    value = (15, 120)
    #    self.assignValue(["Wind"], 0, 12, 'NAM12', value)
    #    # Weather
    #    from WxMethods import *
    #    value = WxString("Sct RW")
    #    self.assignValue(["Wx"], 0, 12, 'NAM12', value)

    # Example: Copy RUC80 0-12, NAM12 13-48, GFS80 49-72 for T, Wx, and Wind,
    # and then interpolate from hours 0 - 24.
    #
    #
    #       self.copy(['T','Wx', 'Wind'], 0, 12, 'RUC80')
    #       self.copy(['T','Wx', 'Wind'], 13, 48, 'NAM12')
    #       self.copy(['T','Wx', 'Wind'], 49, 72, 'GFS80')
    #       self.interpolate(['T','Wx', 'Wind'], 0, 24, 'RUC80')

    def copyCmd(self, elements, databaseID, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        if isinstance(databaseID, DatabaseID.DatabaseID):
            databaseID = databaseID.toJavaObj()
        for element in elements:
            self.__parmOp.copyCmd(element, databaseID, timeRange)

    def copyToCmd(self, elements, databaseID, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        if isinstance(databaseID, DatabaseID.DatabaseID):
            databaseID = databaseID.toJavaObj()
        for src, dst in elements:
            self.__parmOp.copyToCmd(src, dst, databaseID, timeRange)

    def deleteCmd(self, elements, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        for element in elements:
            self.__parmOp.deleteCmd(element, timeRange)

    def zeroCmd(self, elements, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        for element in elements:
            self.__parmOp.zeroCmd(element, timeRange)

    def interpolateCmd(self, elements, timeRange,
                    interpMode="GAPS", interpState="SYNC", interval=0,
                    duration=0):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        # Convert interval to seconds
        interval = interval * 3600
        for element in elements:
            self.__parmOp.interpolateCmd(element, timeRange,
                                         interpMode, interpState, interval,
                                         duration)

    def createFromScratchCmd(self, elements, timeRange, repeat=0, duration=0):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        # Convert repeat and duration to seconds
        repeat = repeat * 3600
        duration = duration * 3600
        for element in elements:
            self.__parmOp.createFromScratchCmd(element, timeRange, repeat, duration)

    def timeShiftCmd(self, elements, copyOnly, shiftAmount, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        shiftAmount = shiftAmount * 3600
        for element in elements:
            self.__parmOp.timeShiftCmd(element, timeRange, copyOnly,
                                       shiftAmount)

    def splitCmd(self, elements, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        for element in elements:
            self.__parmOp.splitCmd(element, timeRange)

    def fragmentCmd(self, elements, timeRange):
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()
        for element in elements:
            self.__parmOp.fragmentCmd(element, timeRange)

    def assignValueCmd(self, elements, timeRange, value):        
        from com.raytheon.viz.gfe.core.wxvalue import ScalarWxValue, VectorWxValue, WeatherWxValue
        if isinstance(timeRange, TimeRange.TimeRange):
            timeRange = timeRange.toJavaObj()        
        for element in elements:
            parm = self.__parmMgr.getParmInExpr(element, 1)
            if type(value) == types.TupleType:
                newvalue = VectorWxValue(float(value[0]), float(value[1]), parm)
            elif type(value) == types.StringType:
                newvalue = WeatherKey(value)
                newvalue = WeatherWxValue(newvalue, parm)
            else:                
                newvalue = ScalarWxValue(float(value), parm)            
            self.__parmOp.assignValueCmd(element, timeRange, newvalue)

    def __getUserFile(self, name, category):
        from com.raytheon.uf.common.localization import PathManagerFactory, LocalizationContext
        from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
        from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel
        pathMgr = PathManagerFactory.getPathManager()
        path = 'gfe/userPython/' + category + '/' + name
        lc = pathMgr.getContext(LocalizationType.valueOf('CAVE_STATIC'), LocalizationLevel.valueOf('USER'))
        lf = pathMgr.getLocalizationFile(lc, path)
        return lf
        
    
    def saveObject(self, name, object, category):
        import cPickle        
        # Save a Python object (e.g. a Numeric grid)
        # in the server under the given name
        #   Example:
        #   self.saveObject("MyGrid", numericGrid, "DiscrepancyValueGrids")
        #
        lf = self.__getUserFile(name, category)
        fullpath = lf.getFile().getPath()
        idx = fullpath.rfind("/")
        if not os.path.exists(fullpath[:idx]):
            os.makedirs(fullpath[:idx])
        openfile = open(fullpath, 'w')                
        cPickle.dump(object, openfile)
        openfile.close()        
        lf.save()
    
    def getObject(self, name, category):
        import cPickle        
        # Returns the given object stored in the server
        #   Example:
        #   discrepancyValueGrid = self.getObject("MyGrid","DiscrepancyValueGrids")
        #        
        lf = self.__getUserFile(name, category)
        fullpath = lf.getFile().getPath()
        openfile = open(fullpath, 'r')                
        obj = cPickle.load(openfile)                
        openfile.close()        
        return obj

    def deleteObject(self, name, category):        
        # Delete the given object stored in the server
        #    Example:
        #    self.deleteObject("MyGrid", "DiscrepancyValueGrids")
        #
        lf = self.__getUserFile(name, category)
        lf.delete()

    def myOfficeType(self):
        #returns my configured office type, such as "wfo" or "rfc"
        return self.__dataMgr.getOfficeType()

    def officeType(self, siteid):
        #returns the office type for the given site identifier
        #returns None if unknown site id
        a = self.__dataMgr.officeType(siteid)
        if len(a):
            return a
        else:
            return None
        
    def availableDatabases(self):
        dbs = []
        availDbs = self.__parmMgr.getAvailableDbs()
        for i in range(availDbs.size()):
            dbId = availDbs.get(i)
            dbs.append(DatabaseID.DatabaseID(dbId))
        return dbs
    
    def knownOfficeTypes(self):
        import JUtil
        return JUtil.javaStringListToPylist(self.__dataMgr.knownOfficeTypes())

    # Retrieves a text product from the text database
    def getTextProductFromDB(self, productID):
        from com.raytheon.viz.gfe.product import TextDBUtil
        
        opMode = self.gfeOperatingMode()=="OPERATIONAL"
        fullText = TextDBUtil.retrieveProduct(productID, opMode)
        textList =  fullText.splitlines(True)
        return textList


