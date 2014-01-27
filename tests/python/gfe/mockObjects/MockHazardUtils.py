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
import types
from HazardUtils import HazardUtils
import MockParm

##
# This class is intended to allow HazardUtils to be tested. 
# It does this by overriding SmartScript methods used by HazardUtils with 
# methods that return dummy objects.
class MockHazardUtils(HazardUtils):
    
    def __init__(self, dbss, eaMgr, mdMode=None, toolType="numeric"):
        ""
        HazardUtils.__init__(self, dbss, eaMgr, mdMode=None, toolType="numeric")
        self.discreteKeys = {}
        self.grids = {}
        self.gridInfo = {}
        self.log = None
        self.parms = {}

    ##
    # Override of SmartScript.getGridInfo() to bypass use of
    # the parm manager. This just returns whatever has been set
    # for self.gridinfo[element], or throws an exception if 
    # self.gridinfo[element] does not exist..
    def getGridInfo(self,  model, element, level, timeRange,
                    mostRecentModel=0):
        ""
        if self.gridInfo.has_key(element):
            return self.gridInfo[element]
        raise RuntimeError, "getGridInfo: No Weather Element "+exprName

    ##
    # Override of SmartScript.getGrids() to bypass use of the
    # parm manager. This just returns whatever has been set
    # in self.grids for element (since model and level
    # are usually "Fcst" and "SFC").
    # Added the ability to return grids based on element and timerange
    # for testing of _consecutiveIdenticalGrids(). 
    def getGrids(self, model, element, level, timeRange,
                 mode="TimeWtAverage",
                 noDataError=1, mostRecentModel=0,
                 cache=1):
        start = timeRange.getStart().getTime()
        if self.grids.has_key((element, start)):
            return self.grids[(element, start)]
        return self.grids[element]
    
    ##
    # Override of SmartScript.getParm() to bypass use of the
    # parm manager. This just returns whatever has been set
    # in self.parms for element (since model and level
    # are usually "Fcst" and "SFC"). 
    def getParm(self, model, element, level, timeRange=None, mostRecent=0):
        if element in self.parms:
            return self.parms[element]
        return None
    
    ##
    # Override of SmartScript.getGridShape() to bypass use of the 
    # parm manager.
    def getGridShape(self):
        return (145, 145)
    
    ##
    # Override of SmartScript.createGrid() to bypass use of the
    # parm manager. This method writes all its non-default arguments
    # to the log and creates a Python MockParm for the element if it
    # is not in self.parms.
    def createGrid(self, model, element, elementType, numericGrid, timeRange,
                   descriptiveName=None, timeConstraints=None,
                   precision=None, minAllowedValue=None,
                   maxAllowedValue=None, units=None, rateParm=0,
                   discreteKeys=None, discreteOverlap=None,
                   discreteAuxDataLength=None, defaultColorTable=None):
        if self.log is not None:
            self.log.write("createGrid(): ")
            self.__dumpParms(model=model, element=element, elementType=elementType, 
                             numericGrid=numericGrid, timeRange=timeRange,
                             descriptiveName=descriptiveName, timeConstraints=timeConstraints,
                             precision=precision, minAllowedValue=minAllowedValue,
                             maxAllowedValue=maxAllowedValue, units=units, rateParm=rateParm,
                             discreteKeys=discreteKeys, discreteOverlap=discreteOverlap,
                             discreteAuxDataLength=discreteAuxDataLength, 
                             defaultColorTable=defaultColorTable)
        # HazardUtils.addHazard() calls _removeOldGrids(element), and
        # isn't smart enough to skip the call if element is a temp hazard
        # that it just created. _removeOldGrids() raises an exception if it
        # can't obtain the parm, so we need to add it to self.parms. 
        if not element in self.parms:
            self.parms[element] = MockParm.MockParm(element, self.log)

    ##
    # Dump names and values of all parameters to the log.
    # Arguments whose value is None are not dumped; string argvalues are single-quoted.
    # The parms are written as a long string with spaces for separators.
    def __dumpParms(self, **kwargs):
        if self.log is not None:
            sep = ""
            for argName in sorted(kwargs.keys()):
                argVal = kwargs[argName]
                if argVal is not None:
                    self.log.write(sep)
                    self.log.write(argName)
                    self.log.write("=")
                    if isinstance(argVal,types.StringType):
                        self.log.write("'")
                        self.log.write(argVal)
                        self.log.write("'")
                    elif isinstance(argVal,types.TupleType):
                        self.log.write("(")
                        self.__dumpArgs(*argVal)
                        self.log.write(")")
                    elif "shape" in dir(argVal):
                        # Assume it's a numpy array
                        typeName = str(type(argVal))
                        self.log.write(typeName)
                        self.log.write(argVal.shape)
                    else:
                        self.log.write(str(argVal))
                    sep = " "

    ##
    # Dump values of sequences
    def __dumpArgs(self, *args):
        if self.log is not None:
            sep = ""
            for arg in args:
                self.log.write(sep)
                if isinstance(arg, types.StringType):
                    self.log.write("'")
                    self.log.write(arg)
                    self.log.write("'")
                elif isinstance(arg, types.TupleType):
                    self.log.write("(")
                    self.__dumpArgs(*arg)
                    self.log.write(")")
                elif "shape" in dir(arg):
                    # Assume it's a numpy array
                    typeName = str(type(arg))
                    self.log.write(typeName)
                    self.log.write(arg.shape)
                else:
                    self.log.write(arg)
                sep = ", "
                    
    def getDiscreteKeys(self, element):
        if element in self.discreteKeys:
            return self.discreteKeys[element]
        return None
    
    # Override for SmartScript.loadedParms.
    # Returns a list of 3-tuples, where each tuple[0] is the
    # key of an entry in self.parms and tuple[1] and [2] are
    # always 'SFC' and 'Fcst' (because this mock object doesn't
    # keep track of level or model).
    def loadedParms(self):
        lenp = len(self.parms)
        ans = zip( self.parms.keys(), ['SFC'] * lenp, ['Fcst'] * lenp)
        return ans
    
    def statusBarMsg(self, message, status, category="GFE"):
        if message is None:
            raise RuntimeError, "no message"
        elif status is None:
            raise RuntimeError, "no status"
        elif not status in ['A', 'R', 'U', 'S']:
            raise RuntimeError, "bad status value: " + status
        if self.log is not None:
            self.log.write("statusBarMsg(message='%s', status='%s', category='%s')\n" 
                           % (message, status, category))
