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
#
#  Limit_Values version 1.5
#
#  Limits gridvalues to user-specified limits.  Can be useful when lots of
#  disconnected areas have values greater or less than a certain amount, and
#  you want to remove them.  Sure, you could do it with a query to find the
#  edit areas, and then a set value, but this makes it much faster and user
#  friendly.  The dialog checks to see which element you are editing so that
#  it can make the sliderbars match the available values for that parameter.
#  At the start, the sliderbars set to the minimum/maximum for the current
#  grid and the current edit area.
#
#  The "resolution" for the sliderbar is sort of clunky - hard coded with
#  the parameter resolution for now, until the gridInfo routine returns
#  the grid value resolution that is set by the configuration
#
#  Tim Barker - 07/15/02 - Original Version
#               07/29/02 - Made sliderbars set to the values in the edit
#                          area - not the whole grid.
#               10/07/03 - More graceful handling of query edit areas
#               08/27/04 - version 1.5 - now handles query edit areas
#
########################################################################
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Feb 06, 2018  6878     randerso  Fixed for query based edit areas

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

#====================================================================
ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList = ["SCALAR", "VECTOR"]
#====================================================================
from numpy import *
import numpy
import ProcessVariableList
import SmartScript

toolName = 'LimitValues'

class Tool (SmartScript.SmartScript):
   def __init__(self, dbss):
      SmartScript.SmartScript.__init__(self, dbss)

   def preProcessGrid(self, editArea, variableElement,
                      variableElement_GridInfo, WEname):
      #
      #  Need a mask of the current edit area, so the
      #  dialog can figure out the current max/min in
      #  the area
      #
      editAreaMask = editArea.getGrid().getNDArray().astype(numpy.bool)
      #
      #  Need a grid of the min/max value for this
      #  element so that we can accurately figure
      #  the min/max in the edit area
      #
      minval = variableElement_GridInfo.getMinValue()
      minvalgrid = self.newGrid(minval)
      maxval = variableElement_GridInfo.getMaxValue()
      maxvalgrid = self.newGrid(maxval)
      #
      #  setup valgrid with the grid that will be
      #  limited (i.e. for vectors the speed)
      #
      if type(variableElement) is list:
         (valgrid, direction) = variableElement
      else:
         valgrid = variableElement
      #
      #  When checking for mins, need all areas outside the edit
      #  area set to the max value for this element - so that the
      #  min will be found accurately.  Conversely for maxs.
      #
      mincheck = where(editAreaMask, valgrid, maxvalgrid)
      maxcheck = where(editAreaMask, valgrid, minvalgrid)
      #
      #  Find the current max/min in the edit area
      #
      currmax = float(maximum.reduce(maximum.reduce(maxcheck)))
      currmin = float(minimum.reduce(minimum.reduce(mincheck)))

      #
      #  Figure the resolution for the variable - wish
      #  you could get this from GridInfo, but you can't
      #
      resolution = 1
      if (WEname == "QPF" or WEname == "IceAccum"):
         resolution = 0.01
      if (WEname == "SnowAmt"):
         resolution = 0.1
      if (WEname == "HrsOfSun"):
         resolution = 0.1
      if (WEname == "FzLevel"):
         resolution = 100.0
      #
      #  Make the "variable list" for the dialog with the min/max
      #

      VList = []
      VList.append(["Maximum Value:", currmax, "scale", [minval, maxval], resolution])
      VList.append(["Minimum Value:", currmin, "scale", [minval, maxval], resolution])

      #
      #  Run the dialog
      #
      varReturn = {}
      processVarList = ProcessVariableList.ProcessVariableList("Set Limits", VList, varReturn)
      varReturn = processVarList.varDict()
      status = processVarList.status()

      if status != "OK":
        self.cancel()

      #
      #  Get the min/max returned from the dialog
      #  and don't let them set min more than max
      #
      if status == "OK":
        self.maxlimit = varReturn["Maximum Value:"]
        self.minlimit = varReturn["Minimum Value:"]
        if (self.minlimit > self.maxlimit):
            temp = self.minlimit
            self.minlimit = self.maxlimit
            self.maxlimit = temp

            return
   #========================================================================
   #
   #  Main routine that reads in the variableElement and limits it to
   #  the values set up via the dialog in the preProcessGrid routine
   #
   def execute(self, variableElement):
        "limit gridvalues between user-set limits"
        #
        #  set val to the grid we will limit
        #  (if it is a vector - get the speed)
        #
        if type(variableElement) is list:
            val = variableElement[0]
            direction = variableElement[1]
        else:
            val = variableElement
        #
        #  limit the values, max first, then min
        #
        valC = val.copy()
        valC[greater(val, self.maxlimit)] = self.maxlimit
        valC[less(val, self.minlimit)] = self.minlimit
        #
        #  put the trimmed values back
        #

        if type(variableElement) is list:
            variableElement = (valC, direction)
        else:
            variableElement = valC

        return variableElement
