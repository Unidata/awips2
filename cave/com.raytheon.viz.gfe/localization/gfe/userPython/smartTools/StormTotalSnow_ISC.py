##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StormTotalSnow_ISC
#
# This very simple tool will assign the sum of the SnowAmt grids over the time range of 
# the StormTotalSnow grid. For example, if the user creates/stretches a StormTotalSnow
# grid which runs from 00z to 18z and runs this tool, the result will be the addition
# of the 00-06z, 06z-12z, and 12z-18z ISC SnowAmt grids.
#
# Author: Brian Meade, GRR, 12/7/06
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "StormTotalSnow"
HideTool = 0

## Set up Class
import SmartScript, ProcessVariableList
## For available commands, see SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessGrid(self, varDict):
        #makeVariable List for the dialog, get all of the office types used
        #by the ISC database for SnowAmt. This dialog only appears if there
        #is more than one SnowAmt*** element in the ISC database.
        for databaseID in self.availableDatabases():
            if databaseID.type() == "":
                if databaseID.modelName() == "ISC":
                    allOfficeTypes = self.knownOfficeTypes()
                    iscOfficeTypes = [self.myOfficeType()]
                    parms = self.availableParms()
                    for pname, plevel, pdb in parms:
                        if pdb != databaseID or pname[0:7] != "SnowAmt":
                            continue   #only want ISC ones and SnowAmt*** ones
                        for ot in allOfficeTypes:
                            idx = pname.find(ot)
                            if idx != -1 and pname[idx:] == ot and \
                              ot not in iscOfficeTypes:
                                iscOfficeTypes.append(ot)

        if len(iscOfficeTypes) == 1:
            self._officeType = iscOfficeTypes[0]   #only 1, no dialog needed
            return

        VList = [("ISC Office Type:", "", "radio", iscOfficeTypes)]
        self.varReturn = {}
        processVarList = ProcessVariableList.ProcessVariableList(
          "StormTotalSnow_ISC", VList, self.varReturn)
        status = processVarList.status()
        if status != "Ok":
            self.cancel()
        self._officeType = self.varReturn.get("ISC Office Type:", 
          self.myOfficeType())
                        

    # Required Method: Execute
    #  Called once for each grid
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, GridTimeRange, StormTotalSnow, SnowAmt):
        "Put your tool description here"

        #get the Snow name for the desired office type
        if self._officeType == self.myOfficeType():
            sourceWE = "SnowAmt"
        else:
            sourceWE = "SnowAmt" + self._officeType

        StormTotalSnow=self.getGrids("ISC", sourceWE, "SFC",GridTimeRange,"Sum")

        # Return the new value
        return StormTotalSnow
    
