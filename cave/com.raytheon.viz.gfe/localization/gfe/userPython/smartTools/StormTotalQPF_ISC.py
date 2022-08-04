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
# StormTotalQPF_ISC
#
# This very simple tool will assign the sum of the QPF grids over the time range of 
# the StormTotalQPF grid. For example, if the user creates/stretches a StormTotalQPF
# grid which runs from 00z to 18z and runs this tool, the result will be the addition
# of the 00-06z, 06z-12z, and 12z-18z ISC QPF grids.
#
# Author: Brian Meade, GRR, 12/7/06
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

ToolType = "numeric"
WeatherElementEdited = "StormTotalQPF"
HideTool = 0

import SmartScript, ProcessVariableList

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def preProcessGrid(self):
        #makeVariable List for the dialog, get all of the office types used
        #by the ISC database for QPF. This dialog only appears if there are
        #multiple QPF*** weather elements in the ISC database.
        for databaseID in self.availableDatabases():
            if databaseID.type() == "":
                if databaseID.modelName() == "ISC":
                    allOfficeTypes = self.knownOfficeTypes()
                    iscOfficeTypes = [self.myOfficeType()]
                    parms = self.availableParms()
                    for pname, plevel, pdb in parms:
                        if pdb != databaseID or pname[0:3] != "QPF":
                            continue   #only want ISC ones and QPF*** ones
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
          "StormTotalQPF_ISC", VList, self.varReturn)
        status = processVarList.status()
        if status != "OK":
            self.cancel()
        self._officeType = self.varReturn.get("ISC Office Type:", 
          self.myOfficeType())
                        

    def execute(self, GridTimeRange):
        #get the QPF name for the desired office type
        if self._officeType == self.myOfficeType():
            sourceWE = "QPF"
        else:
            sourceWE = "QPF" + self._officeType

        StormTotalQPF = self.getGrids("ISC", sourceWE, "SFC", GridTimeRange, "Sum")

        # Return the new value
        return StormTotalQPF
    
