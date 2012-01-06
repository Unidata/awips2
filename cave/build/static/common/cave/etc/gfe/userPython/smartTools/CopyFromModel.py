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
# Copy_from_Model - Takes average of model grids and stores them into
#                   current grid (in current edit area).  For QPF and
#                   SnowAmt, does a sum rather than an average.  For
#                   PoP does a Max rather than an average (to support
#                   floating PoP). User gets to choose the model and
#                   timeoffset.  If TimeOffset is negative, it means
#                   get model data from earlier and put in this time -
#                   if TimeOffset is positive, it means get model
#                   data from later and put in this time.
#
# Author: Tim Barker - SOO Boise, ID
#    03/26/2002 - Copy from Copy_from_Model routine
#    03/07/2003 - Added model time offset - so you can time-shift model
#                 data too.  Also added stuff so that if your grid crosses
#                 more than one model PoP grid, it gets the Maximum PoP in
#                 the "floating PoP" sense.
#----------------------------------------------------------------------------
ToolType="numeric"
WeatherElementEdited = "variableElement"
from numpy import *
import TimeRange

#VariableList = [
#    ("Model:","","model"),
#    ("Taper at Edges:","No","radio",["Yes","No"]),
#    ("Hour offset:",0,"scale",[-48,48]),
#    ]
import SmartScript
import ProcessVariableList
class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    def preProcessGrid(self, editArea, Topo, varDict):
        #make Variable List for the dialog
        models = []
        for databaseID in self.availableDatabases():
            if databaseID.type() == "":
                if databaseID.modelName() == "ISC":
                    allOfficeTypes = self.knownOfficeTypes()
                    iscOfficeTypes = [self.myOfficeType()]
                    parms = self.availableParms()
                    for pname, plevel, pdb in parms:
                        if pdb != databaseID:
                            continue   #only want ISC ones
                        for ot in allOfficeTypes:
                            idx = pname.find(ot)
                            if idx != -1 and pname[idx:] == ot and \
                              ot not in iscOfficeTypes:
                                iscOfficeTypes.append(ot)
                    for ot in iscOfficeTypes:
                        models.append(databaseID.modelIdentifier() + " (" + \
                          ot + ")")
                        
                elif databaseID.modelName() not in ['Fcst', 'Official', 'Slider']:
                    models.append(databaseID.modelIdentifier())
        VList = [
          ("Model:","","radio", models),
          ("Taper at Edges:","No","radio",["Yes","No"]),
          ("Hour offset:",0,"scale",[-48,48]),
          ]
        self.varReturn={}
        processVarList=ProcessVariableList.ProcessVariableList(
         "CopyFromModel",VList,self.varReturn)
        status=processVarList.status().upper()
        if status != "OK":
           self.cancel()

        processVarList.varDict() # fills self.varReturn
        if self.varReturn["Taper at Edges:"]=="Yes":
            self.__taperGrid = self.taperGrid(editArea, 5)
        else:
            self.__taperGrid = (Topo*0)+1.0

    def execute(self,  variableElement_GridInfo, GridTimeRange, WEname):
        "Copy data from model database"
        #
        #  read mode different for QPF/SnowAmt, and also PoP
        #
        mode="TimeWtAverage"
        wxType=str(variableElement_GridInfo.getGridType())
        if "WEATHER" == wxType:
            mode="Average"
        if ((WEname =="QPF")or(WEname=="SnowAmt")):
            mode="Sum"
        if (WEname=="PoP"):
            mode="Max"

        # Determine source name, may need to rename for ISC
        # model comes in as ISCmodelName (officeType) for ISC database
        WEnameSource = WEname
        modelA=self.varReturn["Model:"].split(' ')
        model = modelA[0]
        if len(modelA) == 2:
            owant = modelA[1][1:-1]   #what office type wanted
            if owant != self.myOfficeType():
                WEnameSource = WEname + owant

        #
        #  grid to read from model may be timeshifted
        #
        offsettime=self.varReturn["Hour offset:"]*3600
        ModelTimeRange=TimeRange.TimeRange(GridTimeRange.startTime()+offsettime,GridTimeRange.endTime()+offsettime)
        #
        #  read the model data
        #
        v0=self.getGrids(model,WEnameSource,"SFC",ModelTimeRange,mode,noDataError=0)
        #
        #  read the current forecast data
        #
        v1=self.getGrids("Fcst",WEname,"SFC",GridTimeRange,mode,noDataError=0)
        #
        #  If either current or new is missing, do nothing
        #
        if v1 is None:
            return v1
        if v0 is None:
            return v1
        #
        #  for vectors, use taper in speed/direction
        #
        if "SCALAR"==wxType:
            return v1+((v0-v1)*self.__taperGrid)
        if "VECTOR"==wxType:
            spd0=v0[0]
            dir0=v0[1]
            spd1=v1[0]
            dir1=v1[1]
            newspd=spd1+((spd0-spd1)*self.__taperGrid)
            newdir=dir1+((dir0-dir1)*self.__taperGrid)
            return(newspd,newdir)
        #
        #  For weather types...just return what the
        #  model has, without any taper
        #
        return v0
