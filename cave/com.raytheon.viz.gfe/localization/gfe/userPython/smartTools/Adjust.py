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
# Adjust - Version 1.2
#
#          A single dialog providing a combination of many simple mathematical
#          operations (add, subtract, multiply, divide) optionally with
#          elevation effects (most adjustment at mountains (highest elevation)
#          or valleys (lowest elevation) or a specific elevation), optionally
#          with edge effects (edge (ramp up over edge thickness) or taper
#          (ramp up to middle)) and providing a single place to specify delta
#          value, edge thickness and vector edit mode.  When multiple effects
#          are combined (except a constant change)- the tool carefully
#          re-scales the adjustment so that at least one gridpoint gets the
#          "delta value" adjustment and one gridpoint gets no adjustment.
#
#          Makes many old single-use tools obsolete (Up, UpMtn, UpVly, Down,
#          DownMtn, DownVly, UpwEdge, UpwTaper, DownwEdge, DownwTaper, Back,
#          BackwEdge, BackwTaper, Veer, VeerwEdge, VeerwTaper, MultiplyDivide),
#          and also adds some new combinations (Multiply Mountain with Taper,
#          for example)
#
#          Originally the idea of Greg Martin at SGX to combine all these
#          tools into a single dialog - Thanks Greg!
#
# Author: Tim Barker - SOO Boise, ID
# History: 06/03/03 - Original Version
#          10/07/03 - More graceful handling of Query edit areas
#          02/23/04 - Version 1.1.  Fix error when first tool run after GFE
#                     startup and no edit area specified (implying run over
#                     entire grid).
#          08/27/04 - Version 1.2.  Fix handling of Query edit areas
#          07/21/10 - Ajust only magnitude when vector mode is set to BOTH
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList=["SCALAR","VECTOR"]
import numpy
from numpy import *
import SmartScript

VariableList=[]
VariableList.append(("Value:",1,"numeric"))
VariableList.append(("Action:","Add","radio",["Add","Subtract","Multiply","Divide"]))
VariableList.append(("Elevation:","None","radio",["None","Mountain","Valley","Specific"]))
VariableList.append(("Vectors:","Magnitude","radio",["Magnitude","Direction"]))
VariableList.append(("Edge:","Flat","radio",["Flat","Edge","Taper"]))
VariableList.append(("Edge Width:",5,"scale",[1,30],1))
VariableList.append(("Specific Elevation:",5000,"numeric"))

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
    def preProcessTool(self):
        self.savemode=self.getVectorEditMode()
        self.setVectorEditMode("Magnitude")
    def postProcessTool(self):
        self.setVectorEditMode(self.savemode)
    def preProcessGrid(self,Topo,editArea,varDict):
        #if type(editArea) is types.StringType:
        #    editArea=self.getEditArea(editArea)
        ea=self.encodeEditArea(editArea)
        #
        #  if no editArea is specified GFE will pass in an editArea that
        #  covers the whole grid.  However, if this is the first tool
        #  called after GFE starts up - the edit area is None - so in that
        #  case just make ea one at all gridpoints
        #
        if ea is None:
            ea=numpy.ones(Topo.shape, "int8")
        elev=varDict["Elevation:"]
        edge=varDict["Edge:"]
        edgewidth=varDict["Edge Width:"]
        elevmid=varDict["Specific Elevation:"]
        #
        #  Make elevgrid 0-1 across edit area
        #
        if (elev=="None"):
           elevgrid=ea.astype("float32")
        else:
           editAreaMask=ea
           topoMask=editAreaMask*Topo
           topoMaskMin=where(equal(topoMask,0),100000,topoMask)
           maxtopo=maximum.reduce(maximum.reduce(topoMask))
           mintopo=minimum.reduce(minimum.reduce(topoMaskMin))
           topodiff=(maxtopo-mintopo)+0.0001
           if (elev=="Mountain"):
               elevgrid=editAreaMask*((Topo-mintopo)/topodiff)
           elif (elev=="Valley"):
               elevgrid=editAreaMask*((maxtopo-Topo)/topodiff)
           else:
               elevgrid=Topo*0.0
               if maxtopo>elevmid:
                   abovediff=(maxtopo-elevmid)+0.0001
                   elevgrid=where(greater_equal(Topo,elevmid),(maxtopo-Topo)/abovediff,elevgrid)
               if mintopo<elevmid:
                   belowdiff=(elevmid-mintopo)+0.0001
                   elevgrid=where(less(Topo,elevmid),(Topo-mintopo)/belowdiff,elevgrid)
               elevgrid*=editAreaMask
        #
        #  Make edgegrid 0-1 across edit area
        #
        if (edge=="Flat"):
           edgegrid=ea
        elif (edge=="Edge"):
           edgegrid=self.taperGrid(editArea,int(edgewidth))
        else:
           wval=0
           edgegrid=self.taperGrid(editArea,int(wval))
        #
        #  combine elevgrid and edgegrid
        #
        self.deltagrid=elevgrid*edgegrid
        #
        #  combined factor may not equal 1 anywhere - stretch it so it does
        #
        maxdelta=maximum.reduce(maximum.reduce(self.deltagrid))
        if (maxdelta<1.0):
            #
            #  If there is no change at all - set change to
            #  constant everywhere in edit area
            #
            if (maxdelta<0.01):
                self.deltagrid=ea
            else:
                self.deltagrid*=(1.0/maxdelta)
    def execute(self, variableElement, variableElement_GridInfo, varDict):
        "Various Adjustments"
        deltavalue=varDict["Value:"]
        vect=varDict["Vectors:"]
        action=varDict["Action:"]
   
        if variableElement_GridInfo.getGridType().toString() != 'VECTOR':
           
           if (action=="Add"):
              newval=variableElement+(self.deltagrid*deltavalue)
           elif (action=="Subtract"):
              newval=variableElement-(self.deltagrid*deltavalue)
           elif (action=="Multiply"):
              newval=variableElement*(((deltavalue-1.0)*self.deltagrid)+1.0)
           elif (action=="Divide"):
              newval=variableElement/(((deltavalue-1.0)*self.deltagrid)+1.0)
           newval = newval.astype(variableElement[0].dtype)
        else:
           mag=variableElement[0]
           dir=variableElement[1]
           if (vect=="Magnitude"):
              if (action=="Add"):
                 newmag=mag+(self.deltagrid*deltavalue)
              elif (action=="Subtract"):
                 newmag=mag-(self.deltagrid*deltavalue)
              elif (action=="Multiply"):
                 newmag=mag*(((deltavalue-1.0)*self.deltagrid)+1.0)
              elif (action=="Divide"):
                 newmag=mag/(((deltavalue-1.0)*self.deltagrid)+1.0)
           else:
              newmag=mag
           if (vect=="Direction"):
              if (action=="Add"):
                 newdir=dir+(self.deltagrid*deltavalue)
              elif (action=="Subtract"):
                 newdir=dir-(self.deltagrid*deltavalue)
              elif (action=="Multiply"):
                 newdir=dir*(((deltavalue-1.0)*self.deltagrid)+1.0)
              elif (action=="Divide"):
                 newdir=dir*(((deltavalue-1.0)*self.deltagrid)+1.0)
           else:
              newdir=dir
           newmag = newmag.astype(mag.dtype)
           newdir = newdir.astype(dir.dtype)
           newval=(newmag,newdir)
        return newval

