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
# Show_Evolution - part of BOIVerify version 2.0
#
#   Add grids to the grid manager (from the verification database) that show
#   how this grid has evolved over time for all model runs.
#
# Author: Tim Barker - SOO BOI
#   2005/07/20 - Original Implmentation
#   2005/07/29 - Version 0.1 - updated grid database structure
#   2006/11/06 - Version 1.0 - no changes - just part of BOIVerify upgrade
#   2007/10/25 - Version 2.0 - no changes - just part of BOIVerify upgrade
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    1/30/13         15536         ryu            Made necessary changes to get tool to run
#
# ----------------------------------------------------------------------------
ToolType = "numeric"
WeatherElementEdited = "None"
ScreenList=["MaxT","MinT","MaxRH","MinRH","TdMrn","TdAft","T","Td","RH"]

from numpy import *
import time
import ProcessVariableList
import SmartScript
import BOIVerifyUtility


class Tool (SmartScript.SmartScript):
   def __init__(self, dbss):
      self._dbss=dbss
      SmartScript.SmartScript.__init__(self, dbss)
   def preProcessTool(self,WEname):
      if WEname not in ScreenList:
         self.statusBarMsg("No Verification Data for %s"%WEname,"A")
         self.docancel=1
         return
      self.docancel=0
      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.VU.logMsg("Running Show_Evolution")
      self.VU.logMsg("WEname=%s"%WEname)
      #
      #  Make the "variable list" for the dialog - containing
      #  all the models that we have in the verification database
      #  except that Official must come first in the list
      #
      VList=[];
      modelList=self.VU.listModels()
      if "Official" in modelList:
         modelList.remove("Official")
      modelList[0:0]=["Official",]
      VList.append(["Model:","Official","radio",modelList])
      #
      #  Run the dialog
      #
      varReturn={}
      processVarList=ProcessVariableList.ProcessVariableList(
         "Pick Model",VList,varReturn)
      status=processVarList.status()
      if status.lower() != "ok":
         self.docancel=1
         return
      self.modelName=varReturn["Model:"]
      
      #
      #  Remove any current VER grids from gridManager
      #
      ##parmList = self._dbss.dataManager().parmMgr().displayedParms()
      parmList = self._dbss.getParmManager().getDisplayedParms()
      for parm in parmList:
          pid=parm.getParmID()
          pmodel = pid.getDbId().getModelId()
          name=pid.getParmName()
          level=pid.getParmLevel()
          if pmodel == "Ver":
             self.unloadWE("Ver",name,level)

   def preProcessGrid(self):
      if self.docancel==1:
         self.cancel()
         
   def postProcessTool(self):
      self.VU.logMsg("Finished Show_Evolution")
               
   def execute(self,WEname,GridTimeRange):
      "Show history of grids over time"
      self.VU.logMsg("   executing for time:%s"%GridTimeRange)
      mutableModel=self.mutableID().modelName()
      tomorrow=time.time()+86400
      parm=WEname
      self.VU.logMsg("   running for parm:%s"%parm)
      self.VU.logMsg("   running for model:%s"%self.modelName)
      #
      #  Get the color table and range for the parm
      #
      (self.parmUnits,self.parmPrecision,self.parmMinval,self.parmMaxval,
       self.parmRateFlag,self.parmColorTable,self.parmDisplayMinval,
       self.parmDisplayMaxval)=self.getParmInfo(mutableModel,WEname)
      #
      #  Get start/end of current timerange and get verification
      #  model records that match
      #
      starttime=GridTimeRange.startTime().unixTime()
      endtime=GridTimeRange.endTime().unixTime()
      recs=self.VU.listRecords(parm,self.modelName,starttime,endtime,"verify")
      #
      #  Loop over verification model records
      #
      for rec in recs:
         rec = int(rec)
         self.VU.logMsg("rec=%d"%rec)
         #
         #  read grid - clip to min/max for this parm
         #
         fcstgrid=self.VU.readRecord(parm,self.modelName,rec)
         newgrid=clip(fcstgrid,self.parmMinval,self.parmMaxval)
         #
         #  to put grids on the grid manager in the right order, the
         #  name will include the parm, a number based on the basetime
         #  of the run compared to TOMORROW - and the actual day/hour
         #  of the run.
         #
         fbtime=self.VU.fncBtime[rec]
         fstime=self.VU.fncStime[rec]
         fetime=self.VU.fncEtime[rec]
         basetuple=time.gmtime(fbtime)
         starttuple=time.gmtime(fstime)
         endtuple=time.gmtime(fetime)
         runTime="%4.4d%2.2d%2.2d%2.2d"%(basetuple[0],basetuple[1],basetuple[2],basetuple[3])
         startTime="%4.4d%2.2d%2.2d%2.2d"%(starttuple[0],starttuple[1],starttuple[2],starttuple[3])
         endTime="%4.4d%2.2d%2.2d%2.2d"%(endtuple[0],endtuple[1],endtuple[2],endtuple[3])
         self.VU.logMsg("runTime=%s starttime=%s endtime=%s"%(runTime,startTime,endTime))
         runHours=self.VU.getFcstHour(fbtime,tomorrow)
         run=(runHours/6)+1
         parmname="%srun%3.3dfrom%s"%(parm,run,runTime[6:10])
         if self.modelName!="Official":
            parmname+=self.modelName
         #
         #  Create the grid and set the color table
         #
         self.createGrid("Ver",parmname,"SCALAR",newgrid,GridTimeRange,
                   "Forecast",None,self.parmPrecision,self.parmMinval,
                   self.parmMaxval,self.parmUnits)

         self.setActiveElement("Ver",parmname,"SFC",GridTimeRange,
                   self.parmColorTable,(float(self.parmDisplayMinval),
                   float(self.parmDisplayMaxval)),0)
      return

   def getParmInfo(self,mutableModel,parm):
      units="units"
      precision=0
      minval=0
      maxval=100
      rateflag=0
      colorTable="Gridded Data"
      displayMinval=0
      displayMaxval=100
      parm=self.getParm(mutableModel,parm,"SFC")
      if parm is not None:
         parmInfo = parm.getGridInfo()
         units=parmInfo.getUnitString()
         precision=parmInfo.getPrecision()
         minval=parmInfo.getMinValue()
         maxval=parmInfo.getMaxValue()
         rateflag=parmInfo.isRateParm()
         from com.raytheon.viz.gfe.rsc import DiscreteDisplayUtil
         ctInfo = DiscreteDisplayUtil.buildColorMapParameters(parm)
         if ctInfo is not None:
             colorTable = ctInfo.getColorMapName()
             displayMinval = ctInfo.getColorMapMin()
             displayMaxval = ctInfo.getColorMapMax()
      return(units,precision,minval,maxval,rateflag,colorTable,displayMinval,displayMaxval)
