
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerifyBiasCorr2 - Version 2.0.5
#
#    Run the BOIVerifyBiasCorr tool
#
# Author: Tim Barker - SOO BOI
#   2005/07/01 - version 0.0 - original implementation
#   2005/07/29 - version 0.1 - update to grid database structure
#   2006/11/06 - version 1.0 - Make only one procedure!  And have parms
#                    specified via the file name FILE (constant below).
#   2007/10/25 - version 2.0 - Everything now in procedure - for simpler
#                    maintenance and less tool/procedure complexity.
#                    Tones down regression estimate when forecast value is
#                    well outside recent forecasts - falling back to simple
#                    average error over past VERPERIOD days. Obs database
#                    must now be specified. Better logging. Better memory
#                    usage.
#   2008/05/28 - version 2.0.5 - bugfix for basetime/starttime when an
#                    archived grid is constant.
#
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/21/13        16770         ryu            Change name of temporary files
#                                                 for dual domain.
#=============================================================================
#
#  Do not show this in any menu.  Should only be run via runProcedure after
#  putting the force flag and model to correct in /tmp/<siteId>_FILE
#
#MenuItems = ["Verify"]
#
MenuItems = ["None"]
#
#  Constants
#
FILE="BOIVerifyBiasCorr.txt"
HOURSECS=60*60
DAYSECS=24*HOURSECS
#
#  imports
#
from numpy import *
import os, time
import SmartScript,BOIVerifyUtility
import AbsTime
import Exceptions
#
#
DSPATTR_PORT = 0  # TODO - display attributes not working the same way
#
#
class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      SmartScript.SmartScript.__init__(self, dbss)
      self._dbss=dbss
        
   def execute(self, editArea):
      (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime()
      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.VU.logMsg("BOIVerifyBiasCorr Procedure Start")

      self._empty = self.VU._empty
      #
      #
      #  BIASCORR_DAYS configuration should hold number of days usually used
      #  in the bias correction.  If it doesn't...we default to 30
      #
      VERPERIOD=self.VU.getCFG('BIASCORR_DAYS')
      if VERPERIOD is None:
         VERPERIOD=30
      #
      #  BIASCORR_MINDAYS configuration should hold minimum number of days
      #  needed to do a regression.  If it doesn't...we default to 14
      #
      MINDAYS=self.VU.getCFG('BIASCORR_MINDAYS')
      if MINDAYS is None:
         MINDAYS=14
      #
      #  BIASCORR_EXTRAPOLATE_PERCENTAGE configuration should hold the
      #  percentage of range of recent forecasts that we will extrapolate
      #  with regression.  Outside of this...drop back toward using
      #  only average bias
      #
      FUZZ=self.VU.getCFG('BIASCORR_EXTRAPOLATE_PERCENTAGE')
      if FUZZ is None:
         FUZZ=10
      fuzz=FUZZ/100
      #
      #  Thes are the Parms we currently know how to bias correct
      #
      parmList=["T","MaxT","MinT","RH","MaxRH","MinRH"]
      self.transformParms=["RH","MaxRH","MinRH"]
      #
      #  Read the force flag, model, and obsmodel from the FILE file
      #
      force="0"
      model=""
      obsmodel=""
      filename="/tmp/%s_%s"% (self.getSiteID(), FILE)
      if ((os.path.exists(filename))and(os.path.isfile(filename))):
         try:
            infile=file(filename,"r")
            inlines=infile.readlines()
            infile.close()
            os.remove(filename)
            if len(inlines)>0:
               line=inlines[0]
               force=line.strip()
            if len(inlines)>1:
               line=inlines[1]
               model=line.strip()
            if len(inlines)>2:
               line=inlines[2]
               obsmodel=line.strip()
         except:
            self.VU.logMsg("Difficulty reading %s"%filename)
      ##
      ##  testing
      ##
      ##model="NAM12"
      ##obsmodel="Obs"
      ##force="1"
      ##self.VU.setDebug(10)
      #
      #  If model is blank - give error and stop
      #
      if (model==""):
          self.VU.logMsg("No model to correct - cannot continue")
          return
      if (obsmodel==""):
          self.VU.logMsg("No obs model specified - cannot continue")
          return
      #
      #  if obsmodel is not a valid obsmodel - stop
      #
      if obsmodel not in self.VU.getCFG('OBSMODELS'):
         self.VU.logMsg("%s is not a valid name for observations"%obsmodel)
         return
      #
      #  Get corresponding (non-bc) model run with same basetime
      #
      mutableModel=self.mutableID().modelName()
      modeltime=self.mutableID().modelTime().unixTime()
      mtime=time.strftime("%Y%m%d_%H%M",time.gmtime(modeltime))
      foundit=0
      for run in xrange(0,-10,-1):
         dbid=self.findDatabase(model,version=run)
         if dbid is not None:
            inputtime=dbid.modelTime().unixTime()
            if inputtime==modeltime:
               foundit=1
               break
      if foundit==0:
         self.VU.logMsg("%s run from %s could not be found"%(model,mtime))
         self.VU.logMsg("Aborting!")
         return
      (myr,mmo,mdy,mhr,mmi,mse,mwd,myd,mds)=time.gmtime(modeltime)
      yesterday=modeltime-DAYSECS
      modelCycle=mhr
      bigtr=self.createTimeRange(0,500,"Database",dbid)
      self.VU.logMsg("for %s at %2.2dZ"%(model,modelCycle))
      #
      #  Loop over parms
      #
      for parm in parmList:
         self.VU.logMsg("Working on %s"%parm)
         (self.parmUnits,self.parmPrecision,self.parmMinval,self.parmMaxval,
          self.parmColorTable,self.parmDisplayMinval,
          self.parmDisplayMaxval)=self.getParmInfo(model,parm)
         obsparm=self.VU.getObsParm(parm)
         #
         #  get corresponding pairs of observed/fcst grids for last VERPERIOD days
         #
         dateStyle="Verifying on"
         dateType="Period Length"
         self.VU.logMsg("    getting cases",5)
         cases=self.VU.getCases(parm,model,obsparm,obsmodel,dateStyle,dateType,
                                fromDay=yesterday,numDays=VERPERIOD,
                                cycles=mhr,fhrEnd=240)
         caseInfo=cases[model]
         del cases
         fhrInfo={}
         for key in caseInfo.keys():
            (basestr,startstr,endstr)=key.split(",")
            basetime=int(basestr)
            starttime=int(startstr)
            fhrInfo[key]=(starttime-basetime)/HOURSECS
         self.VU.logMsg("    done with getting cases",5)
         #
         #  Loop over all forecast grids
         #
         try:
            gridInfoList=self.getGridInfo(dbid,parm,"SFC",bigtr)
         except Exception, e:
            self.VU.logMsg(str(e))
            continue
         
         for gridInfo in gridInfoList:
            starttime=gridInfo.gridTime().startTime().unixTime()
            endtime=gridInfo.gridTime().endTime().unixTime()
            forecastHour=int((starttime-modeltime)/3600.0)
            endHour=int((endtime-modeltime)/3600.0)
            self.VU.logMsg("  %d-hr forecast"%forecastHour)
            #
            #  Check to see if guess grid is newer than output grid
            #  (with 10 minute overlap...)
            #
            origtr=self.createTimeRange(forecastHour,endHour,"Database",dbid)
            guesstime=self.getLastUpdate(dbid,parm,"SFC",origtr)
            outtime=self.getLastUpdate(mutableModel,parm,"SFC",origtr)
            if ((guesstime<=(outtime-(10*60)))and(force=="0")):
               self.VU.logMsg("    no new info - skipping")
               continue
            #
            #  Read the guess grid
            #
            inputGrid=self.getGrids(dbid,parm,"SFC",origtr,noDataError=0,cache=0)
            if inputGrid is None:
               self.VU.logMsg("Could not read input grid")
               continue
            if parm in self.transformParms:
               inputGrid=self.Ztrans(inputGrid)
            #
            #  Get old forecasts for this forecast hour
            #
            t1=time.time()
            result=self.getOldForecasts(caseInfo,fhrInfo,parm,model,obsparm,
                                        obsmodel,forecastHour,self.parmMinval,
                                        self.parmMaxval)
            t2=time.time()
            self.VU.logMsg("    getting old forecasts took %6.2f"%(t2-t1),5)
            (obsList,fcstList)=result
            numGrids=len(obsList)
            del result
            #
            #  Do not calculate any grids if no previous observations/forecasts
            #
            if numGrids<1:
               continue
            #
            #
            if numGrids<MINDAYS:
               self.VU.logMsg("    Only %d old forecast/observation grids"%numGrids)
               self.VU.logMsg("    Not enough to make regression - skipping")
               continue
            #
            #  Turn list of obs grids and fcst grids into large array
            #
            obsGrids=array(obsList)
            fcstGrids=array(fcstList)
            del obsList
            del fcstList
            #
            #  Get average errors
            #
            avgFcst=add.reduce(fcstGrids)/numGrids
            avgErr=add.reduce(fcstGrids-obsGrids)/numGrids
            maxFcst=maximum.reduce(fcstGrids)
            minFcst=minimum.reduce(fcstGrids)
            #
            #  If more than MINDAYS old forecast/observation pairs...add
            #  regression information
            #
            if numGrids>=MINDAYS:
               self.VU.logMsg("    %d old obs/fcst grids used for regression"%numGrids,2)
               #
               #
               #  Linear regression of forecast anomalies to non-average
               #  forecast errors.
               #
               t1=time.time()
               result=self.getRegression(avgFcst,avgErr,obsGrids,fcstGrids)
               t2=time.time()
               self.VU.logMsg("    getting Regression took %6.3f"%(t2-t1),5)
               (correlation,stdind,stddep)=result
               del result
               del obsGrids
               del fcstGrids
               #
               #  Create the regressed part of error grid from the linear regression
               #
               regErr=(((correlation*stddep)/stdind)*(inputGrid-avgFcst))
               del avgFcst
               del stddep
               del stdind
               del correlation
            else:
               self.VU.logMsg("   %d old obs/fcst grids - used plain bias"%numGrids,2)
               regErr=self._empty
            #
            #  Make a multiplier for the regressed error.  Normally 1.0...but
            #  when the forecast is more than fuzz% beyond the range of forecasts
            #  in the training period...start cutting back correction amount
            #  until getting back to zero regressed amount at (2*fuzz)% beyond.
            #
            multiplier=self._empty+1.0
            fuzzGrid=maximum((maxFcst-minFcst)*fuzz,0.01) # dont have zero
            max1=maxFcst+fuzzGrid
            multiplier=where(greater(inputGrid,max1),1.0-((inputGrid-max1)/fuzzGrid),multiplier)
            min1=minFcst-fuzzGrid
            multiplier=where(less(inputGrid,min1),1.0-((min1-inputGrid)/fuzzGrid),multiplier)
            multiplier[less(multiplier,0.0)] = 0.0
            del min1
            del max1
            del minFcst
            del maxFcst
            del fuzzGrid
            if self.VU.getDebug>=1:
               count=add.reduce(add.reduce(less(multiplier,0.98)))
               count2=add.reduce(add.reduce(less(multiplier,0.02)))
               if count>0:
                  self.VU.logMsg("    %d fcst points are outliers and regression was reduced"%count,1)
                  if count2>0:
                     self.VU.logMsg("       %d points so far out that only average error used"%count2,1)
            #
            #  Correct the forecast with the predicted error
            #
            corrected=inputGrid-(avgErr+(regErr*multiplier))
            del regErr
            del avgErr
            del inputGrid
            del multiplier
            #
            #  For Transformed Parms (like RH) transform it back to the real variable
            #
            if parm in self.transformParms:
               corrected=self.Rtrans(corrected)
            self.VU.logMsg("    done with making corrected grid",5)
            #
            #  clip to legal values and save
            #
            corrected=clip(corrected,self.parmMinval,self.parmMaxval)
            self.createGrid(mutableModel,parm,"SCALAR",corrected,origtr,
                                 parm,None,self.parmPrecision,self.parmMinval,
                                 self.parmMaxval,self.parmUnits)
            del corrected
            self.VU.logMsg("    mem:%d res:%d"%(memory(),resident()),10)
         self.saveGrid(mutableModel,parm)
      #
      #  Fix issues that can arise via doing each parm independently
      #
      #
      #  Make sure MaxT is as high as the max of all T grids
      #
      self.VU.logMsg("Checking for hourly T grids higher than MaxT")
      gridInfoList=self.getGridInfo(mutableModel,"MaxT","SFC",bigtr)
      for gridInfo in gridInfoList:    
         newtr=gridInfo.gridTime()
         maxtgrid=self.getGrids(mutableModel,"MaxT","SFC",newtr,noDataError=0,cache=0)
         if maxtgrid is not None:
            maxoftgrid=self.getGrids(mutableModel,"T","SFC",newtr,mode="Max",noDataError=0,cache=0)
            if maxoftgrid is not None:
               maxt=where(greater(maxoftgrid,maxtgrid),maxoftgrid,maxtgrid)
               changed=add.reduce(add.reduce(greater(maxt,maxtgrid)))
               if changed>0:
                  fhr=int((newtr.startTime().unixTime()-modeltime)/3600.0)
                  self.VU.logMsg("Had to update MaxT at %d-hrs to match hourly T grids at %d points"%(fhr,changed))
                  self.createGrid(mutableModel,"MaxT","SCALAR",maxt,newtr)
      self.saveGrid(mutableModel,"MaxT")
      #
      #  Make sure Mint is as low as the min of all T grids
      #
      self.VU.logMsg("Checking for hourly T grids lower than MinT")
      gridInfoList=self.getGridInfo(mutableModel,"MinT","SFC",bigtr)
      for gridInfo in gridInfoList:    
         newtr=gridInfo.gridTime()
         mintgrid=self.getGrids(mutableModel,"MinT","SFC",newtr,noDataError=0,cache=0)
         if mintgrid is not None:
            minoftgrid=self.getGrids(mutableModel,"T","SFC",newtr,mode="Min",noDataError=0,cache=0)
            if minoftgrid is not None:
               mint=where(less(minoftgrid,mintgrid),minoftgrid,mintgrid)
               changed=add.reduce(add.reduce(less(mint,mintgrid)))
               if changed>0:
                  fhr=int((newtr.startTime().unixTime()-modeltime)/3600.0)
                  self.VU.logMsg("Had to update MinT at %d-hrs to match hourly T grids at %d points"%(fhr,changed))
                  self.createGrid(mutableModel,"MinT","SCALAR",mint,newtr)
      self.saveGrid(mutableModel,"MinT")
      #
      #  Make sure MaxRH is as high as the max of all RH grids
      #
      self.VU.logMsg("Checking for hourly RH grids higher than MaxRH")
      gridInfoList=self.getGridInfo(mutableModel,"MaxRH","SFC",bigtr)
      for gridInfo in gridInfoList:    
         newtr=gridInfo.gridTime()
         maxrhgrid=self.getGrids(mutableModel,"MaxRH","SFC",newtr,noDataError=0,cache=0)
         if maxrhgrid is not None:
            maxofrhgrid=self.getGrids(mutableModel,"RH","SFC",newtr,mode="Max",noDataError=0,cache=0)
            if maxofrhgrid is not None:
               maxrh=where(greater(maxofrhgrid,maxrhgrid),maxofrhgrid,maxrhgrid)
               changed=add.reduce(add.reduce(greater(maxrh,maxrhgrid)))
               if changed>0:
                  fhr=int((newtr.startTime().unixTime()-modeltime)/3600.0)
                  self.VU.logMsg("Had to update MaxRH at %d-hrs to match hourly RH grids at %d points"%(fhr,changed))
                  self.createGrid(mutableModel,"MaxRH","SCALAR",maxrh,newtr)
      self.saveGrid(mutableModel,"MaxRH")
      #
      #  Make sure MinRH is as low as the min of all RH grids
      #
      self.VU.logMsg("Checking for hourly RH grids lower than MinRH")
      gridInfoList=self.getGridInfo(mutableModel,"MinRH","SFC",bigtr)
      for gridInfo in gridInfoList:    
         newtr=gridInfo.gridTime()
         minrhgrid=self.getGrids(mutableModel,"MinRH","SFC",newtr,noDataError=0,cache=0)
         if minrhgrid is not None:
            minofrhgrid=self.getGrids(mutableModel,"RH","SFC",newtr,mode="Min",noDataError=0,cache=0)
            if minofrhgrid is not None:
               minrh=where(less(minofrhgrid,minrhgrid),minofrhgrid,minrhgrid)
               changed=add.reduce(add.reduce(less(minrh,minrhgrid)))
               if changed>0:
                  fhr=int((newtr.startTime().unixTime()-modeltime)/3600.0)
                  self.VU.logMsg("Had to update MinRH at %d-hrs to match hourly RH grids at %d points"%(fhr,changed))
                  self.createGrid(mutableModel,"MinRH","SCALAR",minrh,newtr)
      self.saveGrid(mutableModel,"MinRH")
      #
      #  Create Td from T and RH
      #
      self.VU.logMsg("Making Td from T and RH")
      (self.parmUnits,self.parmPrecision,self.parmMinval,self.parmMaxval,
       self.parmColorTable,self.parmDisplayMinval,
       self.parmDisplayMaxval)=self.getParmInfo(mutableModel,"Td")
      Funits=0
      if self.parmUnits.find("F")>-1:
         Funits=1
      gridInfoList=self.getGridInfo(mutableModel,"T","SFC",bigtr)
      for gridInfo in gridInfoList:    
         newtr=gridInfo.gridTime()
         tgrid=self.getGrids(mutableModel,"T","SFC",newtr,noDataError=0,cache=0)
         if tgrid is not None:
            rhgrid=self.getGrids(mutableModel,"RH","SFC",newtr,noDataError=0,cache=0)
            if rhgrid is not None:
               if Funits==1:
                  tc=(tgrid-32.0)*(5.0/9.0)
               else:
                  tc=tgrid
               rh=clip(rhgrid,0.5,99.5)/100.0
               x=(log(rh)/17.67)+(tc/(tc+243.5))
               tdc=(243.5*x)/(1-x)
               if Funits==1:
                  Td=(tdc*9.0/5.0)+32.0
               else:
                  Td=tdc
               Td=clip(Td,self.parmMinval,self.parmMaxval)
               self.createGrid(mutableModel,"Td","SCALAR",Td,newtr)
      self.saveGrid(mutableModel,"Td")      
      #
      #  Calculate TdMrn/TdAft from MinT/MaxRH and MaxT/MinRH values
      #  Only if the TdMrn parameter exists for the mutable database
      #
      starttime=AbsTime.AbsTime(int(modeltime))
      parmInfoMrn = self.getParmInfo(mutableModel,"TdMrn")
      #if ((self.parmUnits!="NONE")or(self.parmPrecision!=-99)):
      if parmInfoMrn != None:
         self.VU.logMsg("Making TdMrn/TdAft grids")
         (self.parmUnits,self.parmPrecision,self.parmMinval,self.parmMaxval,
          self.parmColorTable,self.parmDisplayMinval,
          self.parmDisplayMaxval) = parmInfoMrn
         (runtr,gridTimes)=self.getGridTimes(mutableModel,"TdMrn","SFC",starttime,240)
         for newtr in gridTimes:
            maxRH=self.getGrids(mutableModel,"MaxRH","SFC",newtr,noDataError=0,cache=0)
            if maxRH is not None:
               minT=self.getGrids(mutableModel,"MinT","SFC",newtr,noDataError=0,cache=0)
               if minT is not None:
                  if Funits==1:
                     tc=(minT-32.0)*(5.0/9.0)
                  else:
                     tc=MinT
                  rh=clip(maxRH,0.5,99.5)/100.0
                  x=(log(rh)/17.67)+(tc/(tc+243.5))
                  tdc=(243.5*x)/(1-x)
                  if Funits==1:
                     Td=(tdc*9.0/5.0)+32.0
                  else:
                     Td=tdc
                  Td=clip(Td,self.parmMinval,self.parmMaxval)
                  self.createGrid(mutableModel,"TdMrn","SCALAR",Td,newtr)
         self.saveGrid(mutableModel,"TdMrn")
         (self.parmUnits,self.parmPrecision,self.parmMinval,self.parmMaxval,
          self.parmColorTable,self.parmDisplayMinval,
          self.parmDisplayMaxval)=self.getParmInfo(mutableModel,"TdAft")
         (runtr,gridTimes)=self.getGridTimes(mutableModel,"TdAft","SFC",starttime,240)
         for newtr in gridTimes:
            minRH=self.getGrids(mutableModel,"MinRH","SFC",newtr,noDataError=0,cache=0)
            if minRH is not None:
               maxT=self.getGrids(mutableModel,"MaxT","SFC",newtr,noDataError=0,cache=0)
               if maxT is not None:
                  if Funits==1:
                     tc=(maxT-32.0)*(5.0/9.0)
                  else:
                     tc=maxT
                  rh=clip(minRH,0.5,99.5)/100.0
                  x=(log(rh)/17.67)+(tc/(tc+243.5))
                  tdc=(243.5*x)/(1-x)
                  if Funits==1:
                     Td=(tdc*9.0/5.0)+32.0
                  else:
                     Td=tdc
                  Td=clip(Td,self.parmMinval,self.parmMaxval)
                  self.createGrid(mutableModel,"TdAft","SCALAR",Td,newtr)
         self.saveGrid(mutableModel,"TdAft")
      self.VU.logMsg("BOIVerifyBiasCorr Procedure Finished")
      return
   #=================================================================
   #  getLastUpdate(self,model,parmname,level,tr)
   #
   #  Gets the last update time for the grid for the model, parmname,
   #  level and timerange.  Should only call this for a timerange where
   #  only one grid is located. If multiple grids are found - gives the
   #  last update for the FIRST one in the list.  If no grids are found,
   #  it returns 0.
   #
   def getLastUpdate(self,model,parmname,level,tr):
      ret=-1
      try:
         historyList = self.getGridHistory(model, parmname, level, tr)
      except:
         return ret
      if not historyList:
         return ret
      for history in historyList[0]:
         if history[5] is not None:
            upd = history[5].unixTime()
         else:
            upd = None
         if upd>ret:
            ret=upd

      return ret
   #=================================================================
   #
   #  RH transforms
   #
   def Ztrans(self,ingrid):
      c=clip(ingrid,0.1,99.9)
      return 0.5*log(c/(100-c))
   def Rtrans(self,ingrid):
      v=exp(ingrid*2.0)
      return (100*v)/(1.0+v)
   #=================================================================
   #  getOldForecasts - Get list of matching forecast grids and
   #                    observed grids for the specified forecast
   #                    hour.
   #
   def getOldForecasts(self,caseInfo,fhrInfo,parm,model,obsparm,obsmodel,forecastHour,
                        minval,maxval):
      obsList=[]
      fcstList=[]
      #
      #
      #
      for key in caseInfo.keys():
         (freclist,oreclist)=caseInfo[key]
         #
         #  Have to have both forecast and observed grids
         #
         if ((len(freclist)<1)or(len(oreclist)<1)):
            continue
         #
         #  Use only ones that match the current forecastHour
         #
         fhr=fhrInfo[key]         
         if fhr!=forecastHour:
            continue
         #
         #  Get average of non-bad forecast grids
         #
         numgrids=0
         fcstgrid=self._empty.copy()
         for frec in freclist:
            testgrid=self.VU.readRecord(parm,model,frec)
            if self.badGrid(testgrid,minval,maxval):
               basetime=self.VU.fncBtime[frec]
               (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(basetime)
               self.VU.logMsg("    %d-hr forecast from %4.4d/%2.2d/%2.2d %2.2dZ run appears bad - so skipped"%(fhr,
                              byea,bmon,bday,bhou),2)
               continue
            fcstgrid+=testgrid
            numgrids+=1
         if numgrids==0:
            continue
         fcstgrid/=float(numgrids)
         #
         #  Get average of non-bad observed grids
         #
         numgrids=0
         obsgrid=self._empty.copy()
         for orec in oreclist:
            testgrid=self.VU.readRecord(obsparm,obsmodel,orec)
            if self.badGrid(testgrid,minval,maxval):
               starttime=self.VU.oncStime[orec]
               (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(starttime)
               self.VU.logMsg("    Observed grid at %4.4d/%2.2d/%2.2d %2.2dZ appears bad - so skipped"%(syea,
                              smon,sday,shou),2)
               continue
            obsgrid+=testgrid
            numgrids+=1
         if numgrids==0:
            continue
         obsgrid/=float(numgrids)
         #
         #  Transform the ob/fcst if needed
         #
         if parm in self.transformParms:
            obsgrid=self.Ztrans(obsgrid)
            fcstgrid=self.Ztrans(fcstgrid)
         #
         #  Add grid to lists of grids
         #
         obsList.append(obsgrid)
         fcstList.append(fcstgrid)
      #
      #  Return lists of forecast/observed grids
      #
      return(obsList,fcstList)
   #=================================================================
   #  getRegression
   #
   #  get linear regression predictors of non-average forecast errors
   #  from forecast anomalies (difference of forecast from average
   #  forecast) from the pairList forecasts
   #
   def getRegression(self,avgFcst,avgErr,obsGrids,fcstGrids):
      #
      #  The independent variable is forecast anomaly (forecast-avgFcst)
      #
      ind=subtract(fcstGrids,avgFcst)
      #
      #  The dependent variable is error anomaly (error-avgErr)
      #
      dep=subtract(fcstGrids-obsGrids,avgErr)
      #
      #  Get covariance, and square of dependent and independent variables
      #  across the cases
      #
      covsum=add.reduce(dep*ind)
      depsqr=add.reduce(dep*dep)
      indsqr=add.reduce(ind*ind)
      #  
      #  Calculate standard deviation of indpendent variables
      #
      num=obsGrids.shape[0]
      stdind=(indsqr/num)**0.5 # standard deviation of independent variable
      stddep=(depsqr/num)**0.5 # standard deviation of dependent variable
      stdind[less(stdind,0.001)] = 0.001
      stddep[less(stddep,0.001)] = 0.001
      covariance=covsum/num    # covariance
      correlation=covariance/(stdind*stddep)  # correlation coefficient
      return(correlation,stdind,stddep)
   #==================================================================
   #
   #  badGrid - very crude way to flag bad grids from calculations.
   #            Just looks for grids with virtually no variance.
   #            (this won't work for precip that typically does have
   #             no variance on days with no precip)
   #            returns 1 if bad, 0 if OK.
   #
   def badGrid(self,grid,minvalue,maxvalue):
      numpts=int(add.reduce(add.reduce(self._empty+1)))
      avg=add.reduce(add.reduce(grid))/float(numpts)
      sqr=add.reduce(add.reduce(grid*grid))/float(numpts)
      var=sqr-(avg*avg)
      if var<((maxvalue-minvalue)/1000.0):
         return 1
      return 0
   #==================================================================
   #
   #
   def getParmInfo(self,mutableModel,parm):
      units="NONE"
      precision=-99
      minval=0
      maxval=100
      colorTable=""
      displayMinval=0
      displayMaxval=100
      parm=self.getParm(mutableModel,parm,"SFC")
      if parm is not None:
         parmInfo = parm.getGridInfo()
         units=parmInfo.getUnitString()
         precision=parmInfo.getPrecision()
         minval=parmInfo.getMinValue()
         maxval=parmInfo.getMaxValue()
         if DSPATTR_PORT:
            ctInfo=parmInfo.getDisplayAttributes().colorTable()
            if ctInfo is not None:
               colorTable=ctInfo.name()
               displayMinval=ctInfo.minLimit()
               displayMaxval=ctInfo.maxLimit()
         return(units,precision,minval,maxval,colorTable,
             displayMinval,displayMaxval)
   #-----------------------------------------------------------------
   #
   #  Save grid for the specified outdb and parmname
   #
   def saveGrid(self,outdb,parmname):
      p=self.getParm(outdb,parmname,"SFC")
      if p is not None:
         p.saveParameter(1)
      return
#
#  debug stuff for memory usage
#
_proc_status="/proc/%d/status"%os.getpid()
_scale={'kB':1024.0,'mB':1024.0*1024.0,
        'KB':1024.0,'MB':1024.0*1024.0}
def _VmB(VmKey):
   try:
      t=open(_proc_status)
      v=t.read()
      t.close()
   except IOError:
      return 0.0
   i=v.index(VmKey)
   v=v[i:].split(None,3)
   if len(v)<3:
      return 0.0
   return float(v[1])*_scale[v[2]]
def memory():
   return _VmB('VmSize:')
def resident():
   return _VmB('VmRSS:')
   
