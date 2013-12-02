# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerifySave - Version 2.0.5
#
#    Save grids from the model specified in the BOIVerifySaveModel.txt
#    file (and immediately remove the BOIVerifySaveModel.txt file)
#
# Author: Tim Barker - SOO BOI
#   2005/07/01 - version 0.0 - original implementation
#   2005/07/29 - version 0.1 - update to grid database structure
#   2005/11/06 - version 1.0 - make only one procedure!  And have model
#                    specified via the file named FILE (constant below).  If
#                    the editable grid is not there - create it temporarily
#                    while the tool runs.
#   2007/10/25 - version 2.0 - Switched from tool to a procedure.  If no
#                    model is specified in the input /tmp/$FILE...then loop
#                    over all configured SAVEMODELS that are not in ObsModels
#                    and not singleton databases.
#   2008/05/28 - version 2.0.5 - fixed issue with reading ISC grids when
#                    this procedure is run via runProcedure.
#
#   2010/04/23  ryu  Initial port to AWIPS II.
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
#  putting the model to save in /tmp/<siteId>_FILE
#
#MenuItems = ["Verify"] # for debug
#
MenuItems = ["None"]
#
#  Constants
#
FILE="BOIVerifySaveModel.tmp"
PROGNAME="BOIVerify"
#
#  Imports
#
from numpy import *
import time,calendar,os
import SmartScript,BOIVerifyUtility

class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      SmartScript.SmartScript.__init__(self, dbss)
      self._dbss=dbss
        
   def execute(self, editArea):
      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.VU.logMsg("%sSave Start"%PROGNAME)
      #
      ObsModels=self.VU.getCFG('OBSMODELS')
      #
      #  Get list of Models to save
      #
      saveModelList=self.getSaveModels()
      #
      #  loop over specified models
      #
      for modelname in saveModelList:
         self.VU.logMsg("Save data for %s model"%modelname,5)
         #
         #  Observed models handled in saveObsModel
         #
         if modelname in ObsModels:
            self.saveObsModel(modelname)
         else:
            dbid=self.findDatabase(modelname,0)

            if not dbid.isValid():
               continue

            basetime=dbid.modelTime().unixTime()
            #
            #  Singleton models handled in saveSingletonModel
            #
            if basetime==0:
               self.saveSingletonModel(modelname)
            #
            #  All others handled in saveFcstModel
            #
            else:
               self.saveFcstModel(modelname)
      #
      #  Done
      #
      self.VU.logMsg("%sSave Finished"%PROGNAME)
      return
   #==================================================================
   #
   #  saveSingletonModel(modelname) - save grids from a singleton model
   #                                 The basetime is determined from
   #                                 the current time on the computer
   #
   def saveSingletonModel(self,modelname):
      totalsaved=0
      dbid=self.findDatabase(modelname)
      if not dbid.isValid():
         self.VU.logMsg("Could not find %s database to save"%modelname)
         return
      #
      #
      #
      if modelname=="ISC":
         iscflag=1
      else:
         iscflag=0
      #
      #  Setup basetime - based on the current computer system time.
      #                   If saved between 18Z and 06Z its from 12Z run
      #                                    06Z and 18Z its from 00Z run
      #
      now=time.time()
      (zyea,zmon,zday,zhou,zmin,zsec,zwda,zyda,zdst)=time.gmtime(now)
      if zhou<6:
         (yyea,ymon,yday,yhou,ymin,ysec,ywda,yyda,ydst)=time.gmtime(now-(24*60*60))
         basetime=calendar.timegm((yyea,ymon,yday,12,0,0,0,0,0))
      elif zhou<18:
         basetime=calendar.timegm((zyea,zmon,zday,0,0,0,0,0,0))
      else:
         basetime=calendar.timegm((zyea,zmon,zday,12,0,0,0,0,0))
      #
      #  beforetime is the hours after this basetime when forecast really
      #  starts.  For example, what is in our Official grids at 00Z is based
      #  on the guidance from the 12Z runs, so we label this the 12Z run of
      #  the official grids (see basetime calculation above), but we don't
      #  want to look at the forecast grids at 12Z or 15Z or 18Z - because
      #  they are already history - and weren't really forecasts.  So we
      #  set beforetime to 12-hours into the forecast - the first 'forecast
      #  grid we make.  Basically, we never make 00hr forecasts, only 12hr
      #  and beyond.  Models make 00hr and 03hr, though...
      #
      beforetime=basetime+(12*60*60)
      #
      #  setup log messages for this run of model
      #
      modeltime=basetime
      modeltt=time.gmtime(modeltime)
      itime=self.gstring(modeltt)
      modelLogMessage="%s run of %s model"%(itime,modelname)
      self.VU.logMsg("  Checking %s"%modelLogMessage,1)
      modelused=0
      #
      #  Loop over parms saved in BOIVerify system
      #
      VerParms=self.VU.getVerParms()
      for parm in VerParms:
         #
         #  make sure this parm is available for this model
         #
         exprName = self.getExprName(modelname, parm, "SFC")
         parmStuff = self._dbss.getParmManager().getParmInExpr(exprName, 1)
         if parmStuff is None:
            continue
         del parmStuff
         parmInfo=self.getParm(dbid,parm,"SFC")
         if parmInfo is None:
            continue
         del parmInfo
         #
         #  if save interval is zero - we are saving all grids
         #  if non-zero - we are saving snapshots at particular times
         #
         sint=self.VU.getVerSaveInterval(parm)            
         if sint==0:
            saveInfo=self.saveAllGrids(dbid,parm,isc=iscflag,
                                       exceptBefore=beforetime,
                                       baseTimeForce=basetime)
         else:
            saveInfo=self.saveSnapshots(dbid,parm,sint,isc=iscflag,
                                        exceptBefore=beforetime,
                                        baseTimeForce=basetime)
         #
         #
         #
         (numSaved,numSkipped,numErrs)=saveInfo
         if numSaved>0:
            if modelused==0:
               self.VU.logMsg("  Saving data for %s"%modelLogMessage)
               modelused=1
            if numSkipped>0:
               self.VU.logMsg("    %s: Saved %d new grids, skipped %d already saved"%(parm,numSaved,numSkipped))
            else:
               self.VU.logMsg("    %s: Saved %d new grids"%(parm,numSaved))
         else:
            self.VU.logMsg("    No new %s grids to save"%parm,1)
         #
         #  end loop over parms
         #
         totalsaved+=numSaved
      del dbid
      if totalsaved==0:
         self.VU.logMsg("No new %s grids to archive"%modelname)
      return
   #==================================================================
   #
   #  saveObsModel(modelname) - save grids from an ObsModel.  Saves
   #                            all grids - and basetime is the same
   #                            as the start time for every grid
   #
   def saveObsModel(self,modelname):
      #
      #  Get database ID for model
      #
      dbid=self.findDatabase(modelname,0)
      if not dbid.isValid():
         self.VU.logMsg("Could not find %s database"%modelname)
         return
      #
      #  setup log messages for this run of model
      #
      modelLogMessage="%s Observations"%(modelname)
      self.VU.logMsg("Checking for new %s"%modelLogMessage)
      modelused=0
      #
      #  Loop over parms saved in BOIVerify system
      #
      VerParms=self.VU.getVerParmsObs()
      numSavedAllParms=0
      for parm in VerParms:
         #
         #  make sure this parm is available for this model
         #
         exprName = self.getExprName(modelname, parm, "SFC")
         parmStuff = self._dbss.getParmManager().getParmInExpr(exprName, 1)
         if parmStuff is None:
            continue
         del parmStuff
         parmInfo=self.getParm(dbid,parm,"SFC")
         if parmInfo is None:
            continue
         del parmInfo
         #
         #  Save all data for Obs models, force basetime to be the
         #  same as start time
         #
         saveInfo=self.saveAllGrids(dbid,parm,baseTimeForce=-1)
         #
         #  Log/Track saved grids
         #
         (numSaved,numSkipped,numErrs)=saveInfo
         if numErrs>0:
            if modelused==0:
               self.VU.logMsg("Saving %s"%modelLogMessage)
               modelused=1
            if numSkipped>0:
               self.VU.logMsg("  %s: Saved %d new grids, skipped %d already saved"%(parm,numSaved,numSkipped))
            else:
               self.VU.logMsg("  %s: Saved %d new grids"%(parm,numSaved))
            self.VU.logMsg("  %d had errors when saving"%numErrs)
            numSavedAllParms+=numSaved
         elif numSaved>0:
            if modelused==0:
               self.VU.logMsg("Saving %s"%modelLogMessage)
               modelused=1
            if numSkipped>0:
               self.VU.logMsg("  %s: Saved %d new grids, skipped %d already saved"%(parm,numSaved,numSkipped))
            else:
               self.VU.logMsg("  %s: Saved %d new grids"%(parm,numSaved))
            numSavedAllParms+=numSaved
         else:
            self.VU.logMsg("  No new %s grids to save"%parm)
         #
         #  end loop over parms
         #
      if numSavedAllParms==0:
         self.VU.logMsg("No new %s grids to archive"%modelname)
      return
   #==================================================================
   #
   #  saveFcstModel(modelname) - save grids for a forecast model
   #
   def saveFcstModel(self,modelname):
      totalsaved=0
      #
      #  SAVE_MAXVERSIONS configuration should be the maximum number of
      #  old versions to save.  If not specified, default to 10
      #
      MAXVERSIONS=self.VU.getCFG('SAVE_MAXVERSIONS')
      if MAXVERSIONS is None:
         MAXVERSIONS=10
      #
      #
      #
      oldest=1
      for version in range(-MAXVERSIONS,1):
         self.VU.logMsg("saveFcstModel for %s %d"%(modelname,version),10)
         dbid=self.findDatabase(modelname,version)

         if not dbid.isValid():
            del dbid
            continue
         #
         #  Don't operate on the oldest version - since it might be in the
         #  process of being removed.  However, if the oldest version is
         #  also the latest version - then go ahead and work on it.
         #
         if oldest==1:
            oldest=0
            if version!=0:
               self.VU.logMsg("Skipping oldest version (%d) of %s"%(version,modelname),5)
               continue
         oldest=0
         #
         #  setup log messages for this run of model
         #
         modeltime=dbid.modelTime().unixTime()
         modeltt=time.gmtime(modeltime)
         itime=self.gstring(modeltt)
         modelLogMessage="%s run of %s model"%(itime,modelname)
         self.VU.logMsg("Checking %s"%modelLogMessage,1)
         modelused=0
         #
         #  Loop over parms saved in BOIVerify system
         #
         VerParms=self.VU.getVerParms()
         numSavedAllParms=0
         for parm in VerParms:
            #
            #  make sure this parm is available for this model
            #
            exprName = self.getExprName(modelname, parm, "SFC")
            parmStuff = self._dbss.getParmManager().getParmInExpr(exprName, 1)
            if parmStuff is None:
               continue
            del parmStuff
            parmInfo=self.getParm(dbid,parm,"SFC")
            if parmInfo is None:
               continue
            del parmInfo
            #
            #  if save interval is zero - we are saving all grids
            #  if non-zero - we are saving snapshots at particular times
            #
            sint=self.VU.getVerSaveInterval(parm)            
            if sint==0:
               saveInfo=self.saveAllGrids(dbid,parm)
            else:
               saveInfo=self.saveSnapshots(dbid,parm,sint)
            #
            #
            #
            (numSaved,numSkipped,numErrs)=saveInfo
            if numErrs>0:
               if modelused==0:
                  self.VU.logMsg("Saving %s"%modelLogMessage)
                  modelused=1
               if numSkipped>0:
                  self.VU.logMsg("  %s: Saved %d new grids, skipped %d already saved"%(parm,numSaved,numSkipped))
               else:
                  self.VU.logMsg("  %s: Saved %d new grids"%(parm,numSaved))
               self.VU.logMsg("  %d had errors when saving"%numErrs)
               numSavedAllParms+=numSaved
            elif numSaved>0:
               if modelused==0:
                  self.VU.logMsg("Saving data for %s"%modelLogMessage)
                  modelused=1
               if numSkipped>0:
                  self.VU.logMsg("  %s: Saved %d new grids, skipped %d already saved"%(parm,numSaved,numSkipped))
               else:
                  self.VU.logMsg("  %s: Saved %d new grids"%(parm,numSaved))
               numSavedAllParms+=numSaved
            else:
               self.VU.logMsg("  No new %s grids to save"%parm,1)
            #
            #  end loop over parms
            #
         del dbid
         totalsaved+=numSavedAllParms
         #
         #  end loop over versions
         #
      if totalsaved==0:
         self.VU.logMsg("No new %s grids to archive"%modelname)
      return
   #==================================================================
   #
   #  saveAllGrids - save all grids for the specified dbid and parm.
   #
   #                 if exceptBefore>0 then do not save any grids
   #                 that end before that time.
   #
   #                 if baseTimeForce==0 (the default) then the
   #                 basetime for the saved grid is set to the model
   #                 run basetime
   #                 if baseTimeForce>0 then the basetime is set to
   #                 this value (useful for Official and ISC where
   #                 we set the basetime based on computer time).
   #                 if baseTimeForce<0 then the basetime is set to
   #                 the same time as the start time (useful for
   #                 observed databases)
   #
   #                 Returns (number saved, number skipped, number of
   #                 errors)
   #
   def saveAllGrids(self,dbid,parm,isc=0,exceptBefore=0,baseTimeForce=0):
      #
      MAXHOURS=self.VU.getCFG('MAXFORECASTHOUR')
      if MAXHOURS is None:
         MAXHOURS=240
      #
      #  Figure out the timerange to save grids over.
      #
      #  For singleton databases, if baseTimeForce<0, then look
      #     over MAXHOURS in the past.  Otherwise look MAXHOURS
      #     from the specified baseTimeForce.
      #  For non-singleton databases, look over MAXHOURS from
      #     the basetime
      #
      if dbid.toJavaObj().getModelTime() == "00000000_0000":
         Btime = 0
      else:
         Btime=dbid.modelTime().unixTime()

      if Btime==0:
         (gyea,gmon,gday,ghou,gmin,gsec,gwdy,gydy,gdst)=time.gmtime()
         if baseTimeForce<0:
            fulltr=self.createTimeRange(-MAXHOURS,ghou,"Zulu")
         else:
            Btime=baseTimeForce
            today0z=calendar.timegm((gyea,gmon,gday,0,0,0,0,0,0))
            offsetHours=int(((baseTimeForce-today0z)/(60*60))+0.5)
            fulltr=self.createTimeRange(offsetHours,offsetHours+MAXHOURS,"Zulu")
      else:
         fulltr=self.createTimeRange(0,MAXHOURS,"Database",dbid)
      if fulltr is None:
         return(0,0,0)
      #
      #  Get list of all grids in the timerange
      #
      if isc==0:
         infoList=self.getGridInfo(dbid,parm,"SFC",fulltr)
      else:
         infoList=self.getGridInfo("Official",parm,"SFC",fulltr)
      if infoList is None:
         return(0,0,0)
      if len(infoList)<1:
         return(0,0,0)
      #
      #  get datatype, modelname, model basetime (Btime)
      #
      datatype=self.VU.getVerParmType(parm)
      if isc==0:
         modelname=dbid.modelName()
      else:
         modelname="ISC"
      #
      #  Loop over potential grids to save
      #
      saved=0
      skipped=0
      errs=0
      for info in infoList:
         gridTR=info.gridTime()
         #
         stime=self.tstring(gridTR.startTime())
         etime=self.tstring(gridTR.endTime())
         Stime=gridTR.startTime().unixTime()
         Etime=gridTR.endTime().unixTime()
         (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(Stime)
         (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(Etime)
         self.VU.logMsg("  %s grid for %4.4d/%2.2d/%2.2d %2.2dZ -- %4.4d/%2.2d/%2.2d %2.2dZ"%(parm,
                        syea,smon,sday,shou,eyea,emon,eday,ehou),1)
         #
         #  Btime is normally set to the model basetime - but
         #  special cases are handled here.
         #
         if baseTimeForce>0:
            Btime=baseTimeForce
         elif baseTimeForce<0:
            Btime=Stime
         #
         #  if exceptBefore is non-zero, skip any grids that the middle
         #  of their time is before the exceptBefore time
         #
         if exceptBefore>0:
            if Etime<=exceptBefore:
               if self.VU.getDebug()>=1:
                  t2=self.gstring1(exceptBefore)
                  self.VU.logMsg("    ends before %s - so skipped"%t2,1)
               continue
         #
         #  get History of last time this grid was modified
         #
         username = ""
         updtime = None
         historyList=self.getGridHistory(dbid,parm,"SFC",gridTR)
         for hists in historyList:
            for hist in hists:
               if hist[4] not in [None, "None"]:
                  username=hist[4].split(':')[1]

               if hist[5] is not None:
                  updtime=hist[5].unixTime()

               if self.VU.getDebug()>=1:
                  t2=self.gstring1(time.gmtime(updtime))
                  self.VU.logMsg("    grid last updated at %s"%t2,1)
         #
         #  Get time this grid was last saved (if ever)
         #
         savedTime=self.VU.lastSaved(parm,modelname,Btime,Stime,Etime)
         if savedTime>0:
            self.VU.logMsg("    grid last archived at %s"%(self.gstring1(time.gmtime(savedTime))),1)
         else:
            self.VU.logMsg("    grid not archived yet",1)
         #
         #  If this grid hasn't been updated since it was last saved
         #  then we can skip saving it again
         #
         if savedTime > 0 and updtime<=savedTime:
            skipped+=1
            self.VU.logMsg("    skipped",1)
            continue
         #
         #  We need to save this grid! So read it in.
         #
         self.VU.logMsg("      need to save grid",10)
         valgrid=self.extractGrids(dbid,parm,"SFC",gridTR,modelname,datatype)
         if valgrid is None:
            errs+=1
            self.VU.logMsg("      could not read grid",1)
            continue
         #
         #  Save the grid
         #
         ret1=self.VU.writeVals(parm,modelname,username,Btime,
                                 Stime,Etime,valgrid)
         #ret1=1
         if ret1==0:
            errs+=1
            self.VU.logMsg("      could not save grid",1)
         elif ret1==-1:
            skipped+=1
            self.VU.logMsg("      saved grid is identical",1)
         else:
            saved+=1
            self.VU.logMsg("    saved!",1)
         #
         #  End of loop over all grids for this parm
         #
      return(saved,skipped,errs)
   #==================================================================
   #
   #  saveSnapshots - save hourly grids at sint hour intervals for the
   #                 specified dbid and parm.
   #
   #                 if exceptBefore>0 then do not save any grids
   #                 that end before that time.
   #
   #                 if baseTimeForce==0 (the default) then the
   #                 basetime for the saved grid is set to the model
   #                 run basetime
   #                 if baseTimeForce>0 then the basetime is set to
   #                 this value (useful for Official and ISC where
   #                 we set the basetime based on computer time).
   #                 if baseTimeForce<0 then the basetime is set to
   #                 the same time as the start time (useful for
   #                 observed databases)
   #
   #                 Returns (number saved, number skipped, number of
   #                 errors)
   #
   def saveSnapshots(self,dbid,parm,sint,isc=0,exceptBefore=0,baseTimeForce=0):
      #
      #
      #
      MAXHOURS=self.VU.getCFG('MAXFORECASTHOUR')
      if MAXHOURS is None:
         MAXHOURS=240
      #
      #  get datatype, modelname, model basetime (Btime)
      #
      datatype=self.VU.getVerParmType(parm)
      Btime=dbid.modelTime().unixTime()
      if isc==0:
         modelname=dbid.modelName()
      else:
         modelname="ISC"
      #
      #  Loop over time periods
      #
      saved=0
      skipped=0
      errs=0
      for shour in range(0,MAXHOURS+1,sint):
         gridTR=self.createTimeRange(shour,shour+1,"Database",dbid)
         if gridTR is None:
            continue
         stime=self.tstring(gridTR.startTime())
         etime=self.tstring(gridTR.endTime())
         Stime=gridTR.startTime().unixTime()
         Etime=gridTR.endTime().unixTime()
         #
         #  if exceptBefore is non-zero, skip any grids that the middle
         #  of their time is before the exceptBefore time
         #
         if exceptBefore>0:
            if Etime<=exceptBefore:
               if self.VU.getDebug()>=1:
                  t2=self.gstring1(exceptBefore)
                  self.VU.logMsg("    grid ends before %s - so skipped"%t2,1)
               continue
         #
         #  get History of last time this grid was modified
         #
         historyList=self.getGridHistory(dbid,parm,"SFC",gridTR)
         if len(historyList)<1:
            continue
         for hists in historyList:
            for hist in hists:
               if hist[4] not in [None, "None"]:
                  username=hist[4].split(':')[1]
               else:
                  username=""

               if hist[5] is not None:
                  updtime=hist[5].unixTime()
               else:
                  updtime = None

               if self.VU.getDebug()>=1:
                  (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(Stime)
                  (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(Etime)
                  self.VU.logMsg("  %s %4.4d/%2.2d/%2.2d %2.2dZ -- %4.4d/%2.2d/%2.2d %2.2dZ"%(parm,
                                 syea,smon,sday,shou,eyea,emon,eday,ehou),1)
                  t2=self.gstring1(time.gmtime(updtime))
                  self.VU.logMsg("    grid last updated at %s"%t2,1)
         #
         #  Get time this grid was last saved (if ever)
         #
         savedTime=self.VU.lastSaved(parm,modelname,Btime,Stime,Etime)
         if savedTime>0:
            self.VU.logMsg("    last archived at %s"%(self.gstring1(time.gmtime(savedTime))),1)
         else:
            self.VU.logMsg("    not archived yet",1)
         #
         #  If this grid hasn't been updated since it was last saved
         #  then we can skip saving it again
         #
         if savedTime > 0 and updtime<=savedTime:
            skipped+=1
            self.VU.logMsg("    skipped",1)
            continue
         #
         #  We need to save this grid - read it in!
         #
         self.VU.logMsg("      trying to save",10)
         valgrid=self.extractGrids(dbid,parm,"SFC",gridTR,modelname,datatype)
         if valgrid is None:
            errs+=1
            self.VU.logMsg("      could not read grid",1)
            continue
         #
         #  Btime is normally set to the model basetime - but
         #  special cases are handled here.
         #
         if baseTimeForce>0:
            Btime=baseTimeForce
         elif baseTimeForce<0:
            Btime=Stime
         #
         #  Save the grid
         #
         ret1=self.VU.writeVals(parm,modelname,username,Btime,
                                 Stime,Etime,valgrid)
         #ret1=1
         if ret1==0:
            errs+=1
            self.VU.logMsg("      could not save grid",1)
         elif ret1==-1:
            skipped+=1
            self.VU.logMsg("      saved grid is identical",1)
         else:
            saved+=1
            self.VU.logMsg("    saved!",1)
         #
         #  End of loop over all grids for this parm
         #
      return(saved,skipped,errs)
   #==================================================================
   #
   #  extractGrids - normally just read the grid, but if the modelname
   #                 is ISC and the timeperiod is more than one hour,
   #                 then have to do a LOT more difficult processing to
   #                 see if ISC data exists, read all the ISC grids, etc.
   #
   def extractGrids(self,dbid,parm,level,gridTR,modelname,datatype):
      #
      #  A non-ISC grid read is really easy - just read and return
      #
      if (modelname!="ISC"):
         valgrid=self.getGrids(dbid,parm,level,gridTR,noDataError=0)
         return valgrid
      #
      #  If an hour long ISC grid - also easy - just read from
      #  ISC and return
      #
      stime=gridTR.startTime().unixTime()
      etime=gridTR.endTime().unixTime()
      dur=int(((etime-stime)/(60*60))+0.5)
      if dur<2:
         valgrid=self.getGrids("ISC",parm,level,gridTR,noDataError=0)
         return valgrid
      #
      #  This is a long ISC grid!
      #  Need to read our grid, and construct ISC total/average/max
      #  outside our area.
      #
      #  read the Official grid first - and return right away
      #  if we cant read that
      #
      valgrid=self.getGrids("Official",parm,level,gridTR,noDataError=0)
      if valgrid is None:
         return valgrid
      #
      #  Use getBetterComposite instead of getComposite
      #     from SmartScript.  This is because getComposite
      #     does not seem to work correctly when it is run
      #     with 'runProcedure' - and this procedure will
      #     almost always be run from runProcedure.
      #
      iscOutput=self.getBetterComposite(parm,gridTR)
      if iscOutput is None:
         return valgrid
      if datatype!=1:
         (mask,iscGrid)=iscOutput
         valgrid=where(mask,iscGrid,valgrid)
      else:
         (mask,iscMag,iscDirec)=iscOutput
         (mag,direc)=valgrid
         mag=where(mask,iscMag,mag)
         direc=where(mask,iscDirec,direc)
         valgrid=(mag,direc)
      return valgrid
   #==================================================================
   #
   #  getSaveModels - get list of models to save.  First try reading
   #                  the model name from /tmp/<siteId>_$FILE.  If not there -
   #                  get list of all models in the
   #                  BOIVerify system except Official and ISC
   #
   def getSaveModels(self):
      modelList=[]
      modelname=""
      #
      #  Read the model from the FILE file
      #
      filename="/tmp/%s_%s"% (self.getSiteID(), FILE)
      if ((os.path.exists(filename))and(os.path.isfile(filename))):
         try:
            infile=file(filename,"r")
            modelname=infile.readline().strip()
            infile.close()
            os.remove(filename)
         except:
            self.VU.logMsg("Could not read model from %s"%filename,1)
            self.VU.logMsg("So will save all models",1)
      #
      #  If no modelname was read - get all model names from
      #  SAVE_MODELS configuration...and add in any OBSMODELS
      #  ...but never save Official or ISC in this fashion
      #
      if modelname=="":
          modelList=self.VU.getCFG('SAVE_MODELS')
          if modelList is None:
             modelList=[]
          obsModels=self.VU.getCFG('OBSMODELS')
          if obsModels is None:
             obsModels=[]
          for model in obsModels:
             if model not in modelList:
                modelList.append(model)
          if "Official" in modelList:
             modelList.remove("Official")
          if "ISC" in modelList:
             modelList.remove("ISC")
          modelList.sort()
      else:
          modelList=[modelname,]
      return modelList
   #=================================================================
   #  tstring - given a GFE time object, return a timeString like
   #            YYYYMMDDHH
   #
   def tstring(self, tr):
      return tr.strftime("%Y%m%d%H")
      year=tr.year()
      month=tr.month()
      day=tr.day()
      hour=tr.hour()
      return "%4.4d%2.2d%2.2d%2.2d"%(year,month,day,hour)
   #=================================================================
   #  gstring - given a python time tuple, return a timeString in
   #            YYYYMMDDHH format.
   #
   def gstring(self, timetuple):
      year=timetuple[0]
      month=timetuple[1]
      day=timetuple[2]
      hour=timetuple[3]
      return "%4.4d%2.2d%2.2d%2.2d"%(year,month,day,hour)
   #==================================================================
   #
   #  gstring1 - take a unix time tuple and turn it into a
   #             YYYY/MM/DD HH:MM string
   #
   def gstring1(self,timetuple):
      if type(timetuple) is int:
         timetuple = time.gmtime(timetuple)
      year=timetuple[0]
      month=timetuple[1]
      day=timetuple[2]
      hour=timetuple[3]
      minute=timetuple[4]
      return "%4.4d/%2.2d/%2.2d %2.2d:%2.2d"%(year,month,day,hour,minute)
   #========================================================================
   #
   #  Essentially the same as the SmartScript getComposite routine
   #  but correctly handles multiple ISC grids within the timeRange
   #  of the grid you want.  Can return None if no ISC or specified
   #  grids lie within the TimeRange specified.
   #
   #  2005-01-24 - Changed again because accumulative parms return
   #               different values from getComposite after IFPS16.
   #
   def getBetterComposite(self,parmName, timeRange):
      from com.raytheon.uf.common.dataplugin.gfe.db.objects import GFERecord_GridType as GridType
      #
      #  Get the type, rateParm flag, and limits
      #  for the parameter name passed in.
      #
      mutableID=self.mutableID()
      baseGrid=self.getGrids(mutableID,parmName,"SFC",timeRange,noDataError=0)
      gridInfoList=self.getGridInfo(mutableID,parmName,"SFC",timeRange)
      if (len(gridInfoList)<1):
         return None
      for gridInfo in gridInfoList:
         wxType=gridInfo.type()
         rateParm=gridInfo.rateParm()
         minlimit=gridInfo.minLimit()
         maxlimit=gridInfo.maxLimit()
      #
      #  Get list of all ISC time-blocks that fit in the
      #  timerange of the specified GridTimeRange grid
      #
      iscInfos=self.getGridInfo("ISC",parmName,"SFC",timeRange)
      if (len(iscInfos)<1):
         return None
      alltrs=[]
      for info in iscInfos:
         tr=info.gridTime()
         alltrs.append(tr)
      #
      #  setup sum/counter for average
      #
      if parmName in ("MaxT","MaxRH","PoP"):
         sum=self._empty-150.0
      elif parmName in ("MinT","MinRH"):
         sum=self._empty+150.0
      else:
         sum=self._empty
         if GridType.VECTOR.equals(wxType):
            sumv=self._empty
      cnt = zeros_like(self._empty)
      all = ones_like(self._empty)
      ptcount=add.reduce(add.reduce(all))
      #
      #  foreach time range...get the ISC composite for
      #  that hour
      #
      for tr in alltrs:
         comp=self.getComposite(parmName,tr)
         if comp[0].shape!=sum.shape:
            continue
         #
         #  Add to sums, or min/max
         #
         if GridType.SCALAR.equals(wxType):  # SCALAR
            bits,isc=comp
            #isc=self.getGrids("ISC",parmName,"SFC",tr)
            #
            #  when sites don't give you ISC data - it is returned by
            #  getComposite as the minimum value for this parm.  For
            #  most parms we look for these minimum values and exclude
            #  them from the max/min/average calculations.  For some we
            #  have to keep them - since a 0 PoP or 0.00 QPF, or 0 wind
            #  speed is perfectly valid (but a 0 RH is suspicious - so we
            #  DO remove those)
            #
            if parmName in ("MaxT","MinT","MaxRH","MinRH","TdMrn","TdAft","T","RH","Td"):
               possbits=less(isc,minlimit+0.001)
               bits=logical_xor(bits,possbits)
            if parmName in ("MaxT","MaxRH","PoP"):
               sum=where(bits,maximum(isc,sum),sum)
               cnt[bits] = 1
            elif parmName in ("MinT","MinRH"):
               sum=where(bits,minimum(isc,sum),sum)
               cnt[bits] = 1
            else:
               sum=where(bits,sum+isc,sum)
               cnt[bits] += 1
         if GridType.VECTOR.equals(wxType):  # VECTOR
            bits,mag,direc=comp
            #(mag,dir)=self.getGrids("ISC",parmName,"SFC",tr)
            (u,v)=self.MagDirToUV(mag,direc)
            sum=where(bits,sum+u,sum)
            sumv=where(bits,sumv+v,sumv)
            cnt[bits] += 1
         if GridType.WEATHER.equals(wxType):  # WEATHER
            bits,keys,strings=comp
            #(keys,strings)=self.getGrids("ISC",parmName,"SFC",tr)
      #
      #  now calculate average/max/min, etc.
      #  (count is always 1 for max/min)
      #
      noISC=less(cnt,0.5)
      bits=greater(cnt,0.5)
      if GridType.SCALAR.equals(wxType) or GridType.VECTOR.equals(wxType):
         cnt[less(cnt,1)] = 1
         if GridType.VECTOR.equals(wxType):
            sum=where(noISC,minlimit,sum/cnt)
            sumv=where(noISC,minlimit,sumv/cnt)
            (mag,direc)=self.UVToMagDir(sum,sumv)
            (baseMag,baseDir)=baseGrid
            mag=where(noISC,baseMag,mag)
            direc=where(noISC,baseDir,direc)
            return bits,mag,direc
         else:
            sum=where(noISC,baseGrid,sum/cnt)
            return bits,sum
      else:
         return bits,keys,strings
