
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerifyAutoCalc - Version 2.0.5
#
#    Calculate verification stats for the specified parms for all models
#    and all edit areas listed in the pre-defined edit area list:
#
# Author: Tim Barker - SOO BOI
#   2005/07/01 - version 0.0 - original implementation
#   2005/07/29 - version 0.1 - update to grid database structure
#   2006/11/06 - version 1.0 - Make only one procedure!  And have parms
#                    specified via the file name FILE (constant below). If
#                    the editable grid is not there - create it temporarily
#                    while the tool runs.  It runs fast enough to be OK - but
#                    still very slow.
#   2007/10/25 - version 2.0 - Turned into a procedure rather than a tool.
#                    Lots of work to make it faster.  Still slower than I
#                    would like.
#   2008/05/28 - version 2.0.5 - Removed accidental inclusion in populate
#                    menu.  Made it not run for periods that havent
#                    quite finished yet - or have finished only recently.
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
#
#=============================================================================
#
#  Do not show this in any menu.  Should only be run via runProcedure after
#  putting the parms to save in /tmp/<siteId>_FILE (one on each line)
#
#MenuItems = ["Verify"]
#
MenuItems = ["None"]
#
#  imports
#
from numpy import *
import os,os.path,time,calendar
import SmartScript,BOIVerifyUtility
#
#  CONSTANTS
#
HOURS=60*60
FILE="BOIVerifyAutoCalc.txt"
#
class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      SmartScript.SmartScript.__init__(self, dbss)
      self._dbss=dbss
   #
   #
   #
   def execute(self, editArea):
      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.VU.logMsg("BOIVerifyAutoCalc Procedure Start")

      self._empty = self.VU._empty
      #
      #  When testing - I often set Debug up higher to see what is going on
      #
      #self.VU.setDebug(0)
      #
      #  AUTOCALC_DAYS configuration should hold the number of DAYSBACK
      #  to scan for new observation data.  If not specified - default
      #  to 5 days
      #
      DAYSBACK=self.VU.getCFG('AUTOCALC_DAYS')
      if DAYSBACK is None:
         DAYSBACK=5
      #
      #  AUTOCALC_TOO_RECENT configuration is number of hours that
      #  indicates an observation grid is too recent to make calculations
      #  of it.  Thus figuring that it might get revised later - and we
      #  don't want to take the time to calculate these scores again -
      #  so don't do it yet.
      #
      TOO_RECENT=self.VU.getCFG('AUTOCALC_TOO_RECENT')
      if TOO_RECENT is None:
         TOO_RECENT=12
      #
      #  Read the parms from the FILE file
      #
      parmlist=[]
      filename="/tmp/%s_%s"% (self.getSiteID(), FILE)
      if ((os.path.exists(filename)) and (os.path.isfile(filename))):
         try:
            infile=file(filename,"r")
            parmlines=infile.readlines()
            infile.close()
            os.remove(filename)
            for line in parmlines:
                parm=line.strip()
                if len(parm)>0:
                    parmlist.append(parm)
         except:
            self.VU.logMsg("Difficulty reading %s"%filename)
      #
      #  If no parms in parmlist - run for all parms
      #
      if len(parmlist)==0:
         parmlist=self.VU.listParms()
      #
      #
      #
      now=time.time()
      (nyea,nmon,nday,nhou,nmin,nsec,nwda,nyda,ndst)=time.gmtime(time.time())
      enddate=calendar.timegm((nyea,nmon,nday,0,0,0,0,0,0))
      #
      #  parms and models obtained from data archive
      #  editarea list from configuration
      #
      allParms=self.VU.listParms()
      parms=parmlist
      models=self.VU.listModels()
      #
      editAreaNames=self.VU.listEditAreas()
      maxareas=self.VU.CFG['STATAREAS']
      maxstats=self.VU.CFG['STATTYPES']
      #
      #  Setup big arrays where data to be stored will be written
      #
      sums=zeros((maxareas,maxstats),float32)
      sumsDir=zeros((maxareas,maxstats),float32)
      sumsMag=zeros((maxareas,maxstats),float32)
      #
      #  Setup the edit areas
      #
      #  pts and eas are only as big as the number of edit
      #              areas actually being calculated.  Then we
      #              use numedit to put it in the right slot of
      #              sums.
      #
      numAreas=len(editAreaNames)
      shape=self._empty.shape
      allpts=shape[0] * shape[1]
      eas=zeros((allpts,numAreas))
      numedit=[]
      pts=[]
      for i in xrange(numAreas):
         areaname=editAreaNames[i]
         if areaname=="NONE":
            ea=(self._empty+1).astype(int32)
         else:
            ea=self.encodeEditArea(areaname)
         eas[:,i]=ea.flat
         pt=int(add.reduce(add.reduce(ea)))
         if pt<1:
            pt=1
         pts.append(pt)
         numedit.append(self.VU.getEditAreaNumberFromName(areaname))
      self.VU.logMsg("Using %d edit areas out of %d"%(numAreas,maxareas),1)
      #
      #  Loop over all ObsModels
      #
      obsModels=self.VU.getCFG('OBSMODELS')
      for obsModel in obsModels:
         self.VU.logMsg("Working on %s observations"%obsModel,0)
         #
         #  Loop over parms
         #
         for parm in parms:
            if parm not in allParms:
               continue
            self.VU.logMsg("  %s Grids"%parm,0)
            datatype=self.VU.getVerParmType(parm)
            if datatype is None:
               continue
            if datatype==1:
               statMagSums=zeros((maxstats,allpts),float)
               statDirSums=zeros((maxstats,allpts),float)
            else:
               statSums=zeros((maxstats,allpts),float)
            numthresh=self.VU.getCFG('NUMTHRESH')
            thresholds=self.VU.getVerThresholds(parm)
            if datatype==1:
               magThresholds=thresholds[0]
               dirThresholds=thresholds[1]
            obsParm=self.VU.getObsParm(parm)
            obsReadMode=self.VU.getReadMode(obsModel,obsParm)
            #
            #  Loop over ver models
            #
            for model in models:
               if model in self.VU.getCFG('OBSMODELS'):
                  continue
               self.VU.logMsg("    %s model"%model,0)
               fcstReadMode=self.VU.getReadMode(model,parm)
               #
               #  Get the cases for obs over past DAYSBACK days
               #
               caseInfo=self.VU.getCommonCases(parm,model,obsParm,obsModel,
                           "Verifying on","Period Length",fromDay=enddate,
                           numDays=DAYSBACK, dayList=[], fhrStart=-24,
                           fhrEnd=self.VU.MAXFORECASTHOUR,fcstrs=-1,
                           cycles=-1)
               cases=caseInfo[model]
               casekeys=cases.keys()
               if len(casekeys)<1:
                  continue
               #
               #  sort keys by the starting time...and put in reverse order
               #  so that we work backward
               #
               casekeys.sort() # first sort by basetime
               casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
               casekeys.reverse()
               lastobs=""
               for key in casekeys:
                  pt1=time.time()
                  (basetimestr,stimestr,etimestr)=key.split(",")
                  basetime=int(basetimestr)
                  btup=time.gmtime(basetime)
                  cycle=btup[3]
                  stime=int(stimestr)
                  etime=int(etimestr)
                  etup=time.gmtime(etime)
                  (frecList,orecList)=cases[key]
                  if len(orecList)<1:
                     continue
                  fhour=(stime-basetime)/HOURS
                  if fhour<0:
                     self.VU.logMsg("%13s %8s %3d-hr Fcst ending %4.4d/%2.2d/%2.2d %2.2dZ -- skipped - no negative forecast hours"%(parm,
                                    model,fhour,etup[0],etup[1],etup[2],etup[3]),2)
                     continue
                  #
                  #  Only read the observed grid once
                  #
                  obskey=key.split(",",1)[1]
                  if obskey!=lastobs:
                     obsgrid=self.VU.getVerGrids(obsModel,stime,obsParm,stime,etime,
                                                 obsReadMode,orecList)
                     #if verType==1:
                     #   obsdata=self.getProbVerGrid(readParm,obsdata)
                     #
                     #  If this observation only saved recently...dont
                     #  calculate yet - because it may change
                     #
                     ovtime=0
                     for rec in orecList:
                        ovtime=max(self.VU.oncVtime[rec],ovtime)
                     ovhours=float(now-ovtime)/float(HOURS)
                     if ovhours<TOO_RECENT:
                        self.VU.logMsg("%13s %8s %3d-hr Fcst ending %4.4d/%2.2d/%2.2d %2.2dZ -- skipped - observation too recent"%(parm,
                                    model,fhour,etup[0],etup[1],etup[2],etup[3]),2)
                        continue
                     #
                     #  Calculate the sums that only depend on the observed
                     #  grid once
                     #
                     if datatype==1:
                        (obsMag,obsDir)=obsgrid
                        statMagSums[5]=ravel(obsMag)
                        statMagSums[6]=statMagSums[5]*statMagSums[5]
                        statDirSums[5]=ravel(obsDir)
                        statDirSums[6]=statDirSums[5]*statDirSums[5]
                     else:
                        statSums[5]=ravel(obsgrid)
                        statSums[6]=statSums[5]*statSums[5]
                     lastobs=obskey        
                  #
                  #  Get last time forecast record was updated
                  #
                  fvtime=0
                  for rec in frecList:
                     fvtime=max(self.VU.fncVtime[rec],fvtime)
                  #
                  #  If period ends less than TOO_RECENT hours before now...dont do
                  #  calculations yet
                  #
                  ehours=float(now-etime)/float(HOURS)
                  if ehours<TOO_RECENT:
                     self.VU.logMsg("%13s %8s %3d-hr Fcst ending %4.4d/%2.2d/%2.2d %2.2dZ -- skipped - forcast too recent"%(parm,
                                    model,fhour,etup[0],etup[1],etup[2],etup[3]),2)
                     continue
                  #
                  #  If these stats already exist - see if the
                  #  forecast/observed grids are newer than the
                  #  last time the stats were calculated - if not
                  #  then stats do not need to be re-calculated
                  #
                  if datatype!=1:
                     doAgain=self.VU.getDoAgain(parm,model,obsModel,
                                  basetime,stime,etime,ovtime,fvtime)
                  else:
                     doAgain1=self.VU.getDoAgain("%sDir"%parm,model,
                                  obsModel,basetime,stime,etime,ovtime,
                                  fvtime)
                     doAgain2=self.VU.getDoAgain("%sSpd"%parm,model,
                                  obsModel,basetime,stime,etime,ovtime,
                                  fvtime)
                     doAgain=doAgain1 or doAgain2
                  if doAgain==0:
                     self.VU.logMsg("%13s %8s %3d-hr Fcst ending %4.4d/%2.2d/%2.2d %2.2dZ -- already calculated"%(parm,
                                    model,fhour,etup[0],etup[1],etup[2],etup[3]),1)
                     continue
                  #
                  #  Log the parm,model,time being calculated
                  #
                  self.VU.logMsg("%13s %8s %3d-hr Fcst ending %4.4d/%2.2d/%2.2d %2.2dZ (mem:%d resident:%d)"%(parm,
                                 model,fhour,etup[0],etup[1],etup[2],etup[3],memory(),resident()),0)
                  #
                  #  Read forecast grid and calculate error grid
                  #
                  fcstgrid=self.VU.getVerGrids(model,basetime,parm,stime,etime,
                                                 fcstReadMode,frecList)
                  if fcstgrid is None:
                     self.VU.logMsg("could not read grid",1)
                     continue
                  #fcstgrid=self.VU.readRecord(parm,model,fcstrec)
                  pt2=time.time()
                  #
                  #  If multiple records...it just takes the forecasters associated
                  #  with the last grid.  Don't know what else to do...
                  #
                  fcstrNums=self.VU.getRecFcstrs(frecList[-1])
                  #self.VU.logMsg("took %6.3f to read fcst record"%(pt2-pt1))
                  if datatype==1:
                     (fcstMag,fcstDir)=fcstgrid
                     errDirGrid=fcstDir-obsDir
                     errDirGrid=where(greater(errDirGrid,180.0),360.0-errDirGrid,errDirGrid)
                     errDirGrid=where(less(errDirGrid,-180.0),-(360.0+errDirGrid),errDirGrid)
                     statDirSums[0]=ravel(errDirGrid)
                     statDirSums[1]=statDirSums[0]*statDirSums[0]
                     statDirSums[2]=where(less(statDirSums[0],0.0),-statDirSums[0],statDirSums[0])
                     statDirSums[3]=ravel(fcstDir)
                     statDirSums[4]=statDirSums[3]*statDirSums[3]
                     statDirSums[7]=statDirSums[3]*statDirSums[5]
                     for k in xrange(numthresh):
                        statDirSums[8+k]=less(statDirSums[2],dirThresholds[k])
                     statMagSums[0]=ravel(fcstMag-obsMag)
                     statMagSums[1]=statMagSums[0]*statMagSums[0]
                     statMagSums[2]=where(less(statMagSums[0],0.0),-statMagSums[0],statMagSums[0])
                     statMagSums[3]=ravel(fcstMag)
                     statMagSums[4]=statMagSums[3]*statMagSums[3]
                     statMagSums[7]=statMagSums[3]*statMagSums[5]
                     for k in xrange(numthresh):
                        statMagSums[8+k]=less(statMagSums[2],magThresholds[k])
                     #
                     #  Setup place for sums
                     #
                     pt3=time.time()
                     for i in xrange(numAreas):
                        sumsDir[numedit[i]]=transpose(sum(compress(eas[:,i],statDirSums,axis=1),axis=1)/pts[i]).astype(float32)
                        sumsMag[numedit[i]]=transpose(sum(compress(eas[:,i],statMagSums,axis=1),axis=1)/pts[i]).astype(float32)
                     pt4=time.time()
                     self.VU.logMsg("took %6.3f to calc for editareas"%(pt4-pt3),10)
                     #
                     #  Save the sums
                     #
                     ret=self.VU.writeStats("%sDir"%parm,model,obsModel,
                                 fcstrNums,basetime,stime,etime,cycle,fhour,
                                 sumsDir)
                     ret=self.VU.writeStats("%sSpd"%parm,model,obsModel,
                                 fcstrNums,basetime,stime,etime,cycle,fhour,
                                 sumsMag)
                  else:
                     errgrid=fcstgrid-obsgrid
                     statSums[0]=ravel(errgrid)
                     statSums[1]=statSums[0]*statSums[0]
                     statSums[2]=where(less(statSums[0],0.0),-statSums[0],statSums[0])
                     statSums[3]=ravel(fcstgrid)
                     statSums[4]=statSums[3]*statSums[3]
                     statSums[7]=statSums[3]*statSums[5]
                     for k in xrange(numthresh):
                        statSums[8+k]=less(statSums[2],thresholds[k])
                     #
                     #  Setup place for sums
                     #
                     self.VU.logMsg("Now summing...",10)
                     pt3=time.time()
                     for i in xrange(numAreas):
                        sums[numedit[i]]=transpose(sum(compress(eas[:,i],statSums,axis=1),axis=1)/pts[i]).astype(float32)
                     pt4=time.time()
                     self.VU.logMsg("took %6.3f to calc for editareas"%(pt4-pt3),10)
                     #
                     #  Save the sums
                     #
                     ret=self.VU.writeStats(parm,model,obsModel,fcstrNums,basetime,stime,
                                            etime,cycle,fhour,sums)
                     pt5=time.time()
                     self.VU.logMsg("total time for grid: %6.3f"%(pt5-pt1),10)
               #
               #  When done with a model...delete its cases
               #
               del caseInfo
               del cases
            #
            #  When done with a parm...delete its internal storage space
            #
            if datatype==1:
               del statMagSums
               del statDirSums
            else:
               del statSums
      #
      #  Done
      #
      self.VU.logMsg("BOIVerifyAutoCalc Procedure Finished")
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
