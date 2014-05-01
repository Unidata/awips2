# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerify - version 2.0.5
#
#   Main tool to calculate and display verification information. The main
#   dialog contains tabs to display:
#      (1) the archived grids (or errors)
#      (2) grids of stats calculated for every gridpoint
#      (3) histograms of the errors over the current edit area
#      (4) line graphs of stats over pre-defined edit areas
#      (5) graphs of stats vs. scale
#
#   The BOIVerifySave tool saves the grid into the verification database
#   The BOIVerifyAutoCalc tool calculates and saves the stats for pre-defined
#       editareas.
#
# Author: Tim Barker - SOO BOI
#   2005/07/01 - Original Implmentation
#   2005/07/29 - version 0.1 - update to grid database structure
#   2006/11/06 - version 1.0 - First version with time-series graphs. Still
#                lots of bugs and not what I would like for a version 1.0 but
#                I've put it off way too long anyway.
#   2007/10/25 - version 2.0
#                . moved into a procedure rather than a tool
#                . fixed problem with precision="0" for sites that do
#                  not have WG1 defined
#                . fixed 'flashing' of user interface on startup
#                . fixed so that clicking on 'stop' during long drawing
#                  of many stat grids will stop more quickly.
#                . allowed program name in error messages to be something
#                  other than BOIVerify (could be GridVerify, etc.)
#                . use labels of 'histogram' and 'scatterplot' rather than
#                  errors and values
#                . use date label of 'ending on' rather than 'before'
#                . added limits to forecast hours shown
#                . added support for probabilistic parms
#                . added support for handling common cases
#   2008/05/28 - version 2.0.5
#                . fixed problem with forced large range of line graphs
#                  for QPF bias, etc.  In old code if graph range was less
#                  than 0.01, it was automatically forced to be 1.0. Now,
#                  it only forcibly expands the graph range when the actual
#                  range is less than 1/10 the precision of the parm, and
#                  even then it only expands the graph range upward by 2
#                  times the parm precision.
#
#
#   2010/04/23  ryu   Initial port to AWIPS II. Fixed bugs with the "Stat vs. Scale" tab.
#
# ----------------------------------------------------------------------------
#
MenuItems = ["Verify"]

from numpy import *
from math import *
import Tkinter
import TkDefaults
import AppDialog
import time,calendar,sys,re,traceback,string
import SmartScript
import BOIVerifyUtility
import os # for debug

from com.raytheon.viz.gfe import GFEPreference
from java.lang import Float

PROGNAME="BOIVerify" # you can change it if you dont like BOI.  Shame on you!
COLORLIST=["blue","green","red","cyan","yellow","purple","orange",
           "Gold","Coral","DarkOliveGreen","DarkOrchid","Brown","DeepPink",
           "DodgerBlue","DarkSeaGreen"]
HOURSECS=60*60
DAYSECS=24*HOURSECS


def getFloatPref(key, dflt):
    if not GFEPreference.contains(key):
       print "no config value for", key
       return dflt

    val = GFEPreference.getFloatPreference(key)
    print "preference for %s:" % key, val
    return val

def setFloatPref(key, value):
    if value is not None:
        value = Float.parseFloat(str(value))
    GFEPreference.setPreference(key, value)


class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      print "Start of %s - virtual memory:%d  resident: %d"%(PROGNAME,memory(),resident())
      self._dbss=dbss
      self.userName=self._dbss.getWsId().getUserName()
      SmartScript.SmartScript.__init__(self, dbss)
      self.statusBarMsg("Starting %s"%PROGNAME,"R")
      self.PROGNAME=PROGNAME
      self.HOURSECS=60*60
      self.DAYSECS=24*self.HOURSECS
      self.COLORLIST=COLORLIST
      self.__colorMapParams = {}
      return

   def execute(self):

      print "starting execute routine with memory:%d resident: %d"%(memory(),resident())
      #
      #  See if a child window of GFE is named "%self.PROGNAME Options"
      #  If so...then program is already running and just make
      #  a dialog box telling them that...
      #
      #if alreadyRunning:
      #   self.statusBarMsg("%s is already running"%self.PROGNAME,"U")
      #   return
      #

      try:
          self.__setup()

          self.root.withdraw()
          self.root.mainloop()
      #except:
      #    traceback.print_exc()
      finally:
          try:
              self.root.destroy()
          except:
              pass
          print "Exiting..."
      return

   def __setup(self):
      tk = Tkinter.Tk()
      self.root = tk
      sw = tk.winfo_screenwidth()
      sh = tk.winfo_screenheight()
      tk.geometry("%dx%d+0+0" % (sw,sh))

      TkDefaults.setDefaults(tk)
      #
      #  Splash screen...
      #
      splash=Tkinter.Toplevel(None)
      splash.overrideredirect(1)
      f=Tkinter.Frame(splash,relief=Tkinter.RIDGE,borderwidth=2,
                      background="yellow")
      txtwid=max(len(self.PROGNAME),10)
      text="Starting up\n%s"%self.PROGNAME
      lab=Tkinter.Label(f,justify=Tkinter.CENTER,text=text,
                        fg="black",bg="yellow",width=txtwid+2)
      lab.pack(side=Tkinter.TOP)
      wTxt=Tkinter.StringVar(f)
      lab=Tkinter.Label(f,justify=Tkinter.CENTER,textvariable=wTxt,
                        fg="black",bg="yellow")
      lab.pack(side=Tkinter.TOP)
      f.pack(side=Tkinter.TOP,ipadx=50,ipady=10)
      wTxt.set(".         ")
      splash.update_idletasks()
      ww=splash.winfo_reqwidth()
      wh=splash.winfo_reqheight()
      sw=splash.winfo_screenwidth()
      sh=splash.winfo_screenheight()
      newgeom="%dx%d+%d+%d"%(ww,wh,int(float(sw-ww)/2.0),int(float(sh-wh)/2.0))
      splash.geometry(newgeom)
      splash.wait_visibility()
      splash.update_idletasks()
      #
      #  Start up the utility
      #
      try:
         #
         #  Start up the utility
         #
         self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
         print "after setting up VU: memory:%d  resident:%d"%(memory(),resident())
         self.setToolType("numeric")

         self._empty = self.VU._empty
         #
         #  Setup scaleList.  This contains tuples of (numpts,label) where
         #  numpts is the +/- points to average over, and label is a label
         #  description of that area.
         #
         self.scaleList=[]
         spacing=self.VU.getGridSpacing()
         nominalSpacing=self.VU.getCFG('NOMINALSPACING')
         rspacing=int((float(spacing)/float(nominalSpacing))+0.5)*nominalSpacing
         maxk=max(self._empty.shape[0],self._empty.shape[1])
         for k in xrange(maxk):
            curTxt=wTxt.get()
            last=curTxt[-1]
            rest=curTxt[:-1]
            newTxt=last+rest
            wTxt.set(newTxt)
            splash.update_idletasks()
            if k>0:
               scale=k*2.0*rspacing
            else:
               scale=rspacing
            iscale=int(scale+0.5)
            if ((scale>50)and(scale<100)and(iscale%10!=0)):
               continue
            if ((scale>=100)and(scale<200)and(iscale%25!=0)):
               continue
            if ((scale>=200)and(scale<500)and(iscale%50!=0)):
               continue
            if ((scale>=500)and(iscale%100!=0)):
               continue            
            rdig=0
            for digits in xrange(2):
               mult=10**digits
               iscale=int(scale*mult)
               rscale=int((scale*mult)+0.5)
               if iscale==rscale:
                  rdig=digits
                  break
            rscale=round(scale,rdig)

            if rdig==0:
               lab="%d-km"%int(rscale)
            else:
               fmt="%%.%df-km"%rdig
               lab=fmt%rscale
            self.scaleList.append((k,lab))
         #
         #  Setup the self.pts with number of points in named edit areas
         #
         self.VU.logMsg("Starting points generation memory:%d  resident:%d"%(memory(),resident()))
         maxareas=self.VU.CFG['STATAREAS']
         editAreaNames=self.VU.listEditAreas()
         self.pts=ones(maxareas,)
         for i in xrange(len(editAreaNames)):
            #
            curTxt=wTxt.get()
            last=curTxt[-1]
            rest=curTxt[:-1]
            newTxt=last+rest
            wTxt.set(newTxt)
            splash.update_idletasks()
            #
            areaname=editAreaNames[i]
            self.VU.logMsg("  %3d memory:%d  resident:%d"%(i,memory(),resident()))
            npts=self.getPts(areaname)
            self.VU.logMsg("  %3d after npts memory:%d  resident:%d"%(i,memory(),resident()))
            j=self.VU.getEditAreaNumberFromName(areaname)
            self.pts[j]=npts
            self.VU.logMsg("  %3d after pts memory:%d  resident:%d"%(i,memory(),resident()))
            self.VU.logMsg("  %3d after del ea memory:%d  resident:%d"%(i,memory(),resident()))            
            if self.pts[j]<1:
               self.pts[j]=1
            self.VU.logMsg("  after edit area %3d memory:%d  resident:%d"%(i,memory(),resident()))
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      #  Create all the potential dialogs
      #
      #
      self.VU.logMsg("Starting dialog generation memory:%d  resident:%d"%(memory(),resident()))
      try:
         self.mini=MiniDiag(tk,callbackMethod=self.expandMini,
                           title="Change",buttonText="%s Options"%self.PROGNAME,loc="lr")
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      try:         
         self.cases=Cases(tk,callbackMethod=self.closeCases)
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      try:         
         self.miniCases=MiniDiag(tk, callbackMethod=self.expandCases,
                             title="Display",buttonText="Number of Cases",loc="ur")
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      try:
         self.cd=CanvasDisplay(tk, title="Canvas",callbackMethod=self.closeCD)
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      try:         
         self.dialog=Verif(self.VU,self.userName,self.scaleList, tk, callbackMethod=self.doVerif)
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      #  This one last...so it is always on top during 'working' periods
      #
      try:
         self.working=Working(self.dialog,callbackMethod=self.tryToStop)
      except:
         splash.destroy()
         self.statusBarMsg("%s could not start up"%self.PROGNAME,"R")
         raise Exception
      #
      #  Destroy the 'starting' message box
      #
      splash.destroy()
      del wTxt
      self.statusBarMsg("%s is now running - memory:%d  resident:%d"%(self.PROGNAME,memory(),resident()),"R")

   def getPts(self,areaname):
      if areaname=="NONE":
         ea=(self._empty+1).astype(int)
      else:
         ea=self.encodeEditArea(areaname)
      eb=ravel(ea)
      num=add.reduce(eb)
      pts=int(num)
      del ea
      del eb
      del num
      return pts
   #==================================================================
   #
   #  Routines for the 'working' dialog.
   #
   #  When the stop button is pressed - tryToStop is called and the stop
   #      variable is set to 1.
   #  startWorking sets the stop variable to 0 and sets the label, then
   #     reveals the working dialog and withdraws the main dialog (unless
   #     overridden)
   #  setWorking just updates labels in the working dialog
   #  checkWorking updates the dialog, and returns the stop variable status
   #  stopWorking withdraws the working dialog and raises the main dialog
   #  finishWorking withdraws the working dialog and raises the mini dialog
   #
   def tryToStop(self):
      self.working.stop.set(1)
      print "tryToStop was called - should stop soon"
      return
   def startWorking(self,textString,optionRemove=1):
      if optionRemove==1:
         self.dialog.withdraw()
      self.setWorking(textString)
      self.working.stop.set(0)
      self.working.deiconify()
      self.working.lift()
      return
   def setWorking(self,textString):
      self.working.label.set(textString)
      self.working.update()
      return
   def checkWorking(self):
      self.working.update()
      return self.working.stop.get()
   def setAndCheckWorking(self,textString):
      self.working.label.set(textString)
      self.working.update()
      return self.working.stop.get()
   def stopWorking(self):
      self.working.withdraw()
      self.dialog.deiconify()
      self.dialog.lift()
      self.dialog.update_idletasks()
      return
   def finishWorking(self):
      self.working.withdraw()
      self.working.stop.set(0)
      self.mini.deiconify()
      self.mini.lift()
      self.mini.update_idletasks()
      return
   #==================================================================
   #
   #  Routines for the 'mini' dialog.
   #
   #  When the button is pressed (or X clicked) - expandMini is called
   #
   def expandMini(self):
      self.mini.withdraw()
      self.dialog.deiconify()
      self.dialog.lift()
      return
   #
   #  Hide the main dialog, and reveal the mini-dialog.
   #
   def hideDialog(self):
      self.dialog.withdraw()
      self.mini.deiconify()
      self.mini.lift()
      self.mini.wait_visibility()
      self.mini.update_idletasks()
      return
   #==================================================================
   #
   #  Routines for the cases dialog, and it's mini dialog.
   #
   def expandCases(self):
      geo1=self.miniCases.geometry()
      (wh,of)=geo1.split("+",1)
      (wid1,hgt1)=wh.split("x",1)
      (ofx1,ofy1)=of.split("+",1)
      self.miniCases.withdraw()
      self.cases.deiconify()
      self.cases.lift()
      geo2=self.cases.geometry()
      (wh,of)=geo2.split("+",1)
      (wid2,hgt2)=wh.split("x",1)
      (ofx2,ofy2)=of.split("+",1)
      newgeo="%s+%d+%d"%(wh,int(ofx1)+int(wid1)-int(wid2),int(ofy1))
      self.cases.geometry(newgeo)
      return
   def closeCases(self):
      self.cases.withdraw()
      self.cases.update_idletasks()
      self.miniCases.deiconify()
      self.miniCases.lift()
      self.miniCases.update_idletasks()
      return
   #==================================================================
   #
   #  Routines for the 'canvas' dialog
   #
   def closeCD(self):
      self.cd.withdraw()
      return
   #==================================================================
   #  doVerif - 
   #  This is the routine that really does the verification calculations
   #  It is called when the user clicks on "Run" "Hide" or "Cancel"
   #  in the verification dialog.  The routine is called with the button
   #  type of "Run" (do NOT dismiss dialog), "OK" (DISMISS dialog when done)
   #  or "Quit".  The actual removal of the dialog is handled by the
   #  dialog routines themselves...so all you have to do is return
   #  right away if the user hit cancel, or do the calculations if they
   #  hit anything else.
   #
   def doVerif(self,buttonType):
      if buttonType=="Quit":
         self.root.quit()
         del self.pts
         self.statusBarMsg("%s is finished with memory:%d  resident:%s"%(self.PROGNAME,memory(),resident()),"R")
         return
      if buttonType=="Hide":
         self.hideDialog()
         return
      #
      #  When doing calculations - make sure the cases windows
      #  are closed
      #
      self.cases.withdraw()
      self.miniCases.withdraw()
      #
      #  Do calculations
      #
      try:
         dict=self.dialog.getValues()
         tab=dict["tab"]
         if tab=="Grid Displays":
            self.ShowGrids(dict)
         if tab=="Grid Stats":
            self.ShowGridsStats(dict)
         if tab=="Distributions":
            self.ShowDists(dict)
         if tab=="Point/Area Stats":
            self.ShowStats(dict)
         if tab=="Stat vs. Scale":
            self.ShowScaleStats(dict)
      #
      #  If something goes wrong during calculations - close everything
      #  and raise the exception
      #
      except:
         (exctype,excvalue,trace)=sys.exc_info()
         traceStrings=traceback.format_exception(exctype,excvalue,trace)
         fullstring=string.join(traceStrings)
         self.statusBarMsg("Error in %s:\n%s"%(self.PROGNAME,fullstring),"S")
         self.root.quit()
      return
   #==================================================================
   # showGrids - read and display the archived forecast/observed grids
   #
   def ShowGrids(self,DialogDict):
      self.VU.logMsg("running ShowGrids:",0)
      parmList=DialogDict["Parm"]
      display=DialogDict["Display"]
      groupBy=DialogDict["Group"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Model"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      dateStyle=DialogDict["dateStyle"]
      scale=DialogDict["scale"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      #
      #  Check for good GUI input
      #
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      self.startWorking("Working on Grid Display")
      #
      #
      #
      numdisplayed=0
      ret=self.setupGM(parmList,modelList)
      if ret==1:
         self.stopWorking()
         return
      errpat=re.compile("^(.*?)(spd|dir)Err")
      totaliters=len(parmList)*len(modelList)
      iter=0
      self.VU.logMsg("going into parmList loop",10)
      for parm in parmList:
         self.VU.logMsg("in ShowGrids working on %s"%parm,5)
         datatype=self.VU.getVerParmType(parm)
         verType=self.VU.getVerType(parm)
         errColor=self.VU.getVerErrColor(parm)
         (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
          parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
         logkey="%s_LogFactor"%parm
         logfactor = getFloatPref(logkey, -1)
         #
         #  get info on what that verifies the current parm
         #
         obsParm=self.VU.getObsParm(parm)
         (obsParmUnits,obsParmPrecision,obsParmMinval,obsParmMaxval,obsParmRateFlag,obsParmColorTable,
          obsParmDisplayMinval,obsParmDisplayMaxval)=self.getParmInfo(obsmodel,obsParm)
         logkey="%s_LogFactor"%obsParm
         obslogfactor = getFloatPref(logkey, -1)

         obsGridMode=self.getReadMode(obsmodel,obsParm,0)
         #
         #  Get case times/records for all models - but don't require
         #  observations...since we might want to display just the
         #  forecasts in the future without a verifying observation
         #
         caseInfo=self.VU.getCases(parm,modelList,obsParm,obsmodel,
                                dateStyle,dateType,fromDay=fromDay,
                                numDays=numDays,dayList=dayList,
                                fcstrs=fcstrList,cycles=cycleList,
                                fhrStart=fhrStart,fhrEnd=fhrEnd,
                                accumHours=accumHours,accumFreq=accumFreq,
                                requireObs=0,commonCases=commonCases,
                                basetimeOffsets=1,
                                callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         #
         #
         #
         parmnames=[]
         obsnames=[]
         for model in modelList:
            iter+=1
            if (totaliters>1):
               firstString="Getting (%d of %d) %s %s grids"%(iter,totaliters,model,parm)
            else:
               firstString="Getting %s %s grids"%(model,parm)
            self.setWorking(firstString)
            
            tomorrow=time.time()+self.DAYSECS
            if self.setAndCheckWorking("%s:"%firstString)==1:
               self.stopWorking()
               return
            count=0
            okeys=[]
            fcstGridMode=self.getReadMode(model,parm)
            cases=caseInfo[model]
            casekeys=cases.keys()
            casekeys.sort()
            totalcount=len(casekeys)
            self.VU.logMsg("%d cases for %s"%(len(casekeys),model),1)
            for key in casekeys:
               count+=1
               if self.setAndCheckWorking("%s: %d of %d"%(firstString,count,totalcount))==1:
                  self.stopWorking()
                  return
               (basetimestr,stimestr,etimestr)=key.split(",")
               basetime=int(basetimestr)
               stime=int(stimestr)
               etime=int(etimestr)
               (frecList,orecList)=cases[key]
               tr=self.VU.getVerTimeRange(stime, etime)
               #
               #  If there is an observed grid - read it and display it
               #
               obsavailable=0
               if len(orecList)>0:
                  okey="%s,%s"%(stimestr,etimestr)
                  obsname="%s0bs"%(parm) # zero instead of O so that is sorts before others
                  if accumFreq<accumHours:
                     shr=(stime%86400)/3600
                     obsname+="For%2.2d"%shr
                  obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                              stime,etime,mode=obsGridMode,
                                              recList=orecList)
                  if obsdata is not None:
                     obsavailable=1
                     if datatype!=1:
                        if scale>0:
                           obsdata=self.VU.smoothpm(obsdata,scale)
                        if okey not in okeys:
                           obsclip=clip(obsdata,parmMinval,parmMaxval)
                           self.createGrid("Ver",obsname,"SCALAR",obsclip,tr,"Observed",
                                           None,obsParmPrecision,obsParmMinval,obsParmMaxval,
                                           obsParmUnits)
                           okeys.append(okey)
                           numdisplayed+=1
                     else:
                        (obsmag,obsdirec)=obsdata
                        if scale>0:
                           (u,v)=self.MagDirToUV(obsmag,obsdirec)
                           u=self.VU.smoothpm(u,scale)
                           v=self.VU.smoothpm(v,scale)
                           (obsmag,obsdirec)=self.UVToMagDir(u,v)
                        obsdata=(obsmag,obsdirec)
                        if okey not in okeys:
                           obsmag=clip(obsmag,parmMinval,parmMaxval)
                           obsdirec=clip(obsdirec,0.0,360.0)
                           obsclip=(obsmag,obsdirec)
                           self.createGrid("Ver",obsname,"VECTOR",obsclip,tr,"Observed",
                                           None,obsParmPrecision,obsParmMinval,obsParmMaxval,
                                           obsParmUnits)
                           okeys.append(okey)
                           numdisplayed+=1
                     if obsname not in obsnames:
                        obsnames.append(obsname)
               #
               #  Make forecast grid
               #
               fcstdata=self.VU.getVerGrids(model,basetime,parm,
                                            stime,etime,mode=fcstGridMode,
                                            recList=frecList)
               if fcstdata is not None:               
                  if datatype!=1:
                     if scale>0:
                        fcstdata=self.VU.smoothpm(fcstdata,scale)
                  else:
                     (fcstmag,fcstdirec)=fcstdata
                     if scale>0:
                        (u,v)=self.MagDirToUV(fcstmag,fcstdirec)
                        u=self.VU.smoothpm(u,scale)
                        v=self.VU.smoothpm(v,scale)
                        (fcstmag,fcstdirec)=self.UVToMagDir(u,v)
                        fcstdata=(fcstmag,fcstdirec)
                  #
                  #  part of name based on grouping method - and model name
                  #
                  if groupBy=="Run Time":
                     basetuple=time.gmtime(basetime)
                     runTime="%4.4d%2.2d%2.2d%2.2d"%(basetuple[0],basetuple[1],basetuple[2],basetuple[3])
                     runHours=self.VU.getFcstHour(basetime,tomorrow)
                     run=(runHours/6)+1
                     runname="run%3.3dfrom%s"%(run,runTime[6:10])
                  else:
                     fhr=self.VU.getFcstHour(basetime,stime)
                     if fhr<0:
                        self.VU.logMsg("%d-hour forecasts not shown"%fhr,1)
                        continue
                     runname="f%3.3dHr"%(fhr)
                  if model!="Official":
                     runname+=model
                  #
                  #  calculate errors (if needed) and clip to twice the 'bigerr' range
                  #
                  if display=="Errors":
                     if obsavailable==1:
                        ep=max(self.errPrecision,parmPrecision)
                        if datatype!=1:
                           parmname=parm+"Err"+runname
                           if accumFreq<accumHours:
                              shr=(stime%86400)/3600
                              parmname+="For%2.2d"%shr
                           bigerr=self.VU.getVerBigErr(parm)
                           clipval=bigerr*2.0
                           #
                           #  use obsgrid instead of obsdata...and for
                           #     probability forecasts - calculate the verifying
                           #     obsgrid grid from the condition
                           #
                           obsgrid=obsdata
                           if verType==1:
                              obsgrid=self.getProbVerGrid(parm,obsdata)*100
                           newgrid=fcstdata-obsgrid
                           newgrid=clip(newgrid,-clipval,clipval)
                           self.createGrid("Ver",parmname,"SCALAR",newgrid,tr,
                               "Forecast",None,ep,-clipval,
                               clipval,self.errUnits)
                           numdisplayed+=1
                           if parmname not in parmnames:
                              parmnames.append(parmname)
                        else:
                           (fcstmag,fcstdir)=fcstdata
                           (bigerrmag,bigerrdir)=self.VU.getVerBigErr(parm)
                           (obsmag,obsdir)=obsdata
                           #print "bigerrmag:%d bigerrdir=%d"%(bigerrmag,bigerrdir)
                           errmag=fcstmag-obsmag
                           errdir=fcstdir-obsdir
                           errdir=where(greater(errdir,180.0),360.0-errdir,errdir)
                           errdir=where(less(errdir,-180.0),-(360.0+errdir),errdir)
                           errmag=clip(errmag,-(2*bigerrmag),(2*bigerrmag))
                           parmname=parm+"spdErr"+runname
                           if accumFreq<accumHours:
                              shr=(stime%86400)/3600
                              parmname+="For%2.2d"%shr
                           self.createGrid("Ver",parmname,"SCALAR",errmag,tr,"Forecast",
                                       None,ep,-(2*bigerrmag),(2*bigerrmag),
                                       self.errUnits)
                           numdisplayed+=1
                           #
                           #  direction Error
                           #
                           if parmname not in parmnames:
                              parmnames.append(parmname)
                           parmname=parm+"dirErr"+runname
                           if accumFreq<accumHours:
                              shr=(stime%86400)/3600
                              parmname+="For%2.2d"%shr
                           self.createGrid("Ver",parmname,"SCALAR",errdir,tr,"Forecast",
                                       None,self.errPrecision,-180,180,
                                       self.errUnits)
                           numdisplayed+=1
                           if parmname not in parmnames:
                              parmnames.append(parmname)
                           #
                           #  Vector Error
                           #
                           (fu,fv)=self.MagDirToUV(fcstmag,fcstdir)
                           (ou,ov)=self.MagDirToUV(obsmag,obsdir)
                           eu=fu-ou
                           ev=fv-ov
                           (errmag,errdir)=self.UVToMagDir(eu,ev)
                           vecerr=(errmag,errdir)
                           parmname=parm+"Err"+runname
                           if accumFreq<accumHours:
                              shr=(stime%86400)/3600
                              parmname+="For%2.2d"%shr
                           self.createGrid("Ver",parmname,"VECTOR",vecerr,tr,"Forecast",
                                       None,parmPrecision,parmMinval,parmMaxval,
                                       parmUnits)
                           numdisplayed+=1
                           if parmname not in parmnames:
                              parmnames.append(parmname)
                  else:
                     parmname=parm+runname
                     if accumFreq<accumHours:
                        shr=(stime%86400)/3600
                        parmname+="For%2.2d"%shr
                     if datatype!=1:
                        newgrid=clip(fcstdata,parmMinval,parmMaxval)
                        self.createGrid("Ver",parmname,"SCALAR",newgrid,tr,
                            "Forecast",None,parmPrecision,parmMinval,
                            parmMaxval,parmUnits)
                        numdisplayed+=1
                     else:
                        (fcstmag,fcstdir)=fcstdata
                        fcstmag=clip(fcstmag,parmMinval,parmMaxval)
                        self.createGrid("Ver",parmname,"VECTOR",(fcstmag,fcstdir),tr,
                                        "Forecast",None,parmPrecision,parmMinval,
                                        parmMaxval,parmUnits)
                        numdisplayed+=1
                     if parmname not in parmnames:
                        parmnames.append(parmname)
         #
         #  Set the colorTables for each unique parm added.
         #
         totalcount=len(parmnames)+len(obsnames)
         count=0

         for obsname in obsnames:
            count+=1
            self.setWorking("Setting colorcurves: %d of %d"%(count,totalcount))
            if obslogfactor>-1:
               keyname="%s_LogFactor"%obsname
               setFloatPref(keyname, obslogfactor)
            parmOb=self.getParm("Ver",obsname,"SFC")
            self.setColorTableAndRange(parmOb,obsParmColorTable,obsParmDisplayMinval,obsParmDisplayMaxval)
         for parmname in parmnames:
            count+=1
            self.setWorking("Setting colorcurves: %d of %d"%(count,totalcount))
            parmOb=self.getParm("Ver",parmname,"SFC")
            if display=="Errors":
               if datatype!=1:
                  self.setColorTableAndRange(parmOb,errColor,-bigerr,bigerr)
               else:
                  (errColorMag,errColorDir)=errColor
                  matchObj=errpat.search(parmname)
                  if matchObj is not None:
                     type=matchObj.group(2)
                     if type=="spd":
                        self.setColorTableAndRange(parmOb,errColorMag,-bigerrmag,bigerrmag)
                     else:
                        self.setColorTableAndRange(parmOb,errColorDir,-bigerrdir,bigerrdir)
                  else:
                     self.setColorTableAndRange(parmOb,parmColorTable,parmDisplayMinval,parmDisplayMaxval)
            else:
               if logfactor>=-1:
                  keyname="%s_LogFactor"%parmname
                  setFloatPref(keyname, logfactor)
               self.setColorTableAndRange(parmOb,parmColorTable,parmDisplayMinval,parmDisplayMaxval)
      #
      if numdisplayed==0:
         self.stopWorking()
         self.statusBarMsg("No grids match your selected models/times/parms","U")
         return
      self.finishWorking()
      return

   
   #==================================================================
   # setColorTableAndRange - Set the color table
   #
   #
   def setColorTableAndRange(self,parm,colorTable,displayMinval,displayMaxval):
      spatialMgr = self._dbss.getSpatialDisplayManager()
      if displayMinval or displayMaxval or colorTable:
         rsc = spatialMgr.getResourcePair(parm).getResource()
         from com.raytheon.uf.viz.core.rsc.capabilities import ColorMapCapability
         params = rsc.getCapability(ColorMapCapability).getColorMapParameters()

         if colorTable:
            if self.__colorMapParams.has_key(colorTable):
                colorMap = self.__colorMapParams[colorTable].getColorMap()
            else:
                from com.raytheon.uf.viz.core.drawables import ColorMapLoader
                if "GFE/" not in colorTable:
                    colorTable = "GFE/" + colorTable
                colorMap = ColorMapLoader.loadColorMap(colorTable)
            elemType = parm.getGridInfo().getGridType().toString()
            if ('DISCRETE' == elemType):
               from com.raytheon.viz.gfe.rsc import DiscreteDisplayUtil
               DiscreteDisplayUtil.deleteParmColorMap(parm)
            params.setColorMap(colorMap)
            params.setColorMapName(colorTable)
            logfactor = getFloatPref(parm.getParmID().getParmName()+"_LogFactor", None)
            if logfactor is not None:
                params.setLogFactor(logfactor)
            rsc.issueRefresh()

         if displayMinval or displayMaxval:
            if (displayMinval != displayMaxval):
               params.setColorMapMax(float(displayMaxval))
               params.setColorMapMin(float(displayMinval))
                     
         parm.getListeners().fireColorTableModified(parm)

      return

   #==================================================================
   # showGridsStats - display grid statistics
   #
   #
   def ShowGridsStats(self,DialogDict):
      self.VU.logMsg("running ShowGridsStats:")
      parmList=[]
      parm=DialogDict["Parm"]
      parmList.append(parm)
      display=DialogDict["Display"]
      threshold=DialogDict["Threshold"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Models"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      scale=DialogDict["scale"]
      dateStyle=DialogDict["dateStyle"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      TwoCatType=DialogDict["TwoCatType"]
      TwoCatCond=DialogDict["TwoCatCond"]
      TwoCatValue=DialogDict["TwoCatValue"]
      TwoCatValueString=DialogDict["TwoCatValueString"]
      #
      #  Check for good GUI input
      #
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      #
      #  If a TwoCat stat - check to see that TwoCatType is OK
      #  and setup statID
      #
      if display=="TwoCat":
         statName=TwoCatType
         statCond=TwoCatCond
         statVal=TwoCatValue
         statID=self.VU.getStatID(statName)
         if statID is None:
            self.statusBarMsg("Invalid Statistic Name","U")
            return
      else:
         statID="xxxx"
      #
      #
      #
      self.startWorking("Working on Grid Stats")
      ret=self.setupGM(parmList,modelList)
      if ret==1:
         self.stopWorking()
         return
      casesInfo=[]
      numdisplayed=0
      pctColor=self.VU.getCFG('PERCENT_COLOR')
      #
      #  Loop over parm and model
      #
      totaliters=len(parmList)*len(modelList)
      iter=0
      for parm in parmList:
         readParm=parm
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               readParm=parm[:-3]
         obsParm=self.VU.getObsParm(readParm)
         verType=self.VU.getVerType(readParm)
         datatype=self.VU.getVerParmType(readParm)
         errColor=self.VU.getVerErrColor(readParm)
         bigerr=self.VU.getVerBigErr(readParm)
         thresholds=self.VU.getVerThresholds(readParm)
         if datatype==1:
            (errColorMag,errColorDir)=errColor
            (bigerrmag,bigerrdir)=bigerr
            (threshmag,threshdir)=thresholds
            if last3=="Dir":
               errColor=errColorDir
               bigerr=bigerrdir
               thresholdValue=threshdir[threshold]
               clipval=180               
            else:  #Spd or vector err magnitude
               errColor=errColorMag
               bigerr=bigerrmag
               thresholdValue=threshmag[threshold]
               clipval=bigerr*2
         else:
            thresholdValue=thresholds[threshold]
            clipval=bigerr*2
         #
         #  Get mode for reading obs grids
         #
         obsGridMode=self.getReadMode(obsmodel,obsParm,0)
         #
         #  Get case times/records for all models
         #
         caseInfo=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                                dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                                dayList=dayList,fcstrs=fcstrList,cycles=cycleList,
                                fhrStart=fhrStart,fhrEnd=fhrEnd,
                                accumHours=accumHours,accumFreq=accumFreq,
                                requireObs=1,commonCases=commonCases,
                                basetimeOffsets=1,
                                callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         #
         #
         #
         for model in modelList:
            iter+=1
            if (totaliters>1):
               firstString="Calculating (%d of %d) %s %s stats"%(iter,totaliters,model,parm)
            else:
               firstString="Calculating %s %s stats"%(model,parm)
            #
            #
            #
            fcstGridMode=self.getReadMode(model,readParm)
            (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
            parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(model,readParm)
            #
            #
            #
            parmnames=[]
            gridsave={}
            gridcount={}
            hitssave={}
            misssave={}
            falrsave={}
            cornsave={}
            maxcases=0
            self.setWorking("%s:finding matches"%firstString)
            #
            #  Get all the cases for this model
            #
            cases=caseInfo[model]
            #
            #  Sort them by the start/end time, not the basetime
            #
            casekeys=cases.keys()
            casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
            totalcount=len(casekeys)
            self.VU.logMsg("%d cases for %s"%(len(casekeys),model),1)
            count=0
            lastobs=""
            for key in casekeys:
               count+=1
               self.VU.logMsg("%s : %s"%(model,key),10)
               if self.setAndCheckWorking("%s: %d of %d"%(firstString,count,totalcount))==1:
                  self.stopWorking()
                  return
               (basetimestr,stimestr,etimestr)=key.split(",")
               basetime=int(basetimestr)
               stime=int(stimestr)
               etime=int(etimestr)
               (frecList,orecList)=cases[key]
               #
               #  Dont make stats for obs not yet complete
               #
               if etime>time.time(): # dont make stats for obs not yet complete
                  count+=1
                  continue
               #
               #  check to make sure it is a forecast
               #  string to store grid under depends on forecast and end hour
               #
               fhr=self.VU.getFcstHour(basetime,stime)
               if fhr<0:
                  count+=1
                  continue            
               #
               #  If a new and different obs time - read the obs data
               #
               obskey=key.split(",",1)[1]
               if obskey!=lastobs:
                  obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                              stime,etime,obsGridMode,
                                              orecList)
                  #
                  #  Smooth observed grid...
                  #     unless a TwoCat "areal" type
                  #     and smooth vectors in U/V space...
                  #
                  if scale>0:
                     if ((display!="TwoCat")or(statID[0:1]!="a")):
                        if datatype==1:
                           (obsmag,obsdir)=obsdata
                           (u,v)=self.MagDirToUV(obsmag,obsdir)
                           us=self.VU.smoothpm(u,scale)
                           vs=self.VU.smoothpm(v,scale)
                           (obsmag,obsdir)=self.UVToMagDir(us,vs)
                           obsdata=(obsmag,obsdir)
                        else:
                           obsdata=self.VU.smoothpm(obsdata,scale)
                  #
                  #  For probability types...calculate an obs grid of
                  #  1 or 0, based on whether the observed threshold
                  #  is met.
                  #
                  if verType==1:
                     obsdata=self.getProbVerGrid(readParm,obsdata)
                  #
                  #  Save the 'key' for this obs grid - so that we
                  #  don't have to read and calculate it again every
                  #  time...only when a new obs time is encountered
                  #
                  lastobs=obskey
               #
               #  get parmname to save as...from cycle/fhr/ehr/model
               #
               ehr=self.VU.getFcstHour(basetime,etime)
               basetuple=time.gmtime(basetime)
               fcstcycle=basetuple[3]
               parmname="%2.2d%3.3d%3.3d%s"%(fcstcycle,fhr,ehr,model)
               #
               #  Read forecast grid
               #
               fcstdata=self.VU.getVerGrids(model,basetime,readParm,
                                            stime,etime,fcstGridMode,
                                            frecList)
               #
               #  Smooth forecast grid...
               #     unless a TwoCat "areal" type
               #     and smooth vectors in U/V space...
               #
               if scale>0:
                  if ((display!="TwoCat")or(statID[0:1]!="a")):
                     if datatype==1:
                        (fcstmag,fcstdir)=fcstdata
                        (u,v)=self.MagDirToUV(fcstmag,fcstdir)
                        us=self.VU.smoothpm(u,scale)
                        vs=self.VU.smoothpm(v,scale)
                        (fcstmag,fcstdir)=self.UVToMagDir(us,vs)
                        fcstdata=(fcstmag,fcstdir)
                     else:
                        fcstdata=self.VU.smoothpm(fcstdata,scale)
               #
               #  For TwoCat stats...calculate hits/misses/falsealarms/etc.
               #
               if display=="TwoCat":
                  #
                  #  get the forecast/observed grids into fcstGrid/obsGrid
                  #  Normally this is what is in fcstdata/obsdata - but for
                  #  vectors...need to pick the right component and for
                  #  probabilities - need to divide by 100.
                  #
                  if datatype==1:
                     if last3!="Dir":
                        fcstGrid=fcstdata[0]
                        obsGrid=obsdata[0]
                     else:
                        fcstGrid=fcstdata[1]
                        obsGrid=obsdata[1]
                  else:
                     if verType!=0:
                        fcstGrid=fcstdata/100.0
                     else:
                        fcstGrid=fcstdata
                     obsGrid=obsdata
                  #
                  #  Now get yes/no of forecast/observed occurrence
                  #
                  if statCond==">":
                     obsOccur=greater(obsGrid,statVal)
                     fcstOccur=greater(fcstGrid,statVal)
                  elif statCond==">=":
                     obsOccur=greater_equal(obsGrid,statVal)
                     fcstOccur=greater_equal(fcstGrid,statVal)
                  elif statCond=="<":
                     obsOccur=less(obsGrid,statVal)
                     fcstOccur=less(fcstGrid,statVal)
                  elif statCond=="<=":
                     obsOccur=less_equal(obsGrid,statVal)
                     fcstOccur=less_equal(fcstGrid,statVal)
                  #
                  #  do neighborhood look here
                  #
                  if statID[0:1]=="a":
                     if scale>0:
                        obsOccur=self.VU.arealOccur(obsOccur,scale)
                        fcstOccur=self.VU.arealOccur(fcstOccur,scale)
                  #
                  #  Make grids of hits, misses, false alarms, correct negatives
                  #
                  notFcst=logical_not(fcstOccur)
                  notObs=logical_not(obsOccur)
                  hitsgrid=logical_and(fcstOccur,obsOccur)
                  missgrid=logical_and(notFcst,obsOccur)
                  falrgrid=logical_and(fcstOccur,notObs)
                  corngrid=logical_and(notFcst,notObs)
                  #
                  #  Make space to store these results - if first one 
                  #
                  if parmname not in parmnames:
                     parmnames.append(parmname)
                     hitssave[parmname]=self._empty
                     misssave[parmname]=self._empty
                     falrsave[parmname]=self._empty
                     cornsave[parmname]=self._empty
                  #
                  #  Add to the hits/miss/falr/corn values
                  #
                  hitssave[parmname]+=hitsgrid
                  misssave[parmname]+=missgrid
                  falrsave[parmname]+=falrgrid
                  cornsave[parmname]+=corngrid
               #
               #  For non-TwoCat displays...calculate the errors 
               #
               else:
                  if datatype!=1:
                     if verType==0:
                        errgrid=fcstdata-obsdata
                     else:
                        errgrid=(fcstdata/100.0)-obsdata
                  else:
                     last3=parm[-3:]
                     if (last3=="Spd"):
                        errgrid=fcstdata[0]-obsdata[0]
                     elif (last3=="Dir"):
                        errgrid=fcstdata[1]-obsdata[1]
                        errgrid=where(greater(errgrid,180.0),360.0-errgrid,errgrid)
                        errgrid=where(less(errgrid,-180.0),-(360.0+errgrid),errgrid)
                     else:
                        (fu,fv)=self.MagDirToUV(fcstdata[0],fcstdata[1])
                        (ou,ov)=self.MagDirToUV(obsdata[0],obsdata[1])
                        eu=fu-ou
                        ev=fv-ov
                        (errmag,errdir)=self.UVToMagDir(eu,ev)
                        errgrid=errmag
                  #
                  #  change to different scores
                  #
                  if display=="Mean Abs Error":
                     errgrid=where(less(errgrid,0.0),-errgrid,errgrid)
                  if display in ["RMS Error","Mean Squared Error"]:
                     errgrid*=errgrid
                  if display=="Percent Err <":
                     errgrid=where(less(errgrid,0.0),-errgrid,errgrid)
                     errgrid=less(errgrid,thresholdValue)
                  #
                  #  save list of unique parm names being created
                  #
                  if parmname not in parmnames:
                     parmnames.append(parmname)
                     gridsave[parmname]=self._empty.copy()
                     gridcount[parmname]=0
                  #
                  #  if doing average errors, add errors to sums
                  #  otherwise...display the grid
                  #
                  gridsave[parmname]+=errgrid
                  gridcount[parmname]+=1
            #
            #  Calculate the statistics grids for this parm/model
            #  and display them
            #
            self.VU.logMsg("Creating stat grids")
            pnames=[]
            totalcount=len(parmnames)
            count=0
            for parmname in parmnames:
               #
               #  If they want to stop - stop adding more grids
               #  but break out to set the color tables correctly
               #
               count+=1
               self.setWorking("%s:%d of %d"%(firstString,count,totalcount))
               if self.checkWorking()==1:
                  break
               #
               #  Get timerange to save the final grid into
               #
               cyc=int(parmname[0:2])
               f1=int(parmname[2:5])
               f2=int(parmname[5:8])
               tr=self.createTimeRange(f1+cyc,f2+cyc,"Zulu")
               #
               #  Make name that will be used in grid manager 
               #
               mod=parmname[8:]
               pname="%s%2.2dZ%s"%(parm,cyc,mod)
               #
               #  For TwoCat stats
               #
               if display=="TwoCat":
                  hitsgrid=hitssave[parmname]
                  missgrid=misssave[parmname]
                  falrgrid=falrsave[parmname]
                  corngrid=cornsave[parmname]
                  statgrid=self.VU.getGridBinaryStat(statID,hitsgrid,missgrid,
                                                     falrgrid,corngrid)
                  #
                  #  get case number - for table of cases
                  #
                  totgrid=hitsgrid+missgrid+falrgrid+corngrid
                  n=maximum.reduce(maximum.reduce(totgrid))
                  maxcases=max(n,maxcases)
                  #
                  #  Different stats have different limits
                  #
                  minlim=-1.0
                  maxlim=1.0
                  res=2
                  if statID in ["hits","ahits","miss","amiss","fals","afals",
                                "corn","acorn"]:
                     minlim=0.0
                     maxlim=float(n)
                     res=0
                  elif statID in ["freqo","freqf","fc","afc","pod","apod","far","afar",
                                "pofd","apofd","ts","ats"]:
                     minlim=0.0
                     maxlim=1.0
                  #
                  #  Ones that range from 0 to Infinity : clip at +5.0
                  #
                  elif statID in ["freqbias","afreqbias","oddratio","aoddsratio"]:
                     minlim=0.0
                     maxlim=5.0
                  #
                  #  Equitable Threat clips at -0.333 and 1.0
                  #
                  elif statID in ["ets","aets"]:
                     minlim=-0.3333
                     maxlim=1.0
                  #
                  #  Hansen Kuipers clips at -1.0 to 1.0
                  #
                  elif statID in ["hk","ahk"]:
                     minlim=-1.0
                     maxlim=1.0
                  #
                  #  Heidke ranges from -Infinity to 1, and clips at -5.0
                  #
                  elif statID in ["hss","ahss"]:
                     minlim=-5.0
                     maxlim=1.0
                  #
                  #  Clip the grid
                  #
                  newgrid=clip(statgrid,minlim,maxlim)
                  self.createGrid("Ver",pname,"SCALAR",newgrid,tr,
                                  "Forecast",None,res,minlim,maxlim,
                                  "units")
               #
               #  For normal error displays
               #
               else:  
                  #
                  #  If there weren't any sums saved - dont make
                  #  a grid for it
                  #
                  n=gridcount[parmname]
                  if n<1:
                     continue
                  #
                  #  make newgrid the grid to show
                  #
                  newgrid=gridsave[parmname]/float(n)
                  if display=="RMS Error":
                     newgrid=newgrid**0.5
                  if verType==1:
                     newgrid*=100.0
                  #
                  #  clip the newgrid based on the parm clipping value
                  #
                  newgrid=clip(newgrid,-clipval,clipval)
                  #
                  #  Percent error grids always range from 0 to 100
                  #
                  if display=="Percent Err <":
                     newgrid*=100.0
                     newgrid=clip(newgrid,0.0,100.0)
                     self.createGrid("Ver",pname,"SCALAR",newgrid,tr,
                                     "Forecast",None,0,0.0,100.0,"%")
                  #
                  #  Others can have variable ranges.  We clip the
                  #  values at 2 times the 'bigerr' value
                  #
                  else:
                     ep=max(self.errPrecision,parmPrecision)
                     if datatype!=1:
                        bigerr=self.VU.getVerBigErr(parm)
                        clipval=bigerr*2.0
                     self.createGrid("Ver",pname,"SCALAR",newgrid,tr,
                                  "Forecast",None,ep,-clipval,
                                  clipval,self.errUnits)
               #
               #  Keep track of grids actually put in grid manager
               #
               if pname not in pnames:
                  pnames.append(pname)
               numdisplayed+=1
               casesInfo.append("%-25s|%3.3d|%d"%(pname,f1,n))
            #
            #  Set the colorTables for each unique parm added.
            #
            self.VU.logMsg("Setting color tables",2)
            for pname in pnames:
               parmOb=self.getParm("Ver",pname,"SFC")
               if display=="TwoCat":
                  if res==0:
                     self.setColorTableAndRange(parmOb,pctColor,0,maxcases)
                  else:
                     self.setColorTableAndRange(parmOb,pctColor,minlim,maxlim)
               else:
                  if display=="Percent Err <":
                     self.setColorTableAndRange(parmOb,pctColor,0,100)
                  else:
                     self.setColorTableAndRange(parmOb,errColor,-bigerr,bigerr)
            if self.checkWorking()==1:
               self.stopWorking()
               return
      #
      #
      #
      if numdisplayed==0:
         self.stopWorking()
         self.statusBarMsg("No grids match your selected models/times/parms","U")
         return
      #
      self.finishWorking()
      #
      #  Make text with case info
      #
      casesInfo.sort()
      casesText="Number of Cases:\n"
      lastmod=""
      for info in casesInfo:
         (modlong,fhr,num)=info.split("|")
         mod=modlong.strip()
         if mod!=lastmod:
            casesText+="\n  %s:\n"%mod
            lastmod=mod
         casesText+="     %3d-hr: %5d\n"%(int(fhr),int(num))
      #
      #  Make the case info pop up
      #
      self.cases.updateText(casesText)
      self.miniCases.deiconify()
      self.miniCases.lift()
      self.miniCases.update_idletasks()
      self.VU.logMsg("Done making stat grids")
      return
   #==================================================================
   #  getProbVerGrid - get grid for probability verification, based
   #                   on the obsdata, and the condition/threshold for
   #                   the specified parmName
   #
   def getProbVerGrid(self,parmName,obsdata):
      outdata=obsdata*0
      obsCondition=self.VU.getObsCondition(parmName)
      obsThreshold=self.VU.getObsThreshold(parmName)
      if obsCondition==">":
         outdata=greater(obsdata,obsThreshold)
      elif obsCondition==">=":
         outdata=greater_equal(obsdata,obsThreshold)
      elif obsCondition=="<":
         outdata=less(obsdata,obsThreshold)
      elif obsCondition=="<=":
         outdata=less_equal(obsdata,obsThreshold)
      return outdata
   #==================================================================
   # showGridsDists - display histograms/scatterplots
   #
   #
   def ShowDists(self,DialogDict):
      self.VU.logMsg("running ShowDists:")
      parmList=[]
      parm=DialogDict["Parm"]
      parmList.append(parm)
      display=DialogDict["Display"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Models"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      dateStyle=DialogDict["dateStyle"]
      scale=DialogDict["scale"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      #
      #  Check for good GUI input
      #
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      #
      #  Do seperate processing for each type
      #
      if display=="Error Histogram":
         self.errorHistogram(parmList,cycleList,modelList,obsmodel,fcstrList,
                             fhrStart,fhrEnd,dateType,numDays,fromDay,dayList,
                             dateStyle,scale,commonCases,accumHours,accumFreq)
      elif display=="Value Histogram":
         self.valueHistogram(parmList,cycleList,modelList,obsmodel,fcstrList,
                             fhrStart,fhrEnd,dateType,numDays,fromDay,dayList,
                             dateStyle,scale,commonCases,accumHours,accumFreq)
      elif display=="Expected Value":
         self.expectedValue(parmList,cycleList,modelList,obsmodel,fcstrList,
                             fhrStart,fhrEnd,dateType,numDays,fromDay,dayList,
                             dateStyle,scale,commonCases,accumHours,accumFreq)
      elif display=="Scatterplot":
         self.scatterPlot(parmList,cycleList,modelList,obsmodel,fcstrList,
                             fhrStart,fhrEnd,dateType,numDays,fromDay,dayList,
                             dateStyle,scale,commonCases,accumHours,accumFreq)
      return
   #==================================================================
   # errorHistogram - display error histogram
   #
   #
   def errorHistogram(self,parmList,cycleList,modelList,obsmodel,
                      fcstrList,fhrStart,fhrEnd,dateType,numDays,fromDay,
                      dayList,dateStyle,scale,commonCases,accumHours,
                      accumFreq):
      #
      #
      #  Clear display - setup title
      #
      parm=parmList[0]
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Error Histogram - %s"%parm)
      #
      #  Start 'working' display
      #
      workStart="Working on error histogram"
      self.startWorking(workStart,optionRemove=0)
      #
      #
      #
      NUMTBUTTONS=12  # normal number of time buttons on a row - configure
      NUMMBUTTONS=6   # normal number of model buttons on a row - configure
      #
      #  get the active EditArea into ea.  If the active edit area is
      #  None - then assume they want to run it over the entire grid
      #
      editArea=self.getActiveEditArea()
      editAreaMask=self.encodeEditArea(editArea)
      npts=add.reduce(add.reduce(editAreaMask))
      if (npts==0):
         editArea.invert()
      ea=self.encodeEditArea(editArea)
      eaflat=ravel(ea)
      totalpoints=add.reduce(eaflat)
      #
      #  make space for saving data
      #
      self.histograms={}  # storage for histograms for each model/forecast hour
      self.histoWorseLow={}
      self.histoWorseHigh={}
      self.numCases={}
      self.errSums={}
      self.errSumSquareds={}
      self.errSumAbs={}
      #
      #
      #
      totaliters=len(modelList)
      iter=0
      #
      #  For vectors...the parm to read might be different than
      #  the name of the parm
      #
      readParm=parm
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if ((last3=="Spd")or(last3=="Dir")):
            readParm=parm[:-3]
      #
      #  Get information about the parm we are reading
      #
      (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
       parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
      obsParm=self.VU.getObsParm(readParm)
      verType=self.VU.getVerType(readParm)
      datatype=self.VU.getVerParmType(readParm)
      #
      #  get binwidth and bigerr for parm...but for vectors its
      #  complicated by dir/mag/vecerr options
      #
      binwidth=self.VU.getVerBinWidth(readParm)
      bigerr=self.VU.getVerBigErr(readParm)
      if datatype==1:
         (bwMag,bwDir)=binwidth
         (beMag,beDir)=bigerr
         if last3=="Dir":
            binwidth=bwDir
            bigerr=beDir
         else:
            binwidth=bwMag
            bigerr=beMag
      (binmin,binmax)=self.getBins(binwidth,bigerr)
      self.histosetup(-bigerr,bigerr,binwidth)

      nbin=len(binmin)
      nbins=reshape(arange(nbin),(nbin,1))
      abinmin=reshape(array(binmin),(nbin,1))
      abinmax=reshape(array(binmax),(nbin,1))
      #
      #  Get mode for reading obs grids
      #
      obsGridMode=self.getReadMode(obsmodel,obsParm,0)
      #
      #  Get case times/records for all models
      #
      caseInfo=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                             dateStyle,dateType,fromDay=fromDay,
                             numDays=numDays,dayList=dayList,
                             fcstrs=fcstrList,cycles=cycleList,
                             fhrStart=fhrStart,fhrEnd=fhrEnd,
                             accumHours=accumHours,accumFreq=accumFreq,
                             commonCases=commonCases,basetimeOffsets=1,
                             callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      #
      #  Loop over each model
      #
      for model in modelList:
         iter+=1
         workNow=workStart+":%s (%d of %d)"%(model,iter,totaliters)
         #
         fcstGridMode=self.getReadMode(model,readParm)
         #
         #  Get all the cases for this model
         #
         cases=caseInfo[model]
         #
         #  Sort cases by the start time, not the basetime
         #
         casekeys=cases.keys()
         casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
         totalcount=len(casekeys)
         self.VU.logMsg("reading %d cases for %s"%(totalcount,model),1)
         count=0
         lastobs=""
         for key in casekeys:
            count+=1
            self.VU.logMsg("%s : %s"%(model,key),10)
            if self.setAndCheckWorking("%s: %d of %d"%(workNow,count,totalcount))==1:
               self.stopWorking()
               return
            (basetimestr,stimestr,etimestr)=key.split(",")
            basetime=int(basetimestr)
            stime=int(stimestr)
            etime=int(etimestr)
            (frecList,orecList)=cases[key]
            #
            #  Dont make stats for obs not yet complete
            #
            if etime>time.time():
               continue
            #
            #  Dont include negative forecast hours
            #
            fhr=self.VU.getFcstHour(basetime,stime)
            if fhr<0:
               continue
            #
            #  string to store grid under depends on model and forecast hour
            #
            savekey="%s-%3.3d"%(model,fhr)
            #
            #  If a new and different obs time - read the obs data
            #
            obskey=key.split(",",1)[1]
            if obskey!=lastobs:
               self.VU.logMsg("new Obs grid",10)
               obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                           stime,etime,mode=obsGridMode,
                                           recList=orecList)
               obsdata=self.scaleGrid(obsdata,scale,datatype)
               #
               #  For probabilistic variables...calculate the
               #  observed 'yes/no' value
               #
               if verType==1:
                  obsdata=self.getProbVerGrid(readParm,obsdata)
               #
               #
               #
               if ((datatype!=1)or(last3 in ["Spd","Dir"])):
                  if last3=="Spd":
                     obsgrid=obsdata[0]
                  elif last3=="Dir":
                     obsgrid=obsdata[1]
                  else:
                     obsgrid=obsdata
                  obsonly=compress(eaflat,ravel(obsgrid))
               else:
                  (u,v)=self.MagDirToUV(obsdata[0],obsdata[1])
                  obsuonly=compress(eaflat,ravel(u))
                  obsvonly=compress(eaflat,ravel(v))
               #
               #  save the last obskey that we have read so that
               #  we don't read it again many times
               #
               lastobs=obskey
            #
            #  Read forecast grid
            #
            fcstdata=self.VU.getVerGrids(model,basetime,readParm,
                                         stime,etime,mode=fcstGridMode,
                                         recList=frecList)
            fcstdata=self.scaleGrid(fcstdata,scale,datatype)
            #
            #  Get the error, handling vector error, etc.
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  fcstgrid=fcstdata[0]
               elif last3=="Dir":
                  fcstgrid=fcstdata[1]
               else:
                  fcstgrid=fcstdata
               fcstonly=compress(eaflat,ravel(fcstgrid))
               erronly=fcstonly-obsonly
               if last3=="Dir":
                  erronly=where(greater(erronly,180.0),360.0-erronly,erronly)
                  erronly=where(less(erronly,-180.0),-(360.0+erronly),erronly)
            else:
               (fcstmag,fcstdir)=fcstdata
               (u,v)=self.MagDirToUV(fcstmag,fcstdir)
               uonly=compress(eaflat,ravel(u))
               vonly=compress(eaflat,ravel(v))
               uerr=uonly-obsuonly
               verr=vonly-obsvonly
               (mag,direc)=self.UVToMagDir(uerr,verr)
               erronly=mag
            #
            #  make histograms
            #
            (errCount,worseLow,worseHigh)=self.histo(erronly)
            errSum=add.reduce(erronly)
            errSumSquared=add.reduce(erronly*erronly)
            errabs=abs(erronly)
            errSumAb=add.reduce(errabs)
            if self.histograms.has_key(savekey):
               self.histograms[savekey]+=errCount
               self.histoWorseLow[savekey]+=worseLow
               self.histoWorseHigh[savekey]+=worseHigh
               self.errSums[savekey]+=errSum
               self.errSumSquareds[savekey]+=errSumSquared
               self.errSumAbs[savekey]+=errSumAb
               self.numCases[savekey]+=1
            else:
               self.histograms[savekey]=errCount
               self.histoWorseLow[savekey]=worseLow
               self.histoWorseHigh[savekey]=worseHigh
               self.errSums[savekey]=errSum
               self.errSumSquareds[savekey]=errSumSquared
               self.errSumAbs[savekey]=errSumAb
               self.numCases[savekey]=1
      #
      #  Get all "model-fhr" keys we saved
      #
      fullkeys=self.histograms.keys()
      #
      #  if no data could be read - stop here
      #
      if len(fullkeys)<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return
      #
      #  For buttons...get models/forecasthours actually in the data
      #
      fullkeys.sort()
      fhrstrs=[]
      modkeys=[]
      for fullkey in fullkeys:
         (mod,fhrstr)=fullkey.split("-")
         if fhrstr not in fhrstrs:
            fhrstrs.append(fhrstr)
         if mod not in modkeys:
            modkeys.append(mod)
      #
      #  Change fhrstrs (sorted on 3-character 000-999) into
      #  smaller fhrkeys that are NOT all 3-characters wide
      #
      fhrstrs.sort()
      fhrkeys=[]
      for fhrstr in fhrstrs:
         fhrkeys.append("%d"%int(fhrstr))
      #
      #  If an Official button is in there...make it first
      #
      modkeys.sort()
      if "Official" in modkeys:
         idx=modkeys.index("Official")
         del modkeys[idx]
         modkeys.insert(0,"Official")
      #
      #  set colors for each model
      #
      self.colornames={}
      index=0
      for mod in modkeys:
         self.colornames[mod]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  Setup first row of buttons (forecast hours)
      #
      self.setupBut1(fhrkeys,numbuttons=NUMTBUTTONS,arrows=1,width=3)
      #
      #  Setup second row of buttons (models)
      #
      self.setupBut2(modkeys,numbuttons=NUMMBUTTONS,arrows=1)
      #
      #  find max number in any bin in any of the histograms
      #
      histkey1=self.histograms.keys()[0]
      maxHist=zeros(self.histograms[histkey1].shape)
      for histkey in self.histograms.keys():
         self.histograms[histkey]/=float(self.numCases[histkey])
         maxHist=maximum(maxHist,self.histograms[histkey])
      fullmax=maximum.reduce(maxHist)
      #
      #  Find good tickmark interval for vertical axis and set the
      #  vertical range to be one tick mark above the fullmax (max
      #  number in any histogram)
      #
      numticks=10
      tickInterval=self.niceNumDec(fullmax/(numticks-1),1)
      graphmax=(int(fullmax/tickInterval)+1)*tickInterval
      #
      #
      #  Setup graphing coordinates
      #
      minx=-bigerr
      maxx=bigerr
      maxscore=maxx/2.0           
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(100.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(minx,maxx,0.0,graphmax,left,right,bot,top)
      self.histoaxes(graphmax,-bigerr,bigerr,binwidth,tickInterval)
      #
      #  Draw each histogram
      #
      totalcount=len(self.histograms.keys())
      count=0
      for key in self.histograms.keys():
         count+=1
         if self.setAndCheckWorking("%s: drawing histogram %d of %d"%(workStart,count,totalcount))==1:
            self.stopWorking()
            return
         tagbase=key.split("-")
         mod=tagbase[0]
         fhr=int(tagbase[1])
         fhrstr="f%d"%fhr
         tagtuple=(mod,fhrstr)
         flabel="%d-hr forecast"%fhr
         self.labelLine(flabel,3,justify="right",tags=tagtuple)
             
         colorname=self.colornames[mod]
         bins=self.histograms[key]
         nbin=bins.shape[0]
         for i in xrange(nbin):
            y=bins[i]
            x1=self.histomin+(i*self.histowidth)
            x2=x1+self.histowidth
            if y>0:
               if i==0:
                   (sx1,sy1)=self.graphcoord(x1,0)
               else:
                   (sx1,sy1)=self.graphcoord(x1,bins[i-1])
               (sx2,sy2)=self.graphcoord(x1,y)
               (sx3,sy3)=self.graphcoord(x2,y)
               if ((i+1)==nbin):
                  (sx4,sy4)=self.graphcoord(x2,0)
                  self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
               elif bins[i+1]==0:
                  (sx4,sy4)=self.graphcoord(x2,0)
                  self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
               else:
                  self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,fill=colorname,tags=tagtuple)
         self.but2state[mod]=1
         self.but1state[fhrstr]=1
         lowfcst=self.histoWorseLow[key]
         highfcst=self.histoWorseHigh[key]
         self.showWorse(lowfcst,highfcst,bigerr,15,colorname,tagtuple)
         allpts=self.numCases[key]*totalpoints
         avg=self.errSums[key]/allpts
         mae=self.errSumAbs[key]/allpts
         std=sqrt((self.errSumSquareds[key]/allpts)-(avg*avg))
         rms=sqrt(self.errSumSquareds[key]/allpts)
         self.showAvg(avg,colorname,tagtuple)
         modnum=modkeys.index(mod)
         self.showScores(modnum,mod,self.numCases[key],avg,std,mae,rms,colorname,tagtuple)
         score=100.0-(self.errSumSquareds[key]/allpts)
         self.showScore(score,mod,colorname,tagtuple)
      #
      #  Show first time/model
      #
      startBut1(self)
      startBut2(self)
      #
      #  Label top of graph
      #
      (x,y)=self.graphcoord(0,graphmax)
      self.cd.canvas.create_text(x,y-5,text="Gridpoints per case",fill="black",anchor=Tkinter.S)
      #
      #  Labels
      #
      ul1="Histogram - %s"%parm
      self.cdLabels(ul1,totalpoints,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      #
      #  Bin width
      #
      if binwidth<1.0:
         str="Bin width: %3.1f"%binwidth
      else:
         str="Bin width: %d"%binwidth
      self.labelLine(str,3,justify="left")
      #
      #  table labels
      #
      x=self.cd.curwidth*(80.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="Model",anchor=Tkinter.E,fill="black")
      x=self.cd.curwidth*(130.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="Cases",anchor=Tkinter.E,fill="black")
      x=self.cd.curwidth*(170.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="Avg",anchor=Tkinter.E,fill="black")
      x=self.cd.curwidth*(210.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="Std",anchor=Tkinter.E,fill="black")
      x=self.cd.curwidth*(250.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="MAE",anchor=Tkinter.E,fill="black")
      x=self.cd.curwidth*(290.0/700.0)
      y=self.cd.curheight*(130.0/530.0)
      self.cd.canvas.create_text(x,y,text="RMS",anchor=Tkinter.E,fill="black")
      #
      #  Color Bar
      #
      midx=self.cd.curwidth/2.0
      for i in xrange(0,256):
         x=midx-128+i
         y=50
         colorstr="#%02x%02x00"%(255-i,i)
         self.cd.canvas.create_line(x,y-3,x,y+3,fill=colorstr)
      self.cd.canvas.create_text(midx-128-5,50,text="Bad",anchor=Tkinter.E)
      self.cd.canvas.create_text(midx+128+5,50,text="Good",anchor=Tkinter.W)
         
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      return
   #==================================================================
   # valueHistogram - display value histogram
   #
   #
   def valueHistogram(self,parmList,cycleList,modelList,obsmodel,
                      fcstrList,fhrStart,fhrEnd,dateType,numDays,fromDay,
                      dayList,dateStyle,scale,commonCases,accumHours,
                      accumFreq):
      #
      #  Clear display - setup title
      #
      parm=parmList[0]
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Value Histogram - %s"%parm)
      #
      #
      #
      workStart="Working on value histogram"
      self.startWorking(workStart,optionRemove=0)
      #
      #
      #
      NUMTBUTTONS=12  # normal number of time buttons on a row - configure
      NUMMBUTTONS=6   # normal number of model buttons on a row - configure
      #
      #  get the active EditArea into ea.  If the active edit area is
      #  None - then assume they want to run it over the entire grid
      #
      editArea=self.getActiveEditArea()
      editAreaMask=self.encodeEditArea(editArea)
      npts=add.reduce(add.reduce(editAreaMask))
      if (npts==0):
         editArea.invert()
      ea=self.encodeEditArea(editArea)
      eaflat=ravel(ea)
      totalpoints=add.reduce(eaflat)
      #
      #  make space for saving data
      #
      self.histograms={}  # storage for histograms for each model/forecast hour
      self.numCases={}
      #
      #  Loop over parm and model
      #
      totaliters=len(modelList)
      iter=0
      #
      #  For vectors...the parm to read might be different than
      #  the name of the parm
      #
      readParm=parm
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if ((last3=="Spd")or(last3=="Dir")):
            readParm=parm[:-3]
      #
      #  Get information about the parm we are reading
      #
      (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
       parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
      obsParm=self.VU.getObsParm(readParm)
      verType=self.VU.getVerType(readParm)
      datatype=self.VU.getVerParmType(readParm)
      if ((datatype==1)and(last3=="Dir")):
         parmMinval=0
         parmMaxval=360
      #
      #  get binwidth and bigerr for parm...but for vectors its
      #  complicated by dir/mag/vecerr options
      #
      binwidth=self.VU.getVerBinWidth(readParm)
      if datatype==1:
         (bwMag,bwDir)=binwidth
         if last3=="Dir":
            binwidth=bwDir
         else:
            binwidth=bwMag
      #
      #  Setup histogram binning routines
      #
      self.histosetup(parmMinval,parmMaxval,binwidth)
      #
      #  Get mode for reading obs grids
      #
      obsGridMode=self.getReadMode(obsmodel,obsParm,0)
      #
      #  Get case times/records for all models
      #
      caseInfo=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                             dateStyle,dateType,fromDay=fromDay,
                             numDays=numDays,dayList=dayList,
                             fcstrs=fcstrList,cycles=cycleList,
                             fhrStart=fhrStart,fhrEnd=fhrEnd,
                             accumHours=accumHours,accumFreq=accumFreq,
                             commonCases=commonCases,basetimeOffsets=1,
                             callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      #
      #  Loop over each model
      #
      for model in modelList:
         iter+=1
         workNow=workStart+":%s (%d of %d)"%(model,iter,totaliters)
         #
         fcstGridMode=self.getReadMode(model,readParm)
         #
         #  Get all the cases for this model
         #
         cases=caseInfo[model]
         #
         #  Sort cases by the start/end time, not the basetime
         #
         casekeys=cases.keys()
         casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
         totalcount=len(casekeys)
         self.VU.logMsg("reading %d cases for %s"%(totalcount,model),1)
         count=0
         lastobs=""
         for key in casekeys:
            count+=1
            self.VU.logMsg("%s : %s"%(model,key),10)
            if self.setAndCheckWorking("%s: %d of %d"%(workNow,count,totalcount))==1:
               self.stopWorking()
               return
            (basetimestr,stimestr,etimestr)=key.split(",")
            basetime=int(basetimestr)
            stime=int(stimestr)
            etime=int(etimestr)
            (frecList,orecList)=cases[key]
            #
            #  Dont make stats for obs not yet complete
            #
            if etime>time.time():
               continue
            #
            #  Dont include negative forecast hours
            #
            fhr=self.VU.getFcstHour(basetime,stime)
            if fhr<0:
               continue
            #
            #  string to store grid under depends on model and forecast hour
            #
            saveKey="%s-%3.3d"%(model,fhr)
            #
            #  If a new and different obs time - read the obs data
            #
            obskey=key.split(",",1)[1]
            if obskey!=lastobs:
               self.VU.logMsg("new Obs grid",10)
               obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                           stime,etime,mode=obsGridMode,
                                           recList=orecList)
               obsdata=self.scaleGrid(obsdata,scale,datatype)
               #
               #  For probabilistic variables...calculate the
               #  observed 'yes/no' value
               #
               if verType==1:
                  obsdata=self.getProbVerGrid(readParm,obsdata)*100.0
               #
               #  cant do a value histogram of vector wind
               #  errors...so those get changed to windSpd
               #
               if ((datatype!=1)or(last3 in ["Spd","Dir"])):
                  if last3=="Spd":
                     obsgrid=obsdata[0]
                  elif last3=="Dir":
                     obsgrid=obsdata[1]
                  else:
                     obsgrid=obsdata
                  obsonly=compress(eaflat,ravel(obsgrid))
               else:
                  obsgrid=obsdata[0]
               obsonly=compress(eaflat,ravel(obsgrid))
               (obsCount,below,above)=self.histo(obsonly)
               lastobs=obskey
            #
            #  Add observed bin counts to counts for same model/fhr
            #
            obsSaveKey="%s-%3.3d"%(obsmodel,fhr)
            if self.histograms.has_key(obsSaveKey):
               self.histograms[obsSaveKey]+=obsCount
               self.numCases[obsSaveKey]+=1
            else:
               self.histograms[obsSaveKey]=obsCount
               self.numCases[obsSaveKey]=1
            #
            #  Read forecast grid and calculate error grid
            #
            fcstdata=self.VU.getVerGrids(model,basetime,readParm,
                                         stime,etime,mode=fcstGridMode,
                                         recList=frecList)
            fcstdata=self.scaleGrid(fcstdata,scale,datatype)
            #
            #  Get the error, handling vector error, etc.
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  fcstgrid=fcstdata[0]
               elif last3=="Dir":
                  fcstgrid=fcstdata[1]
               else:
                  fcstgrid=fcstdata
               fcstonly=compress(eaflat,ravel(fcstgrid))
            else:
               fcstgrid=fcstdata[0]
               fcstonly=compress(eaflat,ravel(fcstgrid))
            #
            #  bin the forecast values
            #
            (valCount,below,above)=self.histo(fcstonly)
            #
            #  Add bin counts to counts for same model/fhr
            #
            if self.histograms.has_key(saveKey):
               self.histograms[saveKey]+=valCount
               self.numCases[saveKey]+=1
            else:
               self.histograms[saveKey]=valCount
               self.numCases[saveKey]=1
      #
      #  Get all the keys that will be displayed - we've been storing in
      #  different places for different things
      #
      fullkeys=self.histograms.keys()
      #
      #  if no data could be read - stop here
      #
      if len(fullkeys)<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return
      #
      #  For buttons...get models/forecasthours actually in the data
      #
      fullkeys.sort()
      fhrstrs=[]
      modkeys=[]
      for fullkey in fullkeys:
         (mod,fhrstr)=fullkey.split("-")
         if fhrstr not in fhrstrs:
            fhrstrs.append(fhrstr)
         if mod not in modkeys:
            modkeys.append(mod)
      #
      #  Change fhrstrs (sorted on 3-character 000-999) into
      #  smaller fhrkeys that are NOT all 3-characters wide
      #
      fhrstrs.sort()
      fhrkeys=[]
      for fhrstr in fhrstrs:
         fhrkeys.append("%d"%int(fhrstr))
      #
      #  If an Official button is in there...make it first
      #
      modkeys.sort()
      if "Official" in modkeys:
         idx=modkeys.index("Official")
         del modkeys[idx]
         modkeys.insert(0,"Official")
      #
      #  Put the observed one last
      #
      if obsmodel in modkeys:
         idx=modkeys.index(obsmodel)
         del modkeys[idx]
         modkeys.append(obsmodel)
      #
      #  set colors for each model
      #
      self.colornames={}
      index=0
      for mod in modkeys:
         self.colornames[mod]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  Setup first row of buttons (forecast hours)
      #
      self.setupBut1(fhrkeys,numbuttons=NUMTBUTTONS,arrows=1,width=3)
      #
      #  Setup second row of buttons (models)
      #
      self.setupBut2(modkeys,numbuttons=NUMMBUTTONS,arrows=1)
      #
      #  Get min/max of forecast/observed values that need to be shown
      #
      fullmin=999999.0
      fullmax=-999999.0
      self.setWorking("%s: getting max/min"%(workStart))
      tothisto=zeros((self.histonumbins,))
      maxvalue=zeros((self.histonumbins,))
      minvalue=zeros((self.histonumbins,))+9999999.0
      for key in self.histograms.keys():
         tothisto+=self.histograms[key]
         nums=self.histograms[key]/float(self.numCases[key])
         maxvalue=maximum(maxvalue,nums)
         minvalue=where(greater(nums,0.0),minimum(minvalue,nums),minvalue)
      for i in xrange(self.histonumbins):
         minval=self.histomin+(i*self.histowidth)
         maxval=minval+self.histowidth
         print "%3d %5.3f--%5.3f %d"%(i,minval,maxval,tothisto[i])
         if tothisto[i]>0:
            fullmin=min(minval,fullmin)
            fullmax=max(maxval,fullmax)
      #print "   fullmin:",fullmin
      #print "   fullmax:",fullmax
      #fullmin=0.025
      #fullmax=2.025
      #print "   fullmin:",fullmin
      #print "   fullmax:",fullmax
      #
      #  If not many bins shown (i.e. nearly constant values)...add bins
      #  up and down until we get 15 bins - so our values are 'centered' in
      #  a reaonably wide graph
      #
      numbins=float(fullmax-fullmin-self.histowidth)/float(self.histowidth)
      if numbins<15:
         while numbins<15:
            fullmax=minimum(fullmax+self.histowidth,parmMaxval+self.histohalf)
            fullmin=maximum(fullmin-self.histowidth,parmMinval-self.histohalf)
            numbins=float(fullmax-fullmin-self.histowidth)/float(self.histowidth)
            if ((numbins<15)and(fullmin<parmMinval)and(fullmax>parmMaxval)):
               numbins=16
      #print "   fullmin:",fullmin
      #print "   fullmax:",fullmax
      #
      #
      #
      numticks=25
      tickInterval=self.niceNumDec((fullmax-fullmin)/float(numticks-1),1)
      print "the tickInterval with 25 desired is:",tickInterval
      #
      #  Dont let tick interval be smaller than parm precision
      #
      mintick=10**(-parmPrecision)
      tickInterval=max(tickInterval,mintick)
      print "after checking against precision...tickInterval is:",tickInterval
      #
      #  Set the minimum graph one tick interval below the minimum...but
      #  not below the parm minimum value
      #
      graphmin=(floor(float(fullmin)/float(tickInterval))-1)*tickInterval
      graphmin=maximum(graphmin,parmMinval)
      #
      #  Set the maximum graph one tick interval above the maximum...but
      #  not above the parm maximum value
      #
      graphmax=(floor(float(fullmax)/float(tickInterval))+2)*tickInterval
      graphmax=minimum(graphmax,parmMaxval)
      print "so final x-coordinate graph from min/max:",graphmin,graphmax
      #
      #  Find the maximum Y value for the bins being displayed
      #
      maxnum=0
      minnum=999999
      for i in xrange(self.histonumbins):
         minval=self.histomin+(i*self.histowidth)
         maxval=minval+self.histowidth
         if ((minval>=fullmin)and(maxval<=fullmax)):
            testmax=maxvalue[i]
            maxnum=max(maxnum,testmax)
            testmin=minvalue[i]
            minnum=min(minnum,testmin)
      print "the maximum value to display is:",maxnum
      print "the minimum value to display is:",minnum
      vint=self.niceNumDec(maxnum/20,1)
      print "the vertical tick interval: vint:",vint
      maxnum=(int(float(maxnum)/float(vint))+1)*vint
      print "the maxnumber to graph is:",maxnum
      #
      #
      #     
      #
      left=self.cd.curwidth*(175.0/700.0)
      right=self.cd.curwidth*(525.0/700.0)
      bot=self.cd.curheight*(130.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      if ((verType==1)or(parmRateFlag==1)):
         logflag=1
         logmax=log(maxnum)
         logmin=log(minnum)
         print "old min/max=%f,%f"%(minnum,maxnum)
         print "new log range: %7.3f %7.3f"%(logmin,logmax)
         self.setgraph(graphmin,graphmax,logmin,logmax,left,right,bot,top)
         self.logvalhaxes(graphmin,graphmax,tickInterval,logmin,logmax,parm)
      else:
         logflag=0
         self.setgraph(graphmin,graphmax,0,maxnum,left,right,bot,top)
         self.valhaxes(graphmin,graphmax,tickInterval,maxnum,vint,parm)
         
         
      ul1="Value Histogram - %s"%parm
      self.cdLabels(ul1,totalpoints,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      #
      #  Draw each histogram
      #
      totalcount=len(self.histograms.keys())
      count=0
      for key in self.histograms.keys():
         count+=1
         if self.setAndCheckWorking("%s: drawing histogram %d of %d"%(workStart,count,totalcount))==1:
            self.stopWorking()
            return
         tagbase=key.split("-")
         mod=tagbase[0]
         fhr=int(tagbase[1])
         fhrstr="f%d"%fhr
         tagtuple=(mod,fhrstr)
         flabel="%d-hr forecast"%fhr
         self.labelLine(flabel,3,justify="right",tags=tagtuple)
             
         colorname=self.colornames[mod]
         bins=self.histograms[key]/float(self.numCases[key])
         nbin=bins.shape[0]
         for i in xrange(nbin):
            #
            #  get x-coords of bin, and ignore bins outside the range
            #  of x-coordinates that we are showing
            #
            x1=max(self.histomin+(i*self.histowidth),graphmin)
            x2=min(self.histomin+((i+1)*self.histowidth),graphmax)
            if x1<fullmin:
               continue
            if x2>fullmax:
               continue
            #
            #  Logarithmic y-values a little different
            #
            if logflag==1:
               y=bins[i]
               if y>0.0:
                  logy=log(y)
                  logy=min(logy,logmax)
                  if i==0:
                     (sx1,sy1)=self.graphcoord(x1,logmin)
                  else:
                     yold=bins[i-1]
                     if yold>0.0:
                        logyold=log(yold)
                     else:
                        logyold=logmin
                     (sx1,sy1)=self.graphcoord(x1,logyold)
                  (sx2,sy2)=self.graphcoord(x1,logy)
                  (sx3,sy3)=self.graphcoord(x2,logy)
                  if ((i+1)==nbin):
                     (sx4,sy4)=self.graphcoord(x2,logmin)
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
                  elif bins[i+1]==0:
                     (sx4,sy4)=self.graphcoord(x2,logmin)
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
                  else:
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,fill=colorname,tags=tagtuple)
            #
            #  Normal graphing for non-logarithmic y-values
            #
            else:
               y=bins[i]
               if y>0:
                  y=min(y,maxnum)
                  if i==0:
                     (sx1,sy1)=self.graphcoord(x1,0)
                  else:
                     yold=min(bins[i-1],maxnum)
                     (sx1,sy1)=self.graphcoord(x1,yold)
                  (sx2,sy2)=self.graphcoord(x1,y)
                  (sx3,sy3)=self.graphcoord(x2,y)
                  if ((i+1)==nbin):
                     (sx4,sy4)=self.graphcoord(x2,0)
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
                  elif bins[i+1]==0:
                     (sx4,sy4)=self.graphcoord(x2,0)
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,fill=colorname,tags=tagtuple)
                  else:
                     self.cd.canvas.create_line(sx1,sy1,sx2,sy2,sx3,sy3,fill=colorname,tags=tagtuple)
         self.but2state[mod]=1
         self.but1state[fhrstr]=1
                   
 
      startBut1(self)
      startBut2(self)
         
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      return
   #==================================================================
   # expectedValue - display expected value for forecast values
   #
   #
   def expectedValue(self,parmList,cycleList,modelList,obsmodel,
                      fcstrList,fhrStart,fhrEnd,dateType,numDays,fromDay,
                      dayList,dateStyle,scale,commonCases,accumHours,
                      accumFreq):
      #
      #  Clear display - setup title
      #
      parm=parmList[0]
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Expected Value Distribution - %s"%parm)
      #
      #
      #
      workStart="Working on Expected Value Distribution"
      self.startWorking(workStart,optionRemove=0)
      #
      #
      #
      NUMTBUTTONS=12  # normal number of time buttons on a row - configure
      NUMMBUTTONS=6   # normal number of model buttons on a row - configure
      #
      #  get the active EditArea into ea.  If the active edit area is
      #  None - then assume they want to run it over the entire grid
      #
      editArea=self.getActiveEditArea()
      editAreaMask=self.encodeEditArea(editArea)
      npts=add.reduce(add.reduce(editAreaMask))
      if (npts==0):
         editArea.invert()
         editAreaMask=self.encodeEditArea(editArea)
      eaflat=ravel(editAreaMask)
      totalpoints=add.reduce(eaflat)
      #
      #  make space for saving data
      #
      self.flists={} # storage for fcst values for each model/forecast hour
      self.olists={} # storage for obs  values for each model/forecast hour
      fullmin=999999.0
      fullmax=-999999.0
      #
      #  Loop over parm and model
      #
      totaliters=len(modelList)
      iter=0
      #
      #  For vectors...the parm to read might be different than
      #  the name of the parm
      #
      readParm=parm
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if ((last3=="Spd")or(last3=="Dir")):
            readParm=parm[:-3]
      #
      #  Get information about the parm we are reading
      #
      (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
       parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
      obsParm=self.VU.getObsParm(readParm)
      verType=self.VU.getVerType(readParm)
      datatype=self.VU.getVerParmType(readParm)
      if ((datatype==1)and(last3=="Dir")):
         parmMinval=0
         parmMaxval=360
      #
      #  get binwidth and bigerr for parm...but for vectors its
      #  complicated by dir/mag/vecerr options
      #
      binwidth=self.VU.getVerBinWidth(readParm)
      if datatype==1:
         (bwMag,bwDir)=binwidth
         if last3=="Dir":
            binwidth=bwDir
         else:
            binwidth=bwMag
      #
      #  Setup histogram binning routines
      #
      self.histosetup(parmMinval,parmMaxval,binwidth)
      #
      #  Get mode for reading obs grids
      #
      obsGridMode=self.getReadMode(obsmodel,obsParm,0)
      #
      #  Get case times/records for all models
      #
      caseInfo=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                             dateStyle,dateType,fromDay=fromDay,
                             numDays=numDays,dayList=dayList,
                             fcstrs=fcstrList,cycles=cycleList,
                             fhrStart=fhrStart,fhrEnd=fhrEnd,
                             accumHours=accumHours,accumFreq=accumFreq,
                             commonCases=commonCases,basetimeOffsets=1,
                             callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      #
      #  Loop over each model
      #
      for model in modelList:
         iter+=1
         workNow=workStart+":%s (%d of %d)"%(model,iter,totaliters)
         #
         fcstGridMode=self.getReadMode(model,readParm)
         #
         #  Get all the cases for this model
         #
         cases=caseInfo[model]
         #
         #  Sort cases by the start/end time, not the basetime
         #
         casekeys=cases.keys()
         casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
         totalcount=len(casekeys)
         self.VU.logMsg("reading %d cases for %s"%(totalcount,model),1)
         count=0
         lastobs=""
         for key in casekeys:
            count+=1
            #self.VU.logMsg("%s : %s   memory:%d  resident:%d"%(model,key,memory(),resident()))
            if self.setAndCheckWorking("%s: %d of %d"%(workNow,count,totalcount))==1:
               self.stopWorking()
               return
            (basetimestr,stimestr,etimestr)=key.split(",")
            basetime=int(basetimestr)
            stime=int(stimestr)
            etime=int(etimestr)
            (frecList,orecList)=cases[key]
            #
            #  Dont make stats for obs not yet complete
            #
            if etime>time.time():
               continue
            #
            #  Dont include negative forecast hours
            #
            fhr=self.VU.getFcstHour(basetime,stime)
            if fhr<0:
               continue
            #
            #  string to store grid under depends on model and forecast hour
            #
            saveKey="%s-%3.3d"%(model,fhr)
            #
            #  If a new and different obs time - read the obs data
            #
            obskey=key.split(",",1)[1]
            #self.VU.logMsg("before   getObs: %d %d"%(memory(),resident()))
            
            obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                        stime,etime,mode=obsGridMode,
                                        recList=orecList)
            #obsdata1=copy.copy(obsdata)
            #del obsdata
            obsdata1=obsdata
            obsdata1=self.scaleGrid(obsdata1,scale,datatype)
            #self.VU.logMsg("after   scaling: %d %d"%(memory(),resident()))
            #
            #  For probabilistic variables...calculate the
            #  observed 'yes/no' value
            #
            if verType==1:
               obsdata1=self.getProbVerGrid(readParm,obsdata1)*100.0
            #self.VU.logMsg("after   probing: %d %d"%(memory(),resident()))
            #
            #  cant do a value histogram of vector wind
            #  errors...so those get changed to windSpd
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  obsgrid=obsdata1[0]
               elif last3=="Dir":
                  obsgrid=obsdata1[1]
               else:
                  obsgrid=obsdata1
            else:
               obsgrid=obsdata1[0]
            obsonly=compress(eaflat,ravel(obsgrid))
            obsList=list(obsonly)
            del obsonly
            del obsgrid
            del obsdata1
            #self.VU.logMsg("down to obsList: %d %d"%(memory(),resident()))
            minObs=min(obsList)
            maxObs=max(obsList)
            fullmin=min(minObs,fullmin)
            fullmax=max(maxObs,fullmax)
            if self.olists.has_key(saveKey):
               self.olists[saveKey].extend(obsList)
               self.VU.logMsg("extending")
            else:
               self.olists[saveKey]=[]
               self.olists[saveKey].extend(obsList)
               self.VU.logMsg("new key")
            #self.VU.logMsg("after    adding: %d %d"%(memory(),resident()))
            del obsList
            #self.VU.logMsg("del of  obsList: %d %d"%(memory(),resident()))
            #
            #  Read forecast grid and calculate error grid
            #
            #self.VU.logMsg("before getGrids: %d %d"%(memory(),resident()))
            fcstdata=self.VU.getVerGrids(model,basetime,readParm,
                                         stime,etime,mode=fcstGridMode,
                                         recList=frecList)
            #self.VU.logMsg("after  getGrids: %d %d"%(memory(),resident()))
            #fcstdata1=copy.copy(fcstdata)
            #self.VU.logMsg("after      copy: %d %d"%(memory(),resident()))
            #del fcstdata
            #self.VU.logMsg("after       del: %d %d"%(memory(),resident()))
            fcstdata1=fcstdata
            fcstdata1=self.scaleGrid(fcstdata1,scale,datatype)
            #
            #  Get the error, handling vector error, etc.
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  fcstgrid=fcstdata1[0]
               elif last3=="Dir":
                  fcstgrid=fcstdata1[1]
               else:
                  fcstgrid=fcstdata1
            else:
               fcstgrid=fcstdata1[0]
            fcstonly=compress(eaflat,ravel(fcstgrid))
            self.VU.logMsg("pts to save:%s"%fcstonly.shape)
            del fcstgrid
            del fcstdata1
            fcstList=list(fcstonly)
            del fcstonly
            #self.VU.logMsg("after  fcstList: %d %d"%(memory(),resident()))
            minFcst=min(fcstList)
            maxFcst=max(fcstList)
            fullmin=min(minFcst,fullmin)
            fullmax=max(maxFcst,fullmax)
            #self.VU.logMsg("after maxmin   : %d %d"%(memory(),resident()))
            #
            #  Add values forecast lists for same model/fhr
            #
            #
            if self.flists.has_key(saveKey):
               self.flists[saveKey].extend(fcstList)
               self.VU.logMsg("extending")
            else:
               self.flists[saveKey]=[]
               self.flists[saveKey].extend(fcstList)
               self.VU.logMsg("new key")
            #self.VU.logMsg("after    saving: %d %d"%(memory(),resident()))
            del fcstList
            #self.VU.logMsg("after del fList: %d %d"%(memory(),resident()))
      #
      #  Get all the keys that will be displayed
      #
      fullkeys=self.flists.keys()
      #
      #  if no data could be read - stop here
      #
      if len(fullkeys)<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return
      #
      #  For buttons...get models/forecasthours actually in the data
      #
      fullkeys.sort()
      fhrstrs=[]
      modkeys=[]
      for fullkey in fullkeys:
         (mod,fhrstr)=fullkey.split("-")
         if fhrstr not in fhrstrs:
            fhrstrs.append(fhrstr)
         if mod not in modkeys:
            modkeys.append(mod)
      #
      #  Change fhrstrs (sorted on 3-character 000-999) into
      #  smaller fhrkeys that are NOT all 3-characters wide
      #
      fhrstrs.sort()
      fhrkeys=[]
      for fhrstr in fhrstrs:
         fhrkeys.append("%d"%int(fhrstr))
      #
      #  If an Official button is in there...make it first
      #
      modkeys.sort()
      if "Official" in modkeys:
         idx=modkeys.index("Official")
         del modkeys[idx]
         modkeys.insert(0,"Official")
      #
      #  set colors for each model
      #
      self.colornames={}
      index=0
      for mod in modkeys:
         self.colornames[mod]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  Setup first row of buttons (forecast hours)
      #
      self.setupBut1(fhrkeys,numbuttons=NUMTBUTTONS,arrows=1,width=3)
      #
      #  Setup second row of buttons (models)
      #
      self.setupBut2(modkeys,numbuttons=NUMMBUTTONS,arrows=1)
      #
      #  If not many bins shown (i.e. nearly constant values)...add bins
      #  up and down until we get at least 15 bins - so our values are
      #  'centered' in a reaonably wide graph
      #
      numbins=int(float(fullmax-fullmin)/float(binwidth))+1
      if numbins<15:
         while numbins<15:
            fullmax=min(fullmax+binwidth,parmMaxval)
            fullmin=max(fullmin-binwidth,parmMinval)
            numbins=int(float(fullmax-fullmin)/float(binwidth))+1
            if ((numbins<15)and(fullmin==parmMinval)and(fullmax==parmMaxval)):
               numbins=16
      #
      #
      #
      numticks=25
      tickInterval=self.niceNumDec((fullmax-fullmin)/float(numticks-1),1)
      #
      #  Dont let tick interval be smaller than parm precision
      #
      mintick=10**(-parmPrecision)
      tickInterval=max(tickInterval,mintick)
      #
      #  Set the minimum graph one tick interval below the minimum...but
      #  not below the parm minimum value
      #
      graphmin=(floor(float(fullmin)/float(tickInterval))-1)*tickInterval
      graphmin=maximum(graphmin,parmMinval)
      #
      #  Set the maximum graph one tick interval above the maximum...but
      #  not above the parm maximum value
      #
      graphmax=(floor(float(fullmax)/float(tickInterval))+2)*tickInterval
      graphmax=minimum(graphmax,parmMaxval)
      #
      #
      #
      numTicks=int(float(graphmax-graphmin)/float(tickInterval))+1
      #
      #  Set up the graph axes
      #
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(100.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(graphmin,graphmax,graphmin,graphmax,left,right,bot,top)
      self.expaxes(graphmin,graphmax,tickInterval)
      #
      #  Label the top of the graph
      #
      ul1="Expected %s Value for %s Forecast"%(obsmodel,parm)
      self.cdLabels(ul1,totalpoints,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      #
      #  for rateParms, or probability parms, label the length of periods
      #
      if ((verType==1)or(parmRateFlag==1)):
         self.labelLine("%d-hr periods"%accumHours,3)
      #
      #  Draw
      #
      totalcount=len(fullkeys)
      count=0
      for key in fullkeys:
         count+=1
         self.VU.logMsg("graph %d   memory:%d  resident:%d"%(count,memory(),resident()))
         if self.setAndCheckWorking("%s: drawing graph %d of %d"%(workStart,count,totalcount))==1:
            self.stopWorking()
            return
         tagbase=key.split("-")
         mod=tagbase[0]
         modnum=modelList.index(mod)
         fhr=int(tagbase[1])
         fhrstr="f%d"%fhr
         tagtuple=(mod,fhrstr)
         flabel="%d-hr forecast"%fhr
         self.labelLine(flabel,3,justify="right",tags=tagtuple)
       
         colorname=self.colornames[mod]
         #
         #  Turn lists for this model/time back into arrays
         #
         fcstArray=array(self.flists[key])
         obsArray=array(self.olists[key])
         prevAvg=-99999.9
         for i in xrange(numTicks):
            value=graphmin+(i*tickInterval)
            valuelow=value-(float(tickInterval)/2.0)
            vl1=value-(float(tickInterval)/6.0)
            valuehigh=value+(float(tickInterval)/2.0)
            vh1=value+(float(tickInterval)/6.0)
            pts=logical_and(greater_equal(fcstArray,valuelow),less(fcstArray,valuehigh))
            if sometrue(pts):
               #obsDist=sort(compress(pts,obsArray))
               obsDist=compress(pts,obsArray)
               numCases=obsDist.shape[0]
               #minObs=obsDist[0]
               minObs=minimum.reduce(obsDist)
               #maxObs=obsDist[numCases-1]
               maxObs=maximum.reduce(obsDist)
               avgObs=float(add.reduce(obsDist))/float(numCases)
               avgObsSquared=float(add.reduce(obsDist*obsDist))/float(numCases)
               std=sqrt(avgObsSquared-(avgObs*avgObs))
               #if numCases>1:
               #   midObs=obsDist[numCases/2]
               #else:
               #   midObs=avgObs
               #if numCases>3:
               #   q1Obs=obsDist[numCases/4]
               #   q3Obs=obsDist[(3*numCases)/4]
               #else:
               #   q1Obs=avgObs
               #   q3Obs=avgObs
               #
               #  Graph the average
               #
               (x1,y1)=self.graphcoord(valuelow,avgObs)
               (x2,y2)=self.graphcoord(valuehigh,avgObs)
               if prevAvg>-99999.0:
                  self.cd.canvas.create_line(x1,prevAvg,x1,y1,x2,y2,fill=colorname,tags=tagtuple)
               else:
                  self.cd.canvas.create_line(x1,y1,x2,y2,fill=colorname,tags=tagtuple)
               prevAvg=y1
               #
               #  For everything except probability parms...plot min/max/std
               #
               if verType!=1:
                  #
                  #  Plot the min
                  #
                  (x1,y1)=self.graphcoord(vl1,minObs)
                  (x2,y2)=self.graphcoord(vh1,minObs)
                  self.cd.canvas.create_line(x1,y1,x2,y2,fill=colorname,tags=tagtuple)
                  #
                  #  Plot the max
                  #
                  (x1,y1)=self.graphcoord(vl1,maxObs)
                  (x2,y2)=self.graphcoord(vh1,maxObs)
                  self.cd.canvas.create_line(x1,y1,x2,y2,fill=colorname,tags=tagtuple)
                  q1Obs=avgObs-std
                  q3Obs=avgObs+std
                  (x1,y1)=self.graphcoord(valuelow,q1Obs)
                  (x2,y2)=self.graphcoord(valuehigh,q3Obs)
                  self.cd.canvas.create_polygon(x1,y1,x1,y2,x2,y2,x2,y1,stipple="gray25",fill=colorname,outline="",tags=tagtuple)
            del pts
         self.but2state[mod]=1
         self.but1state[fhrstr]=1
         del fcstArray
         del obsArray

      startBut1(self)
      startBut2(self)

      del self.flists
      del self.olists
      
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      self.VU.logMsg("expected value done  memory:%d  resident:%d"%(memory(),resident()))
      return
   #==================================================================
   # scatterPlot - display scatterplot
   #
   #
   def scatterPlot(self,parmList,cycleList,modelList,obsmodel,
                      fcstrList,fhrStart,fhrEnd,dateType,numDays,fromDay,
                      dayList,dateStyle,scale,commonCases,accumHours,
                      accumFreq):
      #
      #  Clear display - setup title
      #
      parm=parmList[0]
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Scatterplot - %s"%parm)
      #
      #
      #
      workStart="Working on Verifying Value Distribution"
      self.startWorking(workStart,optionRemove=0)
      #
      #
      #
      NUMTBUTTONS=12  # normal number of time buttons on a row - configure
      NUMMBUTTONS=6   # normal number of model buttons on a row - configure
      #
      #  get the active EditArea into ea.  If the active edit area is
      #  None - then assume they want to run it over the entire grid
      #
      editArea=self.getActiveEditArea()
      editAreaMask=self.encodeEditArea(editArea)
      npts=add.reduce(add.reduce(editAreaMask))
      if (npts==0):
         editArea.invert()
         editAreaMask=self.encodeEditArea(editArea)
      eaflat=ravel(editAreaMask)
      totalpoints=add.reduce(eaflat)
      #
      #  make space for saving data
      #
      self.flists={} # storage for fcst values for each model/forecast hour
      self.olists={} # storage for obs  values for each model/forecast hour
      fullmin=999999.0
      fullmax=-999999.0
      #
      #  Loop over parm and model
      #
      totaliters=len(modelList)
      iter=0
      #
      #  For vectors...the parm to read might be different than
      #  the name of the parm
      #
      readParm=parm
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if ((last3=="Spd")or(last3=="Dir")):
            readParm=parm[:-3]
      #
      #  Get information about the parm we are reading
      #
      (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
       parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
      obsParm=self.VU.getObsParm(readParm)
      verType=self.VU.getVerType(readParm)
      datatype=self.VU.getVerParmType(readParm)
      if ((datatype==1)and(last3=="Dir")):
         parmMinval=0
         parmMaxval=360
      #
      #  get binwidth and bigerr for parm...but for vectors its
      #  complicated by dir/mag/vecerr options
      #
      binwidth=self.VU.getVerBinWidth(readParm)
      if datatype==1:
         (bwMag,bwDir)=binwidth
         if last3=="Dir":
            binwidth=bwDir
         else:
            binwidth=bwMag
      #
      #  Setup histogram binning routines
      #
      self.histosetup(parmMinval,parmMaxval,binwidth)
      #
      #  Get mode for reading obs grids
      #
      obsGridMode=self.getReadMode(obsmodel,obsParm,0)
      #
      #  Get case times/records for all models
      #
      caseInfo=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                             dateStyle,dateType,fromDay=fromDay,
                             numDays=numDays,dayList=dayList,
                             fcstrs=fcstrList,cycles=cycleList,
                             fhrStart=fhrStart,fhrEnd=fhrEnd,
                             accumHours=accumHours,accumFreq=accumFreq,
                             commonCases=commonCases,basetimeOffsets=1,
                             callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      #
      #  Loop over each model
      #
      for model in modelList:
         iter+=1
         workNow=workStart+":%s (%d of %d)"%(model,iter,totaliters)
         #
         fcstGridMode=self.getReadMode(model,readParm)
         #
         #  Get all the cases for this model
         #
         cases=caseInfo[model]
         #
         #  Sort cases by the start/end time, not the basetime
         #
         casekeys=cases.keys()
         casekeys.sort(lambda x,y: cmp(x.split(",",1)[1],y.split(",",1)[1]))
         totalcount=len(casekeys)
         self.VU.logMsg("reading %d cases for %s"%(totalcount,model),1)
         count=0
         lastobs=""
         for key in casekeys:
            count+=1
            self.VU.logMsg("%s : %s"%(model,key),10)
            if self.setAndCheckWorking("%s: %d of %d"%(workNow,count,totalcount))==1:
               self.stopWorking()
               return
            (basetimestr,stimestr,etimestr)=key.split(",")
            basetime=int(basetimestr)
            stime=int(stimestr)
            etime=int(etimestr)
            (frecList,orecList)=cases[key]
            #
            #  Dont make stats for obs not yet complete
            #
            if etime>time.time():
               continue
            #
            #  Dont include negative forecast hours
            #
            fhr=self.VU.getFcstHour(basetime,stime)
            if fhr<0:
               continue
            #
            #  string to store grid under depends on model and forecast hour
            #
            saveKey="%s-%3.3d"%(model,fhr)
            #
            #  If a new and different obs time - read the obs data
            #
            obskey=key.split(",",1)[1]
            obsdata=self.VU.getVerGrids(obsmodel,basetime,obsParm,
                                        stime,etime,mode=obsGridMode,
                                        recList=orecList)
            obsdata=self.scaleGrid(obsdata,scale,datatype)
            #
            #  For probabilistic variables...calculate the
            #  observed 'yes/no' value
            #
            if verType==1:
               obsdata=self.getProbVerGrid(readParm,obsdata)*100.0
            #
            #  cant do a value histogram of vector wind
            #  errors...so those get changed to windSpd
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  obsgrid=obsdata[0]
               elif last3=="Dir":
                  obsgrid=obsdata[1]
               else:
                  obsgrid=obsdata
               obsonly=compress(eaflat,ravel(obsgrid))
            else:
               obsgrid=obsdata[0]
               obsonly=compress(eaflat,ravel(obsgrid))
            obsList=list(obsonly)
            minObs=min(obsList)
            maxObs=max(obsList)
            fullmin=min(minObs,fullmin)
            fullmax=max(maxObs,fullmax)
            if self.olists.has_key(saveKey):
               self.olists[saveKey].extend(obsList)
            else:
               self.olists[saveKey]=obsList
            #
            #  Read forecast grid and calculate error grid
            #
            fcstdata=self.VU.getVerGrids(model,basetime,readParm,
                                         stime,etime,mode=fcstGridMode,
                                         recList=frecList)
            fcstdata=self.scaleGrid(fcstdata,scale,datatype)
            #
            #  Get the error, handling vector error, etc.
            #
            if ((datatype!=1)or(last3 in ["Spd","Dir"])):
               if last3=="Spd":
                  fcstgrid=fcstdata[0]
               elif last3=="Dir":
                  fcstgrid=fcstdata[1]
               else:
                  fcstgrid=fcstdata
               fcstonly=compress(eaflat,ravel(fcstgrid))
            else:
               fcstgrid=fcstdata[0]
               fcstonly=compress(eaflat,ravel(fcstgrid))
            fcstList=list(fcstonly)
            minFcst=min(fcstList)
            maxFcst=max(fcstList)
            fullmin=min(minFcst,fullmin)
            fullmax=max(maxFcst,fullmax)
            #
            #  Add values forecast lists for same model/fhr
            #
            #
            if self.flists.has_key(saveKey):
               self.flists[saveKey].extend(fcstList)
            else:
               self.flists[saveKey]=fcstList
      #
      #  Get all the keys that will be displayed
      #
      fullkeys=self.flists.keys()
      #
      #  if no data could be read - stop here
      #
      if len(fullkeys)<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return
      #
      #  For buttons...get models/forecasthours actually in the data
      #
      fullkeys.sort()
      fhrstrs=[]
      modkeys=[]
      for fullkey in fullkeys:
         (mod,fhrstr)=fullkey.split("-")
         if fhrstr not in fhrstrs:
            fhrstrs.append(fhrstr)
         if mod not in modkeys:
            modkeys.append(mod)
      #
      #  Change fhrstrs (sorted on 3-character 000-999) into
      #  smaller fhrkeys that are NOT all 3-characters wide
      #
      fhrstrs.sort()
      fhrkeys=[]
      for fhrstr in fhrstrs:
         fhrkeys.append("%d"%int(fhrstr))
      #
      #  If an Official button is in there...make it first
      #
      modkeys.sort()
      if "Official" in modkeys:
         idx=modkeys.index("Official")
         del modkeys[idx]
         modkeys.insert(0,"Official")
      #
      #  set colors for each model
      #
      self.colornames={}
      index=0
      for mod in modkeys:
         self.colornames[mod]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  Setup first row of buttons (forecast hours)
      #
      self.setupBut1(fhrkeys,numbuttons=NUMTBUTTONS,arrows=1,width=3)
      #
      #  Setup second row of buttons (models)
      #
      self.setupBut2(modkeys,numbuttons=NUMMBUTTONS,arrows=1)
      #
      #  If not many bins shown (i.e. nearly constant values)...add bins
      #  up and down until we get at least 15 bins - so our values are
      #  'centered' in a reaonably wide graph
      #
      numbins=int(float(fullmax-fullmin)/float(binwidth))+1
      if numbins<15:
         while numbins<15:
            fullmax=min(fullmax+binwidth,parmMaxval)
            fullmin=max(fullmin-binwidth,parmMinval)
            numbins=int(float(fullmax-fullmin)/float(binwidth))+1
            if ((numbins<15)and(fullmin==parmMinval)and(fullmax==parmMaxval)):
               numbins=16
      #
      #
      #
      numticks=25
      tickInterval=self.niceNumDec((fullmax-fullmin)/float(numticks-1),1)
      #
      #  Dont let tick interval be smaller than parm precision
      #
      mintick=10**(-parmPrecision)
      tickInterval=max(tickInterval,mintick)
      #
      #  Set the minimum graph one tick interval below the minimum...but
      #  not below the parm minimum value
      #
      graphmin=(floor(float(fullmin)/float(tickInterval))-1)*tickInterval
      graphmin=maximum(graphmin,parmMinval)
      #
      #  Set the maximum graph one tick interval above the maximum...but
      #  not above the parm maximum value
      #
      graphmax=(floor(float(fullmax)/float(tickInterval))+2)*tickInterval
      graphmax=minimum(graphmax,parmMaxval)
      #
      #
      #
      numTicks=int(float(graphmax-graphmin)/float(tickInterval))+1
      #
      #  Set up the graph axes
      #
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(100.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(graphmin,graphmax,graphmin,graphmax,left,right,bot,top)
      self.valaxes(graphmin,graphmax,tickInterval)
      #
      numPts=totalpoints
      ul1="Scatterplot - %s"%parm
      self.cdLabels(ul1,numPts,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      #
      #
      #
      #self.probaxes()
      #
      #
      #
      MaxValuesToShow=1000
      numbins=50
      numPts=totalpoints
      counts={}
      maxcounts={}
      maxnum=0
      self.setWorking("%s: scanning scatterplots"%workStart)
      for key in self.flists.keys():
         maxnum=max(maxnum,len(self.flists[key]))
      if self.checkWorking()==1:
         self.stopWorking()
         return
      if maxnum>MaxValuesToShow:
         binsize=float(graphmax-graphmin)/float(numbins)
         #print "binsize=",binsize
         totaldcount=len(self.flists.keys())
         dcount=0
         for key in self.flists.keys():
            dcount+=1
            self.setWorking("%s: large scatterplot thinning: %d of %d"%(workStart,dcount,totaldcount))
            if self.checkWorking()==1:
               self.stopWorking()
               return
            if len(self.flists[key])>MaxValuesToShow:
               count=zeros((numbins,numbins))
               xpos=minimum(((array(self.olists[key])-graphmin)/binsize).astype(int),numbins-1)
               ypos=minimum(((array(self.flists[key])-graphmin)/binsize).astype(int),numbins-1)
               xl=list(xpos)
               yl=list(ypos)
               for i in xrange(len(xl)):
                  x=xl[i]
                  y=yl[i]
                  count[y,x]+=1
               maxcounts[key]=maximum.reduce(maximum.reduce(count))
               #print "maxcounts[",key,"]=",maxcounts[key]
               counts[key]=count
      #
      #  Draw
      #
      totalcount=len(fullkeys)
      count=0
      for key in fullkeys:
         count+=1
         self.setWorking("%s: drawing scatterplot %d of %d"%(workStart,count,totalcount))
         if self.checkWorking()==1:
            self.stopWorking()
            return
         tagbase=key.split("-")
         mod=tagbase[0]
         modnum=modelList.index(mod)
         fhr=int(tagbase[1])
         fhrstr="f%d"%fhr
         tagtuple=(mod,fhrstr)
         flabel="%d-hr forecast"%fhr
         self.labelLine(flabel,3,justify="right",tags=tagtuple)
       
         colorname=self.colornames[mod]
         
         if key not in maxcounts.keys():
            ylist=self.flists[key]
            xlist=self.olists[key]
            for i in xrange(len(ylist)):
               (x,y)=self.graphcoord(xlist[i],ylist[i])
               self.cd.canvas.create_line(x-2,y,x+2,y,fill=colorname,tags=tagtuple)
               self.cd.canvas.create_line(x,y-2,x,y+2,fill=colorname,tags=tagtuple)
         else:
            for i in xrange(numbins):
               midx=graphmin+((i+0.5)*binsize)
               for j in xrange(numbins):
                  midy=graphmin+((j+0.5)*binsize)
                  width=(float(counts[key][j,i])/float(maxcounts[key]))*binsize*0.5
                  (x0,y0)=self.graphcoord(midx-width,midy-width)
                  (x1,y1)=self.graphcoord(midx+width,midy+width)
                  if width>0.01:
                     self.cd.canvas.create_arc(x0,y0,x1,y1,fill=colorname,outline=colorname,start=0.0,extent=359.9,width=1.0,tags=tagtuple)

         self.but2state[mod]=1
         self.but1state[fhrstr]=1

      startBut1(self)
      startBut2(self)
         
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      return
   #==================================================================
   # scaleGrid - smooth a grid by the scale amount.  Correctly handles
   #             vectors indicated by datatype==1.
   #
   def scaleGrid(self,griddata,scale,datatype):
      if scale>0:
         if datatype!=1:
            griddata=self.VU.smoothpm(griddata,scale)
         else:
            (gridmag,griddir)=griddata
            (u,v)=self.MagDirToUV(gridmag,griddir)
            us=self.VU.smoothpm(u,scale)
            vs=self.VU.smoothpm(v,scale)
            (gridmag,griddir)=self.UVToMagDir(us,vs)
            griddata=(gridmag,griddir)
      return griddata
   #==================================================================
   # moveCD - if the first time self.cd is displayed - move to a good
   #          location
   #
   def moveCD(self):
      if self.cd.firstDisplay==1:
         self.cd.update_idletasks()
         geo=self.cd.geometry()
         (mwh,mof)=geo.split("+",1)
         (mw,mh)=mwh.split("x",1)
         parentgeo=self.root.geometry()
         (wh,of)=parentgeo.split("+",1)
         (w,h)=wh.split("x",1)
         (ox,oy)=of.split("+",1)
         xoff=int(ox)+int(w)-int(mw)
         yoff=int((int(h)-int(mh))/2.0)+int(oy)
         newgeo=mwh+"+%d+%d"%(xoff,yoff)
         self.cd.geometry(newgeo)
         self.cd.firstDisplay=0
      return
   #==================================================================
   # showStats - display point/area statistics
   #
   def ShowStats(self,DialogDict):
      self.VU.logMsg("running ShowStats:")

      plotType=DialogDict["PlotType"]
      if plotType=="vs. Time":
         self.makeTimeSeries(DialogDict)
      if plotType=="vs. Fcst Hour":
         self.makeFhourGraph(DialogDict)
      return
   #==================================================================
   #  makeTimeSeries - display time series for point/area
   #
   def makeTimeSeries(self,DialogDict):
      self.VU.logMsg("running makeTimeSeries:")
      display=DialogDict["Display"]
      areaList=DialogDict["areaList"]
      AreaCombine=DialogDict["AreaCombine"]
      parmList=DialogDict["Parms"]
      threshold=DialogDict["Threshold"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Models"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      dateStyle=DialogDict["dateStyle"]
      plotType=DialogDict["PlotType"]
      scale=DialogDict["scale"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      TwoCatType=DialogDict["TwoCatType"]
      TwoCatCond=DialogDict["TwoCatCond"]
      TwoCatValue=DialogDict["TwoCatValue"]
      TwoCatValueString=DialogDict["TwoCatValueString"]
      #
      #  Check for good GUI input
      #
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      #
      #  Check that we do not have too many things varying
      #
      if len(parmList)>1:
         parmVary=1
      else:
         parmVary=0
      if ((len(areaList)>1)and(AreaCombine==0)):
         areaVary=1
      else:
         areaVary=0
      if len(modelList)>1:
         modelVary=1
      else:
         modelVary=0
      totalVary=parmVary+areaVary+modelVary
      if totalVary>1:
         msg="Can only vary one of parm/area/model when doing 'vs. Time' graphs."
         self.statusBarMsg(msg,"U")
         return
      #
      #  If nothing varying - pick model
      #
      if totalVary==0:
         modelVary=1
      #
      #  get list of names of selected edit areas into areaNames
      #
      areaNames=[]
      nameList=self.VU.listEditAreas()
      descList=self.VU.listEditAreaDescriptions()
      for areaDesc in areaList:
         if areaDesc=="Current":
            areaNames.append("Current")
         elif areaDesc in descList:
            areaNum=descList.index(areaDesc)
            areaNames.append(nameList[areaNum])
      if len(areaNames)<1:
         msg="Invalid Edit Area(s) - contact support"
         self.statusBarMsg(msg,"U")
         return
      print "the areaNames are:",areaNames
      comboArea=self._empty
      if ((AreaCombine==1)and(len(areaNames)>1)):
         for areaName in areaNames:
            if areaName=="Current":
               areaObject=self.getActiveEditArea()
               mask=self.encodeEditArea(areaObject)
               any=add.reduce(add.reduce(mask))
               if any==0:
                  mask=self._empty+1
            elif areaName=="NONE":
               mask=(self._empty)+1
            else:
               mask=self.encodeEditArea(areaName)
            comboArea=logical_or(comboArea,mask)
      #
      #
      #
      statName=display
      if statName=="TwoCat":
         statName=TwoCatType
      statVal=TwoCatValue
      statCond=TwoCatCond
      #
      #  Clear the cd canvas - setup title
      #
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Statistic Time Series")
      workStart="Working on Statistics"
      self.startWorking(workStart,optionRemove=0)
      #
      #
      #
      outdata={}
      fhrList=[]
      timemin=1e32
      timemax=-1e32
      valmin=1.0e32
      valmax=-1.0e32

      countimax=len(parmList)*len(modelList)
      counti=0
      for parm in parmList:
         readParm=parm
         vectorType=-1
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               readParm=parm[:-3]
               if last3=="Spd":
                  vectorType==0
               else:
                  vectorType==1 
         obsParm=self.VU.getObsParm(readParm)
         verType=self.VU.getVerType(readParm)
         datatype=self.VU.getVerParmType(readParm)
         thresholds=self.VU.getVerThresholds(readParm)
         if last3=="Spd":
            thresholds=thresholds[0]
         elif last3=="Dir":
            thresholds=thresholds[1]
         thresholdValue=thresholds[threshold]

         statsCases=self.VU.getStatCases(parm,modelList,obsmodel,dateStyle,dateType,
                        fromDay=fromDay,numDays=numDays,dayList=dayList,
                        fcstrs=fcstrList,cycles=cycleList,fhrStart=fhrStart,
                        fhrEnd=fhrEnd,accumHours=accumHours,accumFreq=accumFreq,
                        commonCases=commonCases,basetimeOffsets=1,
                        callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         gridsCases=self.VU.getCases(readParm,modelList,obsParm,obsmodel,dateStyle,dateType,
                        fromDay=fromDay,numDays=numDays,dayList=dayList,
                        fcstrs=fcstrList,cycles=cycleList,fhrStart=fhrStart,
                        fhrEnd=fhrEnd,accumHours=accumHours,accumFreq=accumFreq,
                        requireObs=1,commonCases=commonCases,basetimeOffsets=1,
                        callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         for model in modelList:
            counti+=1
            workNow="Reading %s %s (%d of %d)"%(parm,model,counti,countimax)
            scases=statsCases[model]
            if model in gridsCases.keys():
               gcases=gridsCases[model]
            else:
               gcases={}
            #
            #  get overall list of keys (both stat and grid)
            #
            skeys=scases.keys()
            gkeys=gcases.keys()
            tkeys=skeys
            for gkey in gkeys:
               if gkey not in tkeys:
                  tkeys.append(gkey)
            
            #
            #  Loop over possible stat or grid cases
            #
            count=0
            totalcount=len(tkeys)
            for key in tkeys:
               count+=1
               self.setWorking("%s: %d of %d"%(workNow,count,totalcount))
               if self.checkWorking()==1:
                  self.stopWorking()
                  return
               if key in scases:
                  srecList=scases[key]
               else:
                  srecList=None
               if key in gcases:
                  grecList=gcases[key]
               else:
                  grecList=None
               #
               #
               #
               (basestr,stimestr,etimestr)=key.split(",")
               basetime=int(basestr)
               starttime=int(stimestr)
               endtime=int(etimestr)
               #
               #  Done show results for grids not yet complete
               #
               if endtime>time.time():
                  continue
               #
               #  Dont show results for grids that start before forecast time
               #
               fhr=(starttime-basetime)/HOURSECS
               if fhr<0:
                  continue
               # 
               #  X-coordinate is 'starttime' when doing "Verifying On" displays
               #     and 'basetime' when doing "Forecast on" displays
               #
               if dateStyle=="Verifying on":
                  x=starttime
               else:
                  x=basetime
               timemin=min(x,timemin)
               timemax=max(x,timemax)
               #
               #  Thresholds are different for different variables
               #
               if statName=="Percent Err <":
                  statVal=thresholdValue
               #
               #  When AreaCombine is on...we already have the combined
               #  edit area ready
               #
               if ((AreaCombine==1)and(len(areaNames)>1)):
                  eaGrid=comboArea
                  
                  outkey="%s,%s,%3.3d,-01"%(parm,model,fhr)
                  if outkey not in outdata.keys():
                     outdata[outkey]=[]

                  valx=self.VU.getVerStat(model,basetime,readParm,starttime,endtime,
                                       obsmodel,statName,statVal=statVal,
                                       statCond=statCond,editArea=eaGrid,
                                       smooth=scale,vectorType=vectorType,
                                       srecList=srecList,grecList=grecList)
                  if valx is None:
                     print "getVerStat returned None"
                     continue
                  valmin=min(valx,valmin)
                  valmax=max(valx,valmax)
                  outdata[outkey].append((x,valx))
               #
               #  When AreaCombine is off...loop over editAreas
               #
               else:
                  for areaName in areaNames:  
                     outkey="%s,%s,%3.3d,%s"%(parm,model,fhr,areaName)
                     if outkey not in outdata.keys():
                        outdata[outkey]=[]
                     if areaName=="Current":
                        areaObject=self.getActiveEditArea()
                        ea=self.encodeEditArea(areaObject)
                        any=add.reduce(add.reduce(ea))
                        if any==0:
                           ea=self._empty+1
                     elif areaName=="NONE":
                        ea=self._empty+1
                     else:
                        ea=areaName
                     valx=self.VU.getVerStat(model,basetime,readParm,starttime,endtime,
                                       obsmodel,statName,statVal=statVal,
                                       statCond=statCond,editArea=ea,
                                       smooth=scale,vectorType=vectorType,
                                       srecList=srecList,grecList=grecList)
                     if valx is None:
                        print "getVerStat returned None"
                        continue
                     valmin=min(valx,valmin)
                     valmax=max(valx,valmax)
                     outdata[outkey].append((x,valx))
               #self.VU.setDebug(0)   
               #
               #
               #
               if fhr not in fhrList:
                  fhrList.append(fhr)
               #
      #
      #  If no data read - don't go further
      #
      if len(outdata.keys())<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return
      #print "done reading"
      #
      #  valmin/valmax usually works - but for bounded stats
      #  we always want 0.0 to be the lower bound
      #
      if display in ["RMS Error","Std Dev","Mean Abs Err"]:
         valmin=0.0
      if display=="Percent Err <":
         valmin=0.0
         valmax=1.0
      #print "value range:",valmin,valmax
      #
      #  time buttons
      #
      fhrList.sort()
      fList=[]
      for fhr in fhrList:
         fList.append("%d"%fhr)
      self.setupBut1(fList,numbuttons=12,arrows=1,width=3)
      #
      #  First part of title line is the type of error
      #
      if display=="TwoCat":
         titleLine="%s Timeseries - "%TwoCatType
      elif display!="Percent Err <":
         titleLine="%s Timeseries - "%display
      else:
         titleLine="%s %d Timeseries - "%(display,threshold)
      #
      #  set varList to the thing that varies: model-parm-area
      #
      if parmVary==1:
         varList=parmList[:]
         varButtons=6
         titleLine+=modelList[0]
      elif modelVary==1:
         if "Official" in modelList:
            idx=modelList.index("Official")
            del modelList[idx]
            modelList.insert(0,"Official")
         varList=modelList[:]
         varButtons=6
         titleLine+=parmList[0]
         if display=="TwoCat":
            titleLine+=" %s %s"%(TwoCatCond,TwoCatValueString)
      else:
         varList=areaList[:]
         varButtons=3
         titleLine+="%s %s"%(modelList[0],parmList[0]) 
      #
      #  Associate colors with the varying model/parm/area
      #
      self.colornames={}
      index=0
      for var in varList:
         self.colornames[var]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  Make buttons
      #
      self.setupBut2(varList,numbuttons=varButtons,arrows=1)
      #
      #  Setup graphing coordinates
      #
      numticks=10
      graphrange=valmax-valmin
      print "graphrange=",graphrange
      tickInterval=self.niceNumDec(graphrange/(numticks-1),1)
      #print "tickInterval=",tickInterval
      
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(130.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(timemin,timemax,valmin,valmax,left,right,bot,top)
      self.graphaxes(timemin,timemax,valmin,valmax)
      #
      #  Draw timeseries lines
      #
      for key in outdata.keys():
         #print "timeseries for ",key
         tagbase=key.split(",")
         fhr="f%d"%int(tagbase[2])
         #
         if parmVary==1:
            varTag=tagbase[0]
         elif modelVary==1:
            varTag=tagbase[1]
         else:
            #varTag=self.VU.EditAreaDescriptions[int(tagbase[3])]
            areaNum=self.VU.getEditAreaNumberFromName(tagbase[3])
            varTag=self.VU.EditAreaDescriptions[areaNum]
         tagtuple=(varTag,fhr)
          
         colorname=self.colornames[varTag]
         points=outdata[key]
         points.sort()
         gpoints=[]
         for point in points:
            (xtime,val)=point
            (x,y)=self.graphcoord(xtime,val)
            gpoints.append(x)
            gpoints.append(y)
         if len(gpoints)>3:
            self.cd.canvas.create_line(gpoints,fill=colorname,tags=tagtuple)
         self.but2state[varTag]=1
         self.but1state[fhr]=1
      #
      #  Label forecast times
      #
      for fhr in fhrList:
         fhrstr="f%d"%fhr
         labelstr="%d-hr Forecast"%fhr
         self.labelLine(labelstr,3,justify='right',tags=(fhrstr))
      #
      #  Turn off all but first
      #
      startBut1(self)
      startBut2(self)
      #
      #  Labels at top of graph
      #
      #if len(areaList)>1:
      #   if AreaCombine==1:
      #      numPts=0
      #      for areaNum in areaNums:
      #        numPts+=self.pts[areaNum]
      #   else:
      #      numPts=-1
      #else:
      #   numPts=self.pts[areaNums[0]]
      numPts=-1
      self.cdLabels(titleLine,numPts,dateStyle,dateType,numDays,fromDay,
                    dayList,cycleList)
      #
      #  Done - show the results
      #
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      
      return

   #==================================================================
   #  makeFhourGraph - display graph of average error at various fhrs
   #
   def makeFhourGraph(self,DialogDict):
      self.VU.logMsg("running makeFhourGraph:")
      display=DialogDict["Display"]
      areaList=DialogDict["areaList"]
      AreaCombine=DialogDict["AreaCombine"]
      parmList=DialogDict["Parms"]
      threshold=DialogDict["Threshold"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Models"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      dateStyle=DialogDict["dateStyle"]
      plotType=DialogDict["PlotType"]
      scale=DialogDict["scale"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      TwoCatType=DialogDict["TwoCatType"]
      TwoCatCond=DialogDict["TwoCatCond"]
      TwoCatValue=DialogDict["TwoCatValue"]
      TwoCatValueString=DialogDict["TwoCatValueString"]
      #
      #  Check for good GUI input
      #
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      #
      #  Check that we do not have too many things varying
      #
      if len(parmList)>1:
         parmVary=1
      else:
         parmVary=0
      if ((len(areaList)>1)and(AreaCombine==0)):
         areaVary=1
      else:
         areaVary=0
      if len(modelList)>1:
         modelVary=1
      else:
         modelVary=0
      totalVary=parmVary+areaVary+modelVary
      if totalVary>2:
         msg="Can only vary two of parm/area/model when doing 'vs. Fcst Hour' graphs"
         self.statusBarMsg(msg,"U")
         return
      #
      #  If only varying one thing - then set the other to model, unless that
      #  is the one already being done - and then set to parm
      #
      if totalVary==1:
         if modelVary==1:
            parmVary=1
         else:
            modelVary=1
      #
      #
      #
      if parmVary==1:
         if modelVary==1:
            varList1=parmList[:]
            varList2=modelList[:]
         else:
            varList1=parmList[:]
            varList2=areaList[:]
      else:
         varList1=areaList[:]
         varList2=modelList[:]
      #
      #  Clear the cd canvas - setup title
      #
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Statistic Graph")
      workStart="Working on Statistics"
      self.startWorking(workStart,optionRemove=0)
      #
      #  get names of selected edit areas
      #
      areaNames=[]
      descList=self.VU.listEditAreaDescriptions()
      nameList=self.VU.listEditAreas()
      for areaDesc in areaList:
         if areaDesc=="Current":
            areaNames.append("Current")
         elif areaDesc in descList:
            areaNum=descList.index(areaDesc)
            areaNames.append(nameList[areaNum])
      if len(areaNames)<1:
         msg="Invalid Edit Area(s) - contact support"
         self.statusBarMsg(msg,"U")
         return
      #
      #  For 'combined areas' - setup the comboArea just once
      #
      comboArea=self._empty
      if ((AreaCombine==1)and(len(areaNames)>1)):
         for areaName in areaNames:
            if areaName=="Current":
               areaObject=self.getActiveEditArea()
               mask=self.encodeEditArea(areaObject)
               any=add.reduce(add.reduce(mask))
               if any==0:
                  mask=self._empty+1
            elif areaName=="NONE":
               mask=self._empty+1
            else:
               mask=self.encodeEditArea(areaName)
            comboArea=logical_or(comboArea,mask)      
      #
      #  If any of the TwoCat stats are requested - then get
      #  the contingency table entries instead
      #
      statName=display
      if statName=="TwoCat":
         statName="cont"
         if TwoCatType[0:1]=="A":
            statName="acont"
      statVal=TwoCatValue
      statCond=TwoCatCond
      statID=self.VU.getStatID(TwoCatType)
      #
      #
      #
      #
      #
      #
      sumdata={}
      cntdata={}
      hitsdata={}
      missdata={}
      falrdata={}
      corndata={}
      #
      timemin=1e32
      timemax=-1e32
      valmin=1.0e32
      valmax=-1.0e32
      
      countimax=len(parmList)*len(modelList)
      counti=0
      for parm in parmList:
         #
         #
         #
         (parmUnits,parmPrecision,parmMinval,parmMaxval,parmRateFlag,parmColorTable,
          parmDisplayMinval,parmDisplayMaxval)=self.getParmInfo(self.mutableID(),parm)
         #
         #  setup readParm - which is usually the same as parm, but
         #  can be different for the Spd/Dir components of a vector
         #
         readParm=parm
         vectorType=-1
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if (last3 in ["Spd","Dir"]):
               readParm=parm[:-3]
               if last3=="Spd":
                  vectorType==0
               else:
                  vectorType==1
         #
         #  Get the observed parm for this parm, the verification type,
         #  the data type and the thresholds
         #
         obsParm=self.VU.getObsParm(readParm)
         verType=self.VU.getVerType(readParm)
         datatype=self.VU.getVerParmType(readParm)
         thresholds=self.VU.getVerThresholds(readParm)
         if last3=="Spd":
            thresholds=thresholds[0]
         elif last3=="dir":
            thresholds=thresholds[1]
         thresholdValue=thresholds[threshold]
         #
         #  If using the threshold stat - set it now
         #
         if statName=="Percent Err <":
            statVal=thresholdValue
         #
         #  Get the statCases for this parm
         #
         statCases=self.VU.getStatCases(parm,modelList,obsmodel,dateStyle,
                      dateType,fromDay=fromDay,numDays=numDays,
                      dayList=dayList,fcstrs=fcstrList,cycles=cycleList,
                      fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                      accumFreq=accumFreq,commonCases=commonCases,
                      basetimeOffsets=1,callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         #
         #  Get the gridCases for this parm
         #
         gridCases=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                      dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                      dayList=dayList,fcstrs=fcstrList,cycles=cycleList,
                      fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                      accumFreq=accumFreq,requireObs=1,commonCases=commonCases,
                      basetimeOffsets=1,callbackMethod=self.workingCommon)
         if self.checkWorking()==1:
            self.stopWorking()
            return
         for model in modelList:
            counti+=1
            workStart="Reading %s %s (%d of %d)"%(parm,model,counti,countimax)
            #
            #  get cases for this model
            #
            scases=statCases[model]
            if model in gridCases.keys():
               gcases=gridCases[model]
            else:
               gcases={}
            #
            #  get overall list of keys (both stat and grid) in tkeys
            #
            skeys=scases.keys()
            gkeys=gcases.keys()
            tkeys=skeys
            for gkey in gkeys:
               if gkey not in tkeys:
                  tkeys.append(gkey)
            #
            #  Loop over possible stat or grid cases
            #
            count=0
            totalcount=len(tkeys)
            for key in tkeys:
               #
               #  Check for user interrupting
               #
               count+=1
               if self.setAndCheckWorking("%s: %d of %d"%(workStart,count,totalcount))==1:
                  self.stopWorking()
                  return
               #
               #  Get times for this case
               #
               (basestr,stimestr,etimestr)=key.split(",")
               basetime=int(basestr)
               starttime=int(stimestr)
               endtime=int(etimestr)
               #
               #  Do not use results for grids that are not yet complete
               #
               if endtime>time.time():
                  continue
               #
               #  Do not show results for grids that start before
               #  forecast time
               #
               fhr=(starttime-basetime)/HOURSECS
               if fhr<0:
                  continue
               #
               #  Get list of records for this cases
               #
               if key in scases:
                  srecList=scases[key]
               else:
                  srecList=None
               if key in gcases:
                  grecList=gcases[key]
               else:
                  grecList=None
               #
               #
               #
               #
               #  When areaCombine is on...we already have the combined
               #  edit area ready
               #
               if ((AreaCombine==1)and(len(areaNames)>1)):
                  eaGrid=comboArea
                  valx=self.VU.getVerStat(model,basetime,readParm,starttime,
                                 endtime,obsmodel,statName,statVal=statVal,
                                 statCond=statCond,editArea=eaGrid,
                                 smooth=scale,vectorType=vectorType,
                                 srecList=srecList,grecList=grecList)
                  if valx is None:
                     print "getVerStat returned None"
                     continue
                  #
                  #  store sums in parm,model,fhr,areaName keys
                  #
                  outkey="%s,%s,%3.3d,-01"%(parm,model,fhr)
                  if display!="TwoCat":
                     if outkey not in sumdata.keys():
                        sumdata[outkey]=0.0
                        cntdata[outkey]=0
                     sumdata[outkey]+=valx
                     cntdata[outkey]+=1
                  else:
                     if outkey not in hitsdata.keys():
                        hitsdata[outkey]=0
                        missdata[outkey]=0
                        falrdata[outkey]=0
                        corndata[outkey]=0
                     (hits,miss,falr,corn)=valx
                     hitsdata[outkey]+=hits
                     missdata[outkey]+=miss
                     falrdata[outkey]+=falr
                     corndata[outkey]+=corn
               else:
                  for areaName in areaNames:
                     if areaName=="Current":
                        areaObject=self.getActiveEditArea()
                        ea=self.encodeEditArea(areaObject)
                        any=add.reduce(add.reduce(ea))
                        if any==0:
                           ea=self._empty+1
                     elif areaName=="NONE":
                        ea=self._empty+1
                     else:
                        ea=areaName
                     valx=self.VU.getVerStat(model,basetime,readParm,starttime,
                                 endtime,obsmodel,statName,statVal=statVal,
                                 statCond=statCond,editArea=ea,
                                 smooth=scale,vectorType=vectorType,
                                 srecList=srecList,grecList=grecList)
                     if valx is None:
                        print "getVerStat returned None"
                        continue
                     #
                     #
                     #
                     outkey="%s,%s,%3.3d,%s"%(parm,model,fhr,areaName)
                     if display!="TwoCat":
                        if display=="RMS Error":
                           valx=valx**2
                        if outkey not in sumdata.keys():
                           sumdata[outkey]=0.0
                           cntdata[outkey]=0
                        sumdata[outkey]+=valx
                        cntdata[outkey]+=1
                     else:
                        if outkey not in hitsdata.keys():
                           hitsdata[outkey]=0
                           missdata[outkey]=0
                           falrdata[outkey]=0
                           corndata[outkey]=0
                           cntdata[outkey]=0
                        (hits,miss,falr,corn)=valx
                        hitsdata[outkey]+=hits
                        missdata[outkey]+=miss
                        falrdata[outkey]+=falr
                        corndata[outkey]+=corn
                        cntdata[outkey]+=1               
      #
      #  if no data could be read - stop here
      #
      if len(cntdata.keys())<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return         
      #
      #  We now have the sums...calculate the scores
      #
      fhrList=[]
      valmin=1e32
      valmax=-1e32
      timemin=0
      timemax=0
      outdata={}
      for key in cntdata.keys():
         if cntdata[key]<1:
            continue
         if display!="TwoCat":
            stat=float(sumdata[key])/float(cntdata[key])
            if display=="RMS Error":
               stat=sqrt(stat)
         else:
            hits=hitsdata[key]
            miss=missdata[key]
            falr=falrdata[key]
            corn=corndata[key]
            stat=self.VU.getTwoCatStat(statID,hits,miss,falr,corn)   
         #
         #
         #
         valmin=min(valmin,stat)
         valmax=max(valmax,stat)
         (parm,model,fhrstr,areaName)=key.split(",")
         areaNum=self.VU.getEditAreaNumberFromName(areaName)
         fhr=int(fhrstr)
         timemax=max(fhr,timemax)
         if fhr not in fhrList:
            fhrList.append(fhr)
         if parmVary==1:
            if modelVary==1:
               outkey="%s,%s"%(parm,model)
            else:
               #
               # ******** ????????  number or name?  What if 'current' or -1?
               #
               outkey="%s,%s"%(parm,self.VU.EditAreaDescriptions[int(areaNum)])
         else:
            outkey="%s,%s"%(self.VU.EditAreaDescriptions[int(areaNum)],model)
         if outkey not in outdata.keys():
            outdata[outkey]=[]
         outdata[outkey].append((fhr,stat))            
      #
      #  Bounded ones always show 0.0
      #
      if display in ["RMS Error","Std Dev","Mean Abs Error"]:
         valmin=0.0
      #
      #  If values are constant (compared to precision of this element)
      #  make the graph show slightly more range
      #
      prec1=10**(-parmPrecision)
      minRange=10**(-(parmPrecision+1))
      #minRange=0.1
      graphrange=valmax-valmin
      if graphrange<minRange:
         #valmax=valmax+1.0
         valmax+=(2*prec1)
      #
      #
      allkeys=outdata.keys()
      allkeys.sort()
      #
      #  Get lists of the actual buttons we have data for
      #
      varBut1=[]
      varBut2=[]
      for key in outdata.keys():
         (but1,but2)=key.split(",")
         if but1 not in varBut1:
            varBut1.append(but1)
         if but2 not in varBut2:
            varBut2.append(but2)
      varBut1.sort()
      varBut2.sort()
      #
      #  In any model list - make sure Official comes first
      #
      if ((parmVary==0)or((parmVary==1)and(modelVary==1))):
         if "Official" in varBut2:
            idx=varBut2.index("Official")
            del varBut2[idx]
            varBut2.insert(0,"Official")      
      #
      #  Associate colors with the varList2
      #
      self.colornames={}
      index=0
      for var in varBut2:
         self.colornames[var]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  setup buttons
      #
      self.setupBut1(varBut1,numbuttons=6,arrows=1)
      self.setupBut2(varBut2,numbuttons=6,arrows=1)
      #
      #  Setup graphing coordinates
      #
      numticks=10
      graphrange=valmax-valmin
      tickInterval=self.niceNumDec(graphrange/(numticks-1),1)
      
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(130.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(timemin,timemax,valmin,valmax,left,right,bot,top)
      self.fhouraxes(timemin,timemax,valmin,valmax)
      #
      #  Draw timeseries lines
      #
      for key in outdata.keys():
          (tag1,tag2)=key.split(",")
          tagtuple=(tag1,tag2)
          colorname=self.colornames[tag2]
          points=outdata[key]
          points.sort()
          gpoints=[]
          for point in points:
             (xtime,val)=point
             (x,y)=self.graphcoord(xtime,val)
             gpoints.append(x)
             gpoints.append(y)
          if len(gpoints)>3:
             self.cd.canvas.create_line(gpoints,fill=colorname,tags=tagtuple)
          self.but1state[tag1]=1
          self.but2state[tag2]=1
      #
      #  Turn off all but first model and first time
      #
      startBut1(self)
      startBut2(self)
      #
      #  Labels
      #
      if len(areaList)>1:
         areaName="Various"
         if AreaCombine==1:
            numPts=0
            for areaNum in areaNums:
               numPts+=self.pts[areaNum]
         else:
            numPts=-1
      else:
         #numPts=self.pts[areaNums[0]]
         numPts=-1
      ul1="Average Error Growth - %s"%parm
      self.cdLabels(ul1,numPts,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      
      return
   #==================================================================
   #  getStat - assuming that the statfile is open correctly, get
   #            the value for the specified 'display', for the
   #            record, area, and threshold number
   #
   def getStat(self,record,areaNum,display,threshold):
      if display=="Bias":
         val=self.VU.sncStats[record,areaNum,0]
      elif display=="Squared Error":
         val=self.VU.sncStats[record,areaNum,1]
      elif display=="RMS Error":
         val=sqrt(self.VU.sncStats[record,areaNum,1])
      elif display=="Std Dev":
         sum=self.VU.sncStats[record,areaNum,0]
         sqr=self.VU.sncStats[record,areaNum,1]
         val=sqrt(sqr-(sum*sum))
      elif display=="Mean Abs Error":
         val=self.VU.sncStats[record,areaNum,2]
      elif display=="Mean Fcst":
         val=self.VU.sncStats[record,areaNum,3]
      elif display=="Mean Squared Fcst":
         val=self.VU.sncStats[record,areaNum,4]
      elif display=="Mean Obs":
         val=self.VU.sncStats[record,areaNum,5]
      elif display=="Mean Squared Obs":
         val=self.VU.sncStats[record,areaNum,6]
      elif display=="Covariance":
         val=self.VU.sncStats[record,areaNum,7]
      elif display=="Percent Err <":
         val=self.VU.sncStats[record,areaNum,8+threshold]
      else:
         print "unknown stat type"
         val=0
      return val
   #==================================================================
   #  ShowScaleStats - make graphs of stat vs scale
   #
   def ShowScaleStats(self,DialogDict):
      self.VU.logMsg("running ShowScaleStats:")
      display=DialogDict["Display"]
      areaList=DialogDict["areaList"]
      AreaCombine=DialogDict["AreaCombine"]
      parm=DialogDict["Parm"]
      threshold=DialogDict["Threshold"]
      cycleList=DialogDict["cycleList"]
      modelList=DialogDict["Models"]
      obsmodel=DialogDict["ObsModel"]
      fcstrList=DialogDict["fcstrList"]
      fhrStart=DialogDict["fhrStart"]
      fhrEnd=DialogDict["fhrEnd"]
      dateType=DialogDict["dateType"]
      numDays=DialogDict["numDays"]
      fromDay=DialogDict["fromDay"]
      dayList=DialogDict["dayList"]
      dateStyle=DialogDict["dateStyle"]
      commonCases=DialogDict["commonCases"]
      accumHours=DialogDict["accumHours"]
      accumFreq=DialogDict["accumFreq"]
      TwoCatType=DialogDict["TwoCatType"]
      TwoCatCond=DialogDict["TwoCatCond"]
      TwoCatValue=DialogDict["TwoCatValue"]
      TwoCatValueString=DialogDict["TwoCatValueString"]
      #
      #  Check for good GUI input
      #
      parmList=[parm]
      ret=self.checkLists(modelList,parmList,cycleList,fcstrList,dateType,
                          dayList)
      if ret==0:
         return
      #
      #  Check that we do not have too many things varying
      #
      parmVary=0
      areaVary=0
      modelVary=1
      #
      #  Clear the cd canvas - setup title
      #
      self.cd.canvas.delete(Tkinter.ALL)
      self.cd.title("Statistic Graph")
      workStart="Working on Statistics vs. Scale"
      self.startWorking(workStart,optionRemove=0)
      #
      #  get names of selected edit areas
      #
      areaNames=[]
      descList=self.VU.listEditAreaDescriptions()
      nameList=self.VU.listEditAreas()
      for areaDesc in areaList:
         if areaDesc=="Current":
            areaNames.append("Current")
         elif areaDesc in descList:
            areaNum=descList.index(areaDesc)
            areaNames.append(nameList[areaNum])
      if len(areaNames)<1:
         msg="Invalid Edit Area(s) - contact support"
         self.statusBarMsg(msg,"U")
         return
      #
      #  Setup the combined area
      #
      comboArea=self._empty
      for areaName in areaNames:
         if areaName=="Current":
            areaObject=self.getActiveEditArea()
            mask=self.encodeEditArea(areaObject)
            any=add.reduce(add.reduce(mask))
            if any==0:
               mask=self._empty+1
         elif areaName=="NONE":
            mask=self._empty+1
         else:
            mask=self.encodeEditArea(areaName)
         comboArea=logical_or(comboArea,mask)      
      #
      #  If any of the TwoCat stats are requested - then get
      #  the contingency table entries instead
      #
      statName=display
      if statName=="TwoCat":
         statName="cont"
         if TwoCatType[0:1]=="A":
            statName="acont"
      statVal=TwoCatValue
      statCond=TwoCatCond
      statID=self.VU.getStatID(TwoCatType)
      #
      #
      #
      sumdata={}
      cntdata={}
      hitsdata={}
      missdata={}
      falrdata={}
      corndata={}
      #
      timemin=1e32
      timemax=-1e32
      valmin=1.0e32
      valmax=-1.0e32
      
      countimax=len(parmList)*len(modelList)
      counti=0
      #
      #  setup readParm - which is usually the same as parm, but
      #  can be different for the Spd/Dir components of a vector
      #
      readParm=parm
      vectorType=-1
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if (last3 in ["Spd","Dir"]):
            readParm=parm[:-3]
            if last3=="Spd":
               vectorType==0
            else:
               vectorType==1
      #
      #  Get the observed parm for this parm, the verification type,
      #  the data type and the thresholds
      #
      obsParm=self.VU.getObsParm(readParm)
      verType=self.VU.getVerType(readParm)
      datatype=self.VU.getVerParmType(readParm)
      thresholds=self.VU.getVerThresholds(readParm)
      if last3=="Spd":
         thresholds=thresholds[0]
      elif last3=="dir":
         thresholds=thresholds[1]
      thresholdValue=thresholds[threshold]
      #
      #  If using the threshold stat - set it now
      #
      if statName=="Percent Err <":
         statVal=thresholdValue
      #
      #  Get the statCases for this parm
      #
      statCases=self.VU.getStatCases(parm,modelList,obsmodel,dateStyle,
                   dateType,fromDay=fromDay,numDays=numDays,
                   dayList=dayList,fcstrs=fcstrList,cycles=cycleList,
                   fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                   accumFreq=accumFreq,commonCases=commonCases,
                   basetimeOffsets=1,callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      #
      #  Get the gridCases for this parm
      #
      gridCases=self.VU.getCases(readParm,modelList,obsParm,obsmodel,
                   dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                   dayList=dayList,fcstrs=fcstrList,cycles=cycleList,
                   fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                   accumFreq=accumFreq,requireObs=1,commonCases=commonCases,
                   basetimeOffsets=1,callbackMethod=self.workingCommon)
      if self.checkWorking()==1:
         self.stopWorking()
         return
      for model in modelList:
         counti+=1
         workStart="Reading %s %s (%d of %d)"%(parm,model,counti,countimax)
         #
         #  get cases for this model
         #
         scases=statCases[model]
         if model in gridCases.keys():
            gcases=gridCases[model]
         else:
            gcases={}
         #
         #  get overall list of keys (both stat and grid) in tkeys
         #
         skeys=scases.keys()
         gkeys=gcases.keys()
         tkeys=skeys
         for gkey in gkeys:
            if gkey not in tkeys:
               tkeys.append(gkey)
         #
         #  Loop over possible stat or grid cases
         #
         count=0
         totalcount=len(tkeys)
         for key in tkeys:
            #
            #  Check for user interrupting
            #
            count+=1
            if self.setAndCheckWorking("%s: %d of %d"%(workStart,count,totalcount))==1:
               self.stopWorking()
               return
            #
            #  Get times for this case
            #
            (basestr,stimestr,etimestr)=key.split(",")
            basetime=int(basestr)
            starttime=int(stimestr)
            endtime=int(etimestr)
            #
            #  Do not use results for grids that are not yet complete
            #
            if endtime>time.time():
               continue
            #
            #  Do not show results for grids that start before
            #  forecast time
            #
            fhr=(starttime-basetime)/HOURSECS
            if fhr<0:
               continue
            #
            #  Get list of records for this cases
            #
            if key in scases:
               srecList=scases[key]
            else:
               srecList=None
            if key in gcases:
               grecList=gcases[key]
            else:
               grecList=None
            #
            #  Loop over scales
            #
            smoothList=[]
            for (scale,text) in self.scaleList:
               smoothList.append(scale)
               
            valx=self.VU.getVerStatScales(model,basetime,readParm,starttime,
                           endtime,obsmodel,statName,statVal=statVal,
                           statCond=statCond,editArea=comboArea,
                           smoothList=smoothList,vectorType=vectorType,
                           grecList=grecList)
            if valx is None:
               print "getVerStatScales returned None"
               continue
            if len(valx)<1:
               print "getVerStatScales returned empty list"
               continue
            
            for i in xrange(len(smoothList)):
               (scale,text)=self.scaleList[i]
               val=valx[i]
               #
               #  store sums in model,fhr,scale keys
               #
               outkey="%s,%3.3d,%4.4d"%(model,fhr,scale)
               if display!="TwoCat":
                  if outkey not in sumdata.keys():
                     sumdata[outkey]=0.0
                     cntdata[outkey]=0
                  sumdata[outkey]+=val
                  cntdata[outkey]+=1
               else:
                  if outkey not in hitsdata.keys():
                     hitsdata[outkey]=0
                     missdata[outkey]=0
                     falrdata[outkey]=0
                     corndata[outkey]=0
                  (hits,miss,falr,corn)=val
                  hitsdata[outkey]+=hits
                  missdata[outkey]+=miss
                  falrdata[outkey]+=falr
                  corndata[outkey]+=corn
      #
      #  if no data could be read - stop here
      #
      if len(cntdata.keys())<1:
         self.stopWorking()
         msg="No verification data could be found matching those criteria"
         self.statusBarMsg(msg,"U")
         return         
      #
      #  We now have the sums...calculate the scores
      #
      fhrList=[]
      valmin=1e32
      valmax=-1e32
      scalemin=0
      scalemax=0
      outdata={}
      for key in cntdata.keys():
         if cntdata[key]<1:
            continue
         if display!="TwoCat":
            stat=float(sumdata[key])/float(cntdata[key])
            if display=="RMS Error":
               stat=sqrt(stat)
         else:
            hits=hitsdata[key]
            miss=missdata[key]
            falr=falrdata[key]
            corn=corndata[key]
            stat=self.VU.getTwoCatStat(statID,hits,miss,falr,corn)   
         #
         #
         #
         valmin=min(valmin,stat)
         valmax=max(valmax,stat)
         (model,fhrstr,scalestr)=key.split(",")
         fhr=int(fhrstr)
         scale=int(scalestr)
         scalemax=max(scale,scalemax)
         if fhr not in fhrList:
            fhrList.append(fhr)
         outkey="%s,%s"%(fhrstr,model)
         if outkey not in outdata.keys():
            outdata[outkey]=[]
         outdata[outkey].append((scale,stat))            
      #
      #  Bounded ones always show 0.0
      #
      if display in ["RMS Error","Std Dev","Mean Abs Error"]:
         valmin=0.0
      #
      #  If values are constant - show one up
      #
      graphrange=valmax-valmin
      if graphrange<0.01:
         valmax+=1.0
      #
      #
      allkeys=outdata.keys()
      allkeys.sort()
      #
      #  Get lists of the actual buttons we have data for
      #
      varBut1=[]
      varBut2=[]
      for key in outdata.keys():
         (but1,but2)=key.split(",")
         if but1 not in varBut1:
            varBut1.append(but1)
         if but2 not in varBut2:
            varBut2.append(but2)
      varBut1.sort()
      varBut2.sort()

      #
      #  In model list - make sure Official comes first
      #
      if "Official" in varBut2:
         idx=varBut2.index("Official")
         del varBut2[idx]
         varBut2.insert(0,"Official")      
      #
      #  Associate colors with the varList2
      #
      self.colornames={}
      index=0
      for var in varBut2:
         self.colornames[var]=self.COLORLIST[index]
         index+=1
         if index==len(self.COLORLIST):
            index=0
      #
      #  setup buttons
      #
      self.setupBut1(varBut1,numbuttons=6,arrows=1)
      self.setupBut2(varBut2,numbuttons=6,arrows=1)
      #
      #  Setup graphing coordinates
      #
      numticks=10
      graphrange=valmax-valmin
      tickInterval=self.niceNumDec(graphrange/(numticks-1),1)
      
      left=self.cd.curwidth*(50.0/700.0)
      right=self.cd.curwidth*(650.0/700.0)
      bot=self.cd.curheight*(130.0/530.0)
      top=self.cd.curheight*(480.0/530.0)
      self.setgraph(scalemin,scalemax,valmin,valmax,left,right,bot,top)
      #self.fhouraxes(timemin,timemax,valmin,valmax)
      #
      #  Draw timeseries lines
      #
      for key in outdata.keys():
          (tag1,tag2)=key.split(",")
          tagtuple=(tag1,tag2)
          colorname=self.colornames[tag2]
          points=outdata[key]
          points.sort()
          gpoints=[]
          for point in points:
             (scale,val)=point
             (x,y)=self.graphcoord(scale,val)
             gpoints.append(x)
             gpoints.append(y)
          if len(gpoints)>3:
             self.cd.canvas.create_line(gpoints,fill=colorname,tags=tagtuple)
          self.but1state[tag1]=1
          self.but2state[tag2]=1
      #
      #  Turn off all but first model and first time
      #
      startBut1(self)
      startBut2(self)
      #
      #  Labels
      #
      if len(areaList)>1:
         areaName="Various"
         if AreaCombine==1:
            numPts=0
            for areaNum in areaNums:
               numPts+=self.pts[areaNum]
         else:
            numPts=-1
      else:
         #numPts=self.pts[areaNums[0]]
         numPts=-1
      ul1="Average Error Growth - %s"%parm
      self.cdLabels(ul1,numPts,dateStyle,dateType,numDays,fromDay,dayList,cycleList)
      
      self.stopWorking()
      self.moveCD()
      self.cd.deiconify()
      self.cd.lift()
      
      return
   #==================================================================
   # getReadMode - figure out if parm is a rateParm...and set mode
   #                     to "Sum" if it is.
   #               If not...and checkProb is set...figure out if the
   #                     parm is a probability parm and set mode to
   #                     "Max" if it is (floating PoP).
   #               Otherwise set to "Average"
   #
   def getReadMode(self,model,parmName,checkProb=1):
      rateFlag=self.VU.getRateFlag(model,parmName)
      if (rateFlag==1):
         readMode="Sum"
      else:
         readMode="TimeWtAverage"
         if checkProb==1:
            verType=self.VU.getVerType(parmName)
            if verType==1:
               readMode="Max"
      return readMode

   #==================================================================
   #  workingCommon - suitable for a callback that provides a message
   #                  like (x of y), so that it sets the 'working'
   #                  display - and returns a 1 if the 'stop' button
   #                  has been set
   #
   def workingCommon(self,message):
      fullmsg="Finding Common Cases: %s"%message
      return self.setAndCheckWorking(fullmsg)
   #==================================================================
   #  setupBut1 - setup button 1 buttons. Names in butList are copied
   #              to self.but1names[]. Desired buttons on a row in
   #              numbuttons.  arrows flag adds 'prev/next' buttons.
   #
   #              After setup, self.but1names[] holds names.
   #                           self.but1{} holds button references
   #                           self.but1state{} holds button state
   def setupBut1(self,butList,numbuttons=5,arrows=0,width=0):
      #
      #  clear old buttons (fbar holds button 1s)
      #
      slaves=self.cd.fbar.pack_slaves()
      if slaves is not None:
         for slave in slaves:
            slave.destroy()
      #
      #  put generic 'move left' button on the side
      #
      if ((arrows==1)and(len(butList)>1)):
         cb=GenericCallback(prevBut1,self)
         prev=Tkinter.Button(self.cd.fbar,text="<",padx=2,pady=0,
                             fg="black",command=cb)
         prev.pack(side=Tkinter.LEFT,fill=Tkinter.Y)
      #
      #  figure number of rows of buttons - and create frames
      #
      numrows=int((float(len(butList))/float(numbuttons))+0.5)
      if numrows<1:
         numrows=1
      self.fbarmodrow=[]
      for i in xrange(numrows):
         self.fbarmodrow.append(Tkinter.Frame(self.cd.fbar))
      numinrow=int(float(len(butList))/float(numrows))+1
      #
      #  Make buttons
      #
      self.but1names=butList[:]
      self.but1text=butList[:]
      for i in xrange(len(self.but1names)):
         but=self.but1names[i]
         if but.isdigit():
            self.but1names[i]="f%s"%but
      num=1
      self.but1={}
      self.but1state={}
      for i in xrange(len(self.but1names)):
          but=self.but1names[i]
          buttext=self.but1text[i]
          cb=GenericCallback(showBut1,self,but)
          row=int(float(num)/float(numinrow))
          if width==0:
             self.but1[but]=Tkinter.Button(self.fbarmodrow[row],text=buttext,
                                           padx=2,pady=0,fg="black",
                                           command=cb)
          else:
             self.but1[but]=Tkinter.Button(self.fbarmodrow[row],text=buttext,
                                           width=width,padx=2,pady=0,fg="black",
                                           command=cb)           
          self.but1[but].pack(side=Tkinter.LEFT)
          num+=1
      #
      #  put generic 'move right' button on the side
      #
      if ((arrows==1)and(len(butList)>1)):
         cb=GenericCallback(nextBut1,self)
         next=Tkinter.Button(self.cd.fbar,text=">",padx=2,pady=0,
                             fg="black",command=cb)
         next.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      #
      #  pack buttons between possible next/prev buttons
      #
      for i in xrange(numrows):
         self.fbarmodrow[i].pack(side=Tkinter.TOP)
      #
      #  Update cd widget - so size of buttonbars don't affect size of
      #  the current canvas
      #
      self.cd.update_idletasks()
      hgt1=self.cd.bar.winfo_reqheight()
      hgt2=self.cd.fbar.winfo_reqheight()
      hgt3=28+536 # size of exit button bar + smallest canvas height
      hgt=hgt1+hgt2+hgt3
      self.cd.minsize(706,hgt)
      geo=self.cd.geometry()
      (wh,of)=geo.split("+",1)
      (wid,oldhgt)=wh.split("x",1)
      if hgt>int(oldhgt):
         self.cd.geometry("%sx%d+%s"%(wid,hgt,of))
      self.cd.update_idletasks()
      return
   #==================================================================
   #  setupBut2 - setup button 2 buttons. Names in butList are copied
   #              to self.but2names[]. Desired buttons on a row in
   #              numbuttons.  arrows flag adds 'prev/next' buttons.
   #
   #              After setup, self.but2names[] holds names.
   #                           self.but2{} holds button references
   #                           self.but2state{} holds button state
   #
   def setupBut2(self,butList,numbuttons=5,arrows=0,width=0):
      #
      #  remove old buttons (bar holds button 2s)
      #
      slaves=self.cd.bar.pack_slaves()
      if slaves is not None:
         for slave in slaves:
            slave.destroy()
      #
      #  put generic 'move left' button on the side
      #
      if ((arrows==1)and(len(butList)>1)):
         cb=GenericCallback(prevBut2,self)
         prev=Tkinter.Button(self.cd.bar,text="<",padx=2,pady=0,
                             fg="black",command=cb)
         prev.pack(side=Tkinter.LEFT,fill=Tkinter.Y)
      #
      #  figure number of rows of buttons - and create frames
      #
      numrows=int((float(len(butList))/float(numbuttons))+0.5)
      if numrows<1:
         numrows=1
      self.barmodrow=[]
      for i in xrange(numrows):
         self.barmodrow.append(Tkinter.Frame(self.cd.bar))
      numinrow=int(float(len(butList))/float(numrows))+1
      #
      #  Make buttons
      #
      self.but2names=butList[:]
      num=1
      self.but2={}
      self.but2state={}
      for i in xrange(len(self.but2names)):
          but=self.but2names[i]
          cb=GenericCallback(showBut2,self,but)
          row=int(float(num)/float(numinrow))
          if width==0:
             self.but2[but]=Tkinter.Button(self.barmodrow[row],text=but,
                                           padx=2,pady=0,fg=self.colornames[but],
                                           command=cb)
          else:
             self.but2[but]=Tkinter.Button(self.barmodrow[row],text=but,width=width,
                                           padx=2,pady=0,fg=self.colornames[but],
                                           command=cb)
          self.but2[but].pack(side=Tkinter.LEFT)
          num+=1
      #
      #  put generic 'move right' button on the side
      #
      if ((arrows==1)and(len(butList)>1)):
         cb=GenericCallback(nextBut2,self)
         next=Tkinter.Button(self.cd.bar,text=">",padx=2,pady=0,
                             fg="black",command=cb)
         next.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      #
      #  pack buttons between possible next/prev buttons
      #
      for i in xrange(numrows):
         self.barmodrow[i].pack(side=Tkinter.TOP)
      #
      #  Update cd widget and its minsize - so size of buttonbar
      #  doesn't affect size of the current canvas
      #
      self.cd.update_idletasks()
      hgt1=self.cd.bar.winfo_reqheight()
      hgt2=self.cd.fbar.winfo_reqheight()
      hgt3=28+536 # size of exit button bar + smallest canvas height
      hgt=hgt1+hgt2+hgt3
      self.cd.minsize(706,hgt)
      geo=self.cd.geometry()
      (wh,of)=geo.split("+",1)
      (oldwid,oldhgt)=wh.split("x",1)
      if hgt>int(oldhgt):
         self.cd.geometry("%sx%d+%s"%(oldwid,hgt,of))
      self.cd.update_idletasks()
      return
   #==================================================================
   #  cdLabels - labels at the top of the canvas
   #
   def cdLabels(self,ul1,numPts,dateStyle,dateType,numDays,fromDay,dayList,cycleList):
      #
      #  Upper Left has variable text
      #
      self.labelLine(ul1,1,justify="left")
      #
      str="Gridpoints in editarea: %d"%numPts
      self.labelLine(str,2,justify="left")
      #
      #  Dates
      #
      timelabel=dateStyle
      if dateType=="Period Length":
         (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(fromDay)
         if numDays==1:
            timelabel+=" %4.4d/%2.2d/%2.2d"%(gyea,gmon,gday)
         else:  
            timelabel+=" the %d days ending on %4.4d/%2.2d/%2.2d"%(numDays,gyea,gmon,gday)
      else:
         if len(dayList)==1:
            (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(dayList[0])            
            timelabel+=" %4.4d/%2.2d/%2.2d"%(gyea,gmon,gday)
         else:
            timelabel+=" several dates"
      self.labelLine(timelabel,1,justify="right")
      #
      #  Cycles
      #
      runlabel=""
      if len(cycleList)>1:
         for cyc in cycleList:
            runlabel+="%2.2d+"%cyc
         runlabel=runlabel[:-1]+" UTC Runs"
      else:
         runlabel="%2.2d"%cycleList[0]+" UTC Run ONLY"
      self.labelLine(runlabel,2,justify="right")
      return
   #==================================================================
   #  labLine - draw a label
   #
   def labelLine(self,text,lineNum,color="black",justify="left",
                 tags=None):
      lineheight=15
      yoff=5
      xoff=5
      
      y=((lineNum-1)*lineheight)+yoff
      if justify=="left":
         x=xoff
         anchorType=Tkinter.NW
      else:
         x=self.cd.curwidth-xoff
         anchorType=Tkinter.NE
      self.cd.canvas.create_text(x,y,text=text,fill=color,
                                 anchor=anchorType,tags=tags)
      return         
   #==================================================================
   #  checkLists - check lists returned from GUI to make sure at least
   #               one is chosen
   #
   def checkLists(self,modelList,parmList,cycleList,fcstrList,dateType,
                  dayList):
      if (len(modelList)<1):
         self.statusBarMsg("Must choose at least one model","U")
         return 0
      if (len(parmList)<1):
         self.statusBarMsg("Must choose at least one parm","U")
         return 0
      if (len(cycleList)<1):
         self.statusBarMsg("Must choose at least one cycle","U")
         return 0
      if (len(fcstrList)<1):
         self.statusBarMsg("Must choose at least one forecaster","U")
         return 0
      if dateType=="List of dates":
         if (len(dayList)<1):
            self.statusBarMsg("Must choose at least one date","U")
            return 0
      return 1
   #==================================================================
   #
   #  code to scale everything on the canvas so that you always display
   #  the same area that you started with
   #
   def resizecanvas(self,event):
      scalex=float(event.width)/self.curwidth
      scaley=float(event.height)/self.curheight
      if ((scalex!=1.0)or(scaley!=1.0)):
         self.canvas.scale("all",0.0,0.0,scalex,scaley)
      self.curwidth=float(event.width)
      self.curheight=float(event.height)
      #bw=2
      #self.canwidth=self.curwidth-((bw+1.0)*2.0)
      #self.canheight=self.canheight-((bw+1.0)*2.0)
      #print "resize canvas gives width/height as: %7.2f,%7.2f"%(self.curwidth,self.curheight)
      return
   #
   #  setup graph coordintes
   #
   def setgraph(self,xmin,xmax,ymin,ymax,sxmin,sxmax,symin,symax):
      self.xmin=xmin
      self.xmax=xmax
      self.ymin=ymin
      self.ymax=ymax
      self.xmult=(sxmax-sxmin)/(xmax-xmin)
      self.xoff=sxmin
      self.ymult=(symax-symin)/(ymax-ymin)
      self.yoff=symax
   def graphcoord(self,x,y):
      newx=((x-self.xmin)*self.xmult)+self.xoff
      newy=self.yoff-((y-self.ymin)*self.ymult)
      return newx,newy
   #==================================================================
   #
   #  draw histogram axes
   #
   def histoaxes(self,maxheight,minx,maxx,binwidth,htick):
      (sx,sy)=self.graphcoord(0.0,0.0)
      (tx,ty)=self.graphcoord(0.0,maxheight)
      self.cd.canvas.create_line(sx,sy,tx,ty)
      self.vtick(0.0,5,0.0,maxheight,htick,label=1,labeloffset=10,
                 skipfirst=1,labelinterval=2)
      #minx=binmin[1]+(binwidth/2.0)
      #maxx=binmax[len(binmax)-2]-(binwidth/2.0)
      (sx,sy)=self.graphcoord(minx,0.0)
      (tx,ty)=self.graphcoord(maxx,0.0)
      self.cd.canvas.create_line(sx,sy,tx,ty)
      numticks=10
      tickInterval=self.niceNumDec(maxx/(numticks-1),1)
      self.htick(0.0,5,0.0,maxx,tickInterval,label=1,
                 labeloffset=5,labelinterval=2,skipfirst=1)
      self.htick(0.0,5,0.0,maxx,tickInterval,label=1,
                 labeloffset=5,labelinterval=2,skipfirst=1,negative=1)
   #==================================================================
   #  probaxes - draw axes for probability reliability diagrams
   #
   def probaxes(self):
      (llx,lly)=self.graphcoord(0,0)
      (urx,ury)=self.graphcoord(100,100)
      self.cd.canvas.create_line(llx,lly,urx,lly,urx,ury,llx,ury,llx,lly,urx,ury)
      self.vtick(0,5,0,100,10,label=1,
                 labelinterval=1,labeloffset=-10.0,labelanchor=Tkinter.E)
      self.vtick(100,5,0,100,10,label=1,
                 labelinterval=1,labeloffset=10.0,labelanchor=Tkinter.W)
      self.htick(0,5,0,100,10,label=1,
                 labelinterval=1,labeloffset=8.0,labelanchor=Tkinter.N)
      self.htick(100,5,0,100,10,label=1,
                 labelinterval=1,labeloffset=-8.0,labelanchor=Tkinter.S)
      (midx,ny)=self.graphcoord(50,0)
      self.cd.canvas.create_text(midx,ny+20,anchor=Tkinter.N,text="Forecast Probability")
      (nx,midy)=self.graphcoord(0,50)
      self.cd.canvas.create_text(nx-35,midy,anchor=Tkinter.E,text="O\nb\ns\ne\nr\nv\ne\nd\n \nF\nr\ne\nq\nu\ne\nc\ny")      
   #
   #==================================================================
   #
   #  draw graph axes
   #
   def graphaxes(self,timemin,timemax,valmin,valmax):
      (llx,lly)=self.graphcoord(timemin,valmin)
      (urx,ury)=self.graphcoord(timemax,valmax)
      self.cd.canvas.create_line(llx,lly,urx,lly,urx,ury,llx,ury,llx,lly)
      zeroline=0
      if ((valmin<0.0)and(valmax>0.0)):
         (lx,zy)=self.graphcoord(timemin,0.0)
         self.cd.canvas.create_line(llx,zy,urx,zy)
         zeroline=1
      numticks=10
      self.timetick(timemin,timemax,numticks,valmin,5,label=1,labeloffset=10)
      self.timetick(timemin,timemax,numticks,valmax,5,label=1,labeloffset=-10,
                    labelanchor=Tkinter.S)
      if zeroline==1:
         self.timetick(timemin,timemax,numticks,0.0,5,label=0)
      #
      numticks=10
      valInterval=self.niceNumDec((valmax-valmin)/(numticks-1),1)
      if zeroline==1:
         self.vtick(timemin,5,0,valmax,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E)
         self.vtick(timemin,5,0,-valmin,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E,negative=1)
         self.vtick(timemax,5,0,valmax,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W)
         self.vtick(timemax,5,0,-valmin,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W,negative=1)
      else:
         self.vtick(timemin,5,valmin,valmax,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E)
         self.vtick(timemax,5,valmin,valmax,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W)
   #==================================================================
   #
   #  draw fhour axes
   #
   def fhouraxes(self,timemin,timemax,valmin,valmax):
      (llx,lly)=self.graphcoord(timemin,valmin)
      (urx,ury)=self.graphcoord(timemax,valmax)
      self.cd.canvas.create_line(llx,lly,urx,lly,urx,ury,llx,ury,llx,lly)
      zeroline=0
      if ((valmin<0.0)and(valmax>0.0)):
         (lx,zy)=self.graphcoord(timemin,0.0)
         self.cd.canvas.create_line(llx,zy,urx,zy)
         zeroline=1
      finterval=6
      if timemax>120:
         finterval=24
      if timemax>48:
         finterval=12
      #finterval=self.niceNumDec((timemax-timemin)/(numticks-1),1)
      self.htick(valmin,5,timemin,timemax,finterval,label=1,labeloffset=+5,
                 labelanchor=Tkinter.N)
      self.htick(valmax,5,timemin,timemax,finterval,label=1,labeloffset=-5,
                 labelanchor=Tkinter.S)
      if zeroline==1:
         self.htick(0.0,5,timemin,timemax,finterval,label=0)
      #self.timetick(timemin,timemax,numticks,valmin,5,label=1,labeloffset=10)
      #self.timetick(timemin,timemax,numticks,valmax,5,label=1,labeloffset=-10,
      #              labelanchor=Tkinter.S)
      #if zeroline==1:
      #   self.timetick(timemin,timemax,numticks,0.0,5,label=0)
      #
      numticks=10
      valInterval=self.niceNumDec((valmax-valmin)/(numticks-1),1)
      if zeroline==1:
         self.vtick(timemin,5,0,valmax,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E)
         self.vtick(timemin,5,0,-valmin,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E,negative=1,
                    skipfirst=1)
         self.vtick(timemax,5,0,valmax,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W)
         self.vtick(timemax,5,0,-valmin,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W,negative=1,
                    skipfirst=1)
      else:
         self.vtick(timemin,5,valmin,valmax,valInterval,label=1,
                    labeloffset=-10,labelanchor=Tkinter.E)
         self.vtick(timemax,5,valmin,valmax,valInterval,label=1,
                    labeloffset=10,labelanchor=Tkinter.W)
   #==================================================================
   #
   def timetick(self,minsecs,maxsecs,desirednum,yval,ywid,label=1,labeloffset=-10,labelanchor=Tkinter.N):
      #print "in timetick with %d-%d, desired:%d"%(minsecs,maxsecs,desirednum)
      numrange=desirednum*0.75
      minnum=desirednum-numrange
      maxnum=desirednum+numrange
      monString=[" ","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"]
      #print "minnum-maxnum: %d-%d"%(minnum,maxnum)
      HOUR=3600
      DAY=24*HOUR
      MONTH=30*DAY
      YEAR=365*DAY
      tryintervals=[(3,HOUR),
                    (6,HOUR),
                    (12,HOUR),
                    (1,DAY),
                    (2,DAY),
                    (5,DAY),
                    (15,DAY),
                    (1,MONTH),
                    (2,MONTH),
                    (3,MONTH),
                    (6,MONTH),
                    (1,YEAR)]

      secondsRange=maxsecs-minsecs
      intervals=[]
      for (interval,base) in tryintervals:
         intervalSeconds=interval*base
         num=int(secondsRange/intervalSeconds)
         if ((num>minnum)and(num<maxnum)):
            #print "potential interval of %d * %d seconds - %d ticks"%(interval,base,num)
            intervals.append((interval,base,num))
      #
      if len(intervals)<1:
         #print "Could not find suitable interval"
         #print "-----"
         return
      #
      mindiff=desirednum
      bestInterval=0
      bestBase=0
      for (interval,base,num) in intervals:
         diff=abs(desirednum-num)
         if diff<mindiff:
            mindiff=diff
            bestInterval=interval
            bestBase=base
      #print "best interval: %d * %d seconds"%(bestInterval,bestBase)
      #
      #
      #
      (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(minsecs)
      gmin=0
      gsec=0
      #
      if bestBase==HOUR:
         ghou=0
      elif bestBase==DAY:
         gday=1
         ghou=0
      else:
         gmon=1
         gday=1
         ghou=0
      newTime=calendar.timegm((gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst))
      
   
      while (newTime<=maxsecs):
         (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(newTime)
         if newTime>minsecs:
            (tx,ty)=self.graphcoord(newTime,yval)
            self.cd.canvas.create_line(tx,ty-ywid,tx,ty+ywid)
            if label==1:
               if bestBase==HOUR:
                  if ((maxsecs-minsecs)/HOUR)>24:
                     labelstring="%d %2.2dZ"%(gday,ghou)
                  else:
                     labelstring="%2.2dZ"%ghou
               elif bestBase==DAY:
                  if ((maxsecs-minsecs)/DAY)>28:
                     labelstring="%d/%d"%(gmon,gday)
                  else:
                     labelstring="%d"%gday
               else:
                  if ((maxsecs-minsecs)/MONTH)>9:
                     labelstring="%d/%2.2d"%(gmon,gyea%100)
                  else:
                     labelstring="%s"%monString[gmon]
               #labelstring="%d/%d %2.2dZ"%(gmon,gday,ghou)
               self.cd.canvas.create_text(tx,ty+labeloffset,anchor=labelanchor,
                                             text=labelstring)
            
            #print "tick at %4.4d/%2.2d/%2.2d %2.2dZ"%(gyea,gmon,gday,ghou)
         if bestBase==HOUR:
            tryTime=calendar.timegm((gyea,gmon,gday,ghou+bestInterval,gmin,gsec,gwda,gyda,gdst))
         elif bestBase==DAY:
            tryTime=calendar.timegm((gyea,gmon,gday+bestInterval,ghou,gmin,gsec,gwda,gyda,gdst))
         else:
            newmon=gmon+bestInterval
            if newmon>12:
               gyea+=1
               newmon=1
            tryTime=calendar.timegm((gyea,newmon,gday,ghou,gmin,gsec,gwda,gyda,gdst))
         (nyea,nmon,nday,nhou,nmin,nsec,nwda,nyda,ndst)=time.gmtime(tryTime)
         if ((nday!=gday)and(bestBase==HOUR)):
            gday+=1
            ghou=0
            tryTime=calendar.timegm((gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst))
         elif (((nmon!=gmon)or(nday>26))and(bestBase==DAY)):
            gmon+=1
            if gmon>12:
               gmon=1
               gyea+=1
            gday=1
            tryTime=calendar.timegm((gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst))
         elif ((nyea!=gyea)and(bestBase==MONTH)):
            gyea+=1
            gmon=1
            tryTime=calendar.timegm((gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst))
         newTime=tryTime
      #print "-----"
      return
   #==================================================================
   #  draw ticks on a horizontal axis at y-value:yval.  The width of
   #    ticks if ywid.  Ticks go between minx and maxx with interval
   #    tickinterval.  If labelinterval is 0, no labels are drawn,
   #    if 1, the every label is drawn.  If labelinterval is 2 then
   #    every 2nd label is drawn.  It labelinterval is 3 then every
   #    3rd label is drawn, etc.  If skipfirst=1 or skiplast=1 then
   #    labelling is skipped for those ticks.
   #
   def htick(self,yval,ywid,minx,maxx,tickInterval,label=0,labeloffset=5,
             labelinterval=1,skipfirst=0,skiplast=0,labelanchor=Tkinter.N,
             negative=0):
      numticks=int((maxx-minx)/tickInterval)+1
      labeldigits=max(-floor(log10(tickInterval)),0)
      neg=1.0
      if negative==1:
         neg=-1.0
      num=0
      for i in xrange(0,numticks):
         x=(minx+(i*tickInterval))*neg
         (tx,ty)=self.graphcoord(x,yval)
         self.cd.canvas.create_line(tx,ty-ywid,tx,ty+ywid)
         if label==1:
            if (i%labelinterval==0):
               if labeldigits==0:
                  labelstring="%d"%x
               else:
                  format="%%.%df"%labeldigits
                  labelstring=format%x
               skip=0
               if ((skipfirst==1)and(i==0))or((skiplast==1)and(i==(numticks-1))):
                  skip=1
               if skip==0:
                  self.cd.canvas.create_text(tx,ty+labeloffset,anchor=labelanchor,
                                          text=labelstring)
   def vtick(self,xval,xwid,miny,maxy,tickInterval,label=0,labeloffset=5,
             labelinterval=1,skipfirst=0,skiplast=0,labelanchor=Tkinter.W,
             negative=0):
      numticks=int((maxy-miny)/tickInterval)+1
      labeldigits=max(-floor(log10(tickInterval)),0)
      neg=1.0
      if negative==1:
         neg=-1.0
      num=0
      for i in xrange(0,numticks):
         y=(miny+(i*tickInterval))*neg
         (tx,ty)=self.graphcoord(xval,y)
         self.cd.canvas.create_line(tx-xwid,ty,tx+xwid,ty)
         if label==1:
            if (i%labelinterval==0):
               if labeldigits==0:
                  labelstring="%d"%y
               else:
                  format="%%.%df"%labeldigits
                  labelstring=format%y
               skip=0
               if ((skipfirst==1)and(i==0))or((skiplast==1)and(i==(numticks-1))):
                  skip=1
               if skip==0:
                  self.cd.canvas.create_text(tx+labeloffset,ty,anchor=labelanchor,
                                          text=labelstring)
      
   #==================================================================
   #
   #  make axes for value distributions
   #
   def valaxes(self,minval,maxval,tickInterval):
      (nx,ny)=self.graphcoord(minval,minval)
      (xx,xy)=self.graphcoord(maxval,maxval)
      self.cd.canvas.create_line(nx,ny,xx,ny,xx,xy,nx,xy,nx,ny,xx,xy)
      self.vtick(minval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-10.0,labelanchor=Tkinter.E)
      self.vtick(maxval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=10.0,labelanchor=Tkinter.W)
      self.htick(minval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=8.0,labelanchor=Tkinter.N)
      self.htick(maxval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-8.0,labelanchor=Tkinter.S)
      (midx,dumy)=self.graphcoord((maxval+minval)/2.0,minval)
      self.cd.canvas.create_text(midx,ny+20,anchor=Tkinter.N,text="Observed")
      (dumx,midy)=self.graphcoord(minval,(maxval+minval)/2.0)
      self.cd.canvas.create_text(nx-35,midy,anchor=Tkinter.E,text="F\no\nr\ne\nc\na\ns\nt")
   #==================================================================
   #
   #  make axes for expected value distributions
   #
   def expaxes(self,minval,maxval,tickInterval):
      (nx,ny)=self.graphcoord(minval,minval)
      (xx,xy)=self.graphcoord(maxval,maxval)
      self.cd.canvas.create_line(nx,ny,xx,ny,xx,xy,nx,xy,nx,ny,xx,xy)
      self.vtick(minval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-10.0,labelanchor=Tkinter.E)
      self.vtick(maxval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=10.0,labelanchor=Tkinter.W)
      self.htick(minval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=8.0,labelanchor=Tkinter.N)
      self.htick(maxval,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-8.0,labelanchor=Tkinter.S)
      (midx,dumy)=self.graphcoord((maxval+minval)/2.0,minval)
      self.cd.canvas.create_text(midx,ny+20,anchor=Tkinter.N,text="Forecast")
      (dumx,midy)=self.graphcoord(minval,(maxval+minval)/2.0)
      self.cd.canvas.create_text(nx-35,midy,anchor=Tkinter.E,text="O\nb\ns\ne\nr\nv\ne\nd")
   #==================================================================
   #
   #  make axes for value histograms
   #
   def valhaxes(self,minval,maxval,tickInterval,maxnum,vint,parm):
      (nx,ny)=self.graphcoord(minval,0)
      (xx,xy)=self.graphcoord(maxval,maxnum)
      self.cd.canvas.create_line(nx,ny,xx,ny,xx,xy,nx,xy,nx,ny)
      #vint=self.niceNumDec(maxnum/20,1)
      self.vtick(minval,5,0.0,maxnum,vint,label=1,
                 labelinterval=3,labeloffset=-10.0,labelanchor=Tkinter.E)
      self.vtick(maxval,5,0.0,maxnum,vint,label=1,
                 labelinterval=3,labeloffset=10.0,labelanchor=Tkinter.W)
      self.htick(0,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=8.0,labelanchor=Tkinter.N)
      self.htick(maxnum,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-8.0,labelanchor=Tkinter.S)
      (midx,dumy)=self.graphcoord((maxval+minval)/2.0,minval)
      self.cd.canvas.create_text(midx,ny+20,anchor=Tkinter.N,text=parm)
      (dumx,midy)=self.graphcoord(minval,(0+maxnum)/2.0)
      self.cd.canvas.create_text(nx-35,midy,anchor=Tkinter.E,text="N\nu\nm\nb\ne\nr\n \np\ne\nr\n \nc\na\ns\ne")
   #==================================================================
   #
   #  make axes for logarithmic value histograms
   #
   def logvalhaxes(self,minval,maxval,tickInterval,logmin,logmax,parm):
      (nx,ny)=self.graphcoord(minval,logmin)
      (xx,xy)=self.graphcoord(maxval,logmax)
      self.cd.canvas.create_line(nx,ny,xx,ny,xx,xy,nx,xy,nx,ny)
      #vint=self.niceNumDec(maxnum/20,1)
      lownum=exp(logmin)
      hignum=exp(logmax)
      print "need ticks from %f to %f"%(lownum,hignum)
      expstart=int(floor(log10(lownum)))
      expend=int(floor(log10(hignum)))
      print "  exponents from %d to %d"%(expstart,expend)
      for j in xrange(expstart,expend+1):
         a=10.0**j
         print "    loop decade=%f"%a
         for i in xrange(1,10):
            if i==1:
               xwid=5
            else:
               xwid=2
            val=float(i)*a
            if ((val>lownum)and(val<hignum)):
               logy=log(val)
               (nx,ty)=self.graphcoord(minval,logy)
               self.cd.canvas.create_line(nx-xwid,ty,nx+xwid,ty)
               (xx,ty)=self.graphcoord(maxval,logy)
               self.cd.canvas.create_line(xx-xwid,ty,xx+xwid,ty)
               if xwid==5:
                  if j>=0:
                     labelstring="%d"%val
                  else:
                    format="%%.%df"%abs(j)
                    labelstring=format%val
                  self.cd.canvas.create_text(nx-10.0,ty,anchor=Tkinter.E,
                                          text=labelstring)
                  self.cd.canvas.create_text(xx+10.0,ty,anchor=Tkinter.W,
                                          text=labelstring)
                  
               
      #self.vtick(minval,5,0.0,maxnum,vint,label=1,
      #           labelinterval=3,labeloffset=-10.0,labelanchor=Tkinter.E)
      #self.vtick(maxval,5,0.0,maxnum,vint,label=1,
      #           labelinterval=3,labeloffset=10.0,labelanchor=Tkinter.W)
      self.htick(logmin,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=8.0,labelanchor=Tkinter.N)
      self.htick(logmax,5,minval,maxval,tickInterval,label=1,
                 labelinterval=3,labeloffset=-8.0,labelanchor=Tkinter.S)
      (midx,dumy)=self.graphcoord((maxval+minval)/2.0,logmin)
      self.cd.canvas.create_text(midx,ny+20,anchor=Tkinter.N,text=parm)
      (dumx,midy)=self.graphcoord(minval,(logmin+logmax)/2.0)
      self.cd.canvas.create_text(nx-35,midy,anchor=Tkinter.E,text="N\nu\nm\nb\ne\nr\n \np\ne\nr\n \nc\na\ns\ne")
   #==================================================================
   #  niceNumDec - pick a nice decimal number - suitable for tick
   #               marks, etc.
   #
   def niceNumDec(self,val,roundit):
      if val==0:
         return 1
      e=floor(log10(val))
      a=10.0**e
      f=val/a
      if roundit>0:
         if f<1.5:
            nf=1
         elif f<3.0:
            nf=2
         elif f<7.0:
            nf=5
         else:
            nf=10
      else:
         if f<=1.0:
            nf=1
         elif f<=2.0:
            nf=2.0
         elif f<=5.0:
            nf=5.0
         else:
            nf=10.0
      return nf*a
   #==================================================================
   #  showScore - draw tick on 'colorcurve' with label of 0-100 score
   #  
   def showScore(self,fullscore,mod,color,taglabel):
      midx=self.cd.curwidth/2.0
      #x=midx+(128*((-fullscore+0.5)/0.5))
      if fullscore<0:
         fullscore=0.0
      x=midx+(128*((fullscore-50.0)/50.0))
      txt="%d"%int(fullscore)
      if mod=="Official":
         self.cd.canvas.create_line(x,50-8,x,50-3,fill=color,tags=taglabel)
         self.cd.canvas.create_text(x,50-8,text=txt,fill=color,anchor=Tkinter.S,tags=taglabel)
      else:
         self.cd.canvas.create_line(x,50+3,x,50+8,fill=color,tags=taglabel)
         self.cd.canvas.create_text(x,50+8,text=txt,fill=color,anchor=Tkinter.N,tags=taglabel)
   #==================================================================
   #  showWorse - show the number in the first/last bins - which are
   #              worse than the error limits
   #
   def showWorse(self,low,high,xmax,yoffset,color,taglist):
      x=xmax
      y=0
      (sx,sy)=self.graphcoord(x,y)
      self.cd.canvas.create_text(sx+5,sy,text="Worse",anchor=Tkinter.W)
      textstring="%d"%high
      self.cd.canvas.create_text(sx+5,sy-yoffset,text=textstring,anchor=Tkinter.W,fill=color,tags=taglist)
      x=-xmax
      y=0
      (sx,sy)=self.graphcoord(x,y)
      self.cd.canvas.create_text(sx-5,sy,text="Worse",anchor=Tkinter.E)
      textstring="%d"%low
      self.cd.canvas.create_text(sx-5,sy-yoffset,text=textstring,anchor=Tkinter.E,fill=color,tags=taglist)
   #==================================================================
   #  showScores - display modelname,n,avg,std,mae,rms on histogram
   #
   def showScores(self,modnum,mod,num,avg,std,mae,rms,color,taglist):
      font=12
      ystart=self.cd.curheight*(135.0/530.0)
      y=ystart+font+(modnum*font)
      x=self.cd.curwidth*(80.0/700.0)
      self.cd.canvas.create_text(x,y,text=mod,anchor=Tkinter.E,fill=color,tags=taglist)
      textstring="%2d"%num
      x=self.cd.curwidth*(130.0/700.0)
      self.cd.canvas.create_text(x,y,text=textstring,anchor=Tkinter.E,
                              fill=color,tags=taglist)
      textstring="%6.2f"%avg
      x=self.cd.curwidth*(170.0/700.0)
      self.cd.canvas.create_text(x,y,text=textstring,anchor=Tkinter.E,
                              fill=color,tags=taglist)
      textstring="%5.2f"%std
      x=self.cd.curwidth*(210.0/700.0)
      self.cd.canvas.create_text(x,y,text=textstring,anchor=Tkinter.E,
                              fill=color,tags=taglist)
      textstring="%5.2f"%mae
      x=self.cd.curwidth*(250.0/700.0)
      self.cd.canvas.create_text(x,y,text=textstring,anchor=Tkinter.E,
                              fill=color,tags=taglist)
      textstring="%5.2f"%rms
      x=self.cd.curwidth*(290.0/700.0)
      self.cd.canvas.create_text(x,y,text=textstring,anchor=Tkinter.E,
                              fill=color,tags=taglist)
      return
   #==================================================================
   #  showAvg - draw arrow on histogram axis at average value
   #
   def showAvg(self,avg,color,taglist):
      (sx,sy)=self.graphcoord(avg,0)
      self.cd.canvas.create_line(sx,sy+30,sx,sy,fill=color,
                              arrow=Tkinter.LAST,tags=taglist)
      textstring="%.2f"%avg
      self.cd.canvas.create_text(sx,sy+30,text=textstring,anchor=Tkinter.N,
                              fill=color,tags=taglist)
   #==================================================================
   #
   #  getBins - given a bin width and maxerr value, return
   #            lists of each bin's min,max, with one of them
   #            centerred on zero.  Last bins may start up to
   #            a half binwidth more than maxerr
   #
   def getBins(self,binwidth,maxerr):
      halfbin=float(binwidth)/2.0
      binmin=[]
      binmax=[]

      mid=0.0
      while ((mid+halfbin)<=(maxerr+halfbin)):
         maxx=mid+halfbin
         binmin.append(-maxx)
         binmax.append(-maxx+binwidth)
         binmin.append(maxx-binwidth)
         binmax.append(maxx)
         mid+=binwidth
      binmin.append(-900000.0)
      binmax.append(-(mid-halfbin))
      binmin.append(mid-halfbin)
      binmax.append(9000000.0)
      binmin.sort()
      binmax.sort()
      return(binmin,binmax)
   #
   def getProbBins(self,binwidth):
      binmin=[]
      binmax=[]
      halfbin=float(binwidth)/2.0
      num=int(100.0/float(binwidth))
      for i in xrange(num):
         binmid=i*binwidth
         bot=max(binmid-halfbin,0)
         top=min(binmid+halfbin,101)
         binmin.append(bot)
         binmax.append(top)
      return(binmin,binmax)
   #==================================================================
   #
   #  binerrs - given a 1-D array of errors, create a 1-D array of
   #            the number of points with errors inside each bin
   #            described by the binmin and binmax limits
   #
   def binerrs(self,err,abinmin,abinmax):
      bincnt=add.reduce(logical_and(greater(err,abinmin[:,NewAxis]),
                                   less_equal(err,abinmax[:,NewAxis])),-1)
      return bincnt
   def histosetup(self,minlimit,maxlimit,binwidth):
      self.histowidth=binwidth
      self.histohalf=binwidth/2.0
      self.histomin=minlimit-self.histohalf
      self.histomax=maxlimit+self.histohalf
      self.histonumbins=int(float(self.histomax-self.histomin)/float(self.histowidth))
      self.histobins=resize(arange(self.histonumbins+1),(self.histonumbins+1,1))
      return
   def histo(self,data):
      worseLow=add.reduce(less(data,self.histomin))
      worseHigh=add.reduce(greater(data,self.histomax))
      data=repeat(data,logical_and(less_equal(data,self.histomax),
                                   greater_equal(data,self.histomin)))
      data=((data-self.histomin)/self.histowidth).astype(int)
      histoData=add.reduce(equal(self.histobins,data),-1)
      histoData[-2]+=histoData[-1]
      return histoData[:-1],worseLow,worseHigh
   def hitcount(self,data,verif):
      numless=add.reduce(less(data,self.histomin))
      numgreater=add.reduce(greater(data,self.histomax))
      data=repeat(data,logical_and(less_equal(data,self.histomax),
                                   greater_equal(data,self.histomin)))
      data=((data-self.histomin)/self.histowidth).astype(int)
      d1=equal(self.histobins,data)
      self.VU.logMsg("shape of d1=%s"%str(d1.shape))
      self.VU.logMsg("shape of verif=%s"%str(verif.shape))
      histoData=add.reduce(d1,-1)
      self.VU.logMsg("done with histoData reduce")
      #hitCount=add.reduce(where(d1,verif,0),-1)
      a=where(d1,verif,0)
      hitCount=add.reduce(a,-1)
      self.VU.logMsg("done with hitCount reduce")
      histoData[-2]+=histoData[-1]
      hitCount[-2]+=hitCount[-1]
      self.VU.logMsg("returning")
      return histoData[:-2],hitCount[:-2]
      
   #=================================================================
   # setupGM - setup Grid Manager - remove all parms from display
   #           except for parms in parmList for models in modelList
   #           (or in mutableModel) and WG1 for the mutableModel
   #           (if available)
   #
   def setupGM(self,parmList,modelList):
      #
      #
      #
      newParmList=[]
      for parm in parmList:
         if (len(parm)>3):
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               realname=parm[:-3]
               if realname not in newParmList:
                  newParmList.append(realname)
            else:
               newParmList.append(parm)
         else:
            newParmList.append(parm)
      mutableModel=self.mutableID().modelName()
      displayObjList=self._dbss.getParmManager().getDisplayedParms()
      totalcount=len(displayObjList)
      count=0
      for parmObj in displayObjList:
         count+=1
         self.setWorking("Cleaning Grid Manager:%d of %d"%(count,totalcount))
         if self.checkWorking()==1:
            return 1
         pid=parmObj.getParmID()
         pmodel=pid.getDbId().getModelName()
         pname=pid.getParmName()
         plevel=pid.getParmLevel()
         if ((pmodel==mutableModel)and(pname in newParmList)):
            continue
         if ((pmodel==mutableModel)and(pname=="WG1")):
            continue
         if ((pmodel in modelList)and(pname in newParmList)):
            continue
         print pmodel, pname, plevel
         self.unloadWE(pmodel,pname,plevel)
      #
      #  if WG1 exists - use that for the units and precision of
      #  error grids - otherwise use default values
      #
      (self.errUnits,self.errPrecision,minval,maxval,rateFlag,
       ct,dminval,dmaxval)=self.getParmInfo(mutableModel,"WG1")
      return 0
   #==================================================================
   #
   #
   #
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
             self.__colorMapParams[colorTable] = ctInfo
      return(units,precision,minval,maxval,rateflag,colorTable,displayMinval,displayMaxval)
#==============================================================================
#
#  Class for other dialogs.
#
"""
class SimpleDialog(AppDialog.Dialog):
    def __init__(self, parent=None, name="Simple Dialog", callbackMethod=None,
                 modal=1):
        self.__parent = parent
        self.__name = name
        self.__modal = modal
        self.__callbackMethod = callbackMethod
        self.__dialog=AppDialog.Dialog.__init__(self,
                                                parent=self.__parent,
                                                title=self.__name,
                                                modal=self.__modal)
        return self.__dialog

    def buttonbox(self):
        buttonFrame = Tkinter.Frame(self)
        if self.__modal == 1:
            Tkinter.Button(buttonFrame, text="Ok",
                command=self.__okCB, width=10, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
        else:
            Tkinter.Button(buttonFrame, text="Run",
                command=self.__runCB, width=10, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
            Tkinter.Button(buttonFrame, text="Run/Dismiss",
                command=self.__okCB, width=12, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
        Tkinter.Button(buttonFrame, text="Cancel", width=10,
          command=self.cancelCB).pack(side=Tkinter.RIGHT, pady=5, padx=10)
        buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
    def body(self, master):
        bodylabel=Tkinter.Label(master,text="This is the body")
        bodylabel.pack(side=Tkinter.BOTTOM)
    def __runCB(self):
        self.__callbackMethod("Run")
    def __okCB(self):
        self.withdraw()
        self.__callbackMethod("OK")
        self.ok()
    def cancelCB(self):
        self.__callbackMethod("Cancel")
        self.cancel()
"""
#=======================================================================
class BVDialog(Tkinter.Toplevel):
    def __init__(self,parent,title=None,modal=1,hide=0):
       self.__modal=modal
       Tkinter.Toplevel.__init__(self,parent)
       try:
          if hide==1:
             self.withdraw()
          ##self.transient(parent)
          if title:
             self.title(title)
          self.parent=parent
          self.buttonbox()
          bodyFrame=Tkinter.Frame(self)
          self.body(bodyFrame)
          bodyFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.BOTH,expand=1)
          self.protocol("WM_DELETE_WINDOW",self.cancel)
          if parent is not None:
              self.geometry("+%d+%d"%(parent.winfo_rootx(),parent.winfo_rooty()))
          if self.__modal==1:
             self.deiconify()
             self.wait_window(self)
       except:
          ##self.destroy()
          raise Exception
          
    def buttonbox(self):
        buttonFrame = Tkinter.Frame(self)
        if self.__modal == 1:
            Tkinter.Button(buttonFrame, text="Ok",
                command=self.ok, width=10, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
        else:
            Tkinter.Button(buttonFrame, text="Run",
                command=self.run, width=10, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
            Tkinter.Button(buttonFrame, text="Run/Dismiss",
                command=self.ok, width=12, state=Tkinter.NORMAL).pack(\
                side=Tkinter.LEFT, pady=5, padx=10)
        Tkinter.Button(buttonFrame, text="Cancel", width=10,
          command=self.cancelCB).pack(side=Tkinter.RIGHT, pady=5, padx=10)
        buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
    def body(self, master):
        pass
    def ok(self,event=None):
       if not self.validate():
          return
       self.withdraw()
       self.update_idletasks()
       self.apply()
       self.cancel()
    def cancel(self,event=None):
       self.destroy()
    def validate(self):
       return 1
    def apply(self):
       pass
    
#=======================================================================
#  Working - is a dialog to give user info while 'working'.  You can
#            set the label for it, or get the value of 'stop', which
#            turns to 1 if they hit 'cancel' while this is displayed.
#            should be 'withdrawn' while not 'working' on something.
#
class Working(BVDialog):
   def __init__(self, parent=None, callbackMethod=None):
      self.__parent=parent
      self.__callbackMethod=callbackMethod
      self.stop=Tkinter.IntVar()
      self.label=Tkinter.StringVar()
      BVDialog.__init__(self,parent=self.__parent,
               title="%s Working"%PROGNAME,modal=0,hide=1)
      self.update()
      self.resizable(0,0)
      return
   def buttonbox(self):
      buttonFrame = Tkinter.Frame(self)
      but=Tkinter.Button(buttonFrame,text="Stop",command=self.__callbackMethod)
      but.pack(side=Tkinter.LEFT,expand=0)
      buttonFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=0)
      return
   def body(self,master):
      lab=Tkinter.Label(master,textvariable=self.label,width=60,
                        anchor=Tkinter.W)
      lab.pack(side=Tkinter.LEFT)
      self.label.set("Default Text")
      return
   def cancel(self):
      self.__callbackMethod
      return
#=======================================================================
#  Cases - a dialog with a scrolled text window showing number of cases
#          info.  It has one button - a close button.
#
class Cases(BVDialog):
   def __init__(self,parent,callbackMethod):
      self.__parent=parent
      self.__callbackMethod=callbackMethod
      BVDialog.__init__(self,parent=self.__parent,title="Number of Cases",modal=0,hide=1)
      self.update()
      geo=self.geometry()
      (wh,of)=geo.split("+",1)
      (w,h)=wh.split("x",1)
      self.minsize(int(w),int(h))
      return
   def buttonbox(self):
      buttonFrame=Tkinter.Frame(self)
      but=Tkinter.Button(self,text="Close",command=self.__callbackMethod)
      but.pack(side=Tkinter.TOP)
      buttonFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=0)
   def body(self,master):
      self.sb=Tkinter.Scrollbar(master)
      self.sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      self.dataText=Tkinter.Text(master,state=Tkinter.DISABLED,width=25,
                                 height=10)
      self.sb.configure(command=self.dataText.yview)
      self.dataText.configure(yscrollcommand=self.sb.set)
      self.dataText.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      self.updateText("Default Text")
      return
   def cancel(self):
      self.__callbackMethod()
      return
   def updateText(self,updateText):
      self.dataText.configure(state=Tkinter.NORMAL)
      self.dataText.delete(1.0,Tkinter.END)
      self.dataText.insert(Tkinter.END,updateText)
      self.dataText.configure(state=Tkinter.DISABLED)
      return
#=======================================================================
#  MiniDiag - is a minimized dialog - to save screen real
#            estate.  It has one button - to go back to the main
#            dialog.
#
class MiniDiag(BVDialog):
   def __init__(self,parent,callbackMethod,title="Title",buttonText="Button",loc="x"):
      self.__parent=parent
      self.__callbackMethod=callbackMethod
      self.__title=title
      self.__buttonText=buttonText
      self.__loc=loc
      BVDialog.__init__(self,parent=self.__parent,title=self.__title,modal=0,hide=1)
      self.update()
      self.resizable(0,0)
      #
      #  Set initial location (ul and ur)
      #
      if self.__loc in ("ur","lr") and parent is not None:
         parentgeo=self.__parent.geometry()
         (wh,of)=parentgeo.split("+",1)
         (w,h)=wh.split("x",1)
         (ox,oy)=of.split("+",1)
         self.update_idletasks()
         geo=self.geometry()
         (mwh,mo)=geo.split("+",1)
         (mw,mh)=mwh.split("x",1)
         if self.__loc=="lr":
            newgeo=mwh+"+%d+%d"%(int(ox)+int(w)-int(mw),int(oy)+int(h)-int(mh))
         elif self.__loc=="ur":
            newgeo=mwh+"+%d+%d"%(int(ox)+int(w)-int(mw),int(oy))
         self.geometry(newgeo)         
      self.update_idletasks()
      return
   def buttonbox(self):
      buttonFrame=Tkinter.Frame(self)
      but=Tkinter.Button(self,text=self.__buttonText,command=self.__callbackMethod)
      but.pack(side=Tkinter.TOP)
      buttonFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=0)
   def cancel(self):
      self.__callbackMethod()
      return
#=====================================================================   
class CanvasDisplay(BVDialog):
   def __init__(self, parent, title="Canvas Display", callbackMethod=None):
      self.__parent=parent
      self.__title=title
      self.__callbackMethod=callbackMethod
      self.curwidth=706.0 # initial canvas width
      self.curheight=536.0 # initial canvas height
      BVDialog.__init__(self,parent=self.__parent,
               title=self.__title,modal=0,hide=1)
      self.update()
      self.firstDisplay=1
      geo=self.geometry()
      (mwh,mof)=geo.split("+",1)
      (mw,mh)=mwh.split("x",1)
      self.minsize(int(mw),int(mh))
      return
   def body(self,master):
      self.bar=Tkinter.Frame(master)
      self.bar.pack(side=Tkinter.BOTTOM)
      self.fbar=Tkinter.Frame(master)
      self.fbar.pack(side=Tkinter.BOTTOM)
      #
      borderwidth=2
      canwidth=self.curwidth-((borderwidth+1)*2)
      canheight=self.curheight-((borderwidth+1)*2)
      self.canvas=Tkinter.Canvas(master,width=canwidth,height=canheight,
                                 borderwidth=borderwidth,relief=Tkinter.SUNKEN)
      self.canvas.bind("<Configure>",self.resizecanvas)
      self.canvas.pack(fill=Tkinter.BOTH,expand=1)     
   def buttonbox(self):
      buttonFrame=Tkinter.Frame(self)
      but=Tkinter.Button(buttonFrame,text="Exit",fg="red",command=self.__callbackMethod)
      but.pack(side=Tkinter.LEFT)
      self.label=Tkinter.Label(buttonFrame,text=" ")
      self.label.pack(side=Tkinter.LEFT,fill=Tkinter.X)
      buttonFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X)
   def cancel(self):
      self.__callbackMethod()

   def resizecanvas(self,event):
      w=float(event.width)
      h=float(event.height)
      #print "resizecanvas called: %s %s"%(event.width,event.height)
      scalex=w/self.curwidth
      scaley=h/self.curheight
      if ((scalex!=1.0)or(scaley!=1.0)):
         self.canvas.scale("all",0.0,0.0,scalex,scaley)
      self.curwidth=w
      self.curheight=h
      return
#=======================================================================
#
#  Custom dialog that provides selection for verification stuff
#
class Verif(BVDialog):
   def __init__(self, VU, userName, scaleList, parent=None, callbackMethod=None):
      self.__VU=VU
      self.__parent=parent
      self.__callbackMethod=callbackMethod
      self.__userName=userName
      self.__scaleList=scaleList
      BVDialog.__init__(self,parent=self.__parent,
               title="%s Options"%PROGNAME,modal=0,hide=1)
      #
      #  find minimum size
      #
      self.update()
      maxw=0
      maxh=0
      self.dispGrids()
      self.update_idletasks()
      geo=self.geometry()
      (maxw,maxh)=self.checkMax(geo,maxw,maxh)
      self.dispGridStats()
      self.update_idletasks()
      geo=self.geometry()
      (maxw,maxh)=self.checkMax(geo,maxw,maxh)
      self.dispDists()
      self.update_idletasks()
      geo=self.geometry()
      (maxw,maxh)=self.checkMax(geo,maxw,maxh)
      self.dispStats()
      self.update_idletasks()
      geo=self.geometry()
      (maxw,maxh)=self.checkMax(geo,maxw,maxh)
      self.minsize(maxw,maxh)
      self.dispGrids()
      self.deiconify()
      self.lift()
      self.wait_visibility()
      self.protocol("WM_DELETE_WINDOW",self.__quitCB)
      return
   def checkMax(self,geo,maxw,maxh):
      (wh,of)=geo.split("+",1)
      (w,h)=wh.split("x",1)
      maxw=max(int(w),maxw)
      maxh=max(int(h),maxh)
      return(maxw,maxh)
   def buttonbox(self):
      buttonFrame = Tkinter.Frame(self)
      Tkinter.Button(buttonFrame, text="Run",command=self.__runCB, width=6,
                     state=Tkinter.NORMAL).pack(\
                        side=Tkinter.LEFT, pady=5, padx=10)
      Tkinter.Button(buttonFrame, text="Hide",
            command=self.__hideCB, width=6, state=Tkinter.NORMAL).pack(\
            side=Tkinter.LEFT, pady=5, padx=10)
      Tkinter.Button(buttonFrame, text="Quit", width=6,
          command=self.__quitCB).pack(side=Tkinter.RIGHT, pady=5, padx=10)
      buttonFrame.pack(side=Tkinter.BOTTOM,expand=0)
   def __runCB(self):
      self.__callbackMethod("Run")
   def __hideCB(self):
      self.__callbackMethod("Hide")
   def __quitCB(self):
      self.cancel()
   def cancel(self):
      self.__callbackMethod("Quit")
      return
   #
   #  Custom body that has tabbed frames
   #
   def body(self, master):
      #
      #  The "tab" buttons at the top
      #
      tabs=[("Grid Displays",self.dispGrids),
            ("Grid Stats",self.dispGridStats),
            ("Distributions",self.dispDists),
            ("Point/Area Stats",self.dispStats),
            ("Stat vs. Scale",self.dispScaleStats),
            ]
      tabFrame=Tkinter.Frame(master,relief="sunken",borderwidth=1)
      self.tabSetting=Tkinter.StringVar()
      self.tabSetting.set("Grid Displays")
      for (text,callback) in tabs:
         x=Tkinter.Radiobutton(tabFrame,text=text,indicatoron=0,
                             command=callback,variable=self.tabSetting,
                             value=text)
         col=x.cget("highlightbackground")
         x.config(selectcolor=col)
         x.pack(side=Tkinter.LEFT)
      tabFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X)
      #
      #  Big "body" part of dialog
      #
      self.BodyFrame=Tkinter.Frame(master)
      #
      self.col4=Tkinter.Frame(self.BodyFrame)
      self.column4(self.col4)
      self.col4.pack(side=Tkinter.RIGHT,fill=Tkinter.Y,expand=0)
      self.col3=Tkinter.Frame(self.BodyFrame)
      self.column3(self.col3)
      self.col3.pack(side=Tkinter.RIGHT,fill=Tkinter.Y,expand=0)
      self.col2=Tkinter.Frame(self.BodyFrame)
      self.column2(self.col2)
      self.col2.pack(side=Tkinter.RIGHT,fill=Tkinter.Y,expand=0)
      #
      self.Grids=Tkinter.Frame(self.BodyFrame)
      self.OptionsGrids(self.Grids)
      #    
      self.GridStats=Tkinter.Frame(self.BodyFrame)
      self.OptionsGridsStats(self.GridStats)
      #
      self.ScaleStats=Tkinter.Frame(self.BodyFrame)
      self.OptionsScaleStats(self.ScaleStats)
      #
      self.Dists=Tkinter.Frame(self.BodyFrame)
      self.OptionsDists(self.Dists)
      #
      self.Stats=Tkinter.Frame(self.BodyFrame)
      self.OptionsStats(self.Stats)
      #
      self.BodyFrame.pack(side=Tkinter.TOP,fill=Tkinter.BOTH,expand=1)
      #
      #  setup scales (updating the GridsScale updates all others)
      #
      self.updateGridsScale()
      #
      #  pack the default one
      #
      cur=self.tabSetting.get()
      for (text,callback) in tabs:
         if cur==text:
            callback()
      return
   #==================================================================
   #
   #  Switch tab frame displayed
   #
   def dispGrids(self):
      self.GridStats.pack_forget()
      self.ScaleStats.pack_forget()
      self.Dists.pack_forget()
      self.Stats.pack_forget()
      self.Grids.pack(side=Tkinter.RIGHT,
                      fill=Tkinter.BOTH,expand=1)
   def dispGridStats(self):
      self.Grids.pack_forget()
      self.ScaleStats.pack_forget()
      self.Dists.pack_forget()
      self.Stats.pack_forget()
      self.GridStats.pack(side=Tkinter.RIGHT,
                          fill=Tkinter.BOTH,expand=1)
   def dispDists(self):
      self.Grids.pack_forget()
      self.GridStats.pack_forget()
      self.ScaleStats.pack_forget()
      self.Stats.pack_forget()
      self.Dists.pack(side=Tkinter.RIGHT,
                      fill=Tkinter.BOTH,expand=1)
   def dispStats(self):
      self.Grids.pack_forget()
      self.GridStats.pack_forget()
      self.ScaleStats.pack_forget()
      self.Dists.pack_forget()
      self.Stats.pack(side=Tkinter.RIGHT,
                      fill=Tkinter.BOTH,expand=1)
   def dispScaleStats(self):
      self.Grids.pack_forget()
      self.GridStats.pack_forget()
      self.Dists.pack_forget()
      self.Stats.pack_forget()
      self.ScaleStats.pack(side=Tkinter.RIGHT,
                           fill=Tkinter.BOTH,expand=1)
   #
   #  Get the values associated with the dialog pieces that
   #  are displayed with the current tab
   #
   def getValues(self):
      values={}
      tabtype=self.tabSetting.get()
      values["tab"]=tabtype
      if tabtype=="Grid Displays":
         values=self.getGridsValues(values)
      if tabtype=="Grid Stats":
         values=self.getGridsStatsValues(values)
      if tabtype=="Stat vs. Scale":
         values=self.getScaleStatsValues(values)
      if tabtype=="Distributions":
         values=self.getDistsValues(values)
      if tabtype=="Point/Area Stats":
         values=self.getStatsValues(values)
      return values
   #
   #  values on with the Grids tab
   #
   def getGridsValues(self,values):
      values["Display"]=self.GridsDisplay.get()
      values["Parm"]=self.getCheckList(self.GridsParms)
      values["Group"]=self.GridsGroup.get()
      values["Model"]=self.getCheckList(self.Models)
      values["ObsModel"]=self.ObsModel.get()
      values["fcstrList"]=self.getForecasterListbox()
      values["fhrStart"]=self.fhrStart.get()
      values["fhrEnd"]=self.fhrEnd.get()
      values["commonCases"]=self.Common.get()
      values["dateStyle"]=self.Datestyle.get()
      values["dateType"]=self.Datetype.get()
      values["numDays"]=self.Ndays.get()
      values["fromDay"]=self.getFromdayListbox()
      values["dayList"]=self.getDaylistListbox()
      values["cycleList"]=self.getCycleVals()
      values["scale"]=self.GridsScale.get()
      values["accumHours"]=self.accumHours.get()
      values["accumFreq"]=self.accumFreq.get()
      return values
   #
   #  values on with the GridsStats tab
   #
   def getGridsStatsValues(self,values):
      values["Display"]=self.GridsStatsDisplay.get()
      #values["Parms"]=self.getCheckList(self.GridsStatsParms)
      values["Parm"]=self.GridsStatsParm.get()
      values["Threshold"]=self.GridsStatsThreshold.get()
      values["Models"]=self.getCheckList(self.Models)
      values["ObsModel"]=self.ObsModel.get()
      values["fcstrList"]=self.getForecasterListbox()
      values["fhrStart"]=self.fhrStart.get()
      values["fhrEnd"]=self.fhrEnd.get()
      values["commonCases"]=self.Common.get()
      values["dateStyle"]=self.Datestyle.get()
      values["dateType"]=self.Datetype.get()
      values["numDays"]=self.Ndays.get()
      values["fromDay"]=self.getFromdayListbox()
      values["dayList"]=self.getDaylistListbox()
      values["cycleList"]=self.getCycleVals()
      values["scale"]=self.GridsStatsScale.get()
      values["accumHours"]=self.accumHours.get()
      values["accumFreq"]=self.accumFreq.get()
      values["TwoCatType"]=self.GridsStatsTwoCatType.get()
      values["TwoCatCond"]=self.GridsStatsTwoCatCond.get()
      str=self.GridsStatsTwoCatValueString.get()
      try:
         val=float(str)
      except:
         val=0.0
      values["TwoCatValue"]=val
      values["TwoCatValueString"]=str
      return values
   #
   #  values on with the Dists tab
   #
   def getDistsValues(self,values):
      values["Display"]=self.DistsDisplay.get()
      #values["Parms"]=self.getCheckList(self.DistsParms)
      values["Parm"]=self.DistsParm.get()
      values["Models"]=self.getCheckList(self.Models)
      values["ObsModel"]=self.ObsModel.get()
      values["fcstrList"]=self.getForecasterListbox()
      values["fhrStart"]=self.fhrStart.get()
      values["fhrEnd"]=self.fhrEnd.get()
      values["commonCases"]=self.Common.get()
      values["dateStyle"]=self.Datestyle.get()
      values["dateType"]=self.Datetype.get()
      values["numDays"]=self.Ndays.get()
      values["fromDay"]=self.getFromdayListbox()
      values["dayList"]=self.getDaylistListbox()
      values["cycleList"]=self.getCycleVals()
      values["scale"]=self.DistsScale.get()
      values["accumHours"]=self.accumHours.get()
      values["accumFreq"]=self.accumFreq.get()
      return values
   #
   #  values on the Stats tab
   #
   def getStatsValues(self,values):
      values["Display"]=self.StatsDisplay.get()
      values["areaList"]=self.getListbox(self.StatsAreasListbox)
      values["AreaCombine"]=self.StatsAreaCombine.get()
      values["Parms"]=self.getCheckList(self.StatsParms)
      values["Threshold"]=self.StatsThreshold.get()
      values["PlotType"]=self.StatsType.get()
      #values["Parm"]=self.StatsParm.get()
      values["Models"]=self.getCheckList(self.Models)
      values["ObsModel"]=self.ObsModel.get()
      values["fcstrList"]=self.getForecasterListbox()
      values["fhrStart"]=self.fhrStart.get()
      values["fhrEnd"]=self.fhrEnd.get()
      values["commonCases"]=self.Common.get()
      values["dateStyle"]=self.Datestyle.get()
      values["dateType"]=self.Datetype.get()
      values["numDays"]=self.Ndays.get()
      values["fromDay"]=self.getFromdayListbox()
      values["dayList"]=self.getDaylistListbox()
      values["cycleList"]=self.getCycleVals()
      values["scale"]=self.StatsScale.get()
      values["accumHours"]=self.accumHours.get()
      values["accumFreq"]=self.accumFreq.get()
      values["TwoCatType"]=self.statsTwoCatType.get()
      values["TwoCatCond"]=self.statsTwoCatCond.get()
      str=self.statsTwoCatValueString.get()
      try:
         val=float(str)
      except:
         val=0.0
      values["TwoCatValue"]=val
      values["TwoCatValueString"]=str
      return values
   #
   #  values on with the ScaleStats tab
   #
   def getScaleStatsValues(self,values):
      values["Display"]=self.ScaleStatsDisplay.get()
      values["areaList"]=self.getListbox(self.ScaleStatsAreasListbox)
      values["AreaCombine"]=self.ScaleStatsAreaCombine.get()
      values["Parm"]=self.ScaleStatsParm.get()
      values["Threshold"]=self.ScaleStatsThreshold.get()
      values["Models"]=self.getCheckList(self.Models)
      values["ObsModel"]=self.ObsModel.get()
      values["fcstrList"]=self.getForecasterListbox()
      values["fhrStart"]=self.fhrStart.get()
      values["fhrEnd"]=self.fhrEnd.get()
      values["commonCases"]=self.Common.get()
      values["dateStyle"]=self.Datestyle.get()
      values["dateType"]=self.Datetype.get()
      values["numDays"]=self.Ndays.get()
      values["fromDay"]=self.getFromdayListbox()
      values["dayList"]=self.getDaylistListbox()
      values["cycleList"]=self.getCycleVals()
      values["scale"]=self.GridsStatsScale.get()
      values["accumHours"]=self.accumHours.get()
      values["accumFreq"]=self.accumFreq.get()
      values["TwoCatType"]=self.scaleStatsTwoCatType.get()
      values["TwoCatCond"]=self.scaleStatsTwoCatCond.get()
      str=self.scaleStatsTwoCatValueString.get()
      try:
         val=float(str)
      except:
         val=0.0
      values["TwoCatValue"]=val
      values["TwoCatValueString"]=str
      return values
   #===============================================================
   #
   #  Column 2 - model
   #
   def column2(self,master):
      #
      #  At bottom - ObsModel being used
      #
      obsModelFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      obsModelLabel=Tkinter.Label(obsModelFrame,text="Observed:")
      obsModelLabel.pack(side=Tkinter.LEFT)
      obsModels=self.__VU.getCFG('OBSMODELS')
      namewidth=0
      for model in obsModels:
         if len(model)>namewidth:
            namewidth=len(model)
      self.ObsModel=Tkinter.StringVar()
      self.ObsModelButton=Tkinter.Menubutton(obsModelFrame,textvariable=self.ObsModel,
                             relief=Tkinter.RAISED,indicatoron=1,width=namewidth+1,anchor=Tkinter.W)
      self.ObsModelButton.pack(side=Tkinter.RIGHT)
      self.ObsModelPopup=Tkinter.Menu(self.ObsModelButton,tearoff=0)
      obsModels=self.__VU.getCFG('OBSMODELS')
      for model in obsModels:
         self.ObsModelPopup.add_radiobutton(label=model,indicatoron=0,value=model,
                                         variable=self.ObsModel)
      self.ObsModel.set(obsModels[0])
      obsModelFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=1)
      self.ObsModelButton.config(menu=self.ObsModelPopup)
      
      #
      #  common Cases checkbox
      #
      commonFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      self.Common=Tkinter.IntVar()
      commonCheck=Tkinter.Checkbutton(commonFrame,text="Common Cases",
                                      variable=self.Common,
                                      onvalue=1,offvalue=0)
      self.Common.set(1)
      commonCheck.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      commonFrame.pack(side=Tkinter.BOTTOM,anchor=Tkinter.N,
                       fill=Tkinter.X,expand=0)
      #
      #  Models checkbox
      #
      self.Models=[]
      models=self.__VU.listModels()
      for model in models:
         self.Models.append(Tkinter.StringVar())
      if "Official" in models:
         defaultModels=["Official",]
      else:
         defaultModels=[models[0],]
      self.checkGroup(master,"Model:",self.Models,models,defaultModels,Tkinter.BOTH,1)
      return
   #===============================================================
   #
   #  Column 3 - Forecaster and common cases
   #
   def column3(self,master):
      XHOUR=self.__VU.getCFG('MAXFORECASTHOUR')
      #
      #  Accumulation Time Periods:
      #
      accumFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      self.accumHours=Tkinter.IntVar()
      self.accumFreq=Tkinter.IntVar()
      freqFrame=Tkinter.Frame(accumFrame)
      flab=Tkinter.Label(freqFrame,text="Every:",width=8)
      flab.pack(side=Tkinter.LEFT,anchor=Tkinter.S)
      flab=Tkinter.Label(freqFrame,text="hrs")
      flab.pack(side=Tkinter.RIGHT,anchor=Tkinter.S)
      scaleFreq=Tkinter.Scale(freqFrame,from_=1,to=24,
                               variable=self.accumFreq,
                               orient=Tkinter.HORIZONTAL,
                               sliderlength=15)
      accumFrequencyDefault=self.__VU.getCFG("ACCUM_FREQUENCY_DEFAULT")
      if accumFrequencyDefault is None:
         accumFrequencyDefault=6
      self.accumFreq.set(accumFrequencyDefault)
      scaleFreq.pack(side=Tkinter.RIGHT,fill=Tkinter.X,expand=1)
      freqFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=1)
      hoursFrame=Tkinter.Frame(accumFrame)
      flab=Tkinter.Label(hoursFrame,text="Length:",width=8)
      flab.pack(side=Tkinter.LEFT,anchor=Tkinter.S)
      flab=Tkinter.Label(hoursFrame,text="hrs")
      flab.pack(side=Tkinter.RIGHT,anchor=Tkinter.S)
      accumResolution=self.__VU.getCFG('ACCUM_RESOLUTION')
      if accumResolution is None:
         accumResolution=6
      scaleHours=Tkinter.Scale(hoursFrame,from_=accumResolution,
                               to=XHOUR,resolution=accumResolution,
                               variable=self.accumHours,
                               orient=Tkinter.HORIZONTAL,
                               sliderlength=15)
      accumDefaultLength=self.__VU.getCFG("ACCUM_LENGTH_DEFAULT")
      if accumDefaultLength is None:
         accumDefaultLength=6
      self.accumHours.set(accumDefaultLength)
      scaleHours.pack(side=Tkinter.RIGHT,fill=Tkinter.X,expand=1)
      hoursFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=1)
      flab=Tkinter.Label(accumFrame,text="Accumulation Time Periods:")
      flab.pack(side=Tkinter.BOTTOM,expand=0)
      accumFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=0)
      #
      #  Forecast Hours start/stop
      #
      fhrFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      self.fhrStart=Tkinter.IntVar()
      self.fhrEnd=Tkinter.IntVar()
      fend=Tkinter.Scale(fhrFrame,from_=0,to=XHOUR,variable=self.fhrEnd,
                         orient=Tkinter.HORIZONTAL,command=self.endMove,
                         sliderlength=15)
      self.fhrEnd.set(XHOUR)
      fend.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=1)
      fstart=Tkinter.Scale(fhrFrame,from_=0,to=XHOUR,variable=self.fhrStart,
                           orient=Tkinter.HORIZONTAL,command=self.startMove,
                           sliderlength=15)
      self.fhrStart.set(0)
      fstart.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=1)
      flab=Tkinter.Label(fhrFrame,text="Forecast Hours:")
      flab.pack(side=Tkinter.BOTTOM,expand=0)
      fhrFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.X,expand=0)
      #
      #  Forecaster names to show...
      #
      forecasters=["ALL"]
      self.forecasterNumbers=[-1]
      #
      trimming=self.__VU.getCFG('FORECASTER_LIST_TRIMMING')
      trimADMIN=self.__VU.getCFG('FORECASTER_LIST_TRIMMING_ADMINISTRATORS')
      fFormat=self.__VU.getCFG('FORECASTER_LIST_FORMAT')
      fSort=self.__VU.getCFG('FORECASTER_LIST_SORT')
      labels=[]
      numstrs=self.__VU.getFcstrNums()
      for numstr in numstrs:
         num=int(numstr)
         id=self.__VU.getFcstrID(num)
         if ((trimming==1)and(self.__userName not in trimADMIN)and(self.__userName!=id)and(num!=0)):
            continue
         name=self.__VU.getFcstrName(num)
         sort=numstr  #defaults to number
         if fSort=="id":
            sort=id
         elif fSort=="name":
            sort=name
         label=name #defaults to name
         if fFormat=="number":
            label=numstr
         elif fFormat=="id":
            label=id
         elif fFormat=="number-name":
            label="%s - %s"%(numstr,name)
         elif fFormat=="number-id":
            label="%s - %s"%(numstr,id)
         labels.append("%s|%s|%s"%(sort,numstr,label))
      labels.sort()
      for entry in labels:
         (sstr,numstr,label)=entry.split("|")
         forecasters.append(label)
         self.forecasterNumbers.append(int(numstr))
      defaultForecasters=["ALL",]
      maxwid=0
      for forecaster in forecasters:
         wid=len(forecaster)
         if wid>maxwid:
            maxwid=wid
      maxheight=10
      fcstrFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      self.ForecasterListbox=self.sListbox(fcstrFrame,"Forecaster:",
             forecasters,defaultForecasters,maxwid+1,maxheight,Tkinter.EXTENDED)
      fcstrFrame.pack(side=Tkinter.BOTTOM,fill=Tkinter.BOTH,expand=1)
      return
   #
   #=================================================================
   #  getForecasterListbox - get list of integer forecast numbers for
   #                         forcasters turned on in ForecasterListbox
   #
   def getForecasterListbox(self):
      outlist=[]
      itemnums=self.ForecasterListbox.curselection()
      try:
         itemnums=map(string.atoi,itemnums)
      except ValueError: pass
      for itemnum in itemnums:
         outlist.append(self.forecasterNumbers[itemnum])
      return outlist
   #==================================================================
   #
   #  If moving fhrStart...check to make sure that it is not more
   #  than fhrEnd...and if it is...move fhrEnd too.
   #
   def startMove(self,event):
      st=self.fhrStart.get()
      en=self.fhrEnd.get()
      if en<st:
         self.fhrEnd.set(st)
      return
   #==================================================================
   #
   #  When moving fhrEnd...check to make sure that it is not less
   #  than fhrStart...and if it is...move fhrStart too.
   #
   def endMove(self,event):
      st=self.fhrStart.get()
      en=self.fhrEnd.get()
      if en<st:
         self.fhrStart.set(en)
      return
   #===============================================================
   #
   #  Column 4 - Date stuff
   #
   def column4(self,master):
      #
      #  datestyle
      #
      self.Datestyle=Tkinter.StringVar()
      datestyles=["Verifying on","Forecast on"]
      defaultDatestyle="Forecast on"
      self.radioGroup(master,"Dates:",self.Datestyle,datestyles,defaultDatestyle,Tkinter.X,0)

      byFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      #
      #  byPeriod
      #
      self.ByPeriod=Tkinter.Frame(byFrame)
      self.Ndays=Tkinter.IntVar()
      self.Ndays.set(7)
      nFrame=Tkinter.Frame(self.ByPeriod)
      labFrame=Tkinter.Frame(nFrame)
      lab=Tkinter.Label(labFrame,text="Length (days)")
      lab.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.NTog=Tkinter.Button(labFrame,text=">",padx=0,pady=0,command=self.toggleNScale)
      self.NTog.pack(side=Tkinter.RIGHT,anchor=Tkinter.E)
      labFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      self.NScale=Tkinter.Scale(nFrame,from_=1,to=50,variable=self.Ndays,
                      showvalue=1,orient=Tkinter.HORIZONTAL,
                      sliderlength=15)
      self.NScale.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.X)
      nFrame.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.X)

      gridDayStrings,self.gridDays=self.getRecentDates(self.__VU.GRIDDAYS)
      defaultDay=[gridDayStrings[0],]
      maxwid=10
      maxheight=5  # number of days to show
      self.FromdayListbox=self.sListbox(self.ByPeriod,"Ending on:",
               gridDayStrings,defaultDay,maxwid,maxheight,Tkinter.BROWSE)
      self.ByPeriod.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.BOTH,expand=1)
      #
      #  byList
      #
      self.ByList=Tkinter.Frame(byFrame)
      #days,daydates=self.getRecentDates(self.__VU.GRIDDAYS)
      defaultDaylist=[]
      for i in xrange(7):
         defaultDaylist.append(gridDayStrings[i])
      maxwid=10
      maxheight=5  #number of days to show
      self.DaylistListbox=self.sListbox(self.ByList,"Include:",
                        gridDayStrings,defaultDaylist,maxwid,maxheight,Tkinter.EXTENDED)
      self.ByList.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.BOTH,expand=1)
      #
      #  datetype
      #
      datetypeFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      datetypeLabel=Tkinter.Label(datetypeFrame,text="Choose Dates by:")
      datetypeLabel.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      self.Datetype=Tkinter.StringVar()
      datetypeDefault="Period Length"
      datetypes=["Period Length","List of dates"]
      for datetype in datetypes:
         a=Tkinter.Radiobutton(datetypeFrame,text=datetype,command=self.setDatetype,
                               variable=self.Datetype,value=datetype)
         if datetype is datetypeDefault:
            a.invoke()
         a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      datetypeFrame.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.X)
      #
      #  Now pack the frame with the "byPeriod" or "byList"
      #
      byFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.BOTH,expand=1)
      #
      #  cycle
      #
      cycleFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      labFrame=Tkinter.Frame(cycleFrame)
      cycleLabel=Tkinter.Label(labFrame,text="Cycle:")
      cycleLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      cycleToggle=Tkinter.Button(labFrame,text="ALL",padx=0,pady=0,command=self.toggleCycles)
      cycleToggle.pack(side=Tkinter.RIGHT,anchor=Tkinter.E)
      labFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      cyclecol1=Tkinter.Frame(cycleFrame)
      cyclecol2=Tkinter.Frame(cycleFrame)
      cycleDefault=self.__VU.getCFG('ALLCYCLES')
      cycles=self.__VU.getCFG('ALLCYCLES')
      self.CycleFlags=[]
      self.CycleVals=[]
      cnt=0
      for cycle in cycles:
         self.CycleFlags.append(Tkinter.IntVar())
         if cnt<int(float(len(cycles))/2.0):
            parent=cyclecol1
         else:
            parent=cyclecol2
         self.CycleVals.append(int(cycle))
         a=Tkinter.Checkbutton(parent,text=cycle,
                               variable=self.CycleFlags[cnt],onvalue=1,
                               offvalue=0)
         if cycle in cycleDefault:
            self.CycleFlags[cnt].set(1)
         a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
         cnt+=1
      cyclecol1.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      cyclecol2.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      cycleFrame.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.X)
      return
   #================================================================
   #
   def toggleNScale(self):
      curN=self.Ndays.get()
      curSet=self.NTog.cget("text")
      if curSet==">":
         self.NScale.configure(to=self.__VU.STATDAYS)
         self.NTog.configure(text="<")
      else:
         if curN>50:
            self.Ndays.set(50)
         self.NScale.configure(to=50)
         self.NTog.configure(text=">")
      return
      #to=self.__VU.STATDAYS      
   #================================================================
   #  toggleCycles - toggles all the cycle buttons
   #
   def toggleCycles(self):
      for cycleFlag in self.CycleFlags:
         val=cycleFlag.get()
         val=abs(val-1)
         cycleFlag.set(val)
      return
   #=================================================================
   #  getCycleVals - get list of values turned on in Cycles
   #
   def getCycleVals(self):
      outlist=[]
      for i in xrange(len(self.CycleFlags)):
         a=self.CycleFlags[i].get()
         if a!=0:
            outlist.append(self.CycleVals[i])
      return outlist
   #=================================================================
   #  getFromdayListbox - get unix date for day listed in Fromday
   #                      listbox
   #
   def getFromdayListbox(self):
      itemnums=self.FromdayListbox.curselection()
      try:
         itemnums=map(string.atoi,itemnums)
      except ValueError: pass
      itemnum=itemnums[0]
      outdate=self.gridDays[itemnum]
      return outdate
   #=================================================================
   #  getDaylistListbox - get list of integer forecast numbers for
   #  forcasters turned on in ForecasterListbox
   #
   def getDaylistListbox(self):
      outlist=[]
      itemnums=self.DaylistListbox.curselection()
      try:
         itemnums=map(string.atoi,itemnums)
      except ValueError: pass
      for itemnum in itemnums:
         outlist.append(self.gridDays[itemnum])
      return outlist
   #==================================================================
   #
   #  Frame that specifies the options for the Grids displays
   #
   def OptionsGrids(self,master):
      #
      #  parameter 
      #
      self.GridsParms=[]
      parms=self.__VU.getVerParms()
      for parm in parms:
         self.GridsParms.append(Tkinter.StringVar())
      defaultParms=[parms[0],]
      self.checkGroup(master,"Parameter:",self.GridsParms,parms,
                      defaultParms,Tkinter.BOTH,1)
      #
      #  display
      #
      self.GridsDisplay=Tkinter.StringVar()
      displays=["Forecasts","Errors"]
      defaultDisplay="Forecasts"
      gridDisplayFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      gdLabelFrame=Tkinter.Frame(gridDisplayFrame)
      gridDisplayLabel=Tkinter.Label(gdLabelFrame,text="Display:")
      gridDisplayLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      
      self.GridsScale=Tkinter.IntVar()
      self.GridsScaleText=Tkinter.StringVar()
      but=Tkinter.Menubutton(gdLabelFrame,textvariable=self.GridsScaleText,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.RIGHT,anchor=Tkinter.W)
      self.GridsScalePopup=Tkinter.Menu(but,tearoff=0)
      for (value,text) in self.__scaleList:
         self.GridsScalePopup.add_radiobutton(label=text,indicatoron=0,value=value,
                                         variable=self.GridsScale,
                                         command=self.updateGridsScale)
      self.GridsScale.set(0)
      #self.updateGridsScale()
      but.config(menu=self.GridsScalePopup)
      
      gdLabelFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=0)
      for item in displays:
         a=Tkinter.Radiobutton(gridDisplayFrame,text=item,
                               variable=self.GridsDisplay,value=item)
         a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
         if item is defaultDisplay:
            self.GridsDisplay.set(item)
      gridDisplayFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.X,expand=0)
      #
      #  Group by 
      #
      self.GridsGroup=Tkinter.StringVar()
      defaultGroup="Forecast Hour"
      groups=["Forecast Hour","Run Time"]
      self.radioGroup(master,"Group by:",self.GridsGroup,groups,defaultGroup,Tkinter.X,0)
      return
   #
   def updateGridsScale(self):
      value=self.GridsScale.get()
      for i in xrange(len(self.__scaleList)):
         (num,text)=self.__scaleList[i]
         if num==value:
            self.GridsScaleText.set(text)
            self.GridsStatsScale.set(num)
            self.GridsStatsScaleText.set(text)
            self.DistsScale.set(num)
            self.DistsScaleText.set(text)
            self.StatsScale.set(num)
            self.StatsScaleText.set(text)
            break
   #==================================================================
   #
   #  Frame that specifies the options for the GridsStats display
   #
   def OptionsGridsStats(self,master):
      #
      #  parameter 
      #
      self.GridsStatsParm=Tkinter.StringVar()
      parms=self.__VU.getVerParmsVect()
      #for parm in parms:
      #   self.GridsStatsParms.append(Tkinter.StringVar())
      defaultParm=parms[0]
      self.radioGroup(master,"Parameter:",self.GridsStatsParm,parms,
                      defaultParm,Tkinter.BOTH,1,callback=self.updateGridsStatsThreshold)
      #
      #  display
      #
      self.GridsStatsDisplay=Tkinter.StringVar()
      displays=["Bias","Mean Abs Error","RMS Error","Mean Squared Error"]
      defaultDisplay="Bias"
      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      scaleFrame=Tkinter.Frame(radioFrame)
      radioLabel=Tkinter.Label(scaleFrame,text="Display:")
      radioLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      
      self.GridsStatsScale=Tkinter.IntVar()
      self.GridsStatsScaleText=Tkinter.StringVar()
      but=Tkinter.Menubutton(scaleFrame,textvariable=self.GridsStatsScaleText,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.RIGHT,anchor=Tkinter.W)
      self.GridsStatsScalePopup=Tkinter.Menu(but,tearoff=0)
      for (value,text) in self.__scaleList:
         self.GridsStatsScalePopup.add_radiobutton(label=text,indicatoron=0,value=value,
                                         variable=self.GridsStatsScale,
                                         command=self.updateGridsStatsScale)
      self.GridsStatsScale.set(0)
      #self.updateGridsStatsScale()
      
      but.config(menu=self.GridsStatsScalePopup)      
      scaleFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      
      a=Tkinter.Radiobutton(radioFrame,text="Bias",
                            variable=self.GridsStatsDisplay,value="Bias")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      self.GridsStatsDisplay.set("Bias")
      a=Tkinter.Radiobutton(radioFrame,text="Mean Abs Error",
                            variable=self.GridsStatsDisplay,value="Mean Abs Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="RMS Error",
                            variable=self.GridsStatsDisplay,value="RMS Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="Mean Squared Error",
                            variable=self.GridsStatsDisplay,value="Mean Squared Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      threshFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(threshFrame,text="Percent Err <",
                            variable=self.GridsStatsDisplay,value="Percent Err <")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.GridsStatsThreshold=Tkinter.IntVar()
      self.GridsStatsThresholdValue=Tkinter.StringVar()
      but=Tkinter.Menubutton(threshFrame,textvariable=self.GridsStatsThresholdValue,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.GridsStatsThresholdPopup=Tkinter.Menu(but,tearoff=0)
      self.GridsStatsThresholdPopup.add_command(label="stuff")
      for i in xrange(self.__VU.getCFG('NUMTHRESH')):
         self.GridsStatsThresholdPopup.add_radiobutton(label="xxxx",indicatoron=0,value=i,
                                          variable=self.GridsStatsThreshold,
                                          command=self.pickGridsStatsThreshold)
      but.config(menu=self.GridsStatsThresholdPopup)
      threshFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)

      twocatFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(twocatFrame,text="",variable=self.GridsStatsDisplay,
                            value="TwoCat")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.GridsStatsTwoCatType=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.GridsStatsTwoCatType,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.GridsStatsTwoCatTypePopup=Tkinter.Menu(but,tearoff=0)
      for stat in ["Hits","Areal Hits","Misses","Areal Misses",
                   "False Alarms","Areal False Alarms","Correct Negatives",
                   "Areal Correct Negatives",
                   "Frequency Observed",
                   "Frequency Forecast",
                   "Fraction Correct","Areal Fraction Correct",
                   "Frequency Bias","Areal Frequency Bias",
                   "POD","Areal POD","FAR","Areal FAR","Threat Score",
                   "Areal Threat Score","Equitable Threat Score",
                   "Areal Equitable Threat Score","True Skill Score",
                   "Areal True Skill Score","Heidke Skill Score",
                   "Areal Heidke Skill Score","Odds Ratio","Areal Odds Ratio"]:
         self.GridsStatsTwoCatTypePopup.add_radiobutton(label=stat,
                        indicatoron=0,value=stat,
                        variable=self.GridsStatsTwoCatType,command=self.updateGridsStatsTwoCatType)
      self.GridsStatsTwoCatType.set("Fraction Correct")
      but.config(menu=self.GridsStatsTwoCatTypePopup)
      self.GridsStatsTwoCatCond=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.GridsStatsTwoCatCond,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.GridsStatsTwoCatCondPopup=Tkinter.Menu(but,tearoff=0)
      for cond in [">",">=","<=","<"]:
         self.GridsStatsTwoCatCondPopup.add_radiobutton(label=cond,
                     indicatoron=0,value=cond,
                     variable=self.GridsStatsTwoCatCond,command=self.updateGridsStatsTwoCatType)
      self.GridsStatsTwoCatCond.set(">")
      but.config(menu=self.GridsStatsTwoCatCondPopup)
      self.GridsStatsTwoCatValueString=Tkinter.StringVar()
      ent=Tkinter.Entry(twocatFrame,textvariable=self.GridsStatsTwoCatValueString,
                        width=5,relief=Tkinter.SUNKEN)
      self.GridsStatsTwoCatValueString.set("0.0")
      ent.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      twocatFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.X,expand=0)
      self.updateGridsStatsThreshold()
      return
   #==================================================================
   #
   def updateGridsStatsScale(self):
      value=self.GridsStatsScale.get()
      for i in xrange(len(self.__scaleList)):
         (num,text)=self.__scaleList[i]
         if num==value:
            self.GridsStatsScaleText.set(text)
            self.GridsScale.set(num)
            self.GridsScaleText.set(text)
            self.DistsScale.set(num)
            self.DistsScaleText.set(text)
            self.StatsScale.set(num)
            self.StatsScaleText.set(text)
            break
   #
   #  When user actually picks a threshold - then also set the display to
   #  use the Percent Err < display
   #
   def pickGridsStatsThreshold(self):
      self.GridsStatsDisplay.set("Percent Err <")
      self.updateGridsStatsThreshold()
      return
   def updateGridsStatsTwoCatType(self):
      self.GridsStatsDisplay.set("TwoCat")
      return
   #
   #  When parm is changed, or when the user picks a threshold - need to
   #  update the chosen thresholds.
   #
   def updateGridsStatsThreshold(self):
      #print "in updateGridsStatsThreshold"
      parm=self.GridsStatsParm.get()
      #print "  parm=",parm
      parmList=[parm,]
      if len(parmList)<1:
         return
      tlist=[]
      plist=[]
      for parm in parmList:
         readParm=parm
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               readParm=parm[:-3]
         datatype=self.__VU.getVerParmType(readParm)
         thresholds=self.__VU.getVerThresholds(readParm)
         #print " thresholds for ",parm
         #print "    are:",thresholds
         if datatype==1:
            (threshmag,threshdir)=thresholds
            if last3=="Dir":
               thresholds=threshdir
            else:
               thresholds=threshmag
            #if last3 in ("Spd","Dir"):
            #   (threshmag,threshdir)=thresholds
            #   if last3=="Spd":
            #      thresholds=threshmag
            #   else:
            #      thresholds=threshdir
         if len(tlist)>0:
            same=1
            for j in xrange(len(tlist)):
               thresh=tlist[j]
               parms=plist[j]
               same=1
               for i in xrange(len(thresh)):
                  if thresh[i]!=thresholds[i]:
                     same=0
                     break
               if same==1:
                  plist[j]+=",%s"%parm
                  break
            if same!=1:
               tlist.append(thresholds)
               plist.append(parm)
         else:
            tlist.append(thresholds)
            plist.append(parm)

      dthresh=[]
      if len(tlist)>1:
         for j in xrange(len(tlist)):
            thresh=tlist[j]
            parms=plist[j]
            for i in xrange(len(thresh)):
               t=thresh[i]
               str="%d"%(t)
               if len(dthresh)<(i+1):
                  dthresh.append(str)
               else:
                  dthresh[i]+=" | %s"%str
      else:
         thresh=tlist[0]
         for i in xrange(len(thresh)):
            t=thresh[i]
            str="%d"%t
            dthresh.append(str)
      #
      #
      parmList=" | ".join(plist)
      self.GridsStatsThresholdPopup.entryconfigure(0,label=parmList)
      for i in xrange(len(dthresh)):
         self.GridsStatsThresholdPopup.entryconfigure(i+1,label=dthresh[i])
         #print "   ",i,dthresh[i]
      self.GridsStatsThresholdValue.set(dthresh[self.GridsStatsThreshold.get()])
      return
   def updateScaleThreshold(self):
      parm=self.GridsStatsParm.get()
      parmList=[parm,]
      if len(parmList)<1:
         return
      tlist=[]
      plist=[]
      for parm in parmList:
         readParm=parm
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               readParm=parm[:-3]
         thresholds=self.__VU.getVerThresholds(readParm)
         print " thresholds for ",parm
         print "    are:",thresholds
         if last3 in ("Spd","Dir"):
            (threshmag,threshdir)=thresholds
            if last3=="Spd":
               thresholds=threshmag
            else:
               thresholds=threshdir
         if len(tlist)>0:
            same=1
            for j in xrange(len(tlist)):
               thresh=tlist[j]
               parms=plist[j]
               same=1
               for i in xrange(len(thresh)):
                  if thresh[i]!=thresholds[i]:
                     same=0
                     break
               if same==1:
                  plist[j]+=",%s"%parm
                  break
            if same!=1:
               tlist.append(thresholds)
               plist.append(parm)
         else:
            tlist.append(thresholds)
            plist.append(parm)

      dthresh=[]
      if len(tlist)>1:
         for j in xrange(len(tlist)):
            thresh=tlist[j]
            parms=plist[j]
            for i in xrange(len(thresh)):
               t=thresh[i]
               str="%d"%(t)
               if len(dthresh)<(i+1):
                  dthresh.append(str)
               else:
                  dthresh[i]+=" | %s"%str
      else:
         thresh=tlist[0]
         for i in xrange(len(thresh)):
            t=thresh[i]
            str="%d"%t
            dthresh.append(str)
      #
      #
      parmList=" | ".join(plist)
      self.ScaleThresholdPopup.entryconfigure(0,label=parmList)
      for i in xrange(len(dthresh)):
         self.ScaleThresholdPopup.entryconfigure(i+1,label=dthresh[i])
         #print "   ",i,dthresh[i]
      self.ScaleThresholdValue.set(dthresh[self.ScaleThreshold.get()])
      return
   #==================================================================
   #
   #  Frame that specifies the options for the Dists display
   #
   def OptionsDists(self,master):
      #
      #  parameter 
      #
      self.DistsParm=Tkinter.StringVar()
      parms=self.__VU.getVerParmsVect()
      defaultParm=parms[0]
      self.radioGroup(master,"Parameter:",self.DistsParm,parms,defaultParm,Tkinter.BOTH,1)
      #
      #  display
      #
      self.DistsDisplay=Tkinter.StringVar()

      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      labFrame=Tkinter.Frame(radioFrame)
      
      radioLabel=Tkinter.Label(labFrame,text="Display:")
      radioLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      
      self.DistsScale=Tkinter.IntVar()
      self.DistsScaleText=Tkinter.StringVar()
      but=Tkinter.Menubutton(labFrame,textvariable=self.DistsScaleText,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.RIGHT,anchor=Tkinter.W)
      self.DistsScalePopup=Tkinter.Menu(but,tearoff=0)
      for (value,text) in self.__scaleList:
         self.DistsScalePopup.add_radiobutton(label=text,indicatoron=0,value=value,
                                         variable=self.DistsScale,
                                         command=self.updateDistsScale)
      self.DistsScale.set(0)
      #self.updateDistsScale()
      but.config(menu=self.DistsScalePopup)      
      labFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      
      a=Tkinter.Radiobutton(radioFrame,text="Error Histogram",
                            variable=self.DistsDisplay,value="Error Histogram")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      self.DistsDisplay.set("Error Histogram")
      a=Tkinter.Radiobutton(radioFrame,text="Value Histogram",
                            variable=self.DistsDisplay,value="Value Histogram")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="Expected Value",
                            variable=self.DistsDisplay,value="Expected Value")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="Scatterplot",
                            variable=self.DistsDisplay,value="Scatterplot")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.X,expand=0)

      #displays=["Error Histogram","Value Histogram","Expected Value","Scatterplot"]
      #defaultDisplay="Error Histogram"
      #self.radioGroup(master,"Display:",self.DistsDisplay,displays,defaultDisplay,Tkinter.X,0)
      return
   #==================================================================
   #
   def updateDistsScale(self):
      value=self.DistsScale.get()
      for i in xrange(len(self.__scaleList)):
         (num,text)=self.__scaleList[i]
         if num==value:
            self.DistsScaleText.set(text)
            self.GridsScale.set(num)
            self.GridsScaleText.set(text)
            self.GridsStatsScale.set(num)
            self.GridsStatsScaleText.set(text)
            self.StatsScale.set(num)
            self.StatsScaleText.set(text)
            break
   #==================================================================
   #
   #  Frame that specifies the options for the Stats display
   #
   def OptionsStats(self,master):
      #
      #  parameter 
      #
      self.StatsParms=[]
      parms=self.__VU.getVerParmsVect()
      for parm in parms:
         self.StatsParms.append(Tkinter.StringVar())
      defaultParms=[parms[0],]
      self.checkGroup(master,"Parameter:",self.StatsParms,parms,
                      defaultParms,Tkinter.X,0,callback=self.updateStatsThreshold)
      #
      #  Area list
      #
      af=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      alist=self.__VU.listEditAreaDescriptions()
      alist[0:0]=["Current"]
      defaultArea=alist[0]
      maxwid=0
      for area in alist:
         if len(area)>maxwid:
            maxwid=len(area)
      if len(alist)>5:
         maxheight=5
      else:
         maxheight=len(alist)
      
      acomb=Tkinter.Frame(af)
      self.StatsAreaCombine=Tkinter.IntVar()
      comb=Tkinter.Checkbutton(acomb,text="Combine",variable=self.StatsAreaCombine)
      self.StatsAreaCombine.set(1)
      comb.pack(side=Tkinter.RIGHT,anchor=Tkinter.E)
      sLabel=Tkinter.Label(acomb,text="Edit Area:")
      sLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      acomb.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X)
      sb=Tkinter.Scrollbar(af,orient=Tkinter.VERTICAL)
      self.StatsAreasListbox=Tkinter.Listbox(af,yscrollcommand=sb.set,
                          selectmode=Tkinter.EXTENDED,width=maxwid,height=maxheight)
      sb.config(command=self.StatsAreasListbox.yview)
      sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      self.StatsAreasListbox.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      idx=0
      for item in alist:
         self.StatsAreasListbox.insert(Tkinter.END,item)
         if item in defaultArea:
            self.StatsAreasListbox.select_set(idx)
         idx+=1
   
      af.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.BOTH,expand=1)
      #
      #  display
      #
      self.StatsDisplay=Tkinter.StringVar()
      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      labFrame=Tkinter.Frame(radioFrame)
      
      radioLabel=Tkinter.Label(labFrame,text="Display:")
      radioLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      
      self.StatsScale=Tkinter.IntVar()
      self.StatsScaleText=Tkinter.StringVar()
      but=Tkinter.Menubutton(labFrame,textvariable=self.StatsScaleText,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.RIGHT,anchor=Tkinter.W)
      self.StatsScalePopup=Tkinter.Menu(but,tearoff=0)
      for (value,text) in self.__scaleList:
         self.StatsScalePopup.add_radiobutton(label=text,indicatoron=0,value=value,
                                         variable=self.StatsScale,
                                         command=self.updateStatsScale)
      self.StatsScale.set(0)
      #self.updateStatsScale()
      but.config(menu=self.StatsScalePopup)      
      labFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      
      a=Tkinter.Radiobutton(radioFrame,text="Bias",
                            variable=self.StatsDisplay,value="Bias")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      self.StatsDisplay.set("Bias")
      a=Tkinter.Radiobutton(radioFrame,text="Mean Abs Error",
                            variable=self.StatsDisplay,value="Mean Abs Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="RMS Error",
                            variable=self.StatsDisplay,value="RMS Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="Mean Squared Error",
                            variable=self.StatsDisplay,value="Mean Squared Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      
      threshFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(threshFrame,text="Percent Err <",
                            variable=self.StatsDisplay,value="Percent Err <")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.StatsThreshold=Tkinter.IntVar()
      self.StatsThresholdValue=Tkinter.StringVar()
      but=Tkinter.Menubutton(threshFrame,textvariable=self.StatsThresholdValue,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.thresholdStatsPopup=Tkinter.Menu(but,tearoff=0)
      self.thresholdStatsPopup.add_command(label="stuff")
      for i in xrange(self.__VU.getCFG('NUMTHRESH')):
         self.thresholdStatsPopup.add_radiobutton(label="xxxx",indicatoron=0,value=i,
                                          variable=self.StatsThreshold,
                                          command=self.pickStatsThreshold)
      but.config(menu=self.thresholdStatsPopup)
      threshFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      twocatFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(twocatFrame,text="",variable=self.StatsDisplay,
                            value="TwoCat")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.statsTwoCatType=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.statsTwoCatType,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.statsTwoCatTypePopup=Tkinter.Menu(but,tearoff=0)
      for stat in ["Hits","Areal Hits","Misses","Areal Misses",
                   "False Alarms","Areal False Alarms","Correct Negatives",
                   "Areal Correct Negatives",
                   "Frequency Observed",
                   "Frequency Forecast",
                   "Fraction Correct",
                   "Areal Fraction Correct",
                   "Frequency Bias","Areal Frequency Bias",
                   "POD","Areal POD","FAR","Areal FAR","Threat Score",
                   "Areal Threat Score","Equitable Threat Score",
                   "Areal Equitable Threat Score","True Skill Score",
                   "Areal True Skill Score","Heidke Skill Score",
                   "Areal Heidke Skill Score","Odds Ratio","Areal Odds Ratio"]:
         self.statsTwoCatTypePopup.add_radiobutton(label=stat,
                        indicatoron=0,value=stat,
                        variable=self.statsTwoCatType,command=self.updateStatsTwoCatType)
      self.statsTwoCatType.set("Fraction Correct")
      but.config(menu=self.statsTwoCatTypePopup)
      self.statsTwoCatCond=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.statsTwoCatCond,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.statsTwoCatCondPopup=Tkinter.Menu(but,tearoff=0)
      for cond in [">",">=","<=","<"]:
         self.statsTwoCatCondPopup.add_radiobutton(label=cond,
                     indicatoron=0,value=cond,
                     variable=self.statsTwoCatCond,command=self.updateStatsTwoCatType)
      self.statsTwoCatCond.set(">")
      but.config(menu=self.statsTwoCatCondPopup)
      self.statsTwoCatValueString=Tkinter.StringVar()
      ent=Tkinter.Entry(twocatFrame,textvariable=self.statsTwoCatValueString,
                        width=5,relief=Tkinter.SUNKEN)
      self.statsTwoCatValueString.set("0.0")
      ent.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      twocatFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.X,expand=0)
      self.updateStatsThreshold()
      #
      #  Stat type
      #
      self.StatsType=Tkinter.StringVar()
      stattypes=["vs. Time","vs. Fcst Hour"]
      defaulttype="vs. Time"
      self.radioGroup(master,"Plot:",self.StatsType,stattypes,defaulttype,Tkinter.X,0)
      return
   #==================================================================
   #
   def updateStatsScale(self):
      value=self.StatsScale.get()
      for i in xrange(len(self.__scaleList)):
         (num,text)=self.__scaleList[i]
         if num==value:
            self.StatsScaleText.set(text)
            self.GridsScale.set(num)
            self.GridsScaleText.set(text)
            self.GridsStatsScale.set(num)
            self.GridsStatsScaleText.set(text)
            self.DistsScale.set(num)
            self.DistsScaleText.set(text)
            break
   def updateStatsTwoCatType(self):
      self.StatsDisplay.set("TwoCat")
      return
   #==================================================================
   #
   def pickStatsThreshold(self):
      self.StatsDisplay.set("Percent Err <")
      self.updateStatsThreshold()
      return
   
   def updateStatsThreshold(self):
      parmList=self.getCheckList(self.StatsParms)
      if len(parmList)<1:
         return
      tlist=[]
      plist=[]
      for parm in parmList:
         readParm=parm
         last3="xxx"
         if len(parm)>3:
            last3=parm[-3:]
            if ((last3=="Spd")or(last3=="Dir")):
               readParm=parm[:-3]
         thresholds=self.__VU.getVerThresholds(readParm)
         if last3 in ("Spd","Dir"):
            (threshmag,threshdir)=thresholds
            if last3=="Spd":
               thresholds=threshmag
            else:
               thresholds=threshdir
         if len(tlist)>0:
            same=1
            for j in xrange(len(tlist)):
               thresh=tlist[j]
               parms=plist[j]
               same=1
               for i in xrange(len(thresh)):
                  if thresh[i]!=thresholds[i]:
                     same=0
                     break
               if same==1:
                  plist[j]+=",%s"%parm
                  break
            if same!=1:
               tlist.append(thresholds)
               plist.append(parm)
         else:
            tlist.append(thresholds)
            plist.append(parm)

      dthresh=[]
      if len(tlist)>1:
         for j in xrange(len(tlist)):
            thresh=tlist[j]
            parms=plist[j]
            for i in xrange(len(thresh)):
               t=thresh[i]
               str="%d"%(t)
               if len(dthresh)<(i+1):
                  dthresh.append(str)
               else:
                  dthresh[i]+=" | %s"%str
      else:
         thresh=tlist[0]
         for i in xrange(len(thresh)):
            t=thresh[i]
            str="%d"%t
            dthresh.append(str)
      #
      #
      parmList=" | ".join(plist)
      self.thresholdStatsPopup.entryconfigure(0,label=parmList)
      for i in xrange(len(dthresh)):
         self.thresholdStatsPopup.entryconfigure(i+1,label=dthresh[i])
         #print "   ",i,dthresh[i]
      self.StatsThresholdValue.set(dthresh[self.StatsThreshold.get()])
      return
   #==================================================================
   #
   #  Frame that specifies the options for the Scale vs Stat display
   #
   def OptionsScaleStats(self,master):
      #
      #  parameter 
      #
      self.ScaleStatsParm=Tkinter.StringVar()
      parms=self.__VU.getVerParmsVect()
      defaultParm=parms[0]
      self.radioGroup(master,"Parameter:",self.ScaleStatsParm,parms,defaultParm,Tkinter.BOTH,1)
      #
      #  Area list
      #
      af=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      alist=self.__VU.listEditAreaDescriptions()
      alist[0:0]=["Current"]
      defaultArea=alist[0]
      maxwid=0
      for area in alist:
         if len(area)>maxwid:
            maxwid=len(area)
      if len(alist)>5:
         maxheight=5
      else:
         maxheight=len(alist)
      
      acomb=Tkinter.Frame(af)
      self.ScaleStatsAreaCombine=Tkinter.IntVar()
      #comb=Tkinter.Checkbutton(acomb,text="Combine",variable=self.StatsAreaCombine)
      self.ScaleStatsAreaCombine.set(1)  # always set
      #comb.pack(side=Tkinter.RIGHT,anchor=Tkinter.E)
      sLabel=Tkinter.Label(acomb,text="Edit Area:")
      sLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      acomb.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X)
      sb=Tkinter.Scrollbar(af,orient=Tkinter.VERTICAL)
      self.ScaleStatsAreasListbox=Tkinter.Listbox(af,yscrollcommand=sb.set,
                          selectmode=Tkinter.EXTENDED,width=maxwid,height=maxheight)
      sb.config(command=self.ScaleStatsAreasListbox.yview)
      sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      self.ScaleStatsAreasListbox.pack(side=Tkinter.LEFT,fill=Tkinter.BOTH,expand=1)
      idx=0
      for item in alist:
         self.ScaleStatsAreasListbox.insert(Tkinter.END,item)
         if item in defaultArea:
            self.ScaleStatsAreasListbox.select_set(idx)
         idx+=1
   
      af.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.BOTH,expand=1)
      #
      #  display
      #
      self.ScaleStatsDisplay=Tkinter.StringVar()
      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      labFrame=Tkinter.Frame(radioFrame)
      
      radioLabel=Tkinter.Label(labFrame,text="Display:")
      radioLabel.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      labFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
      
      a=Tkinter.Radiobutton(radioFrame,text="Bias",
                            variable=self.ScaleStatsDisplay,value="Bias")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      self.ScaleStatsDisplay.set("Bias")
      a=Tkinter.Radiobutton(radioFrame,text="Mean Abs Error",
                            variable=self.ScaleStatsDisplay,value="Mean Abs Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="RMS Error",
                            variable=self.ScaleStatsDisplay,value="RMS Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      a=Tkinter.Radiobutton(radioFrame,text="Mean Squared Error",
                            variable=self.ScaleStatsDisplay,value="Mean Squared Error")
      a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      
      threshFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(threshFrame,text="Percent Err <",
                            variable=self.ScaleStatsDisplay,value="Percent Err <")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.ScaleStatsThreshold=Tkinter.IntVar()
      self.ScaleStatsThresholdValue=Tkinter.StringVar()
      but=Tkinter.Menubutton(threshFrame,textvariable=self.ScaleStatsThresholdValue,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.thresholdScaleStatsPopup=Tkinter.Menu(but,tearoff=0)
      self.thresholdScaleStatsPopup.add_command(label="stuff")
      for i in xrange(self.__VU.getCFG('NUMTHRESH')):
         self.thresholdScaleStatsPopup.add_radiobutton(label="xxxx",indicatoron=0,value=i,
                                          variable=self.ScaleStatsThreshold,
                                          command=self.pickScaleStatsThreshold)
      but.config(menu=self.thresholdScaleStatsPopup)
      threshFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      twocatFrame=Tkinter.Frame(radioFrame)
      a=Tkinter.Radiobutton(twocatFrame,text="",variable=self.ScaleStatsDisplay,
                            value="TwoCat")
      a.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.scaleStatsTwoCatType=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.scaleStatsTwoCatType,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.scaleStatsTwoCatTypePopup=Tkinter.Menu(but,tearoff=0)
      for stat in ["Hits","Areal Hits","Misses","Areal Misses",
                   "False Alarms","Areal False Alarms","Correct Negatives",
                   "Areal Correct Negatives",
                   "Frequency Observed",
                   "Frequency Forecast",
                   "Fraction Correct",
                   "Areal Fraction Correct",
                   "Frequency Bias","Areal Frequency Bias",
                   "POD","Areal POD","FAR","Areal FAR","Threat Score",
                   "Areal Threat Score","Equitable Threat Score",
                   "Areal Equitable Threat Score","True Skill Score",
                   "Areal True Skill Score","Heidke Skill Score",
                   "Areal Heidke Skill Score","Odds Ratio","Areal Odds Ratio"]:
         self.scaleStatsTwoCatTypePopup.add_radiobutton(label=stat,
                        indicatoron=0,value=stat,
                        variable=self.scaleStatsTwoCatType,command=self.updateScaleStatsTwoCatType)
      self.scaleStatsTwoCatType.set("Fraction Correct")
      but.config(menu=self.scaleStatsTwoCatTypePopup)
      self.scaleStatsTwoCatCond=Tkinter.StringVar()
      but=Tkinter.Menubutton(twocatFrame,textvariable=self.scaleStatsTwoCatCond,
                             relief=Tkinter.RAISED,indicatoron=1)
      but.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      self.scaleStatsTwoCatCondPopup=Tkinter.Menu(but,tearoff=0)
      for cond in [">",">=","<=","<"]:
         self.scaleStatsTwoCatCondPopup.add_radiobutton(label=cond,
                     indicatoron=0,value=cond,
                     variable=self.scaleStatsTwoCatCond,command=self.updateScaleStatsTwoCatType)
      self.scaleStatsTwoCatCond.set(">")
      but.config(menu=self.scaleStatsTwoCatCondPopup)
      self.scaleStatsTwoCatValueString=Tkinter.StringVar()
      ent=Tkinter.Entry(twocatFrame,textvariable=self.scaleStatsTwoCatValueString,
                        width=5,relief=Tkinter.SUNKEN)
      self.scaleStatsTwoCatValueString.set("0.0")
      ent.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
      twocatFrame.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.X,expand=0)
      self.updateScaleStatsThreshold()
      return
   def updateScaleStatsTwoCatType(self):
      self.ScaleStatsDisplay.set("TwoCat")
      return
   #==================================================================
   #
   def pickScaleStatsThreshold(self):
      self.ScaleStatsDisplay.set("Percent Err <")
      self.updateScaleStatsThreshold()
      return
   
   def updateScaleStatsThreshold(self):
      parm=self.ScaleStatsParm.get()
      readParm=parm
      last3="xxx"
      if len(parm)>3:
         last3=parm[-3:]
         if ((last3=="Spd")or(last3=="Dir")):
            readParm=parm[:-3]
      thresholds=self.__VU.getVerThresholds(readParm)
      if last3 in ("Spd","Dir"):
         (threshmag,threshdir)=thresholds
         if last3=="Spd":
            thresholds=threshmag
         else:
            thresholds=threshdir

      dthresh=[]
      for i in xrange(len(thresholds)):
         t=thresholds[i]
         str="%d"%t
         dthresh.append(str)
      #
      #
      self.thresholdScaleStatsPopup.entryconfigure(0,label=parm)
      for i in xrange(len(dthresh)):
         self.thresholdScaleStatsPopup.entryconfigure(i+1,label=dthresh[i])
      self.ScaleStatsThresholdValue.set(dthresh[self.ScaleStatsThreshold.get()])
      return
   #=================================================================
   #  displayGroup - make a group of radio buttons with scale stuff
   #
   def displayGroup(self,master,labeltext,var,valuelist,defaultvalue,filltype,expandflag):
      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      radioLabel=Tkinter.Label(radioFrame,text=labeltext)
      radioLabel.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      for item in valuelist:
        a=Tkinter.Radiobutton(radioFrame,text=item,
                              variable=var,value=item)
        a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
        if item is defaultvalue:
           var.set(item)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=filltype,expand=expandflag)
   #=================================================================
   #  radioGroup - make a group of radio buttons
   #
   def radioGroup(self,master,labeltext,var,valuelist,defaultvalue,filltype,
                  expandflag,callback=None):
      radioFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      radioLabel=Tkinter.Label(radioFrame,text=labeltext)
      radioLabel.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      for item in valuelist:
        a=Tkinter.Radiobutton(radioFrame,text=item,
                              variable=var,value=item,command=callback)
        a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
        if item is defaultvalue:
           var.set(item)
      radioFrame.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=filltype,expand=expandflag)
   #=================================================================
   #  checkGroup - make a group of check buttons
   #
   def checkGroup(self,master,labeltext,varlist,valuelist,
                  defaultvalues,filltype,expandflag,callback=None):
      checkFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
      checkLabel=Tkinter.Label(checkFrame,text=labeltext)
      checkLabel.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      cnt=0
      for item in valuelist:
         a=Tkinter.Checkbutton(checkFrame,text=item,variable=varlist[cnt],
                               onvalue=item,offvalue="",command=callback)
         if item in defaultvalues:
            varlist[cnt].set(item)
         a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
         cnt+=1
      checkFrame.pack(side=Tkinter.TOP,fill=filltype,expand=expandflag)
      return varlist
   #=================================================================
   #  getCheckList - get list of values turned on in the checkbutton list
   #
   def getCheckList(self,checklist):
      outlist=[]
      for i in xrange(len(checklist)):
         a=checklist[i].get()
         if a!="":
            outlist.append(a)
      return outlist
   #=================================================================
   #  sListbox - make a listbox with a scrollbar
   #
   def sListbox(self,master,labeltext,itemlist,defaultItems,
                maxwid,maxheight,smode,filltype=Tkinter.BOTH,expandflag=1):
      sLabel=Tkinter.Label(master,text=labeltext)
      sLabel.pack(side=Tkinter.TOP,anchor=Tkinter.W)
      sb=Tkinter.Scrollbar(master,orient=Tkinter.VERTICAL)
      slb=Tkinter.Listbox(master,yscrollcommand=sb.set,
                          selectmode=smode,width=maxwid,height=maxheight)
      sb.config(command=slb.yview)
      sb.pack(side=Tkinter.RIGHT,fill=Tkinter.Y)
      slb.pack(side=Tkinter.LEFT,fill=filltype,expand=expandflag)
      idx=0
      for item in itemlist:
         slb.insert(Tkinter.END,item)
         if item in defaultItems:
            slb.select_set(idx)
         idx+=1
      return slb
   #=================================================================
   #  getListbox - get list of values turned on in the listbox
   #
   def getListbox(self,listbox):
      outlist=[]
      itemnums=listbox.curselection()
      try:
         itemnums=map(string.atoi,itemnums)
      except ValueError: pass
      for itemnum in itemnums:
         outlist.append(listbox.get(itemnum))
      return outlist
   #=================================================================            
   def setDatetype(self):
      type=self.Datetype.get()
      if type=="Period Length":
         self.ByList.pack_forget()
         self.ByPeriod.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.BOTH,expand=1)
      else:
         self.ByPeriod.pack_forget()
         self.ByList.pack(side=Tkinter.TOP,anchor=Tkinter.NW,fill=Tkinter.BOTH,expand=1)
   #==================================================================
   #  getRecentDates - gets a list of date strings from today through
   #                   numdays in the past.  Also returns list of
   #                   unix times for the beginning of each date.
   def getRecentDates(self,numdays):
      recentDateStrings=[]
      recentDates=[]
      (nyea,nmon,nday,nhou,nmin,nsec,nwda,nyda,ndst)=time.gmtime()
      midtoday=calendar.timegm((nyea,nmon,nday,0,0,0,0,0,0))
      for i in xrange(numdays):
         daymid=midtoday-(i*DAYSECS)
         (gyr,gmo,gdy,ghr,gmi,gse,gwd,gyd,gds)=time.gmtime(daymid)
         recentDateStrings.append("%4.4d/%2.2d/%2.2d"%(gyr,gmo,gdy))
         recentDates.append(daymid)
      return recentDateStrings,recentDates
#
#  Special global routines used in Histogram callback stuff to
#  move data on/off the screen
#
def showmodel(self,modname):
    if self.showmod[modname]==1:
        self.cd.canvas.move(modname,0,-self.cd.curheight)
        self.cd.canvas.lower(modname)
        self.showmod[modname]=0
        self.modb[modname].config(fg="grey")
    else:
        self.cd.canvas.move(modname,0,self.cd.curheight)
        self.cd.canvas.lift(modname)
        self.showmod[modname]=1
        self.modb[modname].config(fg=self.colornames[modname])
    return
#=====================================================================
#
#  Toggle stuff with "but" tag in the but1 list
#
def showBut1(self,but):
    if but.isdigit():
       newbut="f%s"%but
       but=newbut
    if self.but1state.get(but)==1:
        self.cd.canvas.move(but,0,-self.cd.curheight)
        self.cd.canvas.lower(but)
        self.but1state[but]=0
        self.but1[but].config(fg="grey")
    else:
        self.cd.canvas.move(but,0,self.cd.curheight)
        self.cd.canvas.lift(but)
        self.but1state[but]=1
        self.but1[but].config(fg="black")
#=====================================================================
#
#  Turn off all but1 tags except first button
#
def startBut1(self):
    for but in self.but1names:
       if but!=self.but1names[0]:
          showBut1(self,but)
#=====================================================================
#
#  Move toggled but1 buttons - one to the left
#
def prevBut1(self):
    newbut=[]
    for but in self.but1names:
       if self.but1state.get(but)==1:
          newbut.append(1)
       else:
          newbut.append(0)
    temp=newbut[0]
    del newbut[0]
    newbut.append(temp)
    for i in xrange(len(self.but1names)):
       but=self.but1names[i]
       now=self.but1state[but]
       after=newbut[i]
       if ((now==1)and(after==0)):
          self.cd.canvas.move(but,0,-self.cd.curheight)
          self.cd.canvas.lower(but)
          self.but1state[but]=0
          self.but1[but].config(fg="grey")
       elif ((now==0)and(after==1)):
          self.cd.canvas.move(but,0,self.cd.curheight)
          self.cd.canvas.lift(but)
          self.but1state[but]=1
          self.but1[but].config(fg="black")
#=====================================================================
#
#  Move toggled but1 buttons - one to the right
#
def nextBut1(self):
    newbut=[]
    for but in self.but1names:
       if self.but1state.get(but)==1:
          newbut.append(1)
       else:
          newbut.append(0)
    temp=newbut.pop()
    newbut[0:0]=[temp]
    for i in xrange(len(self.but1names)):
       but=self.but1names[i]
       now=self.but1state[but]
       after=newbut[i]
       if ((now==1)and(after==0)):
          self.cd.canvas.move(but,0,-self.cd.curheight)
          self.cd.canvas.lower(but)
          self.but1state[but]=0
          self.but1[but].config(fg="grey")
       elif ((now==0)and(after==1)):
          self.cd.canvas.move(but,0,self.cd.curheight)
          self.cd.canvas.lift(but)
          self.but1state[but]=1
          self.but1[but].config(fg="black")
#=====================================================================
#
#  Toggle stuff with "but" tag in the but2 list
#
def showBut2(self,but):
    if self.but2state.get(but)==1:
        self.cd.canvas.move(but,-self.cd.curwidth,0)
        self.cd.canvas.lower(but)
        self.but2state[but]=0
        self.but2[but].config(fg="grey")
    else:
        self.cd.canvas.move(but,self.cd.curwidth,0)
        self.cd.canvas.lift(but)
        self.but2state[but]=1
        self.but2[but].config(fg=self.colornames[but])
#=====================================================================
#
#  Turn off all but2 tags except first button
#
def startBut2(self):
    for but in self.but2names:
       if but!=self.but2names[0]:
          showBut2(self,but)
#=====================================================================
#
#  Move toggled but2 buttons - one to the left
#
def prevBut2(self):
    newbut=[]
    for but in self.but2names:
       if self.but2state.get(but)==1:
          newbut.append(1)
       else:
          newbut.append(0)
    temp=newbut[0]
    del newbut[0]
    newbut.append(temp)
    for i in xrange(len(self.but2names)):
       but=self.but2names[i]
       now=self.but2state[but]
       after=newbut[i]
       if ((now==1)and(after==0)):
          self.cd.canvas.move(but,-self.cd.curwidth,0)
          self.cd.canvas.lower(but)
          self.but2state[but]=0
          self.but2[but].config(fg="grey")
       elif ((now==0)and(after==1)):
          self.cd.canvas.move(but,self.cd.curwidth,0)
          self.cd.canvas.lift(but)
          self.but2state[but]=1
          self.but2[but].config(fg=self.colornames[but])
#=====================================================================
#
#  Move toggled but2 buttons - one to the right
#
def nextBut2(self):
    newbut=[]
    for but in self.but2names:
       if self.but2state.get(but)==1:
          newbut.append(1)
       else:
          newbut.append(0)
    temp=newbut.pop()
    newbut[0:0]=[temp]
    for i in xrange(len(self.but2names)):
       but=self.but2names[i]
       now=self.but2state[but]
       after=newbut[i]
       if ((now==1)and(after==0)):
          self.cd.canvas.move(but,-self.cd.curwidth,0)
          self.cd.canvas.lower(but)
          self.but2state[but]=0
          self.but2[but].config(fg="grey")
       elif ((now==0)and(after==1)):
          self.cd.canvas.move(but,self.cd.curwidth,0)
          self.cd.canvas.lift(but)
          self.but2state[but]=1
          self.but2[but].config(fg=self.colornames[but])
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
#
#  stuff to support a callback with a pre-known variable
#        
def GenericCallback(callback, *firstArgs, **firstKWArgs):
    if firstKWArgs:
        return GC(callback, *firstArgs, **firstKWArgs)
    else:
        return GCNoKWArgs(callback, *firstArgs)
#
#  Classes for callbacks
#
class GC:
    def __init__(self,callback,*firstArgs, **firstKWArgs):
        self.__callback=callback
        self.__firstArgs=firstArgs
        self.__firstKWArgs=firstKWArgs
    def __call__(self, *lastArgs, **kwArgs):
        if kwArgs:
            netKWArgs=self.__firstKWArgs.copy()
            netKWArgs.update(self.__kwArgs)
        else:
            netKWArgs=self.__firstKWArgs
        return self.__callback (*(self.__firstArgs+lastArgs),**netKWArgs)
class GCNoKWArgs:
    def __init__(self, callback, *firstArgs):
        self.__callback=callback
        self.__firstArgs=firstArgs
    def __call__(self, *args, **kwArgs):
        return self.__callback (*(self.__firstArgs+args),**kwArgs)
