# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# BOIVerifySumTemps - Version 2.0
#
#   Make a summary of some verification information about MaxT or MinT
#   and send it as an html-formatted e-mail, and send to a machine to
#   be put on our internal webpage.
#
# Author: Tim Barker - SOO BOI
#   2007/10/x25 - version 2.0 - original implementation of this program
#
#   2010/04/23 ryu  Initial port to AWIPS II.
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/21/13        16770         ryu            Change name of temporary files
#                                                 for dual domain.
#=====================================================================
#
#  S T A R T   C O N F I G U R A T I O N   S E C T I O N
#
#=====================================================================
#
#  Number of days in the past to use for comparisons
#
NUMDAYS=30
#
#  Name of edit area to use for stats
#
EDITAREA="Populated_Areas"
EDITAREA="ID_Boise"
#
#  Name of database to use for "truth"
#
OBSMODEL="Obs"
OBSMODEL="RTMA"
#
#  Main stat is % errors less than THRESHOLD degrees
#
THRESHOLD=3
#
#  Bad stat is % errors greater than BADTHRESHOLD degrees
#
BADTHRESHOLD=10
#
#  List of models for comparisons/rankings.  If the model does not
#  exist in the BOIVerify system, it just skips it - so this list
#  has the most possible models I could think of...
#
MODLIST=["Official","MOSGuideBC","MOSGuide","GFS40","GFS40BC",
         "ADJMAV","ADJMAVBC","NAM12","NAM12BC","DGEX","DGEXBC",
         "ADJMEX","ADJMEXBC","ADJMET","ADJMETBC","SREF","SREFBC",
         "NGM80","NGM80BC","ADJFWC","ADJFWCBC","ADJDGX","ADJDGXBC",]
#
#  The e-mail address to be 'From' when e-mails are sent
#     an example:  EMAIL_FROM_ADDRES="timothy.barker@noaa.gov"
#     (but PLEASE don't use this - I don't want people replying to me!)
#
EMAIL_FROM_ADDRESS="nobody.nobody@noaa.gov"
#
#  Commands to be executed to send the temporary e-mail file.
#  You probably need to reference the EMAIL_TEMP_FILE you
#  specified above.  This works on a typical AWIPS setup, where it
#  scp's the file through the firewall to ls1 (without a password),
#  then does a sendmail command on ls1, and then deletes the temporary
#  file on ls1.
#
EMAIL_CMDS=["scp /tmp/{FILE} ldad@ls1:/tmp/{FILE}",
            "ssh ldad@ls1 sendmail -t </tmp/{FILE}",
            "ssh ldad@ls1 rm /tmp/{FILE}",
           ]
#
#  Always sends summary to these addresses - even if they were not
#  involved in the forecast.  If you don't want to always send it to
#  somebody, just set this to an empty list.
#
#  an example: EMAIL_ADMIN_ADDRESSES=["timothy.barker@noaa.gov",
#                                     "john.jannuzzi@noaa.gov",
#                                    ]
#  (but PLEASE don't use this - I don't want all your e-mails!)
#
EMAIL_ADMIN_ADDRESSES=[]
#
#  Dictionary of GFE user IDs --> email addresses.  If an ID is found
#  that is not in this list - it just isn't e-mailed.  Thus, if you don't
#  want individual e-mails, just make this an empty dictionary with
#  a statement like EMAIL_ADDRESSES={}
#
#  an example: EMAIL_ADDRESSES={"tbarker":"timothy.barker@noaa.gov",
#                               "jjannuzz":"john.jannuzzi@noaa.gov",
#                              }
EMAIL_ADDRESSES={}
#
#  Temporary directory where clean YYYYMMDD_PARM.html output is put
#
SAVE_DIR="/tmp"
#
#  Commands to run after the temporary YYYYMMDD_PARM.html
#  output is created.  You can put "{FILE}" in these and it will be
#  replaced with the filename before execution.
#
SAVE_CMDS=["scp /tmp/{FILE} ldad@ls1:/tmp/{FILE}",
           "ssh ldad@ls1 chmod 666 /tmp/{FILE}",
           "ssh ldad@ls1 cp /tmp/{FILE} /lanfiles/GridVer/{FILE}",
           "ssh ldad@ls1 rm /tmp/{FILE}",
          ]
#
#
#=====================================================================
#
#  E N D   C O N F I G U R A T I O N   S E C T I O N
#
#=====================================================================
#
#  Do not show this in any menu.  Normally run via runProcedure
#
#MenuItems = ["Verify"]
MenuItems = ["None"]
#
#  CONSTANTS
#
HOURSECS=60*60
DAYSECS=24*60*60
MONS=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
      "Oct","Nov","Dec"]
DAYS=["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]
#
#  Imports
#

from numpy import *
import AbsTime, TimeRange
import SmartScript
import BOIVerifyUtility
import time,calendar,os,re,sys
#
#
#
class Procedure (SmartScript.SmartScript):
   def __init__(self, dbss):
      SmartScript.SmartScript.__init__(self, dbss)
      self._dbss=dbss

   def execute(self,):
      self.VU=BOIVerifyUtility.BOIVerifyUtility(self._dbss, None)
      self.VU.setVerbose(0)
      self.VU.logMsg("Starting BOIVerifySumTemps")

      self._empty = self.VU._empty
      #
      #
      #
      self.eaGrid=self.encodeEditArea(EDITAREA)
      self.eaMask=ravel(self.eaGrid)
      self.numpoints=add.reduce(self.eaMask)
      self.obsModel=OBSMODEL
      grid=self._empty
      #
      #  See if a time is provided in a /tmp/<siteId>_SumTemps.time file
      #  or otherwise get the current system time
      #
      now=self.getNow()
      #
      #  Log the 'time' this is running (might be a 'specified' time)
      #
      (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(now)
      self.VU.logMsg("Running at time: %4.4d/%2.2d/%2.2d %2.2d:%2.2dZ"%(gyea,gmon,gday,ghou,gmin))
      #
      #  Between 00Z and 12Z - do summary for "yesterday's" MaxT
      #  Otherwise - do summary for "todays" MinT
      #
      #  *****  THIS IS SPECIFIC TO OUR TIMEZONE - OTHERS MAY DIFFER *****
      #
      if ghou<12:
         parm="MaxT"
         (vyea,vmon,vday,vhou,vmin,vsec,vwda,vyda,vdst)=time.gmtime(now-(24*HOURSECS))
         verifday=calendar.timegm((vyea,vmon,vday,0,0,0,0,0,0))
      else:
         parm="MinT"
         (vyea,vmon,vday,vhou,vmin,vsec,vwda,vyda,vdst)=time.gmtime(now)
         verifday=calendar.timegm((vyea,vmon,vday,0,0,0,0,0,0))
      #
      #  get real start/end time of the grid on this day (verifday)
      #
      self.VU.logMsg("Doing verification for %s on %4.4d/%2.2d/%2.2d"%(parm,vyea,vmon,vday))
      recList=self.VU.getObsPeriod(self.obsModel,parm,verifday,1)
      if len(recList)!=1:
         self.VU.logMsg("Whoa...should have only found 1 observed grid - but found %d"%len(recList))
         self.VU.logMsg("Aborting...")
         return
      stime=self.VU.oncStime[int(recList[0])]
      etime=self.VU.oncEtime[int(recList[0])]
      #
      #  Get forecast hours for the 14 periods
      #
      fhours=self.getPeriods(stime)
      #
      #  get Difficulty Info
      #
      diffInfo=self.getDifficultyRankings(parm,vyea,vmon,vday,NUMDAYS)
      #
      #  Get info for the 14 periods verifying today
      #
      perInfos=self.getTodayInfo(parm,MODLIST,verifday,fhours)
      #
      #  Get average forecast error info for the 14 periods over the past
      #  NUMDAYS days.
      #
      pastInfos=self.getPastInfo(parm,MODLIST,verifday,NUMDAYS,fhours)
      #
      #  Format output with all forecaster IDs
      #
      fulloutput=self.formatOutput(parm,verifday,stime,etime,diffInfo,perInfos,
                                   pastInfos)
      #
      #  Format clean output with NO forecasters IDs
      #
      cleanoutput=self.formatOutput(parm,verifday,stime,etime,diffInfo,perInfos,
                                    pastInfos,showFcstr=0)
      #
      #  Format output for each involved forecaster
      #
      individualOutput={}
      for info in perInfos:
         name=info[3]
         if ((name=="unknown")or(name=="multiple")):
            continue
         if name not in individualOutput.keys():
            output=self.formatOutput(parm,verifday,stime,etime,diffInfo,
                                     perInfos,pastInfos,showFcstr=1,
                                     fcstrName=name)
            individualOutput[name]=output
      #
      #  Send the full message to ADMIN people
      #
      for to in EMAIL_ADMIN_ADDRESSES:
         self.emailMessage(to,parm,verifday,fulloutput)
      #
      #  Send the individual output to known e-mail addresses
      #
      for name in individualOutput.keys():
         if name in EMAIL_ADDRESSES.keys():
            to=EMAIL_ADDRESSES[name]
            self.emailMessage(to,parm,verifday,individualOutput[name])
      #
      #  Save the clean output for use on webpages
      #
      (vyea,vmon,vday,vhou,vmin,vsec,vwda,vyda,vdst)=time.gmtime(verifday)
      outname="%s_%4.4d%2.2d%2.2d_%s.html"%(self.getSiteID(),vyea,vmon,vday,parm)
      fullname="%s/%s_%s"%(SAVE_DIR, outname)
      outfile=file(fullname,"w")
      outfile.write(cleanoutput)
      outfile.close()
      if len(SAVE_CMDS)>0:
         for cmd in SAVE_CMDS:
            newcmd=cmd.replace("{FILE}",outname)
            os.system(newcmd)
      os.system("rm %s"%fullname)
      #
      #
      #
      return
   #==================================================================
   #  getNow - sees if there is a specified time in the
   #           /tmp/<siteId>_SumTemps.time file.  If no file exists, or there
   #           is trouble reading file, it uses the current system
   #           time as 'now'.
   #
   def getNow(self):
      #
      #  If all else fails...it will use the current system time
      #
      now=time.time()
      #
      #  If the file doesn't exist or isn't a real file...return
      #
      timeFile="/tmp/%s_SumTemps.time" % self.getSiteID()
      if not os.path.exists(timeFile):
         return now
      if not os.path.isfile(timeFile):
         return now
      #
      #  Try reading the file and give error message if trouble
      #
      try:
         infile=file(timeFile,"r")
         inlines=infile.readlines()
         infile.close()
         os.unlink(timeFile)
      except:
         self.VU.logMsg("Could not read %s"%timeFile)
         return now
      #
      #  Lines that have ########_#### entries are potential times
      #
      for line in inlines:
         sline=line.strip()
         matchobj=re.search("(\d{8}_\d{4})",sline)
         #
         #  If it doesn't look like a date/time - tell the user
         #
         if matchobj is None:
            self.VU.logMsg("%s doesn't look like a date/time"%sline)
            continue
         #
         #  If time can be read and works as a date...use it!
         #  otherwise tell them that we couldn't decode it
         #
         timecode=matchobj.group()
         iyea=int(timecode[0:4])
         imon=int(timecode[4:6])
         iday=int(timecode[6:8])
         ihou=int(timecode[9:11])
         if ((imon<1)or(imon>12)or(iday<1)or(iday>31)or(ihou<1)or(ihou>23)):
            self.VU.logMsg("Could not decode specified time of %s"%timecode)
         try:
            now=calendar.timegm((iyea,imon,iday,ihou,0,0,0,0,0))
         except:
            self.VU.logMsg("Could not decode specified time of %s"%timecode)
      return now
   #==================================================================
   #  getPeriods - get the forecast hours to the start of the grid
   #               so that we know which 'period' to store forecasts
   #               under.  Assumes that forecasts are from 00 or 12Z.
   #
   def getPeriods(self,starttime):
      (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(starttime)
      #
      #  If grid starts between 6 and 18Z, then last forecast for
      #  this is from a 00Z run of the same day.
      #
      if ((shou>6)and(shou<=18)):
         sbase=calendar.timegm((syea,smon,sday,0,0,0,0,0,0))
      #
      #  Otherwise the last forecast is from 12Z.  That would be
      #  12Z 'today' if the start is late in the UTC day...but
      #  12Z 'yesterday' if the start is early in the UTC day.
      #
      else:
         if shou>12:
            sbase=calendar.timegm((syea,smon,sday,12,0,0,0,0,0))
         else:
            (yyea,ymon,yday,yhou,ymin,ysec,ywda,yyda,ydst)=time.gmtime(starttime-(24*HOURSECS))
            sbase=calendar.timegm((yyea,ymon,yday,12,0,0,0,0,0))
      #
      #  Now get the forecast hours for 14 periods
      #
      fhours=[]
      for per in range(14):
         base=sbase-(12*per*HOURSECS)
         hours=int(float(starttime-base)/float(HOURSECS))
         fhours.append(hours)
      return fhours
   #==================================================================
   #  emailMessage - e-mail the HTML message in 'output' to the e-mail
   #                 address 'to'.  'parm' and 'verifday' used in
   #                 subject
   #
   def emailMessage(self,to,parm,verifday,output):
      (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(verifday)
      subject="%s verification for %s, %s %d, %4.4d"%(parm,DAYS[gwda],
               MONS[gmon-1],gday,gyea)

      EMAIL_TEMP_FILE="/tmp/%s_temp_email.txt" % self.getSiteID()
      outfile=open(EMAIL_TEMP_FILE,"w")
      outfile.write("To:%s\n"%to)
      outfile.write("From:%s\n"%EMAIL_FROM_ADDRESS)
      outfile.write("Subject:%s\n"%subject)
      outfile.write("Content-type: text/html\n")
      outfile.write(output)
      outfile.close()
      if len(EMAIL_CMDS)>0:
         for cmd in EMAIL_CMDS:
            newcmd=cmd.replace("{FILE}", EMAIL_TEMP_FILE)
            os.system(newcmd)
      #os.system("rm %s"%EMAIL_TEMP_FILE)
      return
   #==================================================================
   #  getDifficultyRankings - look through observation grids for the
   #                          past NUMDAYS days, and rank the current
   #                          day compared to all others.  The
   #                          comparisons done in 3 ways.
   #
   #                          Standard Deviation
   #                          24hr Changes
   #                          Climate anomalies
   #
   #
   def getDifficultyRankings(self,parm,vyea,vmon,vday,numdays):
      self.VU.logMsg("getting Difficulty Information")
      self.prevClimoSave=-1
      self.nextClimoSave=-1
      anom={}
      absanom={}
      chgs={}
      abschgs={}
      stds={}
      #
      #  get observed records over last numdays+1 days
      #     (added 1 so that we can calculate the 24hr change on 1st day)
      #
      todaytime=calendar.timegm((vyea,vmon,vday,0,0,0,0,0,0))      
      obsCases=self.VU.getObsCases(parm,self.obsModel,0,0,
                                   "Verifying on","Period Length",
                                   todaytime,numdays+1,[])
      #
      #  sort the keys...and make sure the last one is the same date
      #  as "today".
      #
      obskeys=obsCases.keys()
      obskeys.sort()
      lastkey=obskeys[-1]
      (stimestr,etimestr)=lastkey.split(",")
      midper=float(int(stimestr)+int(etimestr))/2.0
      (pyea,pmon,pday,phou,pmin,psec,pwda,pyda,pdst)=time.gmtime(midper)
      if (pyea!=vyea)or(pmon!=vmon)or(pday!=vday):
         self.VU.logMsg("Could not find obs data for 'today'")
         return
      todaykey=lastkey
      #
      #  Loop over all the previous days
      #
      obskeys=obsCases.keys()
      obskeys.sort()
      for i in range(1,len(obskeys)):
         key=obskeys[i]
         #
         #  The "day" for climo calculations is assumed to be the "day"
         #  in the middle of the time period.  That may not work in all
         #  timezones.
         #
         (stimestr,etimestr)=key.split(",")
         midper=float(int(stimestr)+int(etimestr))/2.0
         (pyea,pmon,pday,phou,pmin,psec,pwda,pyda,pdst)=time.gmtime(midper)
         #
         #  read observed grid for that day
         #
         (stimestr,etimestr)=key.split(",")
         basetime=int(stimestr)
         stime=int(stimestr)
         etime=int(etimestr)
         records=obsCases[key]
         ogrid=self.VU.getVerGrids(self.obsModel,basetime,parm,stime,etime,recList=records)
         if ogrid is None:
            continue
         #
         #  Calculate the variance in that grid
         #
         obsgrid=where(self.eaMask,ravel(ogrid),0)
         obsgrid2=obsgrid*obsgrid
         std=sqrt(float(add.reduce(obsgrid2))/self.numpoints-((float(add.reduce(obsgrid))/self.numpoints)**2))
         stds[key]=std
         #
         #  read observed grid for the day previous to that...and save
         #  average change...and average absolute value of change
         #
         prevkey=obskeys[i-1]
         (pstimestr,petimestr)=prevkey.split(",")
         pbasetime=int(pstimestr)
         hdiff=(basetime-pbasetime)/HOURSECS
         if ((basetime-pbasetime)/HOURSECS)==24:
            pstime=int(pstimestr)
            petime=int(petimestr)
            records=obsCases[prevkey]
            pgrid=self.VU.getVerGrids(self.obsModel,pbasetime,parm,pstime,petime,recList=records)
            if pgrid is not None:
               prevgrid=where(self.eaMask,ravel(pgrid),0)
               chggrid=obsgrid-prevgrid
               chgs[key]=add.reduce(chggrid)/self.numpoints
               abschgs[key]=add.reduce(abs(chggrid))/self.numpoints
               chgsstr="%5.2f (%6.2f)"%(abschgs[key],chgs[key])
         #
         #  Read the climatology for that day...and calculate average
         #  anomaly...and average absolute anomaly.
         #
         anomstr="              "
         cgrid=self.getClimoGrid(parm,pyea,pmon,pday)
         if cgrid is not None:
            climgrid=where(self.eaMask,ravel(cgrid),0)
            anomgrid=obsgrid-climgrid
            anom[key]=add.reduce(anomgrid)/self.numpoints
            absanom[key]=add.reduce(abs(anomgrid))/self.numpoints
            anomstr="%5.2f (%6.2f)"%(absanom[key],anom[key])
         #
         #  For debugging...write the std,abschg,chg,absanom,anom each day
         #
         self.VU.logMsg("For %4.4d/%2.2d/%2.2d: %5.2f %s %s"%(pyea,
               pmon,pday,stds[key],chgsstr,anomstr),5)
      #
      #  Find the difficulty information
      #
      anomInfo=(-999,-999,-999,-999)
      anomkeys=absanom.keys()
      if todaykey in anomkeys:
         anomkeys.sort(lambda x,y: cmp(absanom[x],absanom[y]))
         anomOutOf=len(anomkeys)
         anomRank=anomOutOf-anomkeys.index(todaykey)
         avgAbsAnom=absanom[todaykey]
         avgAnom=anom[todaykey]
         anomInfo=(avgAbsAnom,avgAnom,anomRank,anomOutOf)

      chgsInfo=(-999,-999,-999,-999)
      chgskeys=abschgs.keys()
      if todaykey in chgskeys:
         chgskeys.sort(lambda x,y: cmp(abschgs[x],abschgs[y]))
         chgsOutOf=len(chgskeys)
         chgsRank=chgsOutOf-chgskeys.index(todaykey)
         avgChgs=chgs[todaykey]
         avgAbsChgs=abschgs[todaykey]
         chgsInfo=(avgAbsChgs,avgChgs,chgsRank,chgsOutOf)

      stdsInfo=(-999,-999,-999)
      stdskeys=stds.keys()
      if todaykey in stdskeys:
         stdskeys.sort(lambda x,y: cmp(stds[x],stds[y]))
         stdsOutOf=len(stdskeys)
         stdsRank=stdsOutOf-stdskeys.index(todaykey)
         std=stds[todaykey]
         stdsInfo=(std,stdsRank,stdsOutOf)
      
      return(stdsInfo,chgsInfo,anomInfo)
   #==================================================================
   #  getClimoGrid - Get the PRISM climo grid for a particular day
   #                 using a weighted average between the mid-month
   #                 climo grids
   #
   def getClimoGrid(self,parm,vyea,vmon,vday):
      siteID = self.getSiteID()
      ##dbName = siteID + "_D2D_PRISMClimo" # could use NCDCClimo
      dbid = self.findDatabase("D2D_PRISMClimo")
      if not dbid.isValid():
         return None
         
      if parm=="MaxT":
         parmName="mxt"
      else:
         parmName="mnt"
      #
      #  mid-point day of month (always the 15th except for Feb)  
      #
      vmid=15
      if vmon==2:
         vmid=14
      #
      #  Get previous month
      #
      if vday<=vmid:
         prevmon=vmon-1
         if prevmon<1:
            prevmon=12
      else:
         prevmon=vmon
      #
      #  Get Next month
      #
      if vday<=vmid:
         nextmon=vmon
      else:
         nextmon=vmon+1
         if nextmon>12:
            nextmon=1
      #
      #  Get timeranges surrounding grids in the climo databases
      #
      prevClimoStart=calendar.timegm((1970,prevmon,14,0,0,0,0,0,0))
      prevClimoTR=TimeRange.TimeRange(AbsTime.AbsTime(prevClimoStart),
                               AbsTime.AbsTime(prevClimoStart+(3*DAYSECS)))
      nextClimoStart=calendar.timegm((1970,nextmon,14,0,0,0,0,0,0))
      nextClimoTR=TimeRange.TimeRange(AbsTime.AbsTime(nextClimoStart),
                               AbsTime.AbsTime(nextClimoStart+(3*DAYSECS)))
      #
      #  Weights for prev/next (use a year not near the epoch...1980 works)
      #
      tyear=1980
      today=calendar.timegm((tyear,vmon,vday,0,0,0,0,0,0))
      nyea=1980
      if nextmon<vmon:
         nyea=1981
      nextMid=calendar.timegm((nyea,nextmon,vmid,0,0,0,0,0,0))
      pyea=1980
      if prevmon>vmon:
         pyea=1979
      prevMid=calendar.timegm((pyea,prevmon,vmid,0,0,0,0,0,0))
      range=nextMid-prevMid
      weightPrev=float(nextMid-today)/float(range)
      weightNext=float(today-prevMid)/float(range)
      #
      #  Read the climo grids on either side
      #
      if prevClimoStart!=self.prevClimoSave:
         temp=self.getGrids(dbName,parmName,"SFC",prevClimoTR)
         self.prevClimo=self.convertKtoF(temp)
         self.prevClimoSave=prevClimoStart
      if nextClimoStart!=self.nextClimoSave:
         temp=self.getGrids(dbName,parmName,"SFC",nextClimoTR)
         self.nextClimo=self.convertKtoF(temp)
         self.nextClimoSave=nextClimoStart
      #
      #  Create the climo grid
      #
      if ((self.prevClimo is not None)and(self.nextClimo is not None)):
         climoGrid=(weightPrev*self.prevClimo)+(weightNext*self.nextClimo)
      else:
         climoGrid=None
      return climoGrid
   #==================================================================
   #  convertKtoF - convert a Kelvin grid to a Fahrenheit grid
   #
   def convertKtoF(self,grid):
      if grid is None:
         return None
      return ((grid-273.15)*(9.0/5.0))+32.0
   #==================================================================
   #  getTodayInfo - get list of forecast info for
   #
   def getTodayInfo(self,parm,modlist,verifday,fhours):
      self.VU.logMsg("Getting today's forecast errors")
      perInfos=[]
      errors={}
      bias={}
      maes={}
      badpcts={}
      forecasters={}
      bases={}
      for mod in modlist:
         cycleList=[0,12]
         baseOffset=self.VU.getBaseOffset(mod)
         if baseOffset!=0:
            newCycles=[]
            for cycle in cycleList:
               offsetCycle=cycle-baseOffset
               if offsetCycle<0:
                  offsetCycle+=24
               if offsetCycle>23:
                  offsetCycle-=24
               newCycles.append(offsetCycle)
            cycleList=newCycles
         #
         #  Save time by getting the records...
         #
         fstatcases=self.VU.getStatCases(parm,mod,self.obsModel,"Verifying on",
                       "Period Length",fhrStart=0,fhrEnd=240,cycles=cycleList,
                       fromDay=verifday,numDays=1)
         fullcases=self.VU.getCases(parm,mod,parm,self.obsModel,"Verifying on",
                       "Period Length",fhrStart=0,fhrEnd=240,
                       cycles=cycleList,fromDay=verifday,numDays=1)
         gridcases=fullcases[mod]
         statcases=fstatcases[mod]
         statkeys=statcases.keys()
         gridkeys=gridcases.keys()
         if len(gridkeys)>0:
            self.VU.logMsg("  Reading %3d cases for %s"%(len(gridkeys),mod))
         gridkeys.sort()
         for gridkey in gridkeys:
            if gridkey in statkeys:
               srecList=statcases[gridkey]
            else:
               srecList=None
            (basestr,stimestr,etimestr)=gridkey.split(",")
            base1=int(basestr)
            stime1=int(stimestr)
            etime1=int(etimestr)
            valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"% err below",editArea=EDITAREA,
                          forceCalc=0,statVal=THRESHOLD,grecList=gridcases[gridkey],
                          srecList=srecList)
            fhr=float(stime1-base1)/float(HOURSECS)
            fhr-=baseOffset
            if fhr not in fhours:
               continue
            per=fhours.index(fhr)
            savekey="%s,%2.2d"%(mod,per)
            errors[savekey]=valx
            #
            #  For the official model...we also save the bias in bias{}
            #  The mae in maes{}, and a string with the responsibile
            #  forecaster in forecasters{}.  We also save the basetime
            #  in bases{}
            #
            if mod=="Official":
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"bias",editArea=EDITAREA,
                          forceCalc=0,grecList=gridcases[gridkey],
                          srecList=srecList)
               bias[savekey]=valx
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"mae",editArea=EDITAREA,
                          forceCalc=0,grecList=gridcases[gridkey],
                          srecList=srecList)
               maes[savekey]=valx
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"% err below",editArea=EDITAREA,
                          forceCalc=0,statVal=BADTHRESHOLD,
                          grecList=gridcases[gridkey],
                          srecList=srecList)
               badpcts[savekey]=1.0-valx
               
               #
               #  save string with forecaster ID responsible
               #
               fid="unknown"
               if len(gridcases[gridkey])>0:
                  (frecList,orecList)=gridcases[gridkey]
                  flist=[]
                  for frec in frecList:
                     fcstrs=self.VU.getForecasters(mod,parm,frec)
                     for fcstr in fcstrs:
                        if fcstr>0:
                           if fcstr not in flist:
                              flist.append(fcstr)
                  if len(flist)>1:
                     fid="multiple"
                  elif len(flist)==1:
                     fid=self.VU.getFcstrID(flist[0])
                  else:
                     fid="unknown"
               forecasters[savekey]=fid
               bases[savekey]=base1
      maekeys=maes.keys()
      errorkeys=errors.keys()
      #
      #  Sort data for each period
      #
      for per in range(14):
         #
         #  Get errors for all models making a forecast for this period
         #
         modVals={}
         for errorkey in errorkeys:
            (mod,testper)=errorkey.split(",")
            if int(testper)==per:
               modVals[mod]=errors[errorkey]
         #
         #  Sort to find the model with the best error value for
         #  this period
         #
         modkeys=modVals.keys()
         modkeys.sort(lambda x,y: cmp(modVals[x],modVals[y]))
         modkeys.reverse()
         #
         #  Get Official results - if they exist
         #
         officialkey="Official,%2.2d"%per
         if officialkey in bias.keys():
            perbias=bias[officialkey]
            permaes=maes[officialkey]
            perfcstr=forecasters[officialkey]
            perbase=bases[officialkey]
            badpct=badpcts[officialkey]*100.0
         else:
            perbias=0.0
            permaes=0.0
            perfcstr=""
            perbase=0
            badpct=0.0
         #
         #  Get Official rank - if it exists
         #
         if "Official" in modkeys:
            perval=modVals["Official"]*100.0
            perrank=modkeys.index("Official")+1
         else:
            perval=0.0
            perrank=0
         #
         #  Get rankings of other models
         #
         peroutof=len(modkeys)
         bestmods=modkeys
         if "Official" in modkeys:
            bestmods.remove("Official")
         if len(bestmods)>0:
            perbestmod=bestmods[0]
            perbestmodval=modVals[perbestmod]*100.0
         else:
            perbestmod=""
            perbestmodval=0.0
         if len(bestmods)>1:
            persecondmod=bestmods[1]
            persecondmodval=modVals[persecondmod]*100.0
         else:
            persecondmod=""
            persecondmodval=0.0
         if len(bestmods)>2:
            perworstmod=bestmods[-1]
            perworstmodval=modVals[perworstmod]*100.0
         else:
            perworstmod=""
            perworstmodval=0.0
         fhrstr="%d"%fhours[per]
         perInfo=(per+1,fhrstr,perbase,perfcstr,perbias,permaes,badpct,perval,
                  perrank,peroutof,perbestmod,perbestmodval,
                  persecondmod,persecondmodval,perworstmod,perworstmodval)
         perInfos.append(perInfo)
      return perInfos
   #==================================================================
   #  getPastInfo
   #
   def getPastInfo(self,parm,modlist,verifday,numdays,fhours):
      self.VU.logMsg("Ranking forecast errors over past %d days"%numdays)
      pastInfos=[]
      totals={}
      counts={}
      #
      #  space for saving official stats
      #
      toterr={}
      cnterr={}
      totmae={}
      cntmae={}
      totbad={}
      cntbad={}
      for mod in modlist:
         cycleList=[0,12]
         baseOffset=self.VU.getBaseOffset(mod)
         if baseOffset!=0:
            newCycles=[]
            for cycle in cycleList:
               offsetCycle=cycle-baseOffset
               if offsetCycle<0:
                  offsetCycle+=24
               if offsetCycle>23:
                  offsetCycle-=24
               newCycles.append(offsetCycle)
            cycleList=newCycles
         #
         #  Save time by getting the records...
         #
         fstatcases=self.VU.getStatCases(parm,mod,self.obsModel,"Verifying on",
                       "Period Length",cycles=cycleList,fhrStart=0,
                       fhrEnd=240,fromDay=verifday,numDays=numdays)
         fullcases=self.VU.getCases(parm,mod,parm,self.obsModel,"Verifying on",
                       "Period Length",cycles=cycleList,fhrStart=0,
                       fhrEnd=240,fromDay=verifday,numDays=numdays)
         gridcases=fullcases[mod]
         statcases=fstatcases[mod]
         statkeys=statcases.keys()
         gridkeys=gridcases.keys()
         #
         #  If no grids and no stats for this model...skip it
         #
         if ((len(gridkeys)<1)and(len(statkeys)<1)):
            continue
         #
         #  If there are grids for this model...log that we are reading them
         #
         if len(gridkeys)>0:
            self.VU.logMsg("  Reading %3d cases for %s"%(len(gridkeys),mod))
         gridkeys.sort()
         for gridkey in gridkeys:
            if gridkey in statkeys:
               srecList=statcases[gridkey]
            else:
               srecList=None
            #
            #  Get the forecast hour of this case...and skip if not one
            #  of the ones that we are looking for
            #
            (basestr,stimestr,etimestr)=gridkey.split(",")
            base1=int(basestr)
            stime1=int(stimestr)
            etime1=int(etimestr)
            fhr=float(stime1-base1)/float(HOURSECS)
            fhr-=baseOffset
            savekey="%s,%3.3d"%(mod,fhr)
            if fhr not in fhours:
               continue
            #
            #  Get the main stat for all models - and skip anything further
            #  if it cannot be read or calculated
            #
            valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"% err below",editArea=EDITAREA,
                          forceCalc=0,statVal=THRESHOLD,grecList=gridcases[gridkey],
                          srecList=srecList)
            if valx is None:
               continue
            if savekey in totals.keys():
               totals[savekey]+=valx
               counts[savekey]+=1
            else:
               totals[savekey]=valx
               counts[savekey]=1
            #
            #  For the Official model...we read other stats
            #
            if mod=="Official":
               #
               #  Get % greater than BADTHRESHOLD
               #
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"% err below",editArea=EDITAREA,
                          forceCalc=0,statVal=BADTHRESHOLD,grecList=gridcases[gridkey],
                          srecList=srecList)
               if valx is not None:
                  valy=1.0-valx
                  if savekey in totbad.keys():
                     totbad[savekey]+=valy
                     cntbad[savekey]+=1
                  else:
                     totbad[savekey]=valy
                     cntbad[savekey]=1
               #
               #  Get bias
               #
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"bias",editArea=EDITAREA,
                          forceCalc=0,grecList=gridcases[gridkey],
                          srecList=srecList)
               if valx is not None:
                  if savekey in toterr.keys():
                     toterr[savekey]+=valx
                     cnterr[savekey]+=1
                  else:
                     toterr[savekey]=valx
                     cnterr[savekey]=1
               #
               #  Get MAE
               #
               valx=self.VU.getVerStat(mod,base1,parm,stime1,etime1,
                          self.obsModel,"MAE",editArea=EDITAREA,
                          forceCalc=0,grecList=gridcases[gridkey],
                          srecList=srecList)
               if valx is not None:
                  if savekey in totmae.keys():
                     totmae[savekey]+=valx
                     cntmae[savekey]+=1
                  else:
                     totmae[savekey]=valx
                     cntmae[savekey]=1
      #
      #  Make the final average stats over past numdays days
      #
      pastkeys=totals.keys()
      pastkeys.sort()
      for pastkey in pastkeys:
         totals[pastkey]/=float(counts[pastkey])
      for badkey in totbad.keys():
         totbad[badkey]/=float(cntbad[badkey])
      for errkey in toterr.keys():
         toterr[errkey]/=float(cnterr[errkey])
      for maekey in totmae.keys():
         totmae[maekey]/=float(cntmae[maekey])
      #
      #  Get the forecast periods
      #
      allfhrs=[]
      for pastkey in pastkeys:
         (mod,fhr)=pastkey.split(",")
         if fhr not in allfhrs:
            allfhrs.append(fhr)
      allfhrs.sort()
      #
      #
      #
      percnt=0
      for per in range(len(allfhrs)):
         percnt+=1
         fhrstr=allfhrs[per]
         modVals={}
         for pastkey in pastkeys:
            if pastkey[-3:]==fhrstr:
               (mod,fhr)=pastkey.split(",")
               modVals[mod]=totals[pastkey]
         modkeys=modVals.keys()
         modkeys.sort(lambda x,y: cmp(modVals[x],modVals[y]))
         modkeys.reverse()
         #
         #
         #
         try:
            offkey="Official,%s"%fhrstr
            pererr=toterr[offkey]
            permae=totmae[offkey]
            perbad=totbad[offkey]*100
         except:
            continue
         #
         #
         #
         peroutof=len(modkeys)
         perrank=modkeys.index("Official")+1
         perval=modVals["Official"]*100
         bestmods=modkeys
         bestmods.remove("Official")
         perbestmod=bestmods[0]
         perbestmodval=modVals[perbestmod]*100.0
         persecondmod=bestmods[1]
         persecondmodval=modVals[persecondmod]*100.0
         perworstmod=bestmods[-1]
         perworstmodval=modVals[perworstmod]*100.0
         
         perInfo=(percnt,fhrstr,pererr,permae,perbad,perval,perrank,
                  peroutof,perbestmod,perbestmodval,persecondmod,
                  persecondmodval,perworstmod,perworstmodval)
         pastInfos.append(perInfo)
      return pastInfos
   #==================================================================
   #  getMinMaxFhr - from the list of information for each period - get
   #                 the min and max forecast hour
   #
   def getMinMaxFhr(self,perInfos):
      #
      minfhr=99999
      maxfhr=-99999
      for info in perInfos:
         fhr=int(info[1])
         minfhr=min(minfhr,fhr)
         maxfhr=max(maxfhr,fhr)
      return(minfhr,maxfhr)
   #==================================================================
   #  formatOutput -
   #
   #
   def formatOutput(self,parm,verifday,stime,etime,diffInfo,perInfos,
                    pastInfos,showFcstr=1,fcstrName=""):
      #
      #  Header stuff for start of HTML
      #
      output=""
      output+="<html>\n"
      output+="<head>\n"
      output+="<title>%s Verification Summary</title>\n"%parm
      output+="<style type='text/css'>\n"
      output+="body {font-family: sans-serif; font-size: 10pt}\n"
      output+="td {font-size: smaller}\n"
      output+="th {font-size: smaller}\n"
      output+="td.verygood {background-color: #80FF80}\n"
      output+="td.good     {background-color: #CCFFCC}\n"
      output+="td.bad      {background-color: #FFCCCC}\n"
      output+="td.verybad  {background-color: #FF8080}\n"
      output+="td.veryeasy {background-color: #80FF80}\n"
      output+="td.easy     {background-color: #CCFFCC}\n"
      output+="td.hard     {background-color: #FFCCCC}\n"
      output+="td.veryhard {background-color: #FF8080}\n"
      
      output+="</style>\n"
      output+="</head>\n"
      output+="<body>\n"
      #
      #  Stuff at top with date, parameter, etc.
      #
      (vyea,vmon,vday,vhou,vmin,vsec,vwda,vyda,vdst)=time.gmtime(verifday)
      outstring="%s Verification Summary for %s, %s %d:"%(parm,DAYS[vwda],
                                                   MONS[vmon-1],vday)
      output+="<h1 style=\"margin-bottom: 0px\">%s</h1>\n"%outstring
      #
      #  Starting/Ending time
      #
      (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(stime)
      (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(etime)
      output+="<table style=\"margin-top: 2px\">\n"
      output+="<tr>\n"
      output+="<td align=right><b>Grid Time:</b></td>\n"
      output+="<td align=right style=\"padding-left: 10px\">start:</td><td>%s, %s %2d %2.2dZ</td>\n"%(DAYS[swda],MONS[smon-1],sday,shou)
      output+="<td align=right style=\"padding-left: 10px\">end:</td><td>%s, %s %2d %2.2dZ</td>\n"%(DAYS[ewda],MONS[emon-1],eday,ehou)
      output+="</tr>\n"
      output+="<tr>\n"
      output+="<td align=right><b>Edit Area:</b></td>\n"
      output+="<td colspan=4 align=left>%s&nbsp;&nbsp;&nbsp;(%d gridpoints)</td>\n"%(EDITAREA,self.numpoints)
      output+="</tr>\n"
      output+="</table>\n"
      output+="<br>\n"
      #
      #  Difficulty information
      #
      output+="<h3 style=\"margin-bottom: 0px\">Measures of Difficulty:</h3>\n"
      (stdsInfo,chgsInfo,anomInfo)=diffInfo
      #
      #  Display Difficulty Info
      #
      (std,stdRank,stdOutOf)=stdsInfo
      (absChg,chg,chgRank,chgOutOf)=chgsInfo
      (absAnom,anom,anomRank,anomOutOf)=anomInfo
      output+="<table style=\"margin-top: 2px\">\n"
      output+="<tr>\n"
      if absAnom>-900:
         output+="<td align=left>Avg Anomaly:</td>\n"
         output+="<td align=right>%+6.2f</td>\n"%anom
         output+="<td align=right style=\"padding-left: 20px\">Avg |anomaly|:</td>\n"
         output+="<td align=right>%5.2f</td>\n"%absAnom
         rankClass=self.getDifficultyColor(anomRank,anomOutOf)
         output+="<td align=center%s>Rank:%2d "%(rankClass,anomRank)
         output+="<span style=\"font-size: smaller\">out of last</span> "
         output+="%2d</td>\n"%anomOutOf
      else:
         output+="<td align=left>Avg Anomaly:</td>\n"
         output+="<td align=right>.....</td>\n"
      output+="</tr>\n"
      output+="<tr>\n"
      if absChg>-900:
         output+="<td align=left>Avg 24hr Chg:</td>\n"
         output+="<td align=right>%+6.2f</td>\n"%chg
         output+="<td align=right style=\"padding-left: 20px\">Avg |24hr Chg|:</td>\n"
         output+="<td align=right>%5.2f</td>\n"%absChg
         rankClass=self.getDifficultyColor(chgRank,chgOutOf)
         output+="<td align=center%s>Rank:%2d "%(rankClass,chgRank)
         output+="<span style=\"font-size: smaller\">out of last</span> "
         output+="%2d</td>\n"%chgOutOf
      else:
         output+="<td align=left>Avg 24hr Chg:</td>\n"
         output+="<td align=right>.....</td>\n"
      output+="</tr>\n"
      output+="<tr>\n"
      if std>-900:
         output+="<td align=left>Standard Deviation:</td>\n"
         output+="<td></td>\n"
         output+="<td></td>\n"
         output+="<td align=right>%5.2f</td>\n"%std
         rankClass=self.getDifficultyColor(stdRank,stdOutOf)
         output+="<td align=center%s>Rank:%2d "%(rankClass,stdRank)
         output+="<span style=\"font-size: smaller\">out of last</span> "
         output+="%2d</td>\n"%stdOutOf
      else:
         output+="<td align=left>Standard Deviation:</td>\n"
         output+="<td align=right>.....</td>\n"
      output+="</tr>\n"
      output+="</table>\n"
      output+="<br>\n"
      #
      #  Display Info for all verifying forecasts
      #
      output+="<table>\n"
      output+="<tr>\n"
      output+="<th colspan=2 align=center valign=bottom>Period</th>\n"
      output+="<th colspan=3 align=center valign=bottom>Forecast Made</th>\n"
      if showFcstr==1:
         output+="<th align=center valign=bottom>Made by</th>\n"
      output+="<th align=center valign=bottom>Official<br>MAE</th>\n"
      output+="<th align=center valign=bottom>Official<br>Bias</th>\n"
      output+="<th align=center valign=bottom>Official<br>Percent<br>&gt;%d&deg; Err</th>\n"%BADTHRESHOLD
      output+="<th align=center valign=bottom>Official<br>Percent<br>&lt;%d&deg; Err</th>\n"%THRESHOLD
      output+="<th align=center valign=bottom>Official<br>Rank<br>among<br>Guidance</th>\n"
      output+="<th colspan=2 align=center valign=bottom>Best Guidance</th>\n"
      output+="<th colspan=2 align=center valign=bottom>2nd Best Guidance</th>\n"
      output+="<th colspan=2 align=center valign=bottom>Worst Guidance</th>\n"
      output+="</tr>\n"
      
      for perInfo in perInfos:
         (pernum,fhrstr,basetime,fcstr,bias,mae,badpct,pcterr,rank,outof,
          bestmod,bestmodval,secmod,secmodval,lastmod,lastmodval)=perInfo
         output+="<tr>\n"
         output+="<td align=right>%2d</td>\n"%pernum
         output+="<td align=right>%3d-hr</td>\n"%int(fhrstr)
         if basetime!=0:
            (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(basetime)
            bshift="mid"
            if (bhou>=6)and(bhou<18):
               bshift="day"
            output+="<td align=center>%s</td>\n"%DAYS[bwda]
            output+="<td align=center>%2d/%2d</td>\n"%(bmon,bday)
            output+="<td align=center>%s</td>\n"%bshift
         else:
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"            
         if showFcstr==1:
            if ((fcstrName=="")or(fcstr==fcstrName)):
               output+="<td align=center>%s</td>\n"%fcstr
            else:
               output+="<td aling=center>&nbsp;</td>\n"
         if basetime!=0:
            output+="<td align=right>%5.2f</td>\n"%mae
            output+="<td align=right>%5.2f</td>\n"%bias
            output+="<td align=right>%5.1f%%</td>\n"%badpct
            rankClass=self.getRankColor(rank,outof)
            output+="<td align=right%s>%5.1f%%</td>\n"%(rankClass,pcterr)
            output+="<td align=center%s>%2d "%(rankClass,rank)
            output+="<span style=\"font-size: smaller\">out of</span> "
            output+="%2d</td>\n"%outof
         else:
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
         if bestmod!="":
            output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%bestmod
            output+="<td align=right>%5.1f%%</td>\n"%bestmodval
         else:
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
         if secmod!="":
           output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%secmod
           output+="<td align=right>%5.1f%%</td>\n"%secmodval
         else:
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
         if lastmod!="":
            output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%lastmod
            output+="<td align=right>%5.1f%%</td>\n"%lastmodval
         else:
            output+="<td>&nbsp;</td>\n"
            output+="<td>&nbsp;</td>\n"
         output+="</tr>\n"

      output+="<tr><td>&nbsp;</td></tr>\n"
      output+="<tr>\n"
      output+="<th align=left colspan=13>Average over past %d days:</th>\n"%NUMDAYS
      output+="</tr>\n"
      for pastInfo in pastInfos:
         (pernum,fhrstr,err,mae,bad,pcterr,rank,outof,bestmod,bestmodval,secmod,secmodval,
          lastmod,lastmodval)=pastInfo
         output+="<tr>\n"
         output+="<td align=right>%2d</td>\n"%pernum
         output+="<td align=right>%3d-hr</td>\n"%int(fhrstr)
         output+="<td></td>\n"
         output+="<td></td>\n"
         output+="<td></td>\n"
         if showFcstr==1:
            output+="<td></td>\n"
         output+="<td align=right>%5.2f</td>\n"%mae
         output+="<td align=right>%5.1f</td>\n"%err
         output+="<td align=right>%5.1f%%</td>\n"%bad
         rankClass=self.getRankColor(rank,outof)
         output+="<td align=right%s>%5.1f%%</td>\n"%(rankClass,pcterr)
         output+="<td align=center%s>%2d "%(rankClass,rank)
         output+="<span style=\"font-size: smaller\">out of</span> "
         output+="%2d</td>\n"%outof
         output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%bestmod
         output+="<td align=right>%5.1f%%</td>\n"%bestmodval
         output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%secmod
         output+="<td align=right>%5.1f%%</td>\n"%secmodval
         output+="<td align=right style=\"padding-left: 10px\">%s</td>\n"%lastmod
         output+="<td align=right>%5.1f%%</td>\n"%lastmodval
         output+="</tr>\n"
      output+="</table>\n"
      output+="</body>\n"
      output+="</html>\n"
      return output
   def getDifficultyColor(self,rank,outof):
      if outof==1:
         return ""
      pct=float(rank-1)/float(outof-1)
      if pct<0.10:
         rankClass=" class=\"veryhard\""
      elif pct<0.25:
         rankClass=" class=\"hard\""
      elif pct<0.75:
         rankClass=""
      elif pct<0.90:
         rankClass=" class=\"easy\""
      else:
         rankClass=" class=\"veryeasy\""
      return rankClass
   def getRankColor(self,rank,outof):
      if outof==1:
         return ""
      pct=float(rank-1)/float(outof-1)
      if pct<0.10:
         rankClass=" class=\"verygood\""
      elif pct<0.25:
         rankClass=" class=\"good\""
      elif pct<0.75:
         rankClass=""
      elif pct<0.90:
         rankClass=" class=\"bad\""
      else:
         rankClass=" class=\"verybad\""
      return rankClass
   
