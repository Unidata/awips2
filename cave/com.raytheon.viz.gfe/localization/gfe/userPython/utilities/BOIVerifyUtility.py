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
# BOIVerifyUtility - version 2.0.5
#
#   Main utility supporting maintenance of verification databases of both
#   grids and stats.
#
# Author: Tim Barker - SOO BOI
#   2005/07/20 - Original Implementation
#   2005/07/29 - version 0.1 - update to grid database structure
#                to add time that grid was stored
#   2006/11/06 - version 1.0 - First version with time-series graphs. Still
#                lots of bugs and not what I would like for a version 1.0 but
#                I've put it off way too long anyway.
#   2006/12/04 - fix bug with getStatList getting slightly wrong time periods
#   2007/02/05 - version X.X
#                   fixed hardcoded references to "Obs" in getObsPeriod and
#                      getObsList
#   2007/05/01 - version 1.1 - emergency fix to getPairList for speed
#                and memory usage when large number of grids are stored
#   2007/10/23 - version 2.0 - gobs of new stuff for handling common cases
#                probability parms, rate parms, new scores, etc.
#   2007/11/30 - version 2.0.1 - fix accumulations over periods more than
#                twice as long as models grids (grids totally inside the
#                time period were getting counted multiple times)
#   2007/12/17 - version 2.0.2 - fix list of verifying accumulative
#                periods when specifying list of verifying dates (it was
#                omitting the last period that fit within the time period).
#   2008/05/28 - version 2.0.5 - Added support for new statistics:
#                frequency observed and frequency forecast.
#
#
#   2010/04/23  ryu  Initial port to AWIPS II.
#
#=====================================================================

import TimeRange, AbsTime

from numpy import *
from types import *
import os,string,re,sys
import time,calendar,math,types
try:
   from Scientific.IO import NetCDF
except:
   import NetCDF

import SmartScript
#
#  Import the Configuration Information
#
import BOIVerifyConfig
#try:
#   from BOIVerifyConfig import *
#except:
#   print "Could not import BOIVerifyConfig"
#print "VERDIR=",VERDIR
#print "EDITAREAS=",EDITAREAS
#
#  Contants that should not be changed by the users!
#
VERSION="2.0.2"
#
#  STATAREAS = max number of areas to keep stats for
#  STATTYPES = max number of stats to keep
#  Changing these is NOT recommended - it changes the size of the
#  dimensions in the stat database.  If you already have an existing
#  stat database, all old information will be lost.  Eventually we'll
#  have a program that will convert a stat database for more areas or
#  more stats
#
STATAREAS=150 # max number of areas to keep stats for
STATTYPES=15 # max number of stats to keep
#
#  Do not change - will corrupt the database.  Will eventually have
#  a program that allows the number of forecaster numbers saved for
#  a grid or stat to be changed.
#
MAXFCSTRS=5
#
#  Number of thresholds saved for each parm
#
NUMTHRESH=5
#
#  Forecaster number file
#
FCSTRNUMFILE="FcstrNums.dat" # maintained by software - do not change
HOURSECS=60*60
DAYSECS=24*HOURSECS
SkipParms=[]        # parms that we do not verify yet

class BOIVerifyUtility(SmartScript.SmartScript):
    def __init__(self, dbss, eaMgr, mdMode=None, toolType="numeric"):
        SmartScript.SmartScript.__init__(self, dbss)

        gridLoc = self.getGridLoc()
        gridSize = (gridLoc.getNx().intValue(), gridLoc.getNy().intValue())
        self._empty = zeros((gridSize[1], gridSize[0]), float)
        #
        #  Read in Config info.
        #     Each entry in BOIVerifyConfig is put into a self. variable
        #         for fast access within this utility
        #     And put into a self.CFG dictionary for easy access outside
        #         this utility (through the getCFG method)
        #     (only does variables in BOIVerifyConfig that do NOT start
        #      with an underscore _ )
        #
        names=dir(BOIVerifyConfig)
        
        self.CFG={}
        #names=["VERDIR","EDITAREAS","PERCENT_COLOR","GRIDDAYS","STATDAYS",
        #       "OBSMODELS"]
        for name in names:
           if name[0:1]!="_":
              execstr="self.CFG['"+name+"']=BOIVerifyConfig."+name
              #execstr="self.CFG['"+name+"']="+name
              exec execstr
              execstr="self."+name+"=BOIVerifyConfig."+name
              #execstr="self."+name+"="+name
              exec execstr
        #
        #  Setup the REALLY constant variables
        #
        self.STATAREAS=STATAREAS
        self.CFG['STATAREAS']=STATAREAS
        self.STATTYPES=STATTYPES
        self.CFG['STATTYPES']=STATTYPES
        self.MAXFCSTRS=MAXFCSTRS
        self.CFG['MAXFCSTRS']=MAXFCSTRS
        self.NUMTHRESH=NUMTHRESH
        self.CFG['NUMTHRESH']=NUMTHRESH
        self.FCSTRNUMFILE=FCSTRNUMFILE
        self.CFG['FCSTRNUMFILE']=FCSTRNUMFILE
        #
        #  Setup DEBUG level
        #
        self.DEBUG=0
        #
        #  Setup Observed file info
        #
        self.oncParm=""
        self.oncModel=""
        self.oncModify=0
        self.onumRecs=0
        #
        #  Setup forecast grid file info
        #
        self.fncParm=""
        self.fncModel=""
        self.fncModify=0
        self.fnumRecs=0
        #
        #  Setup forecast stat file info
        #
        self.sncParm=""
        self.sncModel=""
        self.sncObsModel=""
        self.sncModify=0
        self.sncNumRecs=0
        #
        #  stup forecaster numbers, edit areas, and check config
        #
        self.setupFcstrNums()
        self.setupEditAreas()
        self.checkConfig()
        #
        #  setup all the stat names.  statIDs are the 'short' and
	#  'correct' name to use for a stat.  Various optional
	#  names, shortened spellings, etc. are stored in statNames
	#  dictionary for each ID.  allStats contains all the possible
	#  names for all stats in the system.
        #
        self.statNames={"bias":["bias","error","err"],
                        "mae" :["mae","absolute error","abs error","abs err",
                                "mean abs error","mean abs err",
                                "mean absolute error","mean absolute err"],
                        "rms" :["rms","root mean squared error",
                                "root mean squared err","rms error","rms err"],
                        "mse" :["mse","mean squared error","mean squared err",
                                "brier score","brier"],
                        "peb" :["peb","percent error below","percent err below",
                                "% error below","% err below","percent error <",
                                "percent err <","% error <","% err <"],
                        "fc"  :["fc","fraction correct"],
                        "afc" :["afc","areal fraction correct"],
                        "freqo":["freqo","frequency observed"],
                        "freqf":["freqf","frequency forecast"],
                        "freqbias":["freqbias","frequency bias"],
                        "afreqbias":["afreqbias","areal frequency bias"],
                        "pod" :["pod","probability of detection","prob detection"],
                        "apod":["apod","areal probability of detection",
                                "areal prob detection","areal pod"],
                        "far" :["far","false alarm ratio"],
                        "afar":["afar","areal false alarm ratio"],
                        "pofd":["pofd","probability of false detection",
                                "prob of false detection"],
                        "apofd":["apofd","areal probability of false detection",
                                 "areal prob of false detection"],
                        "ts"  :["ts","threat score","csi",
                                "critical success index"],
                        "ats" :["ats","areal threat score","acsi",
                                "areal critical success index"],
                        "ets" :["ets","equitable threat score","gilbert"],
                        "aets":["aets","areal equitable threat score",
                                "agilbert","areal gilbert"],
                        "hk"  :["hk","hanssen and kuipers discriminant",
                                "peirce","peirces skill score",
                                "tss","true skill score"],
                        "ahk" :["ahk","areal hanssen and kuipers discriminant",
                                "apeirce","areal peirces skill score",
                                "atss","areal true skill score"],
                        "hss" :["hss","heidke skill score"],
                        "ahss":["ahss","areal heidke skill score"],
                        "oddsratio":["oddsratio","odds ratio"],
                        "aoddsratio":["aoddsratio","areal odds ratio"],
                        "hits":["hits",],
                        "ahits":["ahits","areal hits"],
                        "miss":["miss","misses"],
                        "amiss":["amiss","areal misses"],
                        "fals":["fals","false alarms"],
                        "afals":["afals","areal false alarms"],
                        "corn":["corn","correct negatives"],
                        "acorn":["acorn","areal correct negatives"],
                        "cont":["cont","contingency table"],
                        "acont":["acont","areal contingency table"],
                       }
        self.statIDs=self.statNames.keys()
        self.allStats=[]
        for statName in self.statIDs:
           names=self.statNames[statName]
           for name in names:
              self.allStats.append(name)
	return

    #=================================================================
    #  checkConfig - cross check configuration data, and make log
    #                messages about problems.
    #          
    #                Return 0 if config is OK, return 1 if there
    #                are any problems
    #
    def checkConfig(self):
       badConfig=0
       mutid=self.mutableID()
       #
       #  Make sure parms are well defined
       #
       parmNames=self.VERCONFIG.keys()
       for parmName in parmNames:
          parmInfo=self.getParm(mutid,parmName,"SFC")
          if parmInfo is None:
             self.logMsg("Could not check VER config for %s"%parmName)
             continue
          config=self.VERCONFIG[parmName]
          #
          #  check for horribly bad config lines
          #
          if type(config)!=TupleType:
             self.logMsg("BOIVerify VERCONFIG of %s is not a tuple - it should be"%parm)
             badConfig=1
             continue
          if len(config)!=8:
             self.logMsg("BOIVerify VERCONFIG of %s does not have 8 elements"%parmName)
             badConfig=1
             continue
          #
          #  check parm type
          #
          if type(config[0])!=IntType:
             self.logMsg("BOIVerify VERCONFIG of %s has bad type:%s"%(parm,config[0]))
             badConfig=1
          else:
             parmType=config[0]
             if ((parmType<0)or(parmType>1)):
                self.logMsg("BOIVerify VERCONFIG of %s has bad type:%d"%(parm,parmType))
                badConfig=1
          #
          #  check ver type
          #
          if type(config[1])!=IntType:
             self.logMsg("BOIVerify VERCONFIG of %s has bad verification type:%s"%(parm,config[1]))
             badConfig=1
          else:
             verType=config[1]
             if ((verType<0)or(verType>1)):
                self.logMsg("BOIVerify VERCONFIG of %s has bad verification type:%d"%(parm,verType))
                badConfig=1
          #
          #  check parm save interval
          #
          if type(config[2])!=IntType:
             self.logMsg("BOIVerify VERCONFIG of %s has bad save interval:%s"%(parm,config[2]))
             badConfig=1
          else:
             saveInt=config[2]
             if ((saveInt<0)or(saveInt>24)):
                self.logMsg("BOIVerify VERCONFIG of %s has bad save interval:%d"%(parm,saveInt))
                badConfig=1
          #
          #  Checks are different for Scalar/Probability vs. Vectors
          #
          ##if parmInfo.dataType()!=1:
          wxType = parmInfo.getGridInfo().getGridType().ordinal() - 1
          if wxType!=1:
             #
             #  check for NUMTHRESH thresholds
             #
             if type(config[3])!=TupleType:
                self.logMsg("BOIVerify VERCONFIG of %s has bad thresholds:%s"%(parm,config[3]))
                badConfig=1
             else:
                thresholds=config[3]
                numthresh=len(thresholds)
                if (numthresh!=self.NUMTHRESH):
                   self.logMsg("BOIVerify VERCONFIG of %s does not have %d thresholds: has %d instead"%(parm,self.NUMTHRESH,numthresh))
                   badConfig=1                  
             #
             #  check for binwidth
             #
       return badConfig
    #=================================================================
    #  CONVENIENCE FUNCTION FOR GETTING VER CONFIGURATION
    #
    def getCFG(self,name):
       if name in self.CFG:
          return self.CFG[name]
       else:
          return None
    #=================================================================
    #  CONVENIENCE FUNCTIONS FOR GETTING VER PARM CONFIGURATION
    #
    #  getVerParms - get list of configured verification parameters
    #
    def getVerParms(self):
       VerParms=self.VERCONFIG.keys()
       VerParms.sort()
       return VerParms
    #=================================================================
    #  getVerParmsVect - gets list of configured verification
    #                    parameters, with any vector parms also
    #                    listed with "Spd" appended, and "Dir"
    #                    appended
    #
    def getVerParmsVect(self):
       parmList=self.getVerParms()
       VerParms=[]
       for parm in parmList:
          if self.getVerParmType(parm)==1:
             VerParms.append(parm)
             VerParms.append(parm+"Spd")
             VerParms.append(parm+"Dir")
          else:
             VerParms.append(parm)
       VerParms.sort()
       return VerParms
    #=================================================================
    #  getVerParmsVal - gets list of configured verification parms
    #                   that are scalars (removes vectors from list)
    #
    def getVerParmsVal(self):
       parmList=self.getVerParms()
       VerParms=[]
       for parm in parmList:
          if self.getVerType(parm)==0:
             VerParms.append(parm)
       VerParms.sort()
       return VerParms
    #=================================================================
    #  getVerParmsObs - gets list of configured verification parms,
    #                   and adds in any observed parm namesget list of
    #                   observed parms
    #
    def getVerParmsObs(self):
       VerParms=[]
       fparms=self.VERCONFIG.keys()
       fparms.sort()
       for key in fparms:
          if self.VERCONFIG[key][1]==1:
             (obsparm,ttype,tval)=self.VERCONFIG[key][7]
          else:
             obsparm=self.VERCONFIG[key][7]
          if obsparm not in VerParms:
             VerParms.append(obsparm)
       VerParms.sort()
       return VerParms
    #=================================================================
    #  getObsParm - for a particular parm, get its corresponding
    #               obsparm name.
    #
    def getObsParm(self,fcstParm):
       obsparm=""
       if fcstParm in self.VERCONFIG.keys():
          if self.VERCONFIG[fcstParm][1]==1:
             (obsparm,ttype,tval)=self.VERCONFIG[fcstParm][7]
          else:
             obsparm=self.VERCONFIG[fcstParm][7]
       return obsparm
    #=================================================================
    #  getObsCondition - for a probability parm, get the condition
    #                    ">", ">=", "<", "<=" etc. for the observed
    #                    parm.  For example, in the US, PoP is verified
    #                    with QPE >= 0.01, so the condition is ">=" in
    #                    this case. If the parm is not a probability parm
    #                    it just returns an empty string.
    #
    def getObsCondition(self,fcstParm):
       obsCondition=""
       if fcstParm in self.VERCONFIG.keys():
          if self.VERCONFIG[fcstParm][1]==1:
             (obsparm,obsCondition,obsThreshold)=self.VERCONFIG[fcstParm][7]
       return obsCondition
    #=================================================================
    #  getObsThreshold - for a probability parm, get the threshold for
    #                    the observed parm.  For example, in the US,
    #                    PoP is verified with QPE >= 0.01.  The threshold
    #                    is 0.01 in this case. If the specified parm is
    #                    not a probability parm, returns zero for the
    #                    threshold.
    #
    def getObsThreshold(self,fcstParm):
       obsThreshold=0
       if fcstParm in self.VERCONFIG.keys():
          if self.VERCONFIG[fcstParm][1]==1:
             (obsparm,obsCondition,obsThreshold)=self.VERCONFIG[fcstParm][7]
       return obsThreshold
    #=================================================================
    #  getVerConfig - get the specified element of the config stuff
    #                 for a parm, or return None if that parm is not
    #                 configured
    #
    def getVerConfig(self,parmName,element):
       if parmName not in self.VERCONFIG.keys():
          return None
       config=self.VERCONFIG[parmName]
       return config[element]
    #=================================================================
    #  getVerParmType - get the type of parm, either 0 for SCALAR or
    #                   1 for VECTOR.  If the parm is not configured it
    #                   also checks to see of the observed parm of any
    #                   configured parm matches - and returns whether
    #                   that parm is a SCALAR or VECTOR
    #
    def getVerParmType(self,parmName):
       parmType=self.getVerConfig(parmName,0)
       if parmType is None:
          parms=self.getVerParms()
          for parm in parms:
             oparm=self.getObsParm(parm)
             if oparm==parmName:
                parmType=self.getVerConfig(parm,0)
                break
       return parmType
    #=================================================================
    #  getVerType - get the type of verification needed for this parm.
    #               Normally 0, meaning just typical value verification.
    #               A value of 1 means 'floating probability' type
    #               verification. A value of 2 means 'probability' type
    #               verification.
    #
    def getVerType(self,parmName):
       return self.getVerConfig(parmName,1)
    #=================================================================
    #  getVerSaveInterval - used for the saving of parms. If set to 0
    #                       then save all grids for this parm. If set
    #                       to 3, then save 'snapshots' every 3 hours
    #                       through the day. If set to 6, then save
    #                       'snapshots' every 6 hours through the day,
    #                       etc.
    #
    def getVerSaveInterval(self,parmName):
       return self.getVerConfig(parmName,2)
    #=================================================================
    #  getVerThresholds - used in BOIVerify autocalc. calculates some
    #                     stats for absolute value of errors less than
    #                     these thresholds. For SCALAR parms, should
    #                     be a tuple with 5 threshold values. For
    #                     VECTOR parms, should be two tuples, each
    #                     with 5 values.
    #
    def getVerThresholds(self,parmName):
       return self.getVerConfig(parmName,3)
    #=================================================================
    #  getVerBinWidth - used in BOIVerify histogram displays. Used
    #                   for the width of bins in the histogram.  For
    #                   a SCALAR should be just one value. For a VECTOR
    #                   will be a tuple with the magnitude binWidth and
    #                   the direction binWidth
    #
    def getVerBinWidth(self,parmName):
       return self.getVerConfig(parmName,4)
    #=================================================================
    #  getVerBigErr - used in BOIVerify error displays. Errors bigger
    #                 than this are sometimes not displayed. SCALARS
    #                 will have one value. VECTORS will have a tuple
    #                 with magnitude bigErr and direction bigErr.
    #
    def getVerBigErr(self,parmName):
       return self.getVerConfig(parmName,5)
    #=================================================================
    #  getVerErrColor - used in BOIVerify error displays. Color table
    #                   name used for errors.  This is in here so that
    #                   dewpoint errors can have a different color
    #                   table than, say, temperature errors. For a
    #                   VECTOR parm, this returns a tuple of two color
    #                   table names
    #
    def getVerErrColor(self,parmName):
       return self.getVerConfig(parmName,6)
    #=================================================================
    #  getVerObsInfo - used in BOIVerify for probability parms. For
    #                  normal value parms, this is just the name of
    #                  the parm that verifies it (so that QPF can be
    #                  verified by QPE, if needed).  For probability
    #                  parms this is a 3-value tuple with name,
    #                  condition, threshold)
    #
    def getVerObsInfo(self,parmName):
       return self.getVerConfig(parmName,7)
    #=================================================================
    #  getBaseOffset - looks through baseOffset in configuration to
    #                  see if model is listed.  If so, returns the
    #                  offset (in hours).  If not, returns zero.
    #
    def getBaseOffset(self,model):
       if model in self.BASE_OFFSET.keys():
          offset=self.BASE_OFFSET[model]
       else:
          offset=0
       return offset
    #=================================================================
    #  setupFcstrNums - sets up the Fcstrs list with the strings of
    #                   forecaster names for each number by reading
    #                   the FCSTRNUMFILE file from disk.
    #                   FcstrNames is a dictionary with names for
    #                      each 2-digit number string
    #                   FcstrIDs is a dictionary with 8-character IDs
    #                      for each 2-digit number string
    #
    def setupFcstrNums(self):
       self.FcstrNames={}
       self.FcstrIDs={}
       self.FcstrNames["00"]="Unknown"
       self.FcstrIDs["00"]="Unknown"
       filename="%s/%s"%(self.VERDIR,self.FCSTRNUMFILE)
       try:
          fs=file(filename,"r")
          lines=fs.readlines()
          fs.close()
       except:
          return 0
       for line in lines:
          nocomment=re.sub("#.*","",line)
          pieces=nocomment.split(",")
          if len(pieces)>1:
             numstr=pieces[0].strip()
             try:
                num=int(numstr)
             except:
                self.logMsg("Bad forecaster number ignored:")
                self.logMsg("   %s"%nocomment)
                continue
             idstr=pieces[1].strip()
             if len(pieces)>2:
                namstr=pieces[2].strip()
             else:
                namstr=idstr
             #
             #  forecaster number 0 is always forced to be Unknown
             #
             if num==0:
                namstr="Unknown"
                idstr="Unknown"
             #
             numstr="%2.2d"%num
             self.FcstrNames[numstr]=namstr
             self.FcstrIDs[numstr]=idstr
       #
       #  If debug is set...pwrint a list of forecaster number:id,name
       #
       if self.DEBUG>0:
          self.logMsg("setupFcstrNums reading verification forecasters:")
          numericList=[]
          for (num,name) in self.FcstrNames.items():
             id=self.FcstrIDs[num]
             numericList.append("%s:%s,%s"%(num,id,name))
          numericList.sort()
          for entry in numericList:
             self.logMsg("  %s"%entry)
       return 1
    #=================================================================
    #  getFcstrNames - return dictionary of forecaster names for each
    #                  number (number is a two-digit string).
    #
    def getFcstrNames(self):
       return self.FcstrNames
    #=================================================================
    #  getFcstrName - given a number (integer), find the full forecaster
    #                 name.  Returns empty string if number is not used
    #
    def getFcstrName(self,num):
       name=""
       numstr="%2.2d"%num
       if numstr in self.FcstrNames.keys():
          name=self.FcstrNames[numstr]
       return name
    #=================================================================
    #  getFcstrIDs - return dictionary of forecast IDs for each number
    #
    def getFcstrIDs(self):
       return self.FcstrIDs
    #=================================================================
    #  getFcstr ID - given a number (integer), find the forecaster ID.
    #                Returns empty string if number is not used
    #
    def getFcstrID(self,num):
       ID=""
       numstr="%2.2d"%num
       if numstr in self.FcstrIDs.keys():
          ID=self.FcstrIDs[numstr]
       return ID
    #=================================================================
    #  getFcstrNums - return list of forecaster numbers (as strings)
    #
    def getFcstrNums(self):
       nums=self.FcstrNames.keys()
       nums.sort()
       return nums
    #=================================================================
    #  setFcstrs - take a name dictionary, and an id dictionary, and
    #              set the self.FcstrNames and self.FcstrIDs
    #              dictionaries in the Utility.  Used in BOIVerifyInfo
    #              when these dictionaries are being updated.
    #
    def setFcstrs(self,nameDict,idDict):
       self.FcstrNames={}
       for (key,value) in nameDict.items():
          self.FcstrNames[key]=value
       self.FcstrIDs={}
       for (key,value) in idDict.items():
          self.FcstrIDs[key]=value
    #=================================================================
    #  findFcstrNumFromID - takes a forecaster ID string - and returns
    #                       the associated integer forecast number.
    #                       If the ID is not found - returns 0.
    #
    def findFcstrNumFromID(self,id):
       num=0		
       if id in self.FcstrIDs.values():
          for (testnum,testid) in self.FcstrIDs.items():
             if testid==id:
                num=int(testnum)
                break
       return num
    #==================================================================
    #  findFcstrNumFromName - takes a forecaster name string - and returns
    #                         the associated integer forecast number.
    #                         if the ID is not found - returns 0.
    #
    def findFcstrNumFromName(self,name):
       num=0		
       if id in self.FcstrNames.values():
          for (testnum,testname) in self.FcstrNames.items():
             if testname==name:
                num=int(testnum)
                break
       return num
    #==================================================================
    #  saveFcstrNum - write the FcstrNames and FcstrIDs info into the
    #                 FCSTRNUMSFILE on disk.  This called by BOIVerifyInfo
    #                 as these are being updated and should not be
    #                 called elsewhere.
    #
    def saveFcstrNums(self):
       filename="%s/%s"%(self.VERDIR,self.FCSTRNUMFILE)
       try:
          fs=file(filename,"w")
          fs.write("#\n")
          fs.write("# This file maintained by the verification programs\n")
          fs.write("# Please DO NOT EDIT\n")
          fs.write("#\n")
          numkeys=self.FcstrNames.keys()
          numkeys.sort()
          for numstr in numkeys:
             name=self.FcstrNames[numstr]
             id=self.FcstrIDs[numstr]
             fs.write("%s,%s,%s\n"%(numstr,id,name))
          fs.close()
          try:
             os.chmod(filename,0666)
          except:
             self.logMsg("%s should have 666 permissions"%self.FCSTRNUMFILE)
             return 0
       except:
          self.logMsg("Error writing to %s"%self.FCSTRNUMFILE)
          return 1
       return 0
    #=================================================================
    #  setupEditAreas - read the EDITAREAS file and sets up internal
    #                   EditAreas list with the names of editareas.
    #                   They names are in the appropriate 'slot' in
    #                   the list via number (i.e. if the file does not
    #                   specify an edit area #35, then slot 35 in the
    #                   list is kept blank.  EditAreaDescriptions keeps
    #                   the long descriptive names in a similar way
    #
    def setupEditAreas(self):
       self.EditAreas=[]
       self.EditAreaDescriptions=[]
       filename="%s/%s"%(self.VERDIR,self.EDITAREAS)
       try:
          fs=file(filename,"r")
          lines=fs.readlines()
          fs.close()
       except:
          return 0
       for line in lines:
          nocomment=re.sub("#.*","",line)
          pieces=nocomment.split(",")
          if len(pieces)>2:
             numstr=pieces[0].strip()
             namstr=pieces[1].strip()
             desstr=pieces[2].strip()
             try:
                num=int(numstr)
             except:
                continue
             if num>self.STATAREAS:
                continue
             if num<0:
                continue
             if num>=len(self.EditAreas):
                for j in range(len(self.EditAreas),num+1):
                   self.EditAreas.append("")
                   self.EditAreaDescriptions.append("")
             self.EditAreas[num]=namstr
             self.EditAreaDescriptions[num]=desstr
       if self.DEBUG>0:
          self.logMsg("Setting up verification edit areas:")
          for i in range(len(self.EditAreas)):
             if self.EditAreas[i]!="":
                self.logMsg("  %d:%s"%(i,self.EditAreas[i]))
       return 1
    #=================================================================
    #  listEditAreas - get a list with just the names of edit areas
    #                  (in other words, just like EditAreas, but
    #                  without the 'blank' entries)
    #
    def listEditAreas(self):
       usedEditAreas=[]
       for area in self.EditAreas:
          if area!="":
             usedEditAreas.append(area)
       return usedEditAreas
    #=================================================================
    #  listEditAreaDescriptions - get a list with just the descriptions
    #                             of all the editAreas, but without
    #                             the 'blank' entries that are in
    #                             EditAreaDescriptions.
    #
    def listEditAreaDescriptions(self):
       usedEditAreaDescriptions=[]
       for area in self.EditAreaDescriptions:
          if area!="":
             usedEditAreaDescriptions.append(area)
       return usedEditAreaDescriptions
    #=================================================================
    #  getEditAreaNumberFromName - given a name, return the number of
    #        that edit area in the MAXEDITAREAS list.  If the name
    #        does not exist, return 0
    #
    def getEditAreaNumberFromName(self,name):
       if name in self.EditAreas:
          j=self.EditAreas.index(name)
       else:
          j=0
       return j
    #=================================================================
    #  listModels - List models in the BOIVerify system by looking
    #               through Grids directories looking for different
    #               names.  Does not include any models that
    #               are in the OBSMODELS list
    #
    def listModels(self):
       Models=[]
       pat=re.compile("(\S+)_\S+_index\.nc")
       parmdirs=os.listdir("%s/Grids"%self.VERDIR)
       for parmdir in parmdirs:
          if parmdir[0]==".":
             continue
          dirname="%s/Grids/%s"%(self.VERDIR,parmdir)
          if os.path.isdir(dirname):
             files=os.listdir(dirname)
             for file in files:
                if file[0]==".":
                   continue
                matchObject=pat.search(file)
                if matchObject is None:
                   continue
                model=matchObject.group(1)
                if ((model not in Models)and(model not in self.OBSMODELS)):
                   Models.append(model)
       Models.sort()
       return Models
    #=================================================================
    #  listParms - DEPRECATED - gets a list of parms in the system, not
    #              by reading the configuration (which is the right way)
    #              but by looping through all the directories looking for
    #              names of files.
    #
    def listParms(self):
       Parms=[]
       files=os.listdir("%s/Grids"%self.VERDIR)
       for file in files:
          if file[0]==".":
             continue
          fname="%s/Grids/%s"%(self.VERDIR,file)
          if os.path.isdir(fname):
             parm=file
             if ((parm not in Parms)and(parm not in SkipParms)):
                Parms.append(parm)
       Parms.sort()
       return Parms
    #=================================================================
    #  listModelParms - given a model, get the parms that have been
    #                   archived - by looking for data files
    #
    def listModelParms(self,model):
       Parms=[]
       files=os.listdir("%s/Grids"%self.VERDIR)
       for file in files:
          if file[0]==".":
             continue
          fname="%s/Grids/%s"%(self.VERDIR,file)
          if os.path.isdir(fname):
             indexExists=os.path.exists("%s/Grids/%s/%s_%s_index.nc"%(self.VERDIR,file,model,file))
             dataExists=os.path.exists("%s/Grids/%s/%s_%s_index.nc"%(self.VERDIR,file,model,file))
             if (indexExists and dataExists and (file not in Parms) and (file not in SkipParms)):
                Parms.append(file)
       Parms.sort()
       return Parms
    #=================================================================
    #  listStatParms - DEPRECATED - gets a list of stat parms in the
    #                  system not by reading through the configuration
    #                  but by looping through all the directories
    #                  looking for names of files.
    #
    def listStatParms(self):
       Parms=[]
       files=os.listdir("%s/Stats"%self.VERDIR)
       for file in files:
          if file[0]==".":
             continue
          fname="%s/Stats/%s"%(self.VERDIR,file)
          if os.path.isdir(fname):
             parm=file
             if ((parm not in Parms)and(parm not in SkipParms)):
                Parms.append(parm)
       Parms.sort()
       return Parms
    #=================================================================
    #  closeObsFile - if an Obs file is open, close it and free up
    #                 all the structures associated with it.
    #
    def closeObsFile(self):
       if (not(self.oncParm=="")):
          self.oncIndex.close()
          self.oncData.close()
          del self.oncIndex
          del self.oncData
          del self.oncFcstr
          del self.oncBtime
          del self.oncStime
          del self.oncEtime
          del self.oncVtime
          del self.oncScale
          del self.oncAddit
          del self.oncValue
          del self.oncRecs
          try:
             del self.oncScale1
             del self.oncAddit1
             del self.oncValue1
          except:
             pass
       self.oncParm=""
       self.oncModel=""
       self.oncModify=0
       self.oncType=0
       return
    #=================================================================
    # closeFcstFile - if a Fcst file is open, close it and free up
    #                 all the structures associated with it.
    #
    def closeFcstFile(self):
       if (not(self.fncParm=="")):
          self.fncIndex.close()
          self.fncData.close()
          del self.fncIndex
          del self.fncData
          del self.fncFcstr
          del self.fncBtime
          del self.fncStime
          del self.fncEtime
          del self.fncVtime
          del self.fncScale
          del self.fncAddit
          del self.fncValue
          del self.fncRecs
          try:
             del self.fncScale1
             del self.fncAddit1
             del self.fncValue1
          except:
             pass
       self.fncParm=""
       self.fncModel=""
       self.fncModify=0
       self.fncType=0
       return
    #=================================================================
    #  makeGridDir - make a directory for grids for the specified
    #                parm.
    #
    def makeGridDir(self,parm,modify):
       newDir="%s/Grids/%s"%(self.VERDIR,parm)
       already=os.path.exists(newDir)
       if ((not already)and(modify!=0)):
          os.umask(0002)
          os.makedirs(newDir)
       return
    #=================================================================
    #  makeStatDir - make a directory for stats for the specified
    #                parm.
    #
    def makeStatDir(self,parm,modify):
       newDir="%s/Stats/%s"%(self.VERDIR,parm)
       already=os.path.exists(newDir)
       if ((not already)and(modify!=0)):
          os.umask(0002)
          os.makedirs(newDir)
       return
    #=================================================================
    #  checkFile - given a parm and model, see if it is open, and if
    #              not - open it.  Takes care of figuring out if it
    #              is an 'observation' model or not.  If modify is
    #              1, then it opens it for writing - which locks it
    #              from writing by others.
    #
    #              returns 0 if there is trouble opening file
    #
    def checkFile(self,parm,model,modify=0,datatype=-1):
       if model in self.OBSMODELS:
          retVal=self.checkObsFile(parm,model,modify=modify,datatype=datatype)
          return retVal
       else:
          retVal=self.checkFcstFile(parm,model,modify=modify,datatype=datatype)
          return retVal
    #=================================================================
    #  checkObsFile - given an parm and obsmodel, see if it is open,
    #                 and if not - open it.  If modify is 1, then it
    #                 opens the file for writing - which locks it
    #                 from writing by others.
    #
    #                 returns 0 if there is trouble opening file
    #
    def checkObsFile(self,parm,model,modify=0,datatype=-1):
       #
       #  If everything is the same...return right away
       #
       if ((parm==self.oncParm)and(model==self.oncModel)and(modify==self.oncModify)):
          return 1
       #
       #  If a file is currently open - close it
       #
       if (not(self.oncParm=="")):
          self.closeObsFile()
       #
       #  Setup the file names and see if they exist
       #
       self.makeGridDir(parm,modify)
       newIndex="%s/Grids/%s/%s_%s_index.nc"%(self.VERDIR,parm,model,parm)
       newData="%s/Grids/%s/%s_%s_data.nc"%(self.VERDIR,parm,model,parm)
       already=os.path.exists(newIndex)
       #
       #  Can't read data from file that does not exist
       #
       if ((not already) and (modify==0)):
          return 0
       #
       #  Figure out read-mode for file
       #
       if modify==0:
          mode="r"
       else:
          mode="a"
       #
       #  Figure data type and number of points in grid
       #
       if datatype<0:
          datatype=self.getVerParmType(parm)
          if datatype is None:
             return 0       
       (ypts,xpts)=self._empty.shape
       #
       #  Open the two obs files: the index and the data
       #
       self.oncIndex=NetCDF.NetCDFFile(newIndex,mode)
       self.oncData=NetCDF.NetCDFFile(newData,mode)
       #
       #  If a new file...create the variables
       #
       if not already:
          self.oncData.createDimension("ypts",ypts)
          self.oncData.createDimension("xpts",xpts)
          self.oncData.createDimension("record",None)
          self.oncIndex.createDimension("record",None)
          self.oncIndex.createDimension("maxfcstrs",self.MAXFCSTRS)
          self.oncFcstr=self.oncIndex.createVariable('fcstr','b',('record','maxfcstrs'))
          self.oncBtime=self.oncIndex.createVariable('btime','i',('record',))
          self.oncStime=self.oncIndex.createVariable('stime','i',('record',))
          self.oncEtime=self.oncIndex.createVariable('etime','i',('record',))
          self.oncVtime=self.oncIndex.createVariable('vtime','i',('record',))
          self.oncScale=self.oncIndex.createVariable('scale','d',('record',))
          self.oncAddit=self.oncIndex.createVariable('addit','d',('record',))
          self.oncValue=self.oncData.createVariable('value','h',('record','ypts','xpts'))
          if datatype==1:
             self.oncScale1=self.oncIndex.createVariable('scale1','d',('record',))
             self.oncAddit1=self.oncIndex.createVariable('addit1','d',('record',))
             self.oncValue1=self.oncData.createVariable('value1','h',('record','ypts','xpts'))
          self.oncIndex.sync()
          self.oncData.sync()
          os.chmod(newIndex,0775)
          os.chmod(newData,0775)
       #
       #  If an old file...hook up variables to the netCDF files
       #
       else:
          ivarnames=self.oncIndex.variables.keys()
          dvarnames=self.oncData.variables.keys()
          for name in ('fcstr','btime','stime','etime','vtime','scale','addit'):
             if name not in ivarnames:
                self.logMsg("Corrupt index file for %s %s detected"%(model,parm))
                return 0
          if 'value' not in dvarnames:
             self.logMsg("Corrupt data file for %s %s detected"%(model,parm))
             return 0
          self.oncFcstr=self.oncIndex.variables['fcstr']
          if len(self.oncFcstr.shape)!=2:
             self.logMsg("Old index file (pre version 1.0) detected for %s %s"%(model,parm))
             return 0
          self.oncBtime=self.oncIndex.variables['btime']
          self.oncStime=self.oncIndex.variables['stime']
          self.oncEtime=self.oncIndex.variables['etime']
          self.oncVtime=self.oncIndex.variables['vtime']
          self.oncScale=self.oncIndex.variables['scale']
          self.oncAddit=self.oncIndex.variables['addit']
          self.oncValue=self.oncData.variables['value']
          if datatype==1:
             if (('scale1' not in ivarnames)or('addit1' not in ivarnames)):
                self.logMsg("Corrupt index file for %s %s detected"%(model,parm))
                return 0
             if 'value1' not in dvarnames:
                self.logMsg("Corrupt data file for %s %s detected"%(model,parm))
                return 0
             self.oncScale1=self.oncIndex.variables['scale1']
             self.oncAddit1=self.oncIndex.variables['addit1']
             self.oncValue1=self.oncData.variables['value1']
       self.oncParm=parm
       self.oncModel=model
       self.oncModify=modify
       self.onumRecs=self.oncStime.shape[0]
       self.oncRecs=indices((self.onumRecs,))[0]
       self.oncType=datatype
       return 1
    #=================================================================
    #  checkFcstFile - given an parm and obsmodel, see if it is open,
    #                  and if not - open it.  If modify is 1, then it
    #                  opens the file for writing - which locks it
    #                  from writing by others.
    #
    #                  returns 0 if there is trouble opening file
    #
    def checkFcstFile(self,parm,model,modify=0,datatype=-1):
       #
       #  If everything is the same...return right away
       #
       if ((parm==self.fncParm)and(model==self.fncModel)and(modify==self.fncModify)):
          return 1
       #
       #  If a file is currently open - close it
       #
       if (not(self.fncParm=="")):
          self.closeFcstFile()
       #
       #  Setup the file names and see if they exist
       #
       self.makeGridDir(parm,modify)
       newIndex="%s/Grids/%s/%s_%s_index.nc"%(self.VERDIR,parm,model,parm)
       newData="%s/Grids/%s/%s_%s_data.nc"%(self.VERDIR,parm,model,parm)
       already=os.path.exists(newIndex)
       #
       #  Can't read data from file that does not exist
       #
       if ((not already) and (modify==0)):
          return 0
       #
       #  Figure out read-mode for file
       #
       if modify==0:
          mode="r"
       else:
          mode="a"
       #
       #  Figure data type and number of points in grid
       #
       if datatype<0:
          datatype=self.getVerParmType(parm)
          if datatype is None:
             return 0       
       (ypts,xpts)=self._empty.shape
       #
       #  Open the two fcst files: the index and the data
       #
       self.fncIndex=NetCDF.NetCDFFile(newIndex,mode)
       self.fncData=NetCDF.NetCDFFile(newData,mode)
       #
       #  If a new file...create the variables
       #
       if not already:
          self.fncData.createDimension("ypts",ypts)
          self.fncData.createDimension("xpts",xpts)
          self.fncData.createDimension("record",None)
          self.fncIndex.createDimension("record",None)
          self.fncIndex.createDimension("maxfcstrs",self.MAXFCSTRS)
          self.fncFcstr=self.fncIndex.createVariable('fcstr', 'b', ('record','maxfcstrs'))
          self.fncBtime=self.fncIndex.createVariable('btime', 'i', ('record',))
          self.fncStime=self.fncIndex.createVariable('stime', 'i',('record',))
          self.fncEtime=self.fncIndex.createVariable('etime', 'i',('record',))
          self.fncVtime=self.fncIndex.createVariable('vtime', 'i',('record',))
          self.fncScale=self.fncIndex.createVariable('scale','d',('record',))
          self.fncAddit=self.fncIndex.createVariable('addit','d',('record',))
          self.fncValue=self.fncData.createVariable('value','h',('record','ypts','xpts'))
          if datatype==1:
             self.fncScale1=self.fncIndex.createVariable('scale1','d',('record',))
             self.fncAddit1=self.fncIndex.createVariable('addit1','d',('record',))
             self.fncValue1=self.fncData.createVariable('value1','h',('record','ypts','xpts'))
          self.fncIndex.sync()
          self.fncData.sync()
          os.chmod(newIndex,0775)
          os.chmod(newData,0775)
       #
       #  If an old file...hook up variables to the netCDF files
       #
       else:
          ivarnames=self.fncIndex.variables.keys()
          dvarnames=self.fncData.variables.keys()
          for name in ('fcstr','btime','stime','etime','vtime','scale','addit'):
             if name not in ivarnames:
                self.logMsg("Corrupt index file for %s %s detected"%(model,parm))
                return 0
          if 'value' not in dvarnames:
             self.logMsg("Corrupt data file for %s %s detected"%(model,parm))
             return 0
          self.fncFcstr=self.fncIndex.variables['fcstr']
          if len(self.fncFcstr.shape)!=2:
             self.logMsg("Old index file (pre version 1.0) detected for %s %s"%(model,parm))
             return 0
          self.fncBtime=self.fncIndex.variables['btime']
          self.fncStime=self.fncIndex.variables['stime']
          self.fncEtime=self.fncIndex.variables['etime']
          self.fncVtime=self.fncIndex.variables['vtime']
          self.fncScale=self.fncIndex.variables['scale']
          self.fncAddit=self.fncIndex.variables['addit']
          self.fncValue=self.fncData.variables['value']
          if datatype==1:
             if (('scale1' not in ivarnames)or('addit1' not in ivarnames)):
                self.logMsg("Corrupt index file for %s %s detected"%(model,parm))
                return 0
             if 'value1' not in dvarnames:
                self.logMsg("Corrupt data file for %s %s detected"%(model,parm))
                return 0
             self.fncScale1=self.fncIndex.variables['scale1']
             self.fncAddit1=self.fncIndex.variables['addit1']
             self.fncValue1=self.fncData.variables['value1']
       self.fncParm=parm
       self.fncModel=model
       self.fncModify=modify
       self.fnumRecs=self.fncStime.shape[0]
       self.fncRecs=indices((self.fnumRecs,))[0]
       self.fncType=datatype
       return 1
    #=================================================================
    #  checkStats - given an parm, model, and obsmodel, see if the
    #               stats file for this is open. If not, close any
    #               current stat file. If modify is 1, then it
    #               opens the file for writing - which locks it
    #               from writing by others.
    #
    #               returns 0 if there is trouble opening file
    #
    def checkStats(self,parm,model,obsmodel,modify=0):
       #
       #  If everything is the same...return right away
       #
       if ((parm==self.sncParm)and(model==self.sncModel)and
           (obsmodel==self.sncObsModel)and(modify==self.sncModify)):
          return 1
       #
       #  If a file is currently open - close it
       #
       if (not(self.sncParm=="")):
          self.closeStatsFile()
       #
       #  Setup the file names and see if they exist
       #
       self.makeStatDir(parm,modify)
       newData="%s/Stats/%s/%s_%s_%s_data.nc"%(self.VERDIR,parm,model,parm,obsmodel)
       newIndex="%s/Stats/%s/%s_%s_%s_index.nc"%(self.VERDIR,parm,model,parm,obsmodel)
       already=os.path.exists(newIndex)
       #
       #  Can't read data from file that does not exist
       #
       if ((not already) and (modify==0)):
          return 0
       #
       #  Figure out read-mode for file
       #
       if modify==0:
          mode="r"
       else:
          mode="a"
       #
       #  Open the two fcst files: the index and the data
       #
       self.sncIndex=NetCDF.NetCDFFile(newIndex,mode)
       self.sncData=NetCDF.NetCDFFile(newData,mode)
       #
       #  If a new file...create the variables
       #
       if not already:
          self.sncData.createDimension("maxareas",self.STATAREAS)
          self.sncData.createDimension("maxstats",self.STATTYPES)
          self.sncData.createDimension("record",None)
          self.sncIndex.createDimension("record",None)
          self.sncIndex.createDimension("maxfcstrs",self.MAXFCSTRS)
          self.sncFcstr=self.sncIndex.createVariable('fcstr','b',('record','maxfcstrs'))
          self.sncBtime=self.sncIndex.createVariable('btime','i',('record',))
          self.sncStime=self.sncIndex.createVariable('stime','i',('record',))
          self.sncEtime=self.sncIndex.createVariable('etime','i',('record',))
          self.sncVtime=self.sncIndex.createVariable('vtime','i',('record',))
          self.sncCycle=self.sncIndex.createVariable('cycle','b',('record',))
          self.sncFhour=self.sncIndex.createVariable('fhour','h',('record',))
          self.sncStats=self.sncData.createVariable('stats','f',('record','maxareas','maxstats'))
          self.sncIndex.sync()
          self.sncData.sync()
          os.chmod(newIndex,0775)
          os.chmod(newData,0775)
       #
       #  If an old file...hook up variables to the netCDF files
       #
       else:
          self.sncFcstr=self.sncIndex.variables['fcstr']
          self.sncBtime=self.sncIndex.variables['btime']
          self.sncStime=self.sncIndex.variables['stime']
          self.sncEtime=self.sncIndex.variables['etime']
          self.sncVtime=self.sncIndex.variables['vtime']
          self.sncCycle=self.sncIndex.variables['cycle']
          self.sncFhour=self.sncIndex.variables['fhour']
          self.sncStats=self.sncData.variables['stats']
       self.sncParm=parm
       self.sncModel=model
       self.sncObsModel=obsmodel
       self.sncModify=modify
       self.sncNumRecs=self.sncBtime.shape[0]
       self.sncRecs=indices((self.sncNumRecs,))[0]
       return 1
    #=================================================================
    # closeStatsFile - if a stat file is open, close it and free up
    #                  all the structures associated with it.
    #
    def closeStatsFile(self):
       if (not(self.sncParm=="")):
          self.sncIndex.close()
          self.sncData.close()
          del self.sncIndex
          del self.sncData
          del self.sncFcstr
          del self.sncBtime
          del self.sncStime
          del self.sncEtime
          del self.sncVtime
          del self.sncCycle
          del self.sncFhour
          del self.sncStats
          del self.sncRecs
       self.sncParm=""
       self.sncModel=""
       self.sncObsModel=""
       self.sncNumRecs=-1
       self.sncModify=0
       return
    #=================================================================
    #  getBases - get sorted list of all base times (model run times)
    #             stored for an inputParm and model.  Can be helpful
    #             when figuring out all possible model run times
    #             (though, this is really only the model run times
    #             we have SAVED - not that COULD exist).
    #
    #             If model is in the OBSMODELS list, then it returns
    #             all the times of the saved OBSMODEL grids - which are
    #             the same as the grid start times.
    #
    #             if no data file exists for inputParm and model, it
    #             returns an empty list.
    #
    def getBases(self,inputParm,model):
       Bases=[]
       if not self.checkFile(inputParm,model):
          return Bases
       if model in self.OBSMODELS:
          for i in range(self.onumRecs):
             if self.oncBtime[i] not in Bases:
                Bases.append(self.oncBtime[i])
       else:
          for i in range(self.fnumRecs):
             if self.fncBtime[i] not in Bases:
                Bases.append(self.fncBtime[i])
       Bases.sort()
       return Bases
    #=================================================================
    #  getStarts - get sorted list of all grid start times stored for
    #              an inputParm and model.  Can be helpful when
    #              figuring out all possible times (though, this is
    #              really only the times we have SAVED - not all those
    #              that COULD exist).  Some forecast ones may not be
    #              verified yet!
    #
    #              if no data file exists for inputParm and model, it
    #              returns an empty list.
    #
    def getStarts(self,inputParm,model):
       Starts=[]
       if not self.checkFile(inputParm,model):
          return Starts
       if model in self.OBSMODELS:
          for i in range(self.onumRecs):
             if self.oncStime[i] not in Starts:
                Starts.append(self.oncStime[i])
       else:
          for i in range(self.fnumRecs):
             if self.fncStime[i] not in Starts:
                Starts.append(self.fncStime[i])
       Starts.sort()
       return Starts
    #==================================================================
    #  getFhours - get sorted list of all forecast hour times stored
    #              for inputParm and model.  Can be helpful when
    #              figuring out all possible times, By default, gets
    #              forecast hours from all cycle times - but if
    #              cycle time is non-negative, then it figures all
    #              the forecast times that have the specified
    #              cycle-time.
    #
    #              if model is in OBSMODELS list, then start times are
    #              the same as base times, and only 0 should be in the
    #              list.
    #
    #              if no data file exists for inputParm and model, it
    #              returns an empty list.
    #
    def getFhours(self,inputParm,model,cycle=-1):
       Fhours=[]
       if not self.checkFile(inputParm,model):
          return Fhours
       if model in self.OBSMODELS:
          Fhours.append(0)
          return Fhours
       fhr=(self.fncStime[:]-self.fncBtime[:])/HOURSECS
       if cycle>=0:
          cyclehrs=(self.fncBtime[:]-((self.fncBtime[:]/DAYSECS).astype(int)*DAYSECS))/HOURSECS
          for i in range(self.fnumRecs):
             if cyclehrs[i]==cycle:
                if fhr[i] not in Fhours:
                   Fhours.append(fhr[i])
       else:
          for i in range(self.fnumRecs):
             if fhr[i] not in Fhours:
                Fhours.append(fhr[i])
       Fhours.sort()
       return Fhours
    #=================================================================
    #  getFcstRecords - gets a sorted list of record numbers where the
    #                   basetime is the same as the specified basetime
    #
    def getFcstRecords(self,parm,model,basetime):
       Records=[]
       ret=self.checkFile(parm,model)
       if ret==0:
          return Records
       if model in self.OBSMODELS:
          use=equal(self.oncBtime[:],basetime)
          a=compress(use,self.oncRecs)
       else:
          use=equal(self.fncBtime[:],basetime)
          a=compress(use,self.fncRecs)
       Records=list(a)
       #for i in range(a.shape[0]):
       #   Records.append(a[i])
       return Records
    #=================================================================
    #  getFcstHour - take a basetime and a starttime and calculate the
    #                number of hours between them
    #
    def getFcstHour(self,Btime,Stime):
       hours=int(round(((Stime-Btime)/3600),0))
       return hours
    #=================================================================
    #  getRecFcstHour - given a record in the forecast file, calculate
    #                   the forecast hour, based on the basetime and
    #                   starttime
    #
    def getRecFcstHour(self,rec):
       rec = int(rec)
       btime=self.fncBtime[rec]
       stime=self.fncStime[rec]
       return self.getFcstHour(btime,stime)
    #=================================================================
    #  getVerTimeRange - given a starttime and endtime, create a
    #                    TimeRange object covering this time
    #
    def getVerTimeRange(self,Stime,Etime):
        start=AbsTime.AbsTime(Stime)
        end=AbsTime.AbsTime(Etime)
        tr=TimeRange.TimeRange(start,end)
        return tr
    #=================================================================
    #  readRecord - read and unpack a gridded data record. handles
    #               opening/closing files, and whether this is an
    #               observed model or a forecast model.  If the parm
    #               is a vector, returns a tuple with mag,dir.  If it
    #               cannot read - then it returns None
    #
    #  Note vertical flip of the grid is done so it's in the AWIPS II order.
    #
    def readRecord(self,parm,model,rec):
       if self.checkFile(parm,model)==0:
          return None
       if model in self.OBSMODELS:
          rec = int(rec)
          if self.oncType==0:
             vals=(self.oncValue[rec].astype(float)*self.oncScale[rec])+self.oncAddit[rec]
             return flipud(vals)
          else:
             mags=(self.oncValue[rec].astype(float)*self.oncScale[rec])+self.oncAddit[rec]
             dirs=(self.oncValue1[rec].astype(float)*self.oncScale1[rec])+self.oncAddit1[rec]
             return (flipud(mags), flipud(dirs))
       else:
          if self.fncType==0:
             vals=(self.fncValue[rec].astype(float)*self.fncScale[rec])+self.fncAddit[rec]
             return flipud(vals)
          else:
             mags=(self.fncValue[rec].astype(float)*self.fncScale[rec])+self.fncAddit[rec]
             dirs=(self.fncValue1[rec].astype(float)*self.fncScale1[rec])+self.fncAddit1[rec]
             return (flipud(mags), flipud(dirs))
    #=================================================================
    #  packIt - convert a scalar grid into packed 16-bit integer
    #           equivalent, with a float scale and offset that can
    #           be used to get back all the data exactly.
    #
    def packIt(self,grid):
       #
       #  convert the grid to its packed equivalent
       #
       minval=minimum.reduce(minimum.reduce(grid))
       maxval=maximum.reduce(maximum.reduce(grid))
       diff=maxval-minval
       diff=diff+(0.05*diff) # make range a little wider so that roundoff
                             # will not make packed integers larger than
                             # what will fit.
       Scale=diff/65534.0
       if Scale==0.0:
          Scale=1.0
       Addit=(minval+maxval)/2.0
       Valgrid=((grid-Addit)/Scale).astype('h')
       return(Scale,Addit,Valgrid)
    #=================================================================
    #  writeVals - write gridded data. Overwrite data for same time
    #              if it already exists, or replace old data in the
    #              file (if any) or append it to the file.
    #
    #              return 0 if a problem writing. Returns -1 if it
    #              skipped writing it because it matches what is
    #              already there.
    #
    def writeVals(self,parm,model,fcstrID,Btime,Stime,Etime,Grid):
       self.logMsg("Starting writeVals in VerifyUtility",10)
       #
       #  get datatype
       #
       datatype=self.getVerParmType(parm)
       if datatype is None:
          return 0
       #
       #  Check that the correct file is open - and ready to modify
       #
       if not self.checkFile(parm,model,modify=1):
          return 0
       #
       # flip grid to AWIPS I grid-point order (as in the netCDF file)
       #
       if datatype != 1:
          Grid = flipud(Grid)
       else:
          Grid = flipud(Grid[0]), flipud(Grid[1])
       #
       #
       #
       if datatype!=1:
          (Scale,Addit,Valgrid)=self.packIt(Grid)
       else:
          (mag,direc)=Grid
          (Scale, Addit, Valgrid )=self.packIt(mag)
          (Scale1,Addit1,Valgrid1)=self.packIt(direc)
       #
       #  Get forecaster number
       #
       if model=="Official":
          fcstrNum=self.findFcstrNumFromID(fcstrID)
       else:
          fcstrNum=0
       #
       #
       #
       recnum=-1
       overrec=-1
       oldest=time.time()
       veryOld=int(time.time())-self.GRIDDAYS*DAYSECS
       #
       #  Figure if this is for the Obs grid or a Fcst grid
       #
       if model in self.OBSMODELS:
          #
          #  If nothing there - just adding record 0
          #
          if self.onumRecs==0:
             recnum=0
          #
          # If there are records...see if one with the exact
          # same times exists - if so we can overwrite.
          #
          if recnum<0:
             s=equal(self.oncStime[:],Stime)
             b=equal(self.oncBtime[:],Btime)
             e=equal(self.oncEtime[:],Etime)
             use=logical_and(logical_and(b,s),e)
             if sometrue(sometrue(use)):
                a=compress(use,self.oncRecs)
                recnum=int(a[0])
                self.logMsg("existing record %d for that time"%recnum,10)
          #
          #  if still no record found - find if there are any old
          #  ones to overwrite
          #
          if (recnum==-1):
             overrec=int(argmin(self.oncStime[:],0))
             if self.DEBUG>0:
                self.logMsg("  oldest record is %s"%overrec)
             oldest=self.oncStime[overrec]
             if oldest<veryOld:
                if self.DEBUG>0:
                   self.logMsg("  and it is old enough to overwrite")
                recnum=overrec
             else:
                if self.DEBUG>0:
                   self.logMsg("   but not old enough to overwrite")
          #
          #  If STILL no record found - add to the current file
          #
          if (recnum==-1):
             recnum=self.onumRecs
          #
          #  Change the data
          #
          recnum = int(recnum)
          self.oncFcstr[recnum,0]=fcstrNum
          self.oncBtime[recnum]=Btime
          self.oncStime[recnum]=Stime
          self.oncEtime[recnum]=Etime
          self.oncVtime[recnum]=time.time()
          self.oncScale[recnum]=Scale
          self.oncAddit[recnum]=Addit
          self.oncValue[recnum]=Valgrid
          if datatype==1:
             self.oncScale1[recnum]=Scale1
             self.oncAddit1[recnum]=Addit1
             self.oncValue1[recnum]=Valgrid1
          #
          #  If we added a record - need to increase
          #  the indicies and the number of records counter
          #
          if recnum==self.onumRecs:
             self.onumRecs=self.onumRecs+1
             self.oncRecs=indices((self.onumRecs,))[0]
          #self.oncIndex.sync()
          #self.oncData.sync()
          self.closeObsFile()
       else:
          #  If nothing there - just adding record 0
          #
          if self.fnumRecs==0:
             recnum=0
          #
          # If there are records...see if one with the exact
          # same times exists - if so we can overwrite.
          #
          if recnum<0:
             s=equal(self.fncStime[:],Stime)
             b=equal(self.fncBtime[:],Btime)
             e=equal(self.fncEtime[:],Etime)
             use=logical_and(logical_and(b,s),e)
             if sometrue(sometrue(use)):
                a=compress(use,self.fncRecs)
                recnum=int(a[0])
                self.logMsg("existing record %d for that time"%recnum,10)
                issame=alltrue(alltrue(equal(self.fncValue[recnum],Valgrid)))
                if issame:
                   self.logMsg("   is exactly the same",10)
                   self.logMsg("   updating archive time",10)
                   self.fncVtime[recnum]=time.time()
                   return -1
          #
          #  if still no record found - find if there are any old
          #  ones to overwrite
          #
          if (recnum==-1):
             overrec=argmin(self.fncStime[:],0)
             overrec = int(overrec)
             if self.DEBUG>0:
                self.logMsg("  oldest record is %s"%overrec)
             oldest=self.fncStime[overrec]
             if oldest<veryOld:
                if self.DEBUG>0:
                   self.logMsg("  and it is old enough to overwrite")
                recnum=overrec
             else:
                if self.DEBUG>0:
                   self.logMsg("   but not old enough to overwrite")
          #
          #  If STILL no record found - add to the current file
          #
          if (recnum==-1):
             recnum=self.fnumRecs
          #
          #  Change the data
          #
          self.fncFcstr[recnum,0]=fcstrNum
          self.fncBtime[recnum]=Btime
          self.fncStime[recnum]=Stime
          self.fncEtime[recnum]=Etime
          self.fncVtime[recnum]=time.time()
          self.fncScale[recnum]=Scale
          self.fncAddit[recnum]=Addit
          self.fncValue[recnum]=Valgrid
          if datatype==1:
             self.fncScale1[recnum]=Scale1
             self.fncAddit1[recnum]=Addit1
             self.fncValue1[recnum]=Valgrid1
          #
          #  If we added a record - need to increase
          #  the indicies and the number of records counter
          #
          if recnum==self.fnumRecs:
             self.fnumRecs=self.fnumRecs+1
             self.fncRecs=indices((self.fnumRecs,))[0]
          self.closeFcstFile()


          self.logMsg("Done with writeVals in VerifyUtility",10)
       return 1
    #=================================================================
    #  getDoAgain - given a parm,model and btime, stime, etime
    #               - see if a record for stats already exists. If so,
    #               then get the time that those stats were calculated
    #               If the time was earlier than the observed or forecast
    #               grid savetime (ovtime, fvtime) then return 1 to
    #               indicate that we need to re-calculate these stats.
    #               If the the record for these stats to NOT exist - then
    #               we have to return 1 to indicate that these stats need
    #               to be calculated the first time.
    #
    def getDoAgain(self,parm,model,obsmodel,btime,stime,etime,ovtime,fvtime):
       self.logMsg("Starting getDoAgain in VerifyUtility",5)
       if not self.checkStats(parm,model,obsmodel):
          return 1
       #
       #  If nothing in current stat file - need to add a record
       #
       if self.sncNumRecs==0:
          return 1
       #
       # If there are records...see if one with the exact
       # same times exists
       #
       s=equal(self.sncStime[:],stime)
       b=equal(self.sncBtime[:],btime)
       e=equal(self.sncEtime[:],etime)
       use=logical_and(logical_and(b,s),e)
       if sometrue(sometrue(use)):
          #
          #  get record number 
          #
          a=compress(use,self.sncRecs)
          recnum=int(a[0])
          #
          #  If the time of the stat save is after BOTH grid
          #  were saved - then do not need to do stats again
          #
          savetime=self.sncVtime[recnum]
          if ((savetime>ovtime)and(savetime>fvtime)):
             return 0
       return 1
    #=================================================================
    #  writeStats - write stat data. Overwrite data for same time if it
    #               already exists, or replace old data in the file (if any)
    #               or append it to the file.
    #
    def writeStats(self,parm,model,obsmodel,fcstrNums,Btime,Stime,Etime,Cycle,
                   Fhour,Stats):
       self.logMsg("Starting writeStats in VerifyUtility",10)
       if not self.checkStats(parm,model,obsmodel,modify=1):
          return 0
       oldest=time.time()
       veryOld=int(time.time())-self.STATDAYS*DAYSECS
       #
       #  set record to missing (-1)
       #
       recnum=-1
       overrec=-1
       #
       #  If nothing in current stat file - just adding record 0
       #
       if self.sncNumRecs==0:
          recnum=0
       #
       # If there are records...see if one with the exact
       # same times exists - if so we can overwrite.
       #
       else:
          s=equal(self.sncStime[:],Stime)
          b=equal(self.sncBtime[:],Btime)
          e=equal(self.sncEtime[:],Etime)
          use=logical_and(logical_and(b,s),e)
          if sometrue(sometrue(use)):
             a=compress(use,self.sncRecs)
             recnum=int(a[0])
             self.logMsg("overwriting existing record:%d"%recnum,5)
       #
       #  if still no record found - find if there are any old
       #  ones to overwrite
       #
       if (recnum==-1):
          overrec=int(argmin(self.sncStime[:],0))
          self.logMsg("  oldest record is %s"%overrec,10)
          oldest=self.sncStime[overrec]
          if oldest<veryOld:
             self.logMsg("  and it is old enough to overwrite",10)
             recnum=overrec
          else:
             self.logMsg("   but not old enough to overwrite",10)
       #
       #  If STILL no record found - add to the current file
       #
       if (recnum==-1):
          recnum=self.sncNumRecs
       #
       #  Change the data
       #
       for j in range(len(fcstrNums)):
          self.sncFcstr[recnum,j]=fcstrNums[j]
       self.sncBtime[recnum]=Btime
       self.sncStime[recnum]=Stime
       self.sncEtime[recnum]=Etime
       self.sncVtime[recnum]=time.time()
       self.sncCycle[recnum]=Cycle
       self.sncFhour[recnum]=Fhour
       self.sncStats[recnum]=Stats
       #
       #  If we added a record - need to increase
       #  the indicies and the number of records counter
       #
       if recnum==self.sncNumRecs:
          self.sncNumRecs=self.sncNumRecs+1
          self.sncRecs=indices((self.sncNumRecs,))[0]

       self.sncIndex.sync()
       self.sncData.sync()
       self.logMsg("Done with writeStats in VerifyUtility",5)
       return
    #=================================================================
    #  deleteRecord - delete records from the grid datafiles.  This
    #                 really just stores dummy data in the records and
    #                 sets the dates to the earliest possible system
    #                 date so that these records are available for
    #                 re-use.  To really remove these records use
    #                 compactFiles (which is VERY slow!)
    #
    def deleteRecord(self,parm,model,dellist):
       (Scale,Addit,Valgrid)=self.packIt(self._empty)
       #
       #
       #
       datatype=self.getVerParmType(parm)
       if datatype is None:
          return 0
       #
       #  Open the correct file - and get it ready to modify
       #
       if not self.checkFile(parm,model,modify=1):
          return 0
       #
       #  Figure if this is for the Obs grid or a Fcst grid
       #
       if model in self.OBSMODELS:
          for rec in dellist:
             rec = int(rec)
             if rec<=self.onumRecs:
                #print "Would delete record %d"%rec
                #
                #  Change the data
                #
                self.oncFcstr[rec,0]=0
                self.oncBtime[rec]=0
                self.oncStime[rec]=0
                self.oncEtime[rec]=0
                self.oncVtime[rec]=0
                self.oncScale[rec]=1.0
                self.oncAddit[rec]=0.0
                self.oncValue[rec]=Valgrid
                if datatype==1:
                   self.oncScale1[rec]=1.0
                   self.oncAddit1[rec]=0.0
                   self.oncValue1[rec]=Valgrid
          self.closeObsFile()
       else:
          for rec in dellist:
             rec = int(rec)
             if rec<=self.fnumRecs:
                #print "Would delete record %d"%rec
                #
                #  Change the data
                #
                self.fncFcstr[rec,0]=0
                self.fncBtime[rec]=0
                self.fncStime[rec]=0
                self.fncEtime[rec]=0
                self.fncVtime[rec]=0
                self.fncScale[rec]=1.0
                self.fncAddit[rec]=0.0
                self.fncValue[rec]=Valgrid
                if datatype==1:
                   self.fncScale1[rec]=1.0
                   self.fncAddit1[rec]=0.0
                   self.fncValue1[rec]=Valgrid1
          self.closeFcstFile()
       return 1
    #=================================================================
    #  findRecords - gets a logical numeric array of forecast records
    #                in "verify" mode:
    #                   gets records that VALID period overlaps the
    #                   starttime to endtime period at least some
    #                in "forecast" mode:
    #                   gets records issued during the starttime to
    #                   endtime period.
    #                if mask is provided - records also have to
    #                   match that logical array (it better be the
    #                   same size as the number of records!)
    #
    def findRecords(self,parm,model,starttime,endtime,mode="verify",
                    mask=None):
       if not self.checkFile(parm,model):
          return None
       #
       #  Get the records that match the time periods
       #
       if mode=="verify":
          if model in self.OBSMODELS:
             match=logical_and(less(self.oncStime,endtime),
                               greater(self.oncEtime,starttime))
          else:
             match=logical_and(less(self.fncStime,endtime),
                               greater(self.fncEtime,starttime))
       else:
          if model in self.OBSMODELS:
             match=logical_and(less(self.oncBtime,endtime),
                               greater_equal(self.oncBtime,starttime))
          else:
             match=logical_and(less(self.fncBtime,endtime),
                               greater_equal(self.fncBtime,starttime))
       #
       #  If a mask is specified - check that too
       #
       if mask!=None:
          if model in self.OBSMODELS:
             if mask.shape[0]==self.oncRecs.shape[0]:
                match=logical_and(mask,match)
             else:
                self.logMsg("Could not mask records - wrong shape",10)
          else:
             if mask.shape[0]==self.fncRecs.shape[0]:
                match=logical_and(mask,match)
             else:
                self.logMsg("Could not mask records - wrong shape",10)
       return match
    #==================================================================
    #  listRecords - same as findRecords, but return a LIST of record
    #                numbers, rather than a logical numeric array of
    #                records used.
    #
    def listRecords(self,parm,model,starttime,endtime,mode="verify",
                    mask=None):
       #
       #  Get the records that match
       #
       matchRecs=self.findRecords(parm,model,starttime,endtime,mode,
                                  mask=mask)
       if matchRecs is None:
          records=[]
          return records
       #
       #  make into list of record numbers
       #
       if model in self.OBSMODELS:
          records=list(compress(matchRecs,self.oncRecs))
       else:
          records=list(compress(matchRecs,self.fncRecs))
       #
       return records
    #=================================================================
    #  findStatRecords - gets a logical numeric array of stat records
    #                in "verify" mode:
    #                   gets records that VALID period overlaps the
    #                   starttime to endtime period at least some
    #                in "forecast" mode:
    #                   gets records issued during the starttime to
    #                   endtime period.
    #                if mask is provided - records also have to
    #                   match that logical array (it better be the
    #                   same size as the number of records!)
    #
    def findStatRecords(self,parm,model,obsmodel,starttime,endtime,
                        mode="verify",mask=None):
       if not self.checkStats(parm,model,obsmodel):
          return None
       #
       #  Get the records that match the time periods
       #
       if mode=="verify":
          match=logical_and(less(self.sncStime,endtime),
                            greater(self.sncEtime,starttime))
       else:
          match=logical_and(less(self.sncBtime,endtime),
                            greater_equal(self.sncBtime,starttime))
       #
       #  If a mask is specified - check that too
       #
       if mask!=None:
          if mask.shape[0]==self.sncRecs.shape[0]:
             match=logical_and(mask,match)
          else:
             self.logMsg("Could not mask records - wrong shape",10)
       return match
    #==================================================================
    #  listStatRecords - same as findStatRecords, but return a LIST of
    #                    record numbers, rather than a logical numeric
    #                    array of records used.
    #
    def listStatRecords(self,parm,model,obsmodel,starttime,endtime,
                        mode="verify",mask=None):
       #
       #  Get the records that match
       #
       matchRecs=self.findStatRecords(parm,model,obsmodel,starttime,
                                      endtime,mode,mask=mask)
       if matchRecs is None:
          records=[]
          return records
       #
       #  make into list of record numbers
       #
       records=list(compress(matchRecs,self.sncRecs))
       #
       return records
    #==================================================================
    #  getPairList - get list of matching record pairs.  Sometimes
    #     either the observed of forecast records may be empty, such
    #     as when we have forecasts for a time that does not yet have
    #     verifying analyses - or a time when you have an observed
    #     record, but no matching forecasts
    #
    def getPairList(self,inputParm,model,obsParm,obsModel,dateStyle,dateType,
                    fromDay,numDays,dayList,cycleList,fcstrIDList,
                    fhrStart=0,fhrEnd=500):
       pairList=[]
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       maxCycles=len(self.getCFG('ALLCYCLES'))
       #
       #  Make sure files are openned right - and if neither
       #  can be openned - return right away.
       #
       fcstFlag=1
       obsFlag=1
       if not self.checkFile(inputParm,model):
          self.logMsg("  Fcst file for %s not open"%model)
          fcstFlag=0
          #return pairList
       if not self.checkFile(obsParm,obsModel):
          self.logMsg(" Obs file for %s not open"%obsModel)
          obsFlag=0
       if ((fcstFlag==0)and(obsFlag==0)):
          return pairList
          #return pairList
       #
       #  Get list of forecast records with right forecaster numbers
       #
       if fcstFlag==1:
          if ((model!="Official")or("ALL" in fcstrIDList)):
             rightFcstr=ones(self.fncFcstr.shape[0])
          else:
             rightFcstr=zeros(self.fncFcstr.shape[0])
             for fid in fcstrIDList:
                fnum=self.findFcstrNumFromID(fid)
                rightFcstr=logical_or(sometrue(equal(self.fncFcstr,fnum),-1),rightFcstr)
          #
          #  Get list of forecast records with right cycle
          #
          if (len(cycleList)==maxCycles):
             rightCycle=ones(self.fncBtime.shape)
          else:
             rightCycle=zeros(self.fncBtime.shape)
             for cycle in cycleList:
	        if (type(cycle) is types.StringType):
                   cyc=int(cycle)*HOURSECS
		else:
		   cyc=cycle*HOURSECS
                rem=remainder(self.fncBtime,DAYSECS).astype(int)
                rightCycle=where(equal(rem,cyc),1,rightCycle)
          #
          #  get list of forecast records with right forecast hours
          #
          fhr=(self.fncStime[:]-self.fncBtime[:])/float(HOURSECS)
          rightFhr=logical_and(greater_equal(fhr,fhrStart),less_equal(fhr,fhrEnd))
       #
       #  Handle "Verifying On" style of dates
       #
       if dateStyleLow=="verifying on":
          #
          #  If there are observed grids - get list of matching records
          #
          if obsFlag==1:
             if dateTypeLow=="period length":
                obrecs=self.getObsPeriod(obsModel,obsParm,fromDay,numDays)
             else:
                obrecs=self.getObsList(obsModel,obsParm,dayList)
          else:
             obrecs=[]
          #
          #  If there are forecast grids - 
          #
          if fcstFlag==1:
             elist1=self.fncEtime[:]
             slist1=self.fncStime[:]
             if obsFlag==1:
                for obrec in obrecs:
                   stime=self.oncStime[obrec]
                   etime=self.oncEtime[obrec]
                   cross1=greater(elist1,stime)
                   cross2=less(slist1,etime)
                   match1=logical_and(cross1,cross2)
                   match=logical_and(logical_and(logical_and(match1,rightFcstr),rightCycle),rightFhr)
                   frecs=compress(match,self.fncRecs)
                   flist=list(frecs)
                   pairList.append(([obrec,],flist))
             else:
                if dateTypeLow=="period length":
                   rawrecs=self.getFcstPeriod(inputParm,fromDay,numDays,model)
                else:
                   rawrecs=self.getFcstList(inputParm,dayList,model)
                fcstrecs=self.trimFcstRecs(rawrecs,model,cycleList,fcstrIDList,
                                           fhrStart=fhrStart,fhrEnd=fhrEnd)
                for fcstrec in fcstrecs:
                   pairList.append(([],[fcstrec,]))
          else:
             for obrec in obrecs:
                pairList.append(([obrec,],[]))
       #
       #  Handle "Forecast On" style of dates
       #
       else:
          #self.logMsg("in forecast on section of getPairList")
          #
          #  If no forecast file is open - then we don't know the
          #  dates that were forecast during this time - so we dont
          #  have obs grids.  Must return a blank list of pairs
          #
          if fcstFlag==0:
             return pairList
          #
          #  Otherwise...we have a forecast file open...and we can
          #  read those records
          #
          if dateTypeLow=="period length":
             rawrecs=self.getFcstPeriod(inputParm,fromDay,numDays,model)
          else:
             rawrecs=self.getFcstList(inputParm,dayList,model)
          fcstrecs=self.trimFcstRecs(rawrecs,model,cycleList,fcstrIDList,
                                     fhrStart=fhrStart,fhrEnd=fhrEnd)
          flists={}
          #self.logMsg("  len of fcstrecs=%d"%len(fcstrecs))
          #
          #  If no obs file is open - then just return fcst recs
          #
          if obsFlag==0:
             for fcstrec in fcstrecs:
                pairList.append(([],[fcstrec,]))
             return pairList
          #
          #  find obs record that matches each forecast record
          #
          for fcstrec in fcstrecs:
             stime=self.fncStime[fcstrec]
             etime=self.fncEtime[fcstrec]
             cross1=greater(etime,self.oncStime)
             cross2=less(stime,self.oncEtime)
             orecs=list(compress(logical_and(cross1,cross2),self.oncRecs))
             if (len(orecs)<1):
                obrec="-10"  #dummy obs record
             else:
                obrec="%d"%orecs[0]
             if obrec in flists.keys():
                flists[obrec].append(fcstrec)
             else:
                flists[obrec]=[fcstrec]
          for obrec in flists.keys():
             iobrec=int(int(obrec)+0.5)
             obslist=[]
             if (iobrec>=0):
                obslist.append(iobrec)
             pairList.append((obslist,flists[obrec]))
       return pairList
    #==================================================================
    #  getCases - gets caseInfo structure...can either be for 'common
    #             cases' or normal.
    #
    def getCases(self,readParm,models,obsParm,obsmodel,dateStyle,
                 dateType,fromDay=0,numDays=0,dayList=[],
                 fcstrs=-1,cycles=-1,fhrStart=-48,fhrEnd=-48,
                 accumHours=12,accumFreq=12,
                 requireObs=1,commonCases=1,basetimeOffsets=0,
                 callbackMethod=None):
       if commonCases==1:
          caseInfo=self.getCommonCases(readParm,models,obsParm,obsmodel,
                     dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                     dayList=dayList,fcstrs=fcstrs,
                     cycles=cycles,fhrStart=fhrStart,
                     fhrEnd=fhrEnd,accumHours=accumHours,
                     accumFreq=accumFreq,requireObs=requireObs,
                     basetimeOffsets=basetimeOffsets,
                     callbackMethod=callbackMethod)
       else:
          caseInfo={}
          for model in models:
             cases=self.getCommonCases(readParm,model,obsParm,obsmodel,
                     dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                     dayList=dayList,fcstrs=fcstrs,cycles=cycles,
                     fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                     accumFreq=accumFreq,requireObs=requireObs,
                     basetimeOffsets=basetimeOffsets,
                     callbackMethod=callbackMethod)
             caseInfo[model]=cases[model]
       return caseInfo
    #==================================================================
    #  getCommonCases - obtain dictionary of records for common cases.
    #                   Keys are each model in modelList. The value for
    #                   each model is, itself, a dictionary, with keys
    #                   of 'basetime,starttime,endtime' and a value of
    #                   a two-element list.  The first element is,
    #                   itself, a list - with records that make up
    #                   the forecast, and the second element is,
    #                   itself, a list - with records that make up the
    #                   observation
    #
    #                   if obsRequired is zero - it will allow
    #                   cases without observations...in which case the
    #                   second list will be empty.
    #
    def getCommonCases(self,parm,models,obsParm,obsModel,dateStyle,
                       dateType,fromDay=0,numDays=0,dayList=[],
                       fcstrs=-1,cycles=-1,fhrStart=-48,
                       fhrEnd=-48,accumHours=12,
                       accumFreq=12,requireObs=1,basetimeOffsets=0,
                       callbackMethod=None):
       self.logMsg("start getCommonCases",10)
       finalCases={}
       self.callbackMethod=callbackMethod
       #
       #  Get all the verifing cases
       #
       self.internalMessage="Observations:"
       obsCases=self.getObsCases(obsParm,obsModel,accumHours,accumFreq,
                    dateStyle,dateType,fromDay,numDays,dayList,
                    self.internCB)
       #
       #  Check to see if stopping 
       #
       if self.callbackMethod is not None:
          exit=self.callbackMethod(self.internalMessage)
          if exit==1:
             return finalCases
       #
       obskeys=obsCases.keys()
       numgrids=len(obskeys)
       self.logMsg("Observed cases:%d"%numgrids,5)
       obskeys.sort()
       if self.getDebug()>=5:
          for obskey in obskeys:
             (st,en)=obskey.split(",")
             (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(int(st))
             (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(int(en))
             self.logMsg("   Obs for %4.4d/%2.2d/%2.2d %2.2dZ - %4.4d/%2.2d/%2.2d %2.2dZ"%(syea,
                         smon,sday,shou,eyea,emon,eday,ehou),10)
       #
       allCases={}
       #
       #  See if models is a list, or a single model - putting them into
       #  modelList for further processing
       #
       modelList=[]
       if ((type(models) is types.ListType) or (type(models) is types.TupleType)):
          for model in models:
             modelList.append(model)
       else:
          modelList.append(models)
       #
       # Loop over all the models
       #
       totalmodels=len(modelList)
       modelcount=0
       for model in modelList:
          modelcount+=1
          if totalmodels>1:
             self.internalMessage="%s (%d of %d):"%(model,modelcount,totalmodels)
          else:
             self.internalMessage="%s:"%model
          cases=self.getModelCases(parm,model,fcstrs,cycles,
                     fhrStart,fhrEnd,accumHours,
                     accumFreq,dateStyle,dateType,fromDay,numDays,
                     dayList,self.internCB)
          if self.callbackMethod is not None:
             exit=self.callbackMethod(self.internalMessage)
             if exit==1:
                return finalCases
          casekeys=cases.keys()
          numgrids=len(casekeys)
          self.logMsg("%s has %d potential cases"%(model,numgrids),5)
          if self.getDebug()>=5:
             casekeys.sort()
             for casekey in casekeys:
                (base,st,en)=casekey.split(",")
                (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(int(base))
                (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(int(st))
                (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(int(en))
                self.logMsg("   Model potential from %4.4d/%2.2d/%2.2d %2.2dZ for %4.4d/%2.2d/%2.2d %2.2dZ - %4.4d/%2.2d/%2.2d %2.2dZ"%(byea,
                            bmon,bday,bhou,syea,smon,sday,shou,eyea,emon,eday,ehou),10)
          #
          #  If obs are required...look through cases and make sure that the
          #  period is available in the obsCases retreived above
          #
          if requireObs!=0:
             noobs=0
             for key in casekeys:
                (base,rest)=key.split(",",1)
                if rest not in obskeys:
                   (st,en)=rest.split(",")
                   (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(int(base))
                   (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(int(st))
                   (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(int(en))
                   self.logMsg("   deleting case for no Obs - from %4.4d/%2.2d/%2.2d %2.2dZ for %4.4d/%2.2d/%2.2d %2.2dZ - %4.4d/%2.2d/%2.2d %2.2dZ"%(byea,
                               bmon,bday,bhou,syea,smon,sday,shou,eyea,emon,eday,ehou),10)
                   del cases[key]
                   noobs=noobs+1
             self.logMsg("   %d cases deleted because they have no obs"%noobs,5)
          allCases[model]=cases
       #
       #  With only 1 model common cases are easy!
       #
       if totalmodels==1:
          finalkeys=allCases[model].keys()
       #
       #  For mulitple models...Get keys for each model...convert them to
       #  the offsetBasetime (if basetimeOffsets==1)...and find the
       #  model with the fewest keys
       #
       else:
          if self.callbackMethod is not None:
             exit=self.callbackMethod("filtering through models")
             if exit==1:
                return finalCases
          finalkeys=[]
          modkeys={}
          minmod=""
          minkeys=-1
          for model in modelList:
             realKeys=allCases[model].keys()
             if basetimeOffsets==1:
                baseOffset=self.getBaseOffset(model)
             else:
                baseOffset=0
             if baseOffset==0:
                testKeys=realKeys
             else:
                testKeys=[]
                for key in realKeys:
                   (basetimeStr,starttimeStr,endtimeStr)=key.split(",")
                   basetime="%d"%(int(basetimeStr)+(baseOffset*HOURSECS))
                   newkey="%s,%s,%s"%(basetime,starttimeStr,endtimeStr)
                   testKeys.append(newkey)
             modkeys[model]=testKeys
             numkeys=len(modkeys[model])
             if ((minkeys==-1)or(numkeys<minkeys)):
                minkeys=numkeys
                minmod=model
          #
          #  For each key...see if it is NOT in any of
          #  the other models.  If not - it doesn't get
          #  added
          #
          mykeys=modkeys[minmod]
          for key in mykeys:
             if key not in finalkeys:
                use=1
                for mod in modelList:
                   if mod!=minmod:
                      if key not in modkeys[mod]:
                         use=0
                         break
                if use==1:
                   finalkeys.append(key)
       #
       #  Log all the common cases
       #
       finalkeys.sort()
       self.logMsg("final number of cases:%d"%len(finalkeys),5)
       if self.getDebug()>=10:
          for key in finalkeys:
             (base,start,end)=key.split(",")
             (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(int(base))
             (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(int(start))
             (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(int(end))
             self.logMsg("   %4.4d/%2.2d/%2.2d %2.2dZ for %4.4d/%2.2d/%2.2d %2.2dZ - %4.4d/%2.2d/%2.2d %2.2dZ"%(byea,
                               bmon,bday,bhou,syea,smon,sday,shou,eyea,emon,eday,ehou),10)
       #
       #  Make the final case structure - with list of forecast/observed
       #  records for each case.  If basetimeOffsets==1, then the finalkeys may
       #  need to be converted back to real keys for each model.  The keys of
       #  returned lists will be with the offset 'basetimes'.
       #
       finalCases={}
       for model in modelList:
          cases={}
          modCases=allCases[model]
          if ((basetimeOffsets==1)and(totalmodels!=1)):
             baseOffset=self.getBaseOffset(model)
          else:
             baseOffset=0
          for key in finalkeys:
             if baseOffset!=0:
                (offsetBasetime,starttime,endtime)=key.split(",")
                realkey="%d,%s,%s"%(int(offsetBasetime)-(baseOffset*HOURSECS),starttime,endtime)
             else:
                realkey=key
             frecList=modCases[realkey]
             (base,rest)=key.split(",",1)
             if rest in obskeys:
                orecList=obsCases[rest]
             else:
                orecList=[]
             cases[key]=[frecList,orecList]
          finalCases[model]=cases
       self.logMsg("end getCommonCases",10)
       return finalCases
    #==================================================================
    #  internCB
    #
    def internCB(self,message):
       fullmessage="%s %s"%(self.internalMessage,message)
       retval=0
       if self.callbackMethod is not None:
          retval=self.callbackMethod(fullmessage)
       return retval
    #=================================================================
    # getModelCases - return a dictionary for the specified model of
    #                 forecast records for the specified periods.
    #                 The keys are "basetime,starttime,endtime", and
    #                 the values in the dictionary are lists of records
    #                 (these are lists, because it can take multiple
    #                 records to cover long time periods for accumulative
    #                 parms, or probability parms).
    #
    def getModelCases(self,parm,model,fcstrs,cycles,fhrStart,fhrEnd,
                      accumHours,accumFreq,dateStyle,
                      dateType,fromDay,numDays,dayList,
                      callbackMethod=None):
       self.logMsg("start getModelCases",10)
       verType=self.getVerType(parm)
       rateFlag=self.getRateFlag(model,parm)
       cases={}
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       #
       #  Give up right away if you cant open the model file
       #
       if not self.checkFile(parm,model):
          return cases
       #
       #  Setup logical array with records that contain the right Forecaster,
       #  the right cycle, and the right forecast hours.  If none...get out
       #  right away.
       #
       rightRecord=self.getFcstrCycleFhr(model,fcstrs,cycles,fhrStart,
                                         fhrEnd)
       numRecs=add.reduce(rightRecord)
       self.logMsg("number of records with right forecaster, cycle, hours:%d"%numRecs,10)
       #
       #  If a probability parm, or an accumulative parm, then find
       #  cases where forecasts completely coverred the possible
       #  periods.
       #
       if ((verType==1)or(rateFlag==1)):
          #
          #  Get potential verifying periods for this accumHour,accumFreq
          #     combination with the dateStyle/dateType/fromDay/numDays/
          #     dayList combination
          #
          verPeriods=self.createObsPeriods(accumHours,accumFreq,dateStyle,
                        dateType,fromDay,numDays,dayList)
          numPeriods=len(verPeriods)
          self.logMsg("number of periods:%d"%numPeriods,10)
          #
          #  Loop over potential periods...and find matching records
          #
          count=0
          if dateStyleLow=="forecast on":
             fromPeriods=self.createFromPeriods(dateType,fromDay,numDays,dayList)
          for verPer in verPeriods:
             count=count+1
             if callbackMethod is not None:
                exit=callbackMethod("%d of %d"%(count,numPeriods))
                if exit==1:
                   return cases
             (stime,etime)=verPer
             totalTime=etime-stime
             recmatch=logical_and(rightRecord,
                                 logical_and(greater(self.fncEtime,stime),
                                          less(self.fncStime,etime)))
             #
             # When there are matching records...find each basetime that
             # forecast for this period
             #
             if sometrue(recmatch):
                recnumberList=list(compress(recmatch,self.fncRecs))
                baselist=[]
                for rec in recnumberList:
                   rec = int(rec)
                   if self.fncBtime[rec] not in baselist:
                      baselist.append(self.fncBtime[rec])
                #
                #  And for each basetime...see if the period was coverred
                #  by forecast grids
                #
                for base in baselist:
                   #
                   #
                   #
                   if dateStyleLow=="forecast on":
                      okbase=0
                      for testper in fromPeriods:
                         (perstart,perend)=testper
                         if ((base>=perstart)and(base<perend)):
                            okbase=1
                            break
                      if okbase==0:
                         continue
                   reclist=[]
                   totcov=0
                   for rec in recnumberList:
                      rec = int(rec)
                      if self.fncBtime[rec]==base:
                         reclist.append(rec)
                         recstart=self.fncStime[rec]
                         recend=self.fncEtime[rec]
                         cover=min(etime-recstart,recend-stime,recend-recstart)
                         totcov=totcov+cover
                   if totcov>=totalTime:
                      key="%d,%d,%d"%(base,stime,etime)
                      cases[key]=reclist
       #
       #  Other parms get forecast periods based on the forecast grids
       #  that were actually made
       #
       else:
          if dateStyleLow=="verifying on":
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                recList=self.getObsPeriod(model,parm,fromDay,numDays,mask=rightRecord)
             else:
                recList=self.getObsList(model,parm,dayList,mask=rightRecord,
                                        callbackMethod=callbackMethod)
          else:
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                starttime=fromDay-((numDays-1)*DAYSECS)
                endtime=fromDay+DAYSECS-1
                recList=self.listRecords(parm,model,starttime,endtime,"forecast",rightRecord)
             else:
                recList=[]
                count=0
                totalDays=len(dayList)
                for date in dayList:
                   count=count+1
                   if callbackMethod is not None:
                      exit=callbackMethod("%d of %d"%(count,totalDays))
                      if exit==1:
                         return cases
                   if type(date) is types.StringType:
		      try:
                         (yea,mon,day)=date.split("/")
                         starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		      except:
		         continue
                   else:
                      starttime=date
                   endtime=starttime+DAYSECS-1
                   tmprecs=self.listRecords(parm,model,starttime,endtime,"forecast",rightRecord)
                   for rec in tmprecs:
                      recList.append(rec)
          #
          #  Now make case entries for each of these records
          #
          for rec in recList:
             rec = int(rec)
             base=self.fncBtime[rec]
             stime=self.fncStime[rec]
             etime=self.fncEtime[rec]
             key="%d,%d,%d"%(base,stime,etime)
             cases[key]=[rec]
       self.logMsg("end getModelCases",10)
       return cases
    #=================================================================
    # getObsCases - return a dictionary for the specified obs model of
    #               records for the specified periods.
    #               The keys are "starttime,endtime", and
    #               the values in the dictionary are lists of records
    #               (these are lists, because it can take multiple
    #               records to cover long time periods for accumulative
    #               parms, or probability parms).
    #
    def getObsCases(self,parm,model,
                    accumHours,accumFreq,dateStyle,
                    dateType,fromDay,numDays,dayList,
                    callbackMethod=None):
       self.logMsg("start getObsCases",10)
       cases={}
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       #
       #  Give up right away if you cant open the model file
       #
       if not self.checkFile(parm,model):
          return cases
       #
       #  If a probability parm, or an accumulative parm, then find
       #  cases where observations completely coverred the possible
       #  periods.
       #
       rateFlag=self.getRateFlag(model,parm)
       if (rateFlag==1):
          #
          #  Get potential verifying periods for this accumHour,accumFreq
          #     combination with the dateStyle/dateType/fromDay/numDays/
          #     dayList combination
          #
          verPeriods=self.createObsPeriods(accumHours,accumFreq,dateStyle,
                        dateType,fromDay,numDays,dayList)
          numPeriods=len(verPeriods)
          self.logMsg("number of periods:%d"%numPeriods,10)
          #
          #  Loop over potential periods...and find matching records
          #
          count=0
          for verPer in verPeriods:
             count=count+1
             if callbackMethod is not None:
                exit=callbackMethod("%d of %d"%(count,numPeriods))
                if exit==1:
                   return cases
             (stime,etime)=verPer
             totalTime=etime-stime
             recmatch=logical_and(greater(self.oncEtime,stime),
                                  less(self.oncStime,etime))
             #
             # When there are matching records...find each basetime that
             # forecast for this period
             #
             if sometrue(recmatch):
                recnumberList=list(compress(recmatch,self.oncRecs))
                totcov=0
                for rec in recnumberList:
                   rec = int(rec)
                   recstart=self.oncStime[rec]
                   recend=self.oncEtime[rec]
                   cover=min(etime-recstart,recend-stime,recend-recstart)
                   totcov=totcov+cover
                if totcov==totalTime:
                   key="%d,%d"%(stime,etime)
                   cases[key]=recnumberList
       #
       #  Other parms get forecast periods based on the forecast grids
       #  that were actually made
       #
       else:
          if dateStyleLow=="verifying on":
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                recList=self.getObsPeriod(model,parm,fromDay,numDays)
             else:
                recList=self.getObsList(model,parm,dayList,callbackMethod=callbackMethod)
          else:
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                soff=self.getStartOffset(parm)
                eoff=self.getEndOffset(parm)
                starttime=fromDay-((numDays-1)*DAYSECS)+soff
                endtime=fromDay+DAYSECS-1+(self.MAXFORECASTHOUR*HOURSECS)+eoff
                recList=self.listRecords(parm,model,starttime,
                                         endtime,"verify")
             else:
                recList=[]
                soff=self.getStartOffset(parm)
                eoff=self.getEndOffset(parm)
                count=0
                totalDays=len(dayList)
                for date in dayList:
                   count=count+1
                   if callbackMethod is not None:
                      exit=callbackMethod("%d of %d"%(count,totalDays))
                      if exit==1:
                         return cases
                   if type(date) is types.StringType:
		      try:
                         (yea,mon,day)=date.split("/")
                         starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))
		      except:
		         continue
                   else:
                      starttime=date
                   endtime=starttime+(self.MAXFORECASTHOUR*HOURSECS)+eoff
		   starttime=starttime+soff
                   tmprecs=self.listRecords(parm,model,starttime,
                                            endtime,"verify")
                   for rec in tmprecs:
                      recList.append(rec)
          #
          #  Now make case entries for each of these records
          #
          for rec in recList:
             rec = int(rec)
             stime=self.oncStime[rec]
             etime=self.oncEtime[rec]
             key="%d,%d"%(stime,etime)
             cases[key]=[rec]
       self.logMsg("end getObsCases",10)
       return cases
    #==================================================================
    #  getStatCases - gets caseInfo structure for stats...can either be
    #                 for 'common cases' or normal.
    #
    def getStatCases(self,parm,models,obsmodel,dateStyle,
                     dateType,fromDay=0,numDays=0,dayList=[],
                     fcstrs=-1,cycles=-1,fhrStart=-48,fhrEnd=-48,
                     accumHours=12,accumFreq=12,
                     commonCases=1,basetimeOffsets=0,
                     callbackMethod=None):
       if commonCases==1:
          caseInfo=self.getStatCommonCases(parm,models,obsmodel,
                     dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                     dayList=dayList,fcstrs=fcstrs,
                     cycles=cycles,fhrStart=fhrStart,
                     fhrEnd=fhrEnd,accumHours=accumHours,
                     accumFreq=accumFreq,
                     basetimeOffsets=basetimeOffsets,
                     callbackMethod=callbackMethod)
       else:
          caseInfo={}
          for model in models:
             cases=self.getStatCommonCases(parm,model,obsmodel,
                     dateStyle,dateType,fromDay=fromDay,numDays=numDays,
                     dayList=dayList,fcstrs=fcstrs,cycles=cycles,
                     fhrStart=fhrStart,fhrEnd=fhrEnd,accumHours=accumHours,
                     accumFreq=accumFreq,
                     basetimeOffsets=basetimeOffsets,
                     callbackMethod=callbackMethod)
             caseInfo[model]=cases[model]
       return caseInfo
    #==================================================================
    #  getStatCommonCases - obtain dictionary of records for common cases.
    #                   Keys are each model in modelList. The value for
    #                   each model is, itself, a dictionary, with keys
    #                   of 'basetime,starttime,endtime' and a value of
    #                   a list of stat records.
    #
    def getStatCommonCases(self,parm,models,obsModel,dateStyle,
                       dateType,fromDay=0,numDays=0,dayList=[],
                       fcstrs=-1,cycles=-1,fhrStart=-48,
                       fhrEnd=-48,accumHours=12,
                       accumFreq=12,basetimeOffsets=0,
                       callbackMethod=None):
       self.logMsg("start getStatCommonCases",10)
       self.callbackMethod=callbackMethod
       allCases={}
       #
       #  See if models is a list, or a single model - putting them into
       #  modelList for further processing
       #
       modelList=[]
       if ((type(models) is types.ListType) or (type(models) is types.TupleType)):
          for model in models:
             modelList.append(model)
       else:
          modelList.append(models)
       #
       # Loop over all the models
       #
       totalmodels=len(modelList)
       modelcount=0
       for model in modelList:
          modelcount+=1
          if totalmodels>1:
             self.internalMessage="%s (%d of %d):"%(model,modelcount,totalmodels)
          else:
             self.internalMessage="%s:"%model
          cases=self.getStatModelCases(parm,model,obsModel,dateStyle,dateType,
                     cycles=cycles,fcstrs=fcstrs,fhrStart=fhrStart,fhrEnd=fhrEnd,
                     fromDay=fromDay,numDays=numDays,dayList=dayList,
                     accumHours=accumHours,accumFreq=accumFreq,
                     callbackMethod=self.internCB)
          casekeys=cases.keys()
          numgrids=len(casekeys)
          self.logMsg("%s has %d pre-calculated cases"%(model,numgrids),5)
          allCases[model]=cases
       #
       #  With only 1 model common cases are easy!
       #
       if totalmodels==1:
          finalkeys=allCases[model].keys()
       #
       #  For mulitple models...Get keys for each model...convert them to
       #  the offsetBasetime (if basetimeOffsets==1)...and find the
       #  model with the fewest keys
       #
       else:
          if self.callbackMethod is not None:
             exit=self.callbackMethod("filtering through models")
             if exit==1:
                finalCases={}
                return finalCases
          finalkeys=[]
          modkeys={}
          minmod=""
          minkeys=-1
          for model in modelList:
             realKeys=allCases[model].keys()
             if basetimeOffsets==1:
                baseOffset=self.getBaseOffset(model)*HOURSECS
             else:
                baseOffset=0
             if baseOffset==0:
                testKeys=realKeys
             else:
                testKeys=[]
                for key in realKeys:
                   (basetimeStr,starttimeStr,endtimeStr)=key.split(",")
                   basetime="%d"%(int(basetimeStr)+baseOffset)
                   newkey="%s,%s,%s"%(basetime,starttimeStr,endtimeStr)
                   testKeys.append(newkey)
             modkeys[model]=testKeys
             numkeys=len(modkeys[model])
             if ((minkeys==-1)or(numkeys<minkeys)):
                minkeys=numkeys
                minmod=model
          #
          #  For each key...see if it is NOT in any of
          #  the other models.  If not - it doesn't get
          #  added
          #
          mykeys=modkeys[minmod]
          for key in mykeys:
             if key not in finalkeys:
                use=1
                for mod in modelList:
                   if mod!=minmod:
                      if key not in modkeys[mod]:
                         use=0
                         break
                if use==1:
                   finalkeys.append(key)
       #
       #  Log all the common cases
       #
       finalkeys.sort()
       self.logMsg("final number of cases:%d"%len(finalkeys),5)
       if self.getDebug()>=10:
          for key in finalkeys:
             (base,start,end)=key.split(",")
             (byea,bmon,bday,bhou,bmin,bsec,bwda,byda,bdst)=time.gmtime(int(base))
             (syea,smon,sday,shou,smin,ssec,swda,syda,sdst)=time.gmtime(int(start))
             (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(int(end))
             self.logMsg("   %4.4d/%2.2d/%2.2d %2.2dZ for %4.4d/%2.2d/%2.2d %2.2dZ - %4.4d/%2.2d/%2.2d %2.2dZ"%(byea,
                               bmon,bday,bhou,syea,smon,sday,shou,eyea,emon,eday,ehou),10)
       #
       #  Make the final case structure - with list of forecast/observed
       #  records for each case.  If basetimeOffsets==1, then the finalkeys may
       #  need to be converted back to real keys for each model.  The keys of
       #  returned lists will be with the offset 'basetimes'.
       #
       finalCases={}
       for model in modelList:
          cases={}
          modCases=allCases[model]
          if ((basetimeOffsets==1)and(totalmodels!=1)):
             baseOffset=self.getBaseOffset(model)*HOURSECS
          else:
             baseOffset=0
          for key in finalkeys:
             if baseOffset!=0:
                (offsetBasetime,starttime,endtime)=key.split(",")
                realkey="%d,%s,%s"%(int(offsetBasetime)-baseOffset,starttime,endtime)
             else:
                realkey=key
             cases[key]=modCases[realkey]
          finalCases[model]=cases
       self.logMsg("end getStatCommonCases",10)
       return finalCases
    #=================================================================
    # getStatModelCases - return a dictionary for the specified model of
    #                     forecast records for the specified periods.
    #                     The keys are "basetime,starttime,endtime", and
    #                     the values in the dictionary are lists of records
    #                     (these are lists, because it can take multiple
    #                     records to cover long time periods for accumulative
    #                     parms, or probability parms).
    #
    def getStatModelCases(self,parm,model,obsmodel,dateStyle,dateType,
                          fromDay=0,numDays=0,dayList=[],
                          fcstrs=-1,cycles=-1,fhrStart=-48,fhrEnd=-48,
                          accumHours=12,accumFreq=12,
                          callbackMethod=None):
       self.logMsg("start getStatModelCases",10)
       verType=self.getVerType(parm)
       rateFlag=self.getRateFlag(model,parm)
       cases={}
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       #
       #  Give up right away if you cant open the model file
       #
       if not self.checkStats(parm,model,obsmodel):
          return cases
       #
       #  Setup logical array with records that contain the right Forecaster,
       #  the right cycle, and the right forecast hours.  If none...get out
       #  right away.
       #
       rightRecord=self.getStatFcstrCycleFhr(model,fcstrs,cycles,
                                             fhrStart,fhrEnd)
       numRecs=add.reduce(rightRecord)
       self.logMsg("number of records with right forecaster, cycle, hours:%d"%numRecs,10)
       #
       #  If a probability parm, or an accumulative parm, then find
       #  cases where forecasts completely coverred the possible
       #  periods.
       #
       if ((verType==1)or(rateFlag==1)):
          #
          #  Get potential verifying periods for this accumHour,accumFreq
          #     combination with the dateStyle/dateType/fromDay/numDays/
          #     dayList combination
          #
          verPeriods=self.createObsPeriods(accumHours,accumFreq,dateStyle,
                        dateType,fromDay,numDays,dayList)
          numPeriods=len(verPeriods)
          self.logMsg("number of periods:%d"%numPeriods,10)
          #
          #  Loop over potential periods...and find matching records
          #
          count=0
          for verPer in verPeriods:
             count=count+1
             if callbackMethod is not None:
                exit=callbackMethod("%d of %d"%(count,numPeriods))
                if exit==1:
                   return cases
             (stime,etime)=verPer
             totalTime=etime-stime
             recmatch=logical_and(rightRecord,
                                 logical_and(greater(self.sncEtime,stime),
                                          less(self.sncStime,etime)))
             #
             # When there are matching records...find each basetime that
             # forecast for this period
             #
             if sometrue(recmatch):
                recnumberList=list(compress(recmatch,self.sncRecs))
                baselist=[]
                for rec in recnumberList:
                   rec = int(rec)
                   if self.fncBtime[rec] not in baselist:
                      baselist.append(self.sncBtime[rec])
                #
                #  And for each basetime...see if the period was coverred
                #  by forecast grids
                #
                for base in baselist:
                   reclist=[]
                   totcov=0
                   for rec in recnumberList:
                      rec = int(rec)
                      if self.sncBtime[rec]==base:
                         reclist.append(rec)
                         recstart=self.sncStime[rec]
                         recend=self.sncEtime[rec]
                         cover=min(etime-recstart,recend-stime,recend-recstart)
                         totcov=totcov+cover
                   if totcov==totalTime:
                      key="%d,%d,%d"%(base,stime,etime)
                      cases[key]=reclist
       #
       #  Other parms get forecast periods based on the forecast grids
       #  that were actually made
       #
       else:
          if dateStyleLow=="verifying on":
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                recList=self.getObsStatPeriod(model,parm,obsmodel,fromDay,
                                              numDays,mask=rightRecord)
             else:
                recList=self.getObsStatList(model,parm,obsmodel,dayList,
                                            mask=rightRecord,
                                            callbackMethod=callbackMethod)
          else:
             if dateTypeLow=="period length":
                if callbackMethod is not None:
                   exit=callbackMethod("1 of 1")
                   if exit==1:
                      return cases
                starttime=fromDay-((numDays-1)*DAYSECS)
                endtime=fromDay+DAYSECS-1+(self.MAXFORECASTHOUR*HOURSECS)
                recList=self.listStatRecords(parm,model,obsmodel,starttime,
                                             endtime,"forecast",rightRecord)
             else:
                recList=[]
                count=0
                totalDays=len(dayList)
                for date in dayList:
                   count=count+1
                   if callbackMethod is not None:
                      exit=callbackMethod("%d of %d"%(count,totalDays))
                      if exit==1:
                         return cases
                   if type(date) is types.StringType:
		      try:
                         (yea,mon,day)=date.split("/")
                         starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		      except:
		         continue
                   else:
                      starttime=date
                   endtime=starttime+(self.MAXFORECASTHOUR*HOURSECS)
                   tmprecs=self.listStatRecords(parm,model,starttime,endtime,
                                                "forecast",rightRecord)
                   for rec in tmprecs:
                      recList.append(rec)
          #
          #  Now make case entries for each of these records
          #
          for rec in recList:
             rec = int(rec)
             base=self.sncBtime[rec]
             stime=self.sncStime[rec]
             etime=self.sncEtime[rec]
             key="%d,%d,%d"%(base,stime,etime)
             cases[key]=[rec]
       self.logMsg("end getStatModelCases",10)
       return cases
    #=================================================================
    # getVerifyingTimeRanges - get list of time periods (start/end)
    #      that match date criteria and actually have observed data
    #
    def getVerifyingTimeRanges(self,obsParm,obsModel,dataType,
                               rateFlag,accumHours,dateStyle,dateType,
                               numDays,fromDay,dayList):
       self.logMsg("start getVerifyingTimeRanges",10)
       perList=[]
       if not self.checkFile(obsParm,obsModel):
          self.logMsg("could not open %s file for %s"%(obsParm,obsModel),10)
          return perList
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       #
       #  If a probability parm, or an accumulative parm, then we
       #  cannot use the observed records for the time range - but
       #  must create timePeriod blocks for the specified times.
       #
       if ((dataType==1)or(rateFlag==1)):
          periods=self.createObsPeriods(accumHours,dateStyle,dateType,
                                        fromDay,numDays,dayList)
          for per in periods:
             (start,end)=per
             verList=self.getVerGridInfo(obsModel,start,obsParm,start,end)
             if self.isCoverred(start,end,verList):
                perList.append((start,end))
       #
       #  Other parms get verifying periods based on the observed grids
       #  that actually exist
       #
       else:
          if dateStyleLow=="verifying on":
             if dateTypeLow=="period length":
                obrecs=self.getObsPeriod(obsModel,obsParm,fromDay,numDays)
             else:
                obrecs=self.getObsList(obsModel,obsParm,dayList)
          else:
             if dateTypeLow=="period length":
                (yea,mon,day)=fromDay.split("/")
                starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))-((numDays-1)*DAYSECS)
                endtime=starttime+(numDays*DAYSECS)+(self.MAXFORECASTHOUR*HOURSECS)
                obrecs=self.listRecords(obsModel,obsParm,starttime,endtime,"verify")
             else:
                obrecs=[]
                for date in dayList:
                   if type(date) is types.StringType:
		      try:
                         (yea,mon,day)=date.split("/")
                         starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		      except:
		         continue
                   else:
                      starttime=date
                   endtime=starttime+(self.MAXFORECASTHOUR*HOURSECS)+eoff
		   starttime=starttime+soff
                   tmprecs=self.listRecords(obsModel,obsParm,starttime,endtime,"verify")
                   for obrec in tmprecs:
                      if obrec not in obrecs:
                         obrecs.append(obrec)
             obrecs.sort(lambda x,y: cmp(self.oncStime[x],self.oncStime[y]))
             #
             #  Add time ranges to list
             #
             for obrec in obrecs:
                start=self.oncStime[obrec]
                end=self.oncEtime[obrec]
                perList.append((start,end))
                
       if self.DEBUG>=10:
          for per in perList:
             (start,end)=per
             (gyea,gmon,gday,ghou,gmin,gsec,gwda,gyda,gdst)=time.gmtime(start)
             (eyea,emon,eday,ehou,emin,esec,ewda,eyda,edst)=time.gmtime(end)
             self.logMsg("  %4.4d/%2.2d/%2.2d %2.2d --> %4.4d/%2.2d/%2.2d %2.2d"%(gyea,gmon,gday,ghou,
                                                                            eyea,emon,eday,ehou),10)
       self.logMsg("end getVerifyingTimeRanges",10)
       return perList
    #=================================================================
    #  createObsPeriods - make list of time periods that are accumHours
    #                     width, and start with accumFreq frequency,
    #                     and cover the time periods requested.  Each
    #                     period in the list is (start,end) times.
    #                      time periods of accumHours width...
    #                     coverring the time periods specified by
    #                     dateStyle, dateType, and fromDay/numDays
    #                     or dayList.
    #
    def createObsPeriods(self,accumHours,accumFreq,dateStyle,dateType,
                         fromDay,numDays,dayList):
       self.logMsg("start createObsPeriods",10)
       accumTime=accumHours*HOURSECS
       accumFreqSecs=accumFreq*HOURSECS
       periods=[]
       dateStyleLow=dateStyle.lower()
       dateTypeLow=dateType.lower()
       if dateStyleLow=="verifying on":          
          if dateTypeLow=="period length":
             endtime=fromDay+DAYSECS
             starttime=fromDay-((numDays-1)*DAYSECS)
             for pstart in range(starttime,endtime,accumFreqSecs):
                pend=pstart+accumTime
                if pend<=endtime:
                   periods.append((pstart,pend))
          else:
             dayList.sort()
             timeRanges=[]
             lastend=0
             for date in dayList:
                if type(date) is types.StringType:
		   try:
                      (yea,mon,day)=date.split("/")
                      starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		   except:
		      continue
                else:
                   starttime=date
                endtime=starttime+DAYSECS
                if starttime!=lastend:
                   timeRanges.append((starttime,endtime))
                   lastend=endtime
                else:
                   (sl,el)=timeRanges[-1]
                   timeRanges[-1]=(sl,endtime)
                   lastend=endtime
             for timerange in timeRanges:
                (starttime,endtime)=timerange
                for pstart in range(starttime,endtime,accumFreqSecs):
                   pend=pstart+accumTime
                   if pend<=endtime:
                      periods.append((pstart,pend))
       else:
          if dateTypeLow=="period length":
             starttime=fromDay-((numDays-1)*DAYSECS)
             endtime=starttime+(numDays*DAYSECS)+(self.MAXFORECASTHOUR*HOURSECS)
             for pstart in range(starttime,endtime,accumFreqSecs):
                pend=pstart+accumTime
                if pend<=endtime:
                   periods.append((pstart,pend))
          else:
             dayList.sort()
             starts=[]
             for date in dayList:
                if type(date) is types.StringType:
		   try:
                      (yea,mon,day)=date.split("/")
                      starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		   except:
		      continue
                else:
                   starttime=date
                endtime=starttime+(self.MAXFORECASTHOUR*HOURSECS)
                for pstart in range(starttime,endtime,accumFreqSecs):
                   if pstart not in starts:
                      pend=pstart+accumTime
                      if pend<=endtime:
                         periods.append((pstart,pend))
                         starts.append(pstart)
       self.logMsg("end createObsPeriods",10)
       return periods
    #=================================================================
    #
    def createFromPeriods(self,dateType,fromDay,numDays,dayList):
       periods=[]
       dateTypeLow=dateType.lower()
       if dateTypeLow=="period length":
          starttime=fromDay-((numDays-1)*DAYSECS)
          endtime=fromDay+DAYSECS
          periods.append((starttime,endtime))
       else:
          for date in dayList:
             if type(date) is types.StringType:
	        try:
                   (yea,mon,day)=date.split("/")
                   starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
		except:
		   continue
             else:
                starttime=date
             endtime=starttime+DAYSECS
             periods.append((starttime,endtime))
       return periods
    #=================================================================
    #  getFcstrCycleFhr - get logical array of records that have the
    #                     rightFcstr,rightCycle,rightFhr
    #
    def getFcstrCycleFhr(self,model,fcstrs,cycles,fhrStart,fhrEnd):
       self.logMsg("start getFcstCycleFhr",10)
       #
       #  Get logical array of records with right forecaster
       #
       fcstrList=[]
       if ((type(fcstrs) is types.TupleType)or(type(fcstrs) is types.ListType)):
          for fcstr in fcstrs:
             fcstrList.append(fcstr)
       else:
          fcstrList.append(fcstrs)
       if ((model!="Official")or(-1 in fcstrList)):
          rightFcstr=ones(self.fncFcstr.shape[0])
       else:
          rightFcstr=zeros(self.fncFcstr.shape[0])
          for fnum in fcstrList:
             rightFcstr=logical_or(sometrue(equal(self.fncFcstr,fnum),-1),rightFcstr)
       #
       #  Get logical array of records with right cycle
       #
       cycleList=[]
       if ((type(cycles) is types.TupleType)or(type(cycles) is types.ListType)):
          for cycle in cycles:
	     if type(cycle) is types.StringType:
	        cycleList.append(int(cycle))
	     else:
                cycleList.append(cycle)
       else:
          if type(cycles) is types.StringType:
	     cycleList.append(int(cycles))
	  else:
	     cycleList.append(cycles)
       if (-1 in cycleList):
          rightCycle=ones(self.fncBtime.shape)
       else:
          rightCycle=zeros(self.fncBtime.shape)
          for cycle in cycleList:
             cyc=cycle*HOURSECS
             rem=remainder(self.fncBtime,DAYSECS).astype('i')
             rightCycle=where(equal(rem,cyc),1,rightCycle)
       #
       #  get logical array of records with right forecast hours
       #
       if fhrEnd<0:
          fhrEnd=self.MAXFORECASTHOUR
       fhr=(self.fncStime[:]-self.fncBtime[:])/float(HOURSECS)
       rightFhr=logical_and(greater_equal(fhr,fhrStart),less_equal(fhr,fhrEnd))
       #
       #  return combined logical array
       #
       rightRecord=logical_and(logical_and(rightFcstr,rightCycle),rightFhr)
       self.logMsg("end getFcstCycleFhr",10)
       return rightRecord
    #=================================================================
    #  getStatFcstrCycleFhr - get logical array of statistic records
    #                         that have the rightFcstr,rightCycle,
    #                         rightFhr
    #
    def getStatFcstrCycleFhr(self,model,fcstrs,cycles,fhrStart,fhrEnd):
       self.logMsg("start getStatFcstCycleFhr",10)
       #
       #  Get logical array of records with right forecaster
       #
       fcstrList=[]
       ftype=type(fcstrs)
       if ((ftype is types.TupleType)or(ftype is types.ListType)):
          for fcstr in fcstrs:
             fcstrList.append(fcstr)
       else:
          fcstrList.append(fcstrs)
       if ((model!="Official")or(-1 in fcstrList)):
          rightFcstr=ones(self.sncFcstr.shape[0])
       else:
          rightFcstr=zeros(self.sncFcstr.shape[0])
          for fnum in fcstrList:
             rightFcstr=logical_or(sometrue(equal(self.sncFcstr,fnum),-1),rightFcstr)
       #
       #  Get logical array of records with right cycle
       #
       cycleList=[]
       ctype=type(cycles)
       if ((ctype is types.TupleType)or(ctype is types.ListType)):
          for cycle in cycles:
	     if type(cycle) is types.StringType:
	        cycleList.append(int(cycle))
	     else:
                cycleList.append(cycle)
       else:
          if type(cycles) is types.StringType:
	     cycleList.append(int(cycles))
	  else:
             cycleList.append(cycles)
       if (-1 in cycleList):
          rightCycle=ones(self.sncBtime.shape)
       else:
          rightCycle=zeros(self.sncBtime.shape)
          for cycle in cycleList:
             cyc=cycle*HOURSECS
             rem=remainder(self.sncBtime,DAYSECS).astype('i')
             rightCycle=where(equal(rem,cyc),1,rightCycle)
       #
       #  get logical array of records with right forecast hours
       #
       fhr=(self.sncStime[:]-self.sncBtime[:])/float(HOURSECS)
       rightFhr=logical_and(greater_equal(fhr,fhrStart),less_equal(fhr,fhrEnd))
       #
       #  return combined logical array
       #
       rightRecord=logical_and(logical_and(rightFcstr,rightCycle),rightFhr)
       self.logMsg("end getFcstCycleFhr",10)
       return rightRecord
    #==================================================================
    #  getRateFlag - given a model name and parm name, return a flag
    #                with 1 if this is a rateParm.  If parms can't be
    #                found - return 0, just like if a parm isn't a
    #                rateParm
    #
    def getRateFlag(self,model,parm):
       parmData=self.getParm(model,parm,"SFC")
       if parmData is not None:
          rateFlag=parmData.getGridInfo().isRateParm()
          return rateFlag
       else:
          return 0
    #=================================================================
    # getStatID - given a stat name like "Areal POD", return a
    #             consistent, lowercase, unique statID used elsewhere
    #             in the system.  Return None if not valid
    #
    def getStatID(self,statName):
       #
       #  make statName lowercase
       #
       statname=statName.lower()
       #
       #  Check that the name is somewhere in all the stat names
       #
       if statname not in self.allStats:
          return None
       #
       #  Find the ID that contains this statname
       #
       statID=""
       for testID in self.statIDs:
          if statname in self.statNames[testID]:
             statID=testID
             break
       if statID=="":
          return None
       return statID
    #==================================================================
    #  getVerStat - Main routine to get a statistic from BOIVerify
    #               for a particular model-run for a particular time
    #               for a particular area.  Tries to get it from the
    #               stat database if it can - otherwise tries to
    #               calculate it from the grids
    #
    #               for vectors, if vectorType is:
    #                  -1  Calculate the stat on the magnitude of the
    #                      vector error.
    #                   0  Calculate the stat on the magnitude error
    #                   1  Calculate the stat on the direction error
    #
    def getVerStat(self,model,basetime,parm,trStart,trEnd,obsmodel,
                   statName,statVal=0,statCond="",editArea=None,
                   smooth=0,vectorType=-1,forceCalc=0,srecList=None,
                   grecList=None):
       self.logMsg("start getVerStat",10)
       retVal=None
       #
       #  Check for stats we know how to calculate
       #
       statID=self.getStatID(statName)
       if statID is None:
          self.logMsg("unknown statName:%s"%statName,2)
          return retVal
       #
       #  Stuff about the parm
       #
       rateFlag=self.getRateFlag(model,parm)
       verType=self.getVerType(parm)
       obsParm=self.getObsParm(parm)
       dataType=self.getVerParmType(parm)
       #
       #  if editArea is None - calculate for the whole grid
       #
       if editArea is None:
          editArea = self.encodeEditArea(self.__refSetMgr.fullRefSet())
       #
       #  if editArea is an array - then it must be a mask of
       #  points to calculate over
       #
       if type(editArea) is ndarray:
          eaGrid=editArea
       #
       #  If editArea is a string...see if BOIVerify pre-calculates
       #  statistics for that editArea name
       #
       elif type(editArea) is StringType:
          eas=self.listEditAreas()
          if editArea in eas:
             eaNum=self.getEditAreaNumberFromName(editArea)
             if (forceCalc==0)and(smooth==0):
                if dataType==1:
                   if vectorType==0:
                      statParm=parm+"Spd"
                   elif vectorType==1:
                      statParm=parm+"Dir"
                   else:
                      statParm=parm
                else:
                   statParm=parm
                retVal=self.readVerStat(model,basetime,statParm,trStart,
                                         trEnd,obsmodel,eaNum,
                                         statID,statVal,vectorType=vectorType,
                                        srecList=srecList)
                if retVal is not None:
                   self.logMsg("got the stat from saved stats",5)
                   return retVal
                else:
                   self.logMsg("tried to get it from saved stats - but failed",5)
          else:
             self.logMsg("not a saved edit area",2)
	  #
	  #  See if the named editArea even exists in the GFE system
	  #
          allEditAreaNames=self.editAreaList()
          if editArea not in allEditAreaNames:
             self.logMsg("editArea %s does not exist"%editArea,2)
             return retVal
          eaGrid=self.encodeEditArea(editArea)
       else:
          self.logMsg("invalid type of editArea provided to getVerStat")
          return retVal
       #
       #  OK...We have to calculate the stat over the eaGrid mask
       #  Make sure that it has at least 1 point.
       #
       numpts=add.reduce(add.reduce(eaGrid))
       if numpts<1:
          self.logMsg("No points specified - so no stats",2)
          return retVal
       fnum=float(numpts)
       #
       #  If some records were provided...split them out
       #
       if grecList is not None:
          (frecList,orecList)=grecList
       else:
          frecList=None
          orecList=None
       #
       #  Get obs grid
       #
       if rateFlag==1:
          gridMode="Sum"
       else:
          gridMode="TimeWtAverage"
       obsGrid=self.getVerGrids(obsmodel,trStart,obsParm,trStart,
                                trEnd,mode=gridMode,recList=orecList)
       if obsGrid is None:
          self.logMsg("could not read observed %s grid for %s"%(obsmodel,obsParm),2)
          return retVal
       if (dataType==1):
          (mag,direc)=obsGrid
          if mag is None:
             self.logMsg("could not read observed %s grid for %s"%(obsmodel,obsParm),2)
             return retVal
       #
       #  get Fcst grid
       #
       if verType==1:
          gridMode="Max"
       else:
          if rateFlag==1:
             gridMode="Sum"
          else:
             gridMode="TimeWtAverage"
       fcstGrid=self.getVerGrids(model,basetime,parm,trStart,trEnd,
                                 mode=gridMode,recList=frecList)
       if fcstGrid is None:
          self.logMsg("could not read %s grid for %s"%(model,parm),2)
          return retVal
       if (dataType==1):
          (mag,direc)=fcstGrid
          if mag is None:
             self.logMsg("could not read observed %s grid for %s"%(model,parm),2)
             return retVal
       #
       #  Basic point stats  
       #
       if statID in ["bias","mae","rms","mse","peb"]:
          #
          #  Handle various types of vector errors
          #
          vectorErr=0
          if dataType==1:
             (omag,odirec)=obsGrid
             (fmag,fdirec)=fcstGrid
             if vectorType==0:
                obsGrid=omag
                fcstGrid=fmag
             elif vectorType==1:
                obsGrid=odirec
                fcstGrid=fdirec
                err=fcstGrid-obsGrid
                err=where(greater(err,180.0),360.0-err,err)
                err=where(less(err,-180.0),-(360.0+err),err)
                vectorErr=1
             else:
                (ou,ov)=self.MagDirToUV(omag,odirec)
                (fu,fv)=self.MagDirToUV(fmag,fdirec)
                (mag,direc)=self.UVToMagDir(fu-ou,fv-ov)
                err=mag
                vectorErr=1
          #
          #  If smoothing is on...smooth obs and fcst grids
          #     inside the edit area...
          #     but for direction errors, and magnitude of
          #     vector errors (vectorErr==1)...smooth the
          #     errors, not the obs/fcst grids.
          #
          if smooth>0:
             ismooth=int(smooth)
             if vectorErr==0:
                obsGrid=self.smoothpm(obsGrid,ismooth,mask=eaGrid)
                fcstGrid=self.smoothpm(fcstGrid,ismooth,mask=eaGrid)
             else:
                err=self.smoothpm(err,ismooth,mask=eaGrid)
          #
          #  For probability parms - need to calculate the obs grid
          #     based on the observed parameter
          #
          if verType==1:
             cond=self.getObsCondition(parm)
             thresh=self.getObsThreshold(parm)
             if cond==">":
                obsGrid=greater(obsGrid,thresh)*100
             elif cond==">=":
                obsGrid=greater_equal(obsGrid,thresh)*100
             elif cond=="<":
                obsGrid=less(obsGrid,thresh)*100
             elif cond=="<=":
                obsGrid=less_equal(obsGrid,thres)*100
          #
          #  get the error - but vector err magnitude has already
          #  been done...so don't do that...
          #
          if vectorErr==0:
             err=where(eaGrid,fcstGrid-obsGrid,0)
          else:
             err=where(eaGrid,err,0)
          #
          #  Now all the stat calculations
          #
          if statID=="bias":
             retVal=add.reduce(add.reduce(err))/fnum
             return retVal
          if statID=="mae":
             err=where(less(err,0.0),-err,err)
             retVal=add.reduce(add.reduce(err))/fnum
             return retVal
          if statID=="rms":
             err=err*err
             retVal=sqrt(add.reduce(add.reduce(err))/fnum)
             return retVal
          if statID=="mse":
             err=err*err
             retVal=add.reduce(add.reduce(err))/fnum
             return retVal
          if statID=="peb":
             err=where(less(err,0.0),-err,err)
             good=logical_and(less(err,statVal),eaGrid)
             retVal=add.reduce(add.reduce(good))/fnum
             return retVal
       elif statID in ["fc","afc","freqo","freqf","freqbias","afreqbias","pod","apod","far","afar",
                         "pofd","apofd","ts","ats","ets","aets","hk","ahk",
                         "hss","ahss","oddsratio","aoddsratio","hits","ahits",
                         "miss","amiss","fals","afals","corn","acorn","cont","acont"]:
          #
          #  threshold for vectors is with magnitude
          #
          if dataType==1:
             (omag,odirec)=obsGrid
             (fmag,fdirec)=fcstGrid
             obsGrid=omag
             fcstGrid=fmag
          #
          #  If smoothing is on...smooth obs and fcst grids
          #     inside the edit area
          #
          if statName[0:1]!="a":
             if smooth>0:
                ismooth=int(smooth)
                obsGrid=self.smoothpm(obsGrid,ismooth,mask=eaGrid)
                fcstGrid=self.smoothpm(fcstGrid,ismooth,mask=eaGrid)
          #
          #  Get grids of yes/no forecast/occurrence
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
          if statName[0:1]=="a":
             if smooth>0:
                ismooth=int(smooth)
                obsOccur=self.arealOccur(obsOccur,ismooth,mask=eaGrid)
                fcstOccur=self.arealOccur(fcstOccur,ismooth,mask=eaGrid)
          #
          #  Calculate hits/misses/falsealarms/correctnegatives
          #
          notFcst=logical_not(fcstOccur)
          notObs=logical_not(obsOccur)
          hits=add.reduce(add.reduce(where(eaGrid,logical_and(fcstOccur,obsOccur),0)))
          miss=add.reduce(add.reduce(where(eaGrid,logical_and(notFcst,obsOccur)  ,0)))
          falr=add.reduce(add.reduce(where(eaGrid,logical_and(fcstOccur,notObs)  ,0)))
          corn=add.reduce(add.reduce(where(eaGrid,logical_and(notFcst,notObs)    ,0)))
          total=hits+miss+falr+corn
          if abs(float(total)-fnum)>0.5:
             self.logMsg("Number in binary histogram not the same as number of points")
             return 0.0
          #
          #  Get the Binary stat and return it
          #
          ret=self.getBinaryStat(statID,hits,miss,falr,corn)
          return ret
       else:
          self.logMsg("Have not yet implemented stat:%s"%statName,0)
          return retVal
    #==================================================================
    #  getBinaryStat - given values of hits/miss/falr/corn, and a
    #                  correct statID (it better be right!) - to the
    #                  calculations and return a value.  In cases where
    #                  no forecasts have been made - return the perfect
    #                  score!
    #
    def getBinaryStat(self,statID,hits,miss,falr,corn):
       total=hits+miss+falr+corn
       if statID in ["hits","ahits"]:
          return hits
       if statID in ["miss","amiss"]:
          return miss
       if statID in ["fals","afals"]:
          return falr
       if statID in ["corn","acorn"]:
          return corn
       if statID in ["cont","acont"]:
          return (hits,miss,falr,corn)
       if statID in ["fc","afc"]:
          if total<1:
             return 1.0
          return float(hits+corn)/float(total)
       if statID in ["freqo",]:
          if total<1:
             return 1.0
          return float(hits+miss)/float(total)
       if statID in ["freqf",]:
          if total<1:
             return 1.0
          return float(hits+falr)/float(total)
       if statID in ["freqbias","afreqbias"]:
          denom=hits+miss
          if denom<1:
             return 1.0
          return float(hits+falr)/float(denom)
       if statID in ["pod","apod"]:
          denom=hits+miss
          if denom<1:
             return 1.0
          return float(hits)/float(denom)
       if statID in ["far","afar"]:
          denom=falr+hits
          if denom<1:
             return 0.0
          return float(falr)/float(denom)
       if statID in ["pofd","apofd"]:
          denom=falr+corn
          if denom<1:
             return 0.0
          return float(falr)/float(denom)
       if statID in ["ts","ats"]:
          denom=hits+miss+falr
          if denom<1:
             return 1.0
          return float(hits)/float(denom)
       if statID in ["ets","aets"]:
          hitsrand=float((hits+miss)*(hits+falr))/float(total)
          denom=hits+miss+falr-hitsrand
          if ((denom>-0.1)and(denom<0.1)):
             return 1.0
          return float(hits-hitsrand)/float(denom)
       if statID in ["hk","ahk"]:
          denom=falr+corn
          if denom<1:
             pofd=0.0
          else:
             pofd=float(falr)/float(denom)
          denom=hits+miss
          if denom<1:
             pod=1.0
          else:
             pod=float(hits)/float(denom)
          return pod-pofd
       if statID in ["hss","ahss"]:
          ecrand=float(((hits+miss)*(hits+falr))+((corn+miss)*(corn+falr)))/float(total)
          denom=float(total)-ecrand
          if ((denom>-0.1)and(denom<0.1)):
             return 1.0
          return float(hits+corn-ecrand)/float(denom)
       if statID in ["oddsratio","aoddsratio"]:
          if ((hits==0)or(corn==0)or(falr==0)or(miss==0)):
             return 200.0
          return float(hits*corn)/float(falr*miss)
       return None
    #==================================================================
    #  getGridBinaryStat - given grids of hits/miss/falr/corn, and a
    #                      correct statID (it better be right!) - do
    #                      the calculations and return a grid of results.
    #                      In cases where no forecasts have been made -
    #                      return the perfect score!
    #
    def getGridBinaryStat(self,statID,hits,miss,falr,corn):
       total=hits+miss+falr+corn
       if statID in ["hits","ahits"]:
          return hits/1.0
       if statID in ["miss","amiss"]:
          return miss/1.0
       if statID in ["fals","afals"]:
          return falr/1.0
       if statID in ["corn","acorn"]:
          return corn/1.0
       if statID in ["fc","afc"]:
          nofcst=less(total,1)
          total=where(nofcst,1,total)
          score=(hits+corn)/total
          score=where(nofcst,1.0,score)
          return score
       if statID in ["freqo",]:
          nofcst=less(total,1)
          total=where(nofcst,1,total)
          score=(hits+miss)/total
          score=where(nofcst,0.0,score)
          return score
       if statID in ["freqf",]:
          nofcst=less(total,1)
          total=where(nofcst,1,total)
          score=(hits+falr)/total
          score=where(nofcst,0.0,score)
          return score
       if statID in ["freqbias","afreqbias"]:
          denom=hits+miss
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          score=(hits+falr)/denom
          score=where(nofcst,1.0,score)
          return score
       if statID in ["pod","apod"]:
          denom=hits+miss
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          score=hits/denom
          score=where(nofcst,1.0,score)
          return score
       if statID in ["far","afar"]:
          denom=falr+hits
          nofcst=less(denom,1)
          denom==where(nofcst,1,denom)
          score=falr/denom
          score=where(nofcst,0.0,score)
          return score
       if statID in ["pofd","apofd"]:
          denom=falr+corn
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          score=falr/denom
          score=where(nofcst,0.0,score)
          return score
       if statID in ["ts","ats"]:
          denom=hits+miss+falr
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          score=hits/denom
          score=where(nofcst,1.0,score)
          return score
       if statID in ["ets","aets"]:
          total=where(less(total,1),1,total)
          hitsrand=((hits+miss)*(hits+falr))/total
          denom=hits+miss+falr-hitsrand
          nofcst=logical_and(greater(denom,-0.1),less(denom,0.1))
          denom=where(nofcst,1,denom)
          score=(hits-hitsrand)/denom
          score=where(nofcst,1.0,score)
          return score
       if statID in ["hk","ahk"]:
          #pofd
          denom=falr+corn
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          pofd=falr/denom
          pofd=where(nofcst,0.0,pofd)
          #pod
          denom=hits+miss
          nofcst=less(denom,1)
          denom=where(nofcst,1,denom)
          pod=hits/denom
          pod=where(nofcst,1.0,pod)
          score=pod-pofd
          return score
       if statID in ["hss","ahss"]:
          total=where(less(total,1),1,total)
          ecrand=(((hits+miss)*(hits+falr))+((corn+miss)*(corn+falr)))/total
          denom=total-ecrand
          nofcst=logical_and(greater(denom,-0.1),less(denom,0.1))
          denom=where(nofcst,1,denom)
          score=(hits+corn-ecrand)/denom
          score=where(nofcst,1.0,score)
          return score
       if statID in ["oddsratio","aoddsratio"]:
          no1=logical_or(less(hits,0.5),less(corn,0.5))
          no2=logical_or(less(falr,0.5),less(miss,0.5))
          nofcst=logical_or(no1,no2)
          denom=falr*miss
          denom=where(less(denom,1),1,denom)
          score=(hits*corn)/denom
          score=where(nofcst,200.0,score)
          return score
    #==================================================================
    #  getVerStatScales - Main routine to get a calculate a statistic
    #                     for many scales.  Same as getVerStat except
    #                     that smooth is provided as a list of smooth
    #                     numbers.  This is always calculated - never
    #                     read from stat files.
    #
    def getVerStatScales(self,model,basetime,parm,trStart,trEnd,obsmodel,
                   statName,statVal=0,statCond="",editArea=None,
                   smoothList=[0,],vectorType=-1,
                   grecList=None):
       self.logMsg("start getVerStatScales",10)
       retVal=[]
       #
       #  Check for stats we know how to calculate
       #
       statID=self.getStatID(statName)
       if statID is None:
          self.logMsg("unknown statName:%s"%statName,2)
          return retVal
       #
       #  Stuff about the parm
       #
       rateFlag=self.getRateFlag(model,parm)
       verType=self.getVerType(parm)
       obsParm=self.getObsParm(parm)
       dataType=self.getVerParmType(parm)
       #
       #  check if editArea is one that we routinely calculate
       #
       if editArea is None:
          editArea = self.encodeEditArea(self.__refSetMgr.fullRefSet())

       if type(editArea) is ndarray:
          eaGrid=editArea
       elif type(editArea) is StringType:
          #
          # get list of editAreas from smartScript
          #
          ealist=self.editAreaList()
          if editArea not in ealist:
             self.logMsg("editArea %s does not exist"%editArea,2)
             return retVal
          eaGrid=self.encodeEditArea(editArea)
       else:
          self.logMsg("invalid type of editArea provided to getVerStatScales")
          return retVal
       #
       #  OK...We have to calculate the stat from the grids
       #
       numpts=add.reduce(add.reduce(eaGrid))
       if numpts<1:
          self.logMsg("No points specified - so no stats",2)
          return retVal
       fnum=float(numpts)
       #
       #  If some records were provided...split them out
       #
       if grecList is not None:
          (frecList,orecList)=grecList
       else:
          frecList=None
          orecList=None
       #
       #  Get obs grid
       #
       if rateFlag==1:
          gridMode="Sum"
       else:
          gridMode="TimeWtAverage"
       obsGrid=self.getVerGrids(obsmodel,trStart,obsParm,trStart,
                                trEnd,mode=gridMode,recList=orecList)
       if obsGrid is None:
          self.logMsg("could not read observed %s grid for %s"%(obsmodel,obsParm),2)
          return retVal
       if (dataType==1):
          (mag,direc)=obsGrid
          if mag is None:
             self.logMsg("could not read observed %s grid for %s"%(obsmodel,obsParm),2)
             return retVal
       #
       #  get Fcst grid
       #
       if verType==1:
          gridMode="Max"
       else:
          if rateFlag==1:
             gridMode="Sum"
          else:
             gridMode="TimeWtAverage"
       fcstGrid=self.getVerGrids(model,basetime,parm,trStart,trEnd,
                                 mode=gridMode,recList=frecList)
       if fcstGrid is None:
          self.logMsg("could not read %s grid for %s"%(model,parm),2)
          return retVal
       if (dataType==1):
          (mag,direc)=fcstGrid
          if mag is None:
             self.logMsg("could not read observed %s grid for %s"%(model,parm),2)
             return retVal
       #
       #  Loop over scales
       #
       for smooth in smoothList:
          self.logMsg("smooth=%d"%smooth)
          #
          #  Basic point stats  
          #
          if statID in ["bias","mae","rms","mse","peb"]:
             #
             #  Handle various types of vector errors
             #
             vectorErr=0
             if dataType==1:
                (omag,odirec)=obsGrid
                (fmag,fdirec)=fcstGrid
                if vectorType==0:
                   obsGrid=omag
                   fcstGrid=fmag
                elif vectorType==1:
                   obsGrid=odirec
                   fcstGrid=fdirec
                   err=fcstGrid-obsGrid
                   err=where(greater(err,180.0),360.0-err,err)
                   err=where(less(err,-180.0),-(360.0+err),err)
                   vectorErr=1
                else:
                   (ou,ov)=self.MagDirToUV(omag,odirec)
                   (fu,fv)=self.MagDirToUV(fmag,fdirec)
                   (mag,direc)=self.UVToMagDir(fu-ou,fv-ov)
                   err=mag
                   vectorErr=1
             #
             #  If smoothing is on...smooth obs and fcst grids
             #     inside the edit area...
             #     but for direction errors, and magnitude of
             #     vector errors (vectorErr==1)...smooth the
             #     errors, not the obs/fcst grids.
             #
             if smooth>0:
                ismooth=int(smooth)
                if vectorErr==0:
                   obsGrid=self.smoothpm(obsGrid,ismooth,mask=eaGrid)
                   fcstGrid=self.smoothpm(fcstGrid,ismooth,mask=eaGrid)
                else:
                   err=self.smoothpm(err,ismooth,mask=eaGrid)
             #
             #  For probability parms - need to calculate the obs grid
             #     based on the observed parameter
             #
             if verType==1:
                cond=self.getObsCondition(parm)
                thresh=self.getObsThreshold(parm)
                if cond==">":
                   obsGrid=greater(obsGrid,thresh)*100
                elif cond==">=":
                   obsGrid=greater_equal(obsGrid,thresh)*100
                elif cond=="<":
                   obsGrid=less(obsGrid,thresh)*100
                elif cond=="<=":
                   obsGrid=less_equal(obsGrid,thres)*100
             #
             #  get the error - but vector err magnitude has already
             #  been done...so don't do that...
             #
             if vectorErr==0:
                err=where(eaGrid,fcstGrid-obsGrid,0)
             else:
                err=where(eaGrid,err,0)
             #
             #  Now all the stat calculations
             #
             if statID=="bias":
                retVal.append(add.reduce(add.reduce(err))/fnum)
             elif statID=="mae":
                err=where(less(err,0.0),-err,err)
                retVal.append(add.reduce(add.reduce(err))/fnum)
             elif statID=="rms":
                err=err*err
                retVal.append(sqrt(add.reduce(add.reduce(err))/fnum))
             if statID=="mse":
                err=err*err
                retVal.append(add.reduce(add.reduce(err))/fnum)
             if statID=="peb":
                err=where(less(err,0.0),-err,err)
                good=logical_and(less(err,statVal),eaGrid)
                retVal.append(add.reduce(add.reduce(good))/fnum)
          elif statID in ["fc","afc","freqo","freqf","freqbias","afreqbias","pod","apod","far","afar",
                            "pofd","apofd","ts","ats","ets","aets","hk","ahk",
                            "hss","ahss","oddsratio","aoddsratio","hits","ahits",
                            "miss","amiss","fals","afals","corn","acorn","cont","acont"]:
             #
             #  threshold for vectors is with magnitude
             #
             if dataType==1:
                (omag,odirec)=obsGrid
                (fmag,fdirec)=fcstGrid
                obsGrid=omag
                fcstGrid=fmag
             #
             #  If smoothing is on...smooth obs and fcst grids
             #     inside the edit area
             #
             if statName[0:1]!="a":
                if smooth>0:
                   ismooth=int(smooth)
                   obsGrid=self.smoothpm(obsGrid,ismooth,mask=eaGrid)
                   fcstGrid=self.smoothpm(fcstGrid,ismooth,mask=eaGrid)
             #
             #  Get grids of yes/no forecast/occurrence
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
             if statName[0:1]=="a":
                if smooth>0:
                   ismooth=int(smooth)
                   obsOccur=self.arealOccur(obsOccur,ismooth,mask=eaGrid)
                   fcstOccur=self.arealOccur(fcstOccur,ismooth,mask=eaGrid)
             #
             #  Calculate hits/misses/falsealarms/correctnegatives
             #
             notFcst=logical_not(fcstOccur)
             notObs=logical_not(obsOccur)
             hits=add.reduce(add.reduce(where(eaGrid,logical_and(fcstOccur,obsOccur),0)))
             miss=add.reduce(add.reduce(where(eaGrid,logical_and(notFcst,obsOccur)  ,0)))
             falr=add.reduce(add.reduce(where(eaGrid,logical_and(fcstOccur,notObs)  ,0)))
             corn=add.reduce(add.reduce(where(eaGrid,logical_and(notFcst,notObs)    ,0)))
             total=hits+miss+falr+corn
             if abs(float(total)-fnum)>0.5:
                self.logMsg("Number in binary histogram not the same as number of points")
                return 0.0
             #
             #  Get the Binary stat and return it
             #
             ret=self.getBinaryStat(statID,hits,miss,falr,corn)
             retVal.append(ret)
          else:
             self.logMsg("Have not yet implemented stat:%s"%statName,0)
       return retVal
    #================================================================
    #  readVerStat -
    #
    def readVerStat(self,model,basetime,element,trStart,trEnd,
                    obsmodel,eaNum,statName,statVal,vectorType=-1,
                    srecList=None):
       self.logMsg("start readVerStat",10)
       retVal=None
       #
       #  Check for stats calculated by AutoCalc
       #
       if statName not in ["error","err","bias","absolute error",
                           "abs error","mae","root mean squared error",
                           "rms error","rms","mean squared error","mse",
                           "brier","peb","percent error below","percent err below",
                           "% error below","% err below"]:
          return retVal
       #
       #  Make sure file can be openned
       #
       dataType=self.getVerParmType(element)
       parm=element
       if dataType==1:
          if vectorType==0:
             parm=element+"Spd"
          elif vectorType==0:
             parm=element+"Dir"
       if not self.checkStats(parm,model,obsmodel):
          self.logMsg("Could not open stat file for %s using %s observations"%(model,obsmodel),2)
          return retVal
       #
       #  make sure any threshold stats use one of the thresholds
       #  we routinely calculate
       #
       if statName in ["peb","percent error below","percent err below",
                       "% error below","% err below"]:
          threshNum=-1
          thresholds=self.getVerThresholds(element)
          if dataType==1:
             (threshMag,threshDir)=thresholds
             if ((vectorType==-1)or(vectorType==0)):
                thresholds=threshMag
             else:
                thresholds=threshDir
          for i in range(len(thresholds)):
             check=thresholds[i]
             if statVal==check:
                threshNum=i
                break
          if threshNum==-1:
             self.logMsg("Not one of the normal thresholds",2)
             return retVal
       #
       #  Find the records - the most time costly part
       #
       if srecList is None:
          self.logMsg("finding appropriate records",10)
          recbase=equal(self.sncBtime,basetime)
          recfit=logical_and(greater(self.sncEtime,trStart),
                             less(self.sncStime,trEnd))
          recmatch=logical_and(recbase,recfit)
          recnumbers=compress(recmatch,self.sncRecs)
          recnumberList=list(recnumbers)
          self.logMsg("done finding appropriate records",10)
       else:
          self.logMsg("used provided records",10)
          recnumberList=srecList
       if len(recnumberList)<1:
          return retVal
       if len(recnumberList)>1:
          recnumberList.sort(lambda x,y: cmp(self.sncStime[x],self.sncStime[y]))
       #
       #   Read the needed stats
       #
       retVal=0.0
       count=0
       for rec in recnumberList:
          stats=self.sncStats[rec,eaNum,:]
          if statName in ["error","err","bias"]:
             retVal=retVal+stats[0]
          elif statName in ["absolute error","abs error","mae"]:
             retVal=retVal+stats[2]
          elif statName in ["root mean squared error","rms error","rms",
                            "mean squared error","mse","brier"]:
             retVal=retVal+stats[1]
          elif statName in ["peb","percent error below","percent err below",
                            "% error below","% err below"]:
             retVal=retVal+stats[8+threshNum]
       #
       #  If we had to read multiple records...then average over those
       #
       if len(recnumberList)>1:
          retVal=retVal/float(len(recnumberList))
       if statName in ["root mean squared error","rms error","rms"]:
          retVal=sqrt(retVal)
       #
       # ????? still need work here.  Need to multiply by number of cases
       # Need to figure out if the percent stats are right...etc.
       #
       self.logMsg("end readVerStat",10)
       return retVal
    #==================================================================
    #  getVerGridInfo - Similar to getGridInfo of SmartScript...but read
    #                   from the verification archive.  Difference is
    #                   that here you must specify the model and the
    #                   BASETIME of the model, rather than just asking
    #                   for the latest version etc.  It returns a list
    #                   of tuples with info on each grid that intersects
    #                   the time period.  The tuple contains
    #                      (recnum,starttime,endtime) and is sorted by
    #                   the starttime.
    #
    def getVerGridInfo(self,model,basetime,element,stime,etime):
       self.logMsg("getVerGridInfo Start",10)
       #
       #  get parm type (scalar/vector) and set default return values
       #
       dataType=self.getVerParmType(element)
       retVal=[]
       #
       #  Make sure file for parm/model exists
       #
       if not self.checkFile(element,model):
          self.logMsg("Could not open file",5)
          return retVal
       #
       #  Get list of records that intersect the timeRange
       #
       self.logMsg("finding grids that intersect",10)
       if model not in self.OBSMODELS:
          recbase=equal(self.fncBtime,basetime)
          recfit=logical_and(greater(self.fncEtime,stime),
                             less(self.fncStime,etime))
          recmatch=logical_and(recbase,recfit)
          recnumbers=compress(recmatch,self.fncRecs)
          recnumberList=list(recnumbers)
          recnumberList.sort(lambda x,y: cmp(self.fncStime[x],self.fncStime[y]))
          for recnum in recnumberList:
             retVal.append((recnum,self.fncStime[recnum],self.fncEtime[recnum]))
       else:
          recmatch=logical_and(greater(self.oncEtime,stime),
                             less(self.oncStime,etime))
          recnumbers=compress(recmatch,self.oncRecs)
          recnumberList=list(recnumbers)
          recnumberList.sort(lambda x,y: cmp(self.oncStime[x],self.oncStime[y]))
          for recnum in recnumberList:
             retVal.append((recnum,self.oncStime[recnum],self.oncEtime[recnum]))
       #
       #
       #for ret in retVal:
       #   (rec,st,en)=ret
       #   self.logMsg("  (%d,%d,%d)"%(rec,st,en),10)
       self.logMsg("getVerGridInfo - End",10)
       return retVal
    #==================================================================
    #  isCoverred(start,end,infoList) - given a start/end time and a list
    #         of getVerGridInfo about grids in this time period - return
    #         1 or 0 if the start/end period is completely coverred.
    #
    def isCoverred(self,start,end,infoList):
       self.logMsg("isCoverred",10)
       if len(infoList)<1:
          return 0
       total=end-start
       totcov=0
       for info in infoList:
          (rec,recstart,recend)=info
          totcov=totcov+min(end-recstart,recend-start)
       if totcov>=total:
          return 1
       return 0
    #==================================================================
    # getReadMode - figure out if parm is a rateParm...and set mode
    #                     to "Sum" if it is.
    #               If not...and checkProb is set...figure out if the
    #                     parm is a probability parm and set mode to
    #                     "Max" if it is (floating PoP).
    #               Otherwise set to "Average"
    #
    def getReadMode(self,model,parmName,checkProb=1):
       rateFlag=self.getRateFlag(model,parmName)
       if (rateFlag==1):
          readMode="Sum"
       else:
          readMode="TimeWtAverage"
          if checkProb==1:
             verType=self.getVerType(parmName)
             if verType is not None:
                if verType==1:
                   readMode="Max"
       return readMode
    #==================================================================
    #  getVerGrids - Similar to getGrids of SmartScript...but read from
    #                the verification archive.  Difference is that here
    #                you must specify the model and the BASETIME of the
    #                model, rather than just asking for the latest
    #                version etc.  There are other routines to help you
    #                figure out the basetime
    #
    #                mode=TimeWtAverage
    #                     Average
    #                     Max
    #                     Min
    #                     Sum
    #                     First
    #                     List
    #
    #                normally stime and etime define the time period
    #                for which you want grids.  However, if recList
    #                is not None - then we assume that recList has
    #                a list of record numbers that span the desired
    #                time period - and we don't search for records
    #                that fit the stime/etime period. This saves
    #                considerable time - and the records are often
    #                known ahead of time from other routines.
    #
    #  Note grids are flipped vertically to AWIPS II ordering.
    #
    def getVerGrids(self,model,basetime,element,stime,etime,
                    mode="TimeWtAverage",recList=None):
       self.logMsg("getVerGrids Start",10)
       #
       #  get parm type (scalar/vector) and set default return values
       #
       dataType=self.getVerParmType(element)
       if mode=="List":
          retVal=[]
       else:
          if dataType==0:
             retVal=None
          else:
             retVal=(None,None)
       #
       #  Make sure file for parm/model exists
       #
       if not self.checkFile(element,model):
          self.logMsg("Could not open file",5)
          return retVal
       #
       rateFlag=self.getRateFlag(model,element)
       #
       #  Get list of records that intersect the timeRange
       #
       if recList is None:
          self.logMsg("finding grids that intersect",10)
          if model not in self.OBSMODELS:
             recbase=equal(self.fncBtime,basetime)
             recfit=logical_and(greater(self.fncEtime,stime),
                                less(self.fncStime,etime))
             recmatch=logical_and(recbase,recfit)
             recnumbers=compress(recmatch,self.fncRecs)
             recList=list(recnumbers)
             recList.sort(lambda x,y: cmp(self.fncStime[int(x)],self.fncStime[int(y)]))
          else:
             recmatch=logical_and(greater(self.oncEtime,stime),
                                 less(self.oncStime,etime))
             recnumbers=compress(recmatch,self.oncRecs)
             recList=list(recnumbers)
             recList.sort(lambda x,y: cmp(self.oncStime[int(x)],self.oncStime[int(y)]))
       self.logMsg("number of intersecting grids:%d"%len(recList),10)
       if len(recList)<1:
          return retVal
       #
       #  Loop over grids
       #
       totalWeights=0
       gridtot=self._empty
       utot=self._empty
       vtot=self._empty
       for rec in recList:
          rec = int(rec)
          self.logMsg("reading grid",5)
          #
          #  get total hours in grid, and amount of grid that intersects
          #  time range
          #
          if model not in self.OBSMODELS:
             gstime=self.fncStime[rec]
             getime=self.fncEtime[rec]
          else:
             gstime=self.oncStime[rec]
             getime=self.oncEtime[rec]
          gridHours=float(getime-gstime)/float(HOURSECS)
          intersectHours=float(min(etime-gstime,getime-stime,getime-gstime))/float(HOURSECS)
          if dataType!=1:
             if model not in self.OBSMODELS:
                grid=(self.fncValue[rec].astype(float)*self.fncScale[rec])+self.fncAddit[rec]
             else:
                grid=(self.oncValue[rec].astype(float)*self.oncScale[rec])+self.oncAddit[rec]

             # flip to AWIPS II order
             grid = flipud(grid)

             #
             #  If a rateParm - chop grid to only the piece being used
             #
             if rateFlag==1:
                grid=grid*(float(intersectHours)/float(gridHours))
             #
             #
             #
             if mode in ["TimeWtAverage","Average","Sum"]:
                if len(recList)>1:
                   weight=1.0
                   if mode=="TimeWtAverage":
                      weight=intersectHours
                   gridtot=gridtot+(grid*weight)
                   totalWeights=totalWeights+weight
                else:
                   retVal=grid
             elif mode=="Max":
                if retVal is None:
                   retVal=grid
                else:
                   retVal=maximum(retVal,grid)
             elif mode=="Min":
                if retVal is None:
                   retVal=grid
                else:
                   retVal=minimum(retVal,grid)
             elif mode=="First":
                if retVal is None:
                   retVal=grid
             elif mode=="List":
                retVal.append(grid) 
          else:
             if model not in self.OBSMODELS:
                mag=  (self.fncValue[rec].astype(float)*self.fncScale[rec])+self.fncAddit[rec]
                direc=(self.fncValue1[rec].astype(float)*self.fncScale1[rec])+self.fncAddit1[rec]
             else:
                mag=  (self.oncValue[rec].astype(float)*self.oncScale[rec])+self.oncAddit[rec]
                direc=(self.oncValue1[rec].astype(float)*self.oncScale1[rec])+self.oncAddit1[rec]

             # flip to AWIPS II order
             mag = flipud(mag)
             direc = flipud(direc)

             if mode in ["TimeWtAverage","Average","Sum"]:
                if len(recList)>1:
                   (u,v)=self.MagDirToUV(mag,direc)
                   weight=1.0
                   if mode=="TimeWtAverage":
                      weight=intersectHours
                   utot=utot+(u*weight)
                   vtot=vtot+(v*weight)
                   totalWeights=totalWeights+weight
                else:
                   retVal=(mag,direc)
             elif mode=="Max":
                if retVal[0] is None:
                   retVal=(mag,direc)
                else:
                   newdir=where(greater(mag,retVal[0]),direc,retVal[1])
                   newmag=maximum(retVal[0],mag)
                   retVal=(newmag,newdir)
             elif mode=="Min":
                if retVal[0] is None:
                   retVal=(mag,direc)
                else:
                   newdir=where(less(mag,retVal[0]),direc,retVal[1])
                   newmag=minimum(retVal[0],mag)
                   retVal=(newmag,newdir)
             elif mode=="First":
                if retVal[0] is None:
                   retVal=(mag,direc)
             elif mode=="List":
                retVal.append((mag,direc)) 
       #
       #  When we had averages/sums and were adding up...
       #
       if len(recList)>1:
          if mode in ["TimeWtAverage","Average","Sum"]:
             if dataType!=1:
                retVal=gridtot
                if mode in ["TimeWtAverage","Average"]:
                   retVal=retVal/totalWeights
             else:
                if mode in ["TimeWtAverage","Average"]:
                   utot=utot/totalWeights
                   vtot=vtot/totalWeights
                (mag,direc)=self.UVToMagDir(utot,vtot)
                retVal=(mag,direc)
       #
       #
       #
       self.logMsg("getVerGrids - End",10)
       return retVal
    #==================================================================
    #  getObsPeriod - get list of observed records within a period of
    #      Ndays (integer) ending on endDay
    #
    def getObsPeriod(self,model,parm,endDay,Ndays,mask=None):
       obrecs=[]
       eoff=self.getEndOffset(parm)
       soff=self.getStartOffset(parm)
       if type(endDay) is types.StringType:
          try:
             (yea,mon,day)=endDay.split("/")
             endtime=calendar.timegm((int(yea),int(mon),int(day),23,59,59,0,0,-1))+eoff
             starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))-((Ndays-1)*DAYSECS)+soff
          except:
             return obrecs
       else:
          endtime=endDay+DAYSECS-1+eoff
          starttime=endDay-((Ndays-1)*DAYSECS)+soff
       obrecs=self.listRecords(parm,model,starttime,endtime,"verify",mask)
       return obrecs
    #==================================================================
    #  getObsStatPeriod - get list of observed records within a period of
    #      Ndays (integer) ending on endDay
    #
    def getObsStatPeriod(self,model,parm,obsmodel,endDay,Ndays,mask=None):
       obrecs=[]
       eoff=self.getEndOffset(parm)
       soff=self.getStartOffset(parm)
       if type(endDay) is types.StringType:
          try:
             (yea,mon,day)=endDay.split("/")
             endtime=calendar.timegm((int(yea),int(mon),int(day),23,59,59,0,0,-1))+eoff
             starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))-((Ndays-1)*DAYSECS)+soff
          except:
             return obrecs
       else:
          endtime=endDay+DAYSECS-1+eoff
          starttime=endDay-((Ndays-1)*DAYSECS)+soff
       obrecs=self.listStatRecords(parm,model,obsmodel,starttime,endtime,"verify",mask)
       return obrecs
    #==================================================================
    #  getEndOffset - gets the END_OFFSET_HOURS for a parm (multiplied
    #      by HOURSECS so that the value returned is in seconds.  If no 
    #      END_OFFSET_HOURS is specified for the parm, then it returns 
    #      zero.
    #
    def getEndOffset(self,parm):
       eoff=0
       keys=self.END_OFFSET_HOURS.keys()
       if parm in keys:
          eoff=self.END_OFFSET_HOURS[parm]*HOURSECS
       return eoff
    #==================================================================
    #  getStartOffset - gets the START_OFFSET_HOURS for a parm 
    #      (multiplied by HOURSECS so that the value returned is in
    #      seconds.  If no START_OFFSET_HOURS is specified for the 
    #      parm, then it returns zero.
    #
    def getStartOffset(self,parm):
       soff=0
       keys=self.START_OFFSET_HOURS.keys()
       if parm in keys:
          soff=self.START_OFFSET_HOURS[parm]*HOURSECS
       return soff
    #==================================================================
    #  getObsList - get list of observed records within days listed in
    #      dayList (each day is "year/mon/day") with eoff and soff
    #      (seconds) added to the end and beginning respectively
    #
    def getObsList(self,model,parm,dayList,mask=None,callbackMethod=None):
       obrecs=[]
       eoff=self.getEndOffset(parm)
       soff=self.getStartOffset(parm)
       count=0
       totalcount=len(dayList)
       for date in dayList:
          count+=1
          if callbackMethod is not None:
             exit=callbackMethod("%d of %d"%(count,totalcount))
             if exit==1:
                return obrecs
          if type(date) is types.StringType:
             try:
                (yea,mon,day)=date.split("/")
                endtime=calendar.timegm((int(yea),int(mon),int(day),23,59,59,0,0,-1))+eoff
                starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))+soff
             except:
                continue
          else:
             starttime=date+soff
             endtime=date+DAYSECS-1+eoff
          recs=self.listRecords(parm,model,starttime,endtime,"verify",mask)
          for rec in recs:
             if rec not in obrecs:
                obrecs.append(rec)
       return obrecs
    #==================================================================
    #  getObsStatList - get list of observed records within days listed in
    #      dayList (each day is "year/mon/day") with eoff and soff
    #      (seconds) added to the end and beginning respectively
    #
    def getObsStatList(self,model,parm,obsmodel,dayList,mask=None,callbackMethod=None):
       obrecs=[]
       eoff=self.getEndOffset(parm)
       soff=self.getStartOffset(parm)
       count=0
       totalcount=len(dayList)
       for date in dayList:
          count+=1
          if callbackMethod is not None:
             exit=callbackMethod("%d of %d"%(count,totalcount))
             if exit==1:
                return obrecs
          if type(date) is types.StringType:
             try:
                (yea,mon,day)=date.split("/")
                endtime=calendar.timegm((int(yea),int(mon),int(day),23,59,59,0,0,-1))+eoff
                starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,-1))+soff
             except:
                continue
          else:
             starttime=date+soff
             endtime=date+DAYSECS-1+eoff
          recs=self.listStatRecords(parm,model,obsmodel,starttime,endtime,"verify",mask)
          for rec in recs:
             if rec not in obrecs:
                obrecs.append(rec)
       return obrecs
    #==================================================================
    #  getForecasters - given a model, element, and records - open the
    #                   correct grid file and get a list of forecaster
    #                   numbers for that record.  Will return an empty
    #                   list if there are problems opening the file
    #
    def getForecasters(self,model,element,rec):
       if not self.checkFile(element,model):
          self.logMsg("Could not open file",5)
          return []
       retVal=self.getRecFcstrs(rec)
       return retVal
    #==================================================================
    #  getRecFcstrs - given a record, get a list of forecaster numbers
    #                 from the currently open forecast grid file
    #
    def getRecFcstrs(self,rec):
       return list(self.fncFcstr[rec,:])
    #==================================================================
    #  trimFcstRecs - get rid of forecast records that do not match
    #     the cycle and forecaster lists
    #
    def trimFcstRecs(self,fcstrecs,model,cycles,fcstrIDlist,fhrStart=0,
                     fhrEnd=0):
       fcstTrimmed=[]
       #
       #  Get cycleList with integer hours
       #
       maxCycles=len(self.getCFG('ALLCYCLES'))
       cycleList=[]
       if ((type(cycles) is types.TupleType)or(type(cycles) is types.ListType)):
          for cycle in cycles:
	     if type(cycle) is types.StringType:
	        cycleList.append(int(cycle))
	     else:
                cycleList.append(cycle)
       else:
          if type(cycles) is types.StringType:
	     cycleList.append(int(cycles))
	  else:
	     cycleList.append(cycles)
       for fcstrec in fcstrecs:
          #
          #  skip forecasts from wrong cycle
          #
          if -1 not in cycleList:
             btime=self.fncBtime[fcstrec]
             basetuple=time.gmtime(btime)
             if basetuple[3] not in cycleList:
                continue
          #
          #  skip forecasts for wrong forecaster
          #
          if "ALL" not in fcstrlist:
             if model=="Official":
                foundfcstr=0
                for j in range(self.MAXFCSTRS):
                   fnum=int(self.fncFcstr[fcstrec,j])
                   fnumstr="%2.2d"%fnum
                   fid=self.FcstrIDs[fnumstr]
                   if fid in fcstrIDlist:
                      foundfcstr=1
                if foundfcstr==0:
                   continue
          #
          #  skip forecasts outside of matching fhrs
          #
          fhr=int(float(self.fncStime[fcstrec]-self.fncBtime[fcstrec])/float(HOURSECS))
          if ((fhr<fhrStart)or(fhr>fhrEnd)):
             continue
          #
          #  Add records that came this far
          #
          fcstTrimmed.append(fcstrec)
       return fcstTrimmed
    #==================================================================
    #  getFcstPeriod - get list of forecast records made within a period
    #     of Ndays (integer) ending on endDay(either a "year/mon/day"
    #     string, or a time integer) by model
    #
    def getFcstPeriod(self,inputParm,endDay,Ndays,model):
       fcstrecs=[]
       if type(endDay) is types.StringType:
          try:
             (yea,mon,day)=endDay.split("/")
             startOfEndDay=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
          except:
             return fcstrecs
       else:
          startOfEndDay=endDay
       endtime=startOfEndDay+DAYSECS-1
       starttime=startOfEndDay-((Ndays-1)*DAYSECS)
       fcstrecs=self.listRecords(inputParm,model,starttime,endtime,"forecast")
       return fcstrecs
    #==================================================================
    #  getFcstList - get list of forecast records made on days listed
    #     in dayList (each day is "year/mon/day") by model
    #
    def getFcstList(self,inputParm,dayList,model):
       fcstrecs=[]
       for date in dayList:
          if type(date) is types.StringType:
	     try:
                (yea,mon,day)=date.split("/")
                starttime=calendar.timegm((int(yea),int(mon),int(day),0,0,0,0,0,0))
	     except:
	        continue
          else:
             starttime=date
          endtime=starttime+DAYSECS
          recs=self.listRecords(inputParm,model,starttime,endtime,"forecast")
          for rec in recs:
             if rec not in fcstrecs:
                fcstrecs.append(rec)
       return fcstrecs
    #=================================================================
    #  logMsg - writes a message to STDOUT with a date/time stamp
    #           and flushes immediately
    #
    def logMsg(self,msg,significance=0):
       if significance<=self.DEBUG:
          gmt=time.gmtime()
          print "%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d:%s"%(gmt[0],gmt[1],
                 gmt[2],gmt[3],gmt[4],gmt[5],msg)
          sys.stdout.flush()
       return
    #=================================================================
    #  setQuiet - set DEBUG level to 0 - suppressing most messages
    #
    def setQuiet(self):
       self.DEBUG=0
       return
    #=================================================================
    #  setVerbose - set DEBUG level (defaults to 1)
    #
    def setVerbose(self,value=1):
       self.DEBUG=value
       return
    def setDebug(self,value=1):
       self.DEBUG=value
       return
    def getDebug(self):
       return self.DEBUG
    def getVerbose(self):
       return self.DEBUG
    #==================================================================
    #  Given a modelname and parmname (and optional level), return
    #  the GFE precision number (0 for integers, 1 for 0.1 resolution,
    #  2 for 0.01 resolution, etc.).  This is useful in many areas.
    #
    def getParmPrecision(self,modelname,parmname,level="SFC"):
       precision=0
       parmInfo=self.getParm(modelname,parmname,level)
       if parmInfo is not None:
          precision=parmInfo.getGridInfo().getPrecision()
       return precision
    #=================================================================
    #  lastSaved - get time that the grid for the specified parm, model,
    #              basetime, starttime, endtime was written to the grid
    #              archive database.  If it has not been written - return
    #              0.
    #
    def lastSaved(self,parm,model,Btime,Stime,Etime):
       self.logMsg("Starting lastSaved in VerifyUtility",10)
       savedTime=0
       #
       #  Check that the correct file is open and ready to modify
       #
       if not self.checkFile(parm,model,modify=1):
          return savedTime
       #
       #  for models in OBSMODELS - use open Obs file
       #
       if model in self.OBSMODELS:
          #
          #  If no records - it hasn't been saved
          #
          if self.onumRecs==0:
             return savedTime
          #
          # See if a record with the exact same times exists
          #
          s=equal(self.oncStime[:],Stime)
          b=equal(self.oncBtime[:],Btime)
          e=equal(self.oncEtime[:],Etime)
          use=logical_and(logical_and(b,s),e)
          if sometrue(use):
             a=compress(use,self.oncRecs)
             recnum=int(a[0])
             savedTime=self.oncVtime[recnum]
       #
       #  for non OBSMODELS models - use open Fcst file
       #
       else:
          #
          #  If no records - it hasn't been saved
          #
          if self.fnumRecs==0:
             return savedTime
          #
          # See if a record with the exact same times exists
          #
          s=equal(self.fncStime[:],Stime)
          b=equal(self.fncBtime[:],Btime)
          e=equal(self.fncEtime[:],Etime)
          use=logical_and(logical_and(b,s),e)
          if sometrue(use):
             a=compress(use,self.fncRecs)
             recnum=int(a[0])
             savedTime=self.fncVtime[recnum]
       return savedTime
    #=======================================================================
    #  smoothpm - smooths grid by averaging over plus and minus k
    #             gridpoints, which means an average over a square 2k+1
    #             gridpoints on a side.  If mask is specified, only
    #             smooth over the points that have mask=1, not any others.
    #
    #             Near the edges it can't average over plus and minus
    #             - since some points would be off the grid - so it
    #             averages over all the points it can.  For example, on
    #             the edge gridpoint - it can only come inside k points -
    #             so the average is over only k+1 points in that direction
    #             (though over all 2k+1 points in the other direction -
    #             if possible)
    #          
    #             Much faster by using the cumsum function in numeric.
    #             Total across the 2k+1 points is the cumsum at the last
    #             point minus the cumsum at the point before the first
    #             point. Only edge points need special handling - and
    #             cumsum is useful here too.
    #
    def smoothpm(self,grid,k,mask=None):
       k=int(k) # has to be integer number of gridpoints
       if (k<1): # has to be a positive number of gridpoints
          return grid
       (ny,nx)=grid.shape
       k2=k*2
       #
       #  Remove the minimum from the grid so that cumsum over a full
       #  row or column of the grid doesn't get so big that precision
       #  might be lost.
       #
       fullmin=minimum.reduce(minimum.reduce(grid))
       gridmin=grid-fullmin
       #
       #  No mask is simpler
       #
       if mask is None:
          #
          #  Average over the first (y) dimension - making the 'mid' grid
          #
          mid=grid*0.0
          c=cumsum(gridmin,0)
          nym1=ny-1
          midy=int((ny-1.0)/2.0)
          ymax=min(k+1,midy+1)
          for j in range(ymax): # handle edges
             jk=min(j+k,nym1)
             jk2=max(nym1-j-k-1,-1)
             mid[j,:]=c[jk,:]/float(jk+1)
             if jk2==-1:
                mid[nym1-j,:]=c[nym1,:]/float(jk+1)
             else:
                mid[nym1-j,:]=(c[nym1,:]-c[jk2,:])/float(jk+1)
          if ((k+1)<=(ny-k)): # middle
             mid[k+1:ny-k,:]=(c[k2+1:,:]-c[:-k2-1,:])/float(k2+1)
          #
          #  Average over the second (x) dimension - making the 'out' grid 
          #
          c=cumsum(mid,1)
          out=grid*0.0
          nxm1=nx-1
          midx=int((nx-1.0)/2.0)
          xmax=min(k+1,midx+1)
          for j in range(xmax): # handle edges
             jk=min(j+k,nxm1)
             jk2=max(nxm1-j-k-1,-1)
             out[:,j]=c[:,jk]/float(jk+1)
             if jk2==-1:
                out[:,nxm1-j]=c[:,nxm1]/float(jk+1)
             else:
                out[:,nxm1-j]=(c[:,nxm1]-c[:,jk2])/float(jk+1)
          if ((k+1)<=(nx-k)): # middle
             out[:,k+1:nx-k]=(c[:,k2+1:]-c[:,:-k2-1])/float(k2+1)
          #
          #  Add the minimum back in
          #
          out=out+fullmin
       #
       #  Mask makes it a bit more difficult - have to find out how many
       #  points were in each cumsum - and have to deal with possible
       #  divide-by-zero errors
       #
       else:
          #
          #  Average over the first (y) dimension - making the 'mid' grid
          #
          mask=clip(mask,0,1)
          gridmin1=where(mask,gridmin,0)
          mid=grid*0.0
          midd=grid*0.0
          c=cumsum(gridmin1,0)
          d=cumsum(mask,0)
          nym1=ny-1
          midy=int((ny-1.0)/2.0)
          ymax=min(k+1,midy+1)
          for j in range(ymax): # handle edges
             jk=min(j+k,nym1)
             jk2=max(nym1-j-k-1,-1)
             mid[j,:]=c[jk,:]
             midd[j,:]=d[jk,:]
             if jk2==-1:
                mid[nym1-j,:]=c[nym1,:]
                midd[nym1-j,:]=d[nym1]
             else:
                mid[nym1-j,:]=(c[nym1,:]-c[jk2,:])
                midd[nym1-j,:]=d[nym1,:]-d[jk2,:]
          if ((k+1)<=(ny-k)): # middle
             mid[k+1:ny-k,:]=(c[k2+1:,:]-c[:-k2-1,:])
             midd[k+1:ny-k,:]=d[k2+1:,:]-d[:-k2-1,:]
          #
          #  Average over the second (x) dimension - making the 'out' grid 
          #
          c=cumsum(mid,1)
          d=cumsum(midd,1)
          out=grid*0.0
          nxm1=nx-1
          midx=int((nx-1.0)/2.0)
          xmax=min(k+1,midx+1)
          for j in range(xmax): # handle edges
             jk=min(j+k,nxm1)
             jk2=max(nxm1-j-k-1,-1)
             out[:,j]=c[:,jk]/maximum(d[:,jk],1)
             if jk2==-1:
                out[:,nxm1-j]=c[:,nxm1]/maximum(d[:,nxm1],1)
             else:
                out[:,nxm1-j]=(c[:,nxm1]-c[:,jk2])/maximum((d[:,nxm1]-d[:,jk2]),1)
          if ((k+1)<=(nx-k)): # middle
             out[:,k+1:nx-k]=(c[:,k2+1:]-c[:,:-k2-1])/maximum((d[:,k2+1:]-d[:,:-k2-1]),1)
          #
          #  Add the minimum back in
          #
          out=where(mask,out+fullmin,grid)
       return out
    #=======================================================================
    #  arealOccur - similar to smoothpm, in that it looks over a square 2k+1
    #               on a side.  But should be used with a logical array of 0
    #               and 1, and just tells you whether an occurrence (a 1)
    #               occurred anywhere in the search square.  If a mask is
    #               specified it only searches over the points that have
    #               mask=1, not any others.
    #
    #               Near the edges it can't search over plus and minus
    #               - since some points would be off the grid - so it
    #               searches over all the points it can.  For example, on
    #               the edge gridpoint - it can only come inside k points -
    #               so the average is over only k+1 points in that direction
    #               (though over all 2k+1 points in the other direction -
    #               if possible)
    #          
    #               Much faster by using the cumsum function in numeric.
    #               Total across the 2k+1 points is the cumsum at the last
    #               point minus the cumsum at the point before the first
    #               point. Only edge points need special handling - and
    #               cumsum is useful here too.
    #
    def arealOccur(self,grid,k,mask=None):
       k=int(k) # has to be integer number of gridpoints
       if (k<1): # has to be a positive number of gridpoints
          return grid
       (ny,nx)=grid.shape
       k2=k*2
       #
       #  No mask is simpler
       #
       if mask is None:
          grid1=grid
       else:
          mask=clip(mask,0,1)
          grid1=where(mask,grid,0)
       #
       #  Average over the first (y) dimension - making the 'mid' grid
       #
       mid=grid*0.0
       c=cumsum(grid1,0)
       nym1=ny-1
       midy=int((ny-1.0)/2.0)
       ymax=min(k+1,midy+1)
       for j in range(ymax): # handle edges
          jk=min(j+k,nym1)
          jk2=max(nym1-j-k-1,-1)
          mid[j,:]=c[jk,:]
          if jk2==-1:
             mid[nym1-j,:]=c[nym1,:]
          else:
             mid[nym1-j,:]=c[nym1,:]-c[jk2,:]
       if ((k+1)<=(ny-k)): # middle
          mid[k+1:ny-k,:]=c[k2+1:,:]-c[:-k2-1,:]
       #
       #  Average over the second (x) dimension - making the 'out' grid 
       #
       c=cumsum(mid,1)
       out=grid*0.0
       nxm1=nx-1
       midx=int((nx-1.0)/2.0)
       xmax=min(k+1,midx+1)
       for j in range(xmax): # handle edges
          jk=min(j+k,nxm1)
          jk2=max(nxm1-j-k-1,-1)
          out[:,j]=c[:,jk]
          if jk2==-1:
             out[:,nxm1-j]=c[:,nxm1]
          else:
             out[:,nxm1-j]=c[:,nxm1]-c[:,jk2]
       if ((k+1)<=(nx-k)): # middle
          out[:,k+1:nx-k]=c[:,k2+1:]-c[:,:-k2-1]
       #
       #  Occur is where non-zero
       #
       out=greater(out,0.5)
       return out
    #-----------------------------------------------------------------------
    #  getGridSpacing - get 'rough grid spacing' by getting the distance
    #                   between the corners of the GFE grid and dividing by
    #                   the number of points.
    #
    def getGridSpacing(self):
       xmax=self._empty.shape[1]
       ymax=self._empty.shape[0]
       (lat1,lon1)=self.getLatLon(0,0)
       (lat2,lon2)=self.getLatLon(xmax-1,ymax-1)
       hypot=math.hypot(xmax-1,ymax-1)
       spacing1=self.getCircleDistance(lat1,lon1,lat2,lon2)/hypot
       (lat1,lon1)=self.getLatLon(0,ymax-1)
       (lat2,lon2)=self.getLatLon(xmax-1,0)
       spacing2=self.getCircleDistance(lat1,lon1,lat2,lon2)/hypot
       avgspacing=(spacing1+spacing2)/2.0
       return avgspacing
    #-----------------------------------------------------------------------
    #  getCircleDistance - get the 'great circle distance' between two lat
    #                      lon points (in km) 
    #
    def getCircleDistance(self,lat1,lon1,lat2,lon2):
       DTR=math.pi/180.0
       lat1r=lat1*DTR
       lon1r=lon1*DTR
       lat2r=lat2*DTR
       lon2r=lon2*DTR
       dl=lon2r-lon1r
       a=(math.acos((math.sin(lat1r)*math.sin(lat2r))+(math.cos(lat1r)*\
          math.cos(lat2r)*math.cos(dl))))/DTR
       return(a*1.852*60)
