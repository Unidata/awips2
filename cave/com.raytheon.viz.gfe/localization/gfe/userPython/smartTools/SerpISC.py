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
# SVN: $Revision$ - $Date$
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# serpISC - version 1.7
#
# Changes an existing grid to blend better into neighboring ISC grids.
# Can be used as an initial or final step in coordination.  Only your grids
# are affected: nothing happens to the ISC grids.  The ISC button must have 
# been clicked on at least once before using this tool.  
#
# Every point on the outer perimeter of CWA (i.e, belonging to selected ISCs)
# takes part in a serp adjustment of the existing grid. If any ISC grids are
# missing or not selected on a CWA boundary, your own grid is used there instead.
#
# You can use this tool on one ISC at a time to see how each one would influence
# your grid.  To fit all ISC boundaries at once you must have all of them clicked
# on.  Running the tool sequentially on each ISC will retain previous results if
# you keep the older ones turned on, but different sequences will yield slightly
# different results.
# 
# Make sure your grid does not have an artificial boundary near the CWA border.
# Otherwise, it might already match your ISC neighbor there, so the tool won't
# adjust anything and your artificial boundary will remain.
#
# You can include or exclude as many sample points within your CWA as you like, but
# sample points close to an ISC border can create unrealistic gradients.
#
# You can match a border only partway if you want.  Suppose you want to meet your 
# ISC neighbor half way. Then set the "percent of full match" to 50.  After sending
# your ISC grid, your neighbor will want to match FULL way (not half) to meet the
# newly received grid.  You can also use "percent of full match" to nudge your
# grid to your neighbors' grids.
#
# If your grid's duration spans several shorter-duration ISC grids, the ISC
# grids will be time-averaged first (except for PoP which always uses the
# maximum value) and the fit will be inexact.  Or, if the ISC grids themselves
# don't match at a CWA boundary (something you can't do in your own grid), the
# the tool will converge intermediate contours to the point of the mismatch,
# and the fit will look artificial.
#
# For winds serp runs twice, once for u and once for v.
#
# This tool cannot be used with Wx grids. 
# 
# Authors: Les Colin - WFO Boise, ID, and Tim Barker - SOO Boise, ID
#
#  2003/06/21 - Revised "remoteness" calculation (to counteract observation-
#               clustering). New module is called getGoodRemoteness.
#               numpy-Python code: Barker.  Algorithm: Colin.
#  2003/06/22 - Analyzes winds in u and v components, rather than by speed
#               and direction.
#  2003/06/23 - Finishes tool by copying ISC data outside CWA.
#  2003/10/29 - Runs serp without considering sample points, then runs it
#               again only on the samples.  ISC-copy feature has been removed.
#  2004/05/30 - Uses improved serp analysis (see Barker). Can include or exclude 
#               various ISC neighbors. Can include or exclude currently displayed
#               samples within your CWA. Samples in the ISC areas are ignored.
#  2004/07/09 - Modified to ignore duplicate sample points (previously, they
#               would hang the tool).  Also modified tool to allow partial match
#               so that CWA grid adjusts only partway toward ISC grid.
#  2004/09/04 - Modified to work on an edit area, perhaps only half way across the
#               home CWA.  The effect is a taper from a full (or partial) adjustment 
#               at designated ISC borders to zero change inside the home CWA where
#               the edit area stops.
#  2004/09/21 - Now works even if preceded by ISC_Copy (by moving the home CWA-border
#               inward one pixel and comparing to nearest ISC neighbor values).
#               Tool completes by running an equivalent ISC_Copy on the selected ISC
#               borders.  Tool now also contains a thinning feature to speed up
#               execution. e.g., thinning by 2 runs the tool on alternate border
#               points, thinning by 3 runs the tool on every third border point, etc.
#  2004/09/25 - Corrected bug in preceding version in which sample points could possibly
#               coincide with the revised home CWA-border points and hang the tool.
#  2004/11/10 - Final ISC_Copy feature made optional.
#  2004/11/17 - Corrected return statement at end of tool, and repaired code when
#               NOT adjusting for elevation.
#  2008/07/31 - added int() for arguments to createTimeRange for OB8.3. /TB
#  2012/07/13 - Version 1.7.  AWIPS2 Port.
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList=["SCALAR","VECTOR"]

#
#====================================================================
#  Part to modify for local configuration
defaultCWA="STO"
VariableList=[
   ("Include these WFOs:",["MTR","EKA","HNX","REV","MFR"],"check",["MTR","EKA","HNX","REV","MFR"]),
   ("Intentional mismatch (CWA minus WFO):","0","alphaNumeric"),
   ("Currently displayed CWA sample points:","Use","radio",["Use","Don't use"]),
   ("Adjust for terrain elevation?","Yes","radio",["Yes","No"]),
   ("Elevation Factor",36,"numeric"),
   ("Tool thinning-factor:",1,"scale",[1,10],1),
   ("Percent of full match",100,"scale",[0,100],1),
   ("Copy ISC data in afterward?","No","radio",["Yes","No"]),
             ]

from numpy import *
import ObjAnal
import SmartScript
import time
from math import sin,cos,acos,pi

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss=dbss
        SmartScript.SmartScript.__init__(self, dbss)
    def preProcessTool(self,varDict):
        self.OA = ObjAnal.ObjAnal(self._dbss)
  
    def execute(self, variableElement, variableElement_GridInfo, editArea, varDict, Topo, WEname, GridTimeRange):

        wxType = variableElement_GridInfo.getGridType().ordinal()
        defCWA=self.getEditArea(defaultCWA)
        defcwa=self.encodeEditArea(defCWA)
        nondefcwa=1-defcwa  # i.e., toggle
        nondefCWA=self.decodeEditArea(nondefcwa)
        defea=self.taperGrid(nondefCWA,2)*2
        
        # The above line defines the default CWA area as defea==0, the outer perimeter of the default CWA 
        # as defea==1, and further outside as defea==2.

        arbea=self.encodeEditArea(editArea)
        nonarbea=1-arbea
        nonarbEA=self.decodeEditArea(nonarbea)
        arbea=self.taperGrid(nonarbEA,2)*2

        cwa=zeros(Topo.shape)
        ISC=varDict["Include these WFOs:"]
        samps=varDict["Currently displayed CWA sample points:"]
        thin=varDict["Tool thinning-factor:"]
        partial=varDict["Percent of full match"]*.01
        
        for WFO in ISC:
            CWA=self.getEditArea(WFO)
            cwa=self.encodeEditArea(CWA)+cwa

        alltrs=self._getAllHourlyTimeRanges(GridTimeRange)
        if ((WEname=="MaxT")or(WEname=="PoP")):
            sum=zeros(Topo.shape)-150.0
        elif (WEname=="MinT"):
            sum=zeros(Topo.shape)+150.0
        else:
            if (wxType==2):
                sum=[zeros(Topo.shape),zeros(Topo.shape)]
            else:
                sum=zeros(Topo.shape)
        cnt=zeros(Topo.shape)

        for tr in alltrs:
            isc=self.getComposite(WEname,tr,0)
            if isc is None:
                
                continue
            #
            #  Add to sums, or min/max
            #
            if wxType==1:  # SCALAR
                bits,iscgrid=isc
                if ((WEname=="MaxT")or(WEname=="PoP")):
                    sum=where(bits,maximum(iscgrid,sum),sum)
                    cnt[bits] = 1
                elif (WEname=="MinT"):
                    sum=where(bits,minimum(iscgrid,sum),sum)
                    cnt[bits] = 1
                else:
                    sum=where(bits,sum+iscgrid,sum)
                    cnt[bits] += 1
            if wxType==2:  # VECTOR
                bits,mag,dir=isc
                (u,v)=self.MagDirToUV(mag,dir)
                sum[0]=where(bits,sum[0]+u,sum[0])
                sum[1]=where(bits,sum[1]+v,sum[1])
                cnt[bits] += 1
        #
        #  now calculate average/max/min, etc.
        #  (count is always 1 for max/min)
        #
        if ((wxType==1)or(wxType==2)):
            if (wxType==2):
                (mag,dir)=variableElement
                (u,v)=self.MagDirToUV(mag,dir)
                sum[0]=where(equal(cnt,0),u,sum[0])
                sum[1]=where(equal(cnt,0),v,sum[1])
            else:
                sum=where(equal(cnt,0),variableElement,sum)
            cnt[equal(cnt,0)] = 1
            new=sum/cnt
            if (wxType==2):
                (mag,dir)=self.UVToMagDir(new[0],new[1])
                newvec=(mag,dir)

        self.elevadjust=0
        self.elevfactor=0.
        if varDict["Adjust for terrain elevation?"]=="Yes":
            self.elevadjust=1
            self.elevfactor=varDict["Elevation Factor"]
            if self.elevfactor<1:
                self.elevfactor=0.
    
        self.xloclist=[]
        self.yloclist=[]
        self.hloclist=[]
        self.zlist=[]
        self.ulist=[]
        self.vlist=[]

        for x in range(1,Topo.shape[1]-1):
          for y in range(1,Topo.shape[0]-1):
            if (x+y)%thin!=0:
                continue
            if (arbea[y,x]<2 and defea[y,x]==0):
                if (cwa[y,x+1]==1) or (cwa[y,x-1]==1) or (cwa[y+1,x]==1) or (cwa[y-1,x]==1):
                   if self.elevadjust==1:
                       self.hloclist.append(Topo[y,x])
                   else:
                       self.hloclist.append(0.)    
                   self.xloclist.append(x)
                   self.yloclist.append(y)
                   if wxType==1:
                       chgval=0.
                       n=0
                       if cwa[y,x+1]==1:
                          if self.elevadjust==0:
                              chgval=chgval+(new[y,x+1]-variableElement[y,x])
                          elif self.elevadjust==1: 
                              elevdif=abs(Topo[y,x]-Topo[y,x+1])
                              if elevdif<5000.:
                              # ISC-CWA neighbors more than 5000 ft apart in elevation are too
                              # dissimilar to compare.
                                  chgval=chgval+(new[y,x+1]-variableElement[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y,x-1]==1:
                          if self.elevadjust==0:
                              chgval=chgval+(new[y,x-1]-variableElement[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y,x-1])
                              if elevdif<5000.:
                                  chgval=chgval+(new[y,x-1]-variableElement[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y+1,x]==1:
                          if self.elevadjust==0:
                              chgval=chgval+(new[y+1,x]-variableElement[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y+1,x])
                              if elevdif<5000.:
                                  chgval=chgval+(new[y+1,x]-variableElement[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y-1,x]==1:
                          if self.elevadjust==0:
                              chgval=chgval+(new[y-1,x]-variableElement[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y-1,x])
                              if elevdif<5000.:
                                  chgval=chgval+(new[y-1,x]-variableElement[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       self.zlist.append((chgval/n)*partial)
                          
                   elif wxType==2:
                       (magcwa,dircwa)=variableElement
                       (ucwa,vcwa)=self.MagDirToUV(magcwa,dircwa)
                       (uisc,visc)=self.MagDirToUV(mag,dir)
                       chgu=0.
                       chgv=0.
                       n=0
                       if cwa[y,x+1]==1:
                          if self.elevadjust==0:
                              chgu=chgu+(uisc[y,x+1]-ucwa[y,x])
                              chgv=chgv+(visc[y,x+1]-vcwa[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y,x+1])
                              if elevdif<5000.:
                                  chgu=chgu+(uisc[y,x+1]-ucwa[y,x])*(1.0-elevdif/5000.)
                                  chgv=chgv+(visc[y,x+1]-vcwa[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y,x-1]==1:
                          if self.elevadjust==0:
                              chgu=chgu+(uisc[y,x-1]-ucwa[y,x])
                              chgv=chgv+(visc[y,x-1]-vcwa[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y,x-1])
                              if elevdif<5000.: 
                                  chgu=chgu+(uisc[y,x-1]-ucwa[y,x])*(1.0-elevdif/5000.)
                                  chgv=chgv+(visc[y,x-1]-vcwa[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y+1,x]==1:
                          if self.elevadjust==0:
                              chgu=chgu+(uisc[y+1,x]-ucwa[y,x])
                              chgv=chgv+(visc[y+1,x]-vcwa[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y+1,x])
                              if elevdif<5000.:
                                  chgu=chgu+(uisc[y+1,x]-ucwa[y,x])*(1.0-elevdif/5000.)
                                  chgv=chgv+(visc[y+1,x]-vcwa[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       if cwa[y-1,x]==1:
                          if self.elevadjust==0:
                              chgu=chgu+(uisc[y-1,x]-ucwa[y,x])
                              chgv=chgv+(visc[y-1,x]-vcwa[y,x])
                          elif self.elevadjust==1:    
                              elevdif=abs(Topo[y,x]-Topo[y-1,x])
                              if elevdif<5000.:
                                  chgu=chgu+(uisc[y-1,x]-ucwa[y,x])*(1.0-elevdif/5000.)
                                  chgv=chgv+(visc[y-1,x]-vcwa[y,x])*(1.0-elevdif/5000.)
                          n=n+1
                       self.ulist.append((chgu/n)*partial)
                       self.vlist.append((chgv/n)*partial)
            if arbea[y,x]==1 and defea[y,x]==0:
                self.pointok=0
                for nn in range(len(self.xloclist)):
                     if (y==self.yloclist[nn]) and (x==self.xloclist[nn]):
                         self.pointok=1
            # In the above line an edit area IS on the screen and here we're looking for boundary points
            # inside the home CWA that are more than one pixel from the border.  We want to hold these
            # points steady (i.e., zero change).
                if self.pointok==1:  # we already have this point, don't use it twice.
                    continue
                self.xloclist.append(x)
                self.yloclist.append(y)
                if self.elevadjust==1:
                    self.hloclist.append(Topo[y,x])
                else:
                    self.hloclist.append(0.)
                if wxType==1:
                    self.zlist.append(0.)
                if wxType==2:
                    self.ulist.append(0.)
                    self.vlist.append(0.)

        if samps=="Use":
            self.samplePoints = self.getSamplePoints(None)
            for sample in self.samplePoints:
                    (x,y)=sample
                    self.sampleok=0
                    for count in range(len(self.xloclist)): 
                        if ((x==self.xloclist[count]) and (y==self.yloclist[count])):
                            self.sampleok=1
                    # self.sampleok becomes 1 for a duplicate entry, so bypass the duplicate.        
                    if self.sampleok==1:
                        continue
                    if x<0 or x>Topo.shape[1]-1:
                        continue
                    if y<0 or y>Topo.shape[0]-1:
                        continue
                    if defea[y,x]!=0:
                        continue
                            
                    if self.elevadjust==1:
                        self.hloclist.append(Topo[y,x])
                    else:
                        self.hloclist.append(0.)
                    self.xloclist.append(x)
                    self.yloclist.append(y)
                    if wxType==1:
                        self.zlist.append(0.)
                    if wxType==2:
                        self.ulist.append(0.)
                        self.vlist.append(0.)
        #
        #  Don't proceed if no points
        #
        if len(self.xloclist)==0:
            self.statusBarMsg("No data available to serp to...","R")
            return variableElement
        else:
            print "  the number of points being used:",len(self.xloclist)
        #
        #
        #
        if wxType==1: # scalar
           zval=self.OA.Serp(self.zlist,self.xloclist,self.yloclist,self.hloclist,self.elevfactor,Topo)
           #  zval is the new scalar-change grid. 
           if varDict["Copy ISC data in afterward?"]=="Yes":
               znew=where(logical_or(equal(defea,0),equal(cwa,0)),variableElement+zval,new)
           else:
               znew=variableElement+zval
        
        if wxType==2: # vector
           zval=self.OA.Serp(self.ulist,self.xloclist,self.yloclist,self.hloclist,self.elevfactor,Topo)
           #  zval is the new u-change grid.
           if varDict["Copy ISC data in afterward?"]=="Yes":
               newu=where(logical_or(equal(defea,0),equal(cwa,0)),ucwa+zval,new[0])
           else:
               newu=ucwa+zval
           zval=self.OA.Serp(self.vlist,self.xloclist,self.yloclist,self.hloclist,self.elevfactor,Topo)
           #  this zval is the new v-change grid.
           if varDict["Copy ISC data in afterward?"]=="Yes":
               newv=where(logical_or(equal(defea,0),equal(cwa,0)),vcwa+zval,new[1])
           else:
               newv=vcwa+zval
           (newspd,newdir)=self.UVToMagDir(newu,newv)
         #  newspd=where(equal(defea+cwa,0),newspd,mag)
         #  newdir=where(equal(defea+cwa,0),newdir,dir)
           
           znew=(newspd,newdir)

        absmax=variableElement_GridInfo.getMaxValue()
        absmin=variableElement_GridInfo.getMinValue()
        
        if wxType==1:
            return clip(znew,absmin,absmax)
        else:
            return znew

    #===================================================================
    #  _getAllHourlyTimeRanges - gets a list of all 1-hour time ranges
    #                            within the specified time range
    #
    def _getAllHourlyTimeRanges(self,tr):
        #
        #  get integer time of UTC midnight today
        #
        secsinhour=60*60
        lt=time.gmtime()
        mid=time.mktime((lt[0],lt[1],lt[2],0,0,0,lt[6],lt[7],lt[8]))
        #
        #  get integer time of input timerange start
        #
        start=tr.startTime()
        year=start.year
        month=start.month
        day=start.day
        hour=start.hour
        trs=time.mktime((year,month,day,hour,0,0,lt[6],lt[7],lt[8]))
        #
        #  get integer time of input timerange end
        #
        end=tr.endTime()
        year=end.year
        month=end.month
        day=end.day
        hour=end.hour
        tre=time.mktime((year,month,day,hour,0,0,lt[6],lt[7],lt[8]))
        #
        #  The difference between start/end determines number of hours
        #
        numhours=int((tre-trs)/secsinhour)
        #
        #  Difference between mid/start determines starting offset
        #
        offset=int((trs-mid)/secsinhour)
        #
        #  create each hourly time range from offset
        #
        alltrs=[]
        for hour in range(0,numhours):
            newtr=self.createTimeRange(int(offset+hour),int(offset+hour+1),"Zulu")
            alltrs.append(newtr)
            
        return alltrs
