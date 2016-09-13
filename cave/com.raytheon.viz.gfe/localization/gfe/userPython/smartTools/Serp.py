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
# SVN: $Revision: 130 $  $Date: 2010-07-30 17:45:24 +0000 (Fri, 30 Jul 2010) $
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Serp - version 2.6 (AWIPS-2) 
#
#  Changes the existing field by asking the user to set values at control 
#  points, then fitting a surface to all the changes (using "serpentine" 
#  curves), and adding that change grid onto the existing grid.  The new 
#  grid will exactly match the values specified at the control points.
#
#  When run over an edit area, only control points "inside" the edit area 
#  are used.  In addition, many "bogus" control points with "no change" are 
#  added around the edge of the edit area, so that the changes made inside 
#  blend in nicely to the areas outside the edit area that are not changed.
#
# Original Serpentine Algorithm Author: Les Colin - WFO Boise, ID
# Python implmentation: Tim Barker - SOO Boise, ID
#
# History:---------------------------------------------------------------------
#  2012/03/27 - version 2.6 : Tim Barker : making clearer GMSG-style config
#               syntax.  And fixing bad version in latest_stable.
#  2012/03/04 - version 2.5 : Tim Barker : changed GMSG-style config syntax
#               again.
#  2012/02/25 - version 2.4 : Tim Barker : GMSG-style config added, Fixed: 
#               problems when using current samples, issues with parm
#               precision, cleanup of code for readability, remove last global
#               passing thru to GUI class instead.
#  2011/03/14 - version 2.3 : Tim Barker : Fix issues with getGridCell now 
#               returning floats
#  2011/03/05 - version 2.2 : Tim Barker : Adding features that were in the
#               AWIPS-1 version 1.15
#  2010/07/30 - verison 2.0 : Paul Jendrowski : Preliminary AWIPS 2 version
#==============================================================================
#
#
#
ToolType="numeric"
WeatherElementEdited = "variableElement"
ScreenList = ["SCALAR","VECTOR"]
#
#  Imports
#
from numpy import *
import ObjAnal
import SmartScript
import copy
import LogStream
from math import log10
import Tkinter
#
# Get site configuration
#
import SerpConfig as SC
#
#  Set defauls if not set in site configuration
#
if "Locations" not in SC.Config:
    SC.Config["Locations"]={"Bad Config 1":[("Bad Config 1",40.0,-110.0),
                                            ("Bad Config 2",40.1,-110.0)],
                            "Bad Config 2":[("Bad Config 3",40.2,-110.0),
                                            ("Bad Config 4",40.3,-110.0)]}
if "DefaultGroup" not in SC.Config:
    SC.Config["DefaultGroup"]="Bad Config 1"
if "MaxPointsInColumn" not in SC.Config:
    SC.Config["MaxPointsInColumn"]=10
if "ElevationDefault" not in SC.Config:
    SC.Config["ElevationDefault"]="On"
#
#  The initial tool dialog - where major options are set
#
VariableList=[]
VariableList.append(("Options for Serp Tool","","label"))
keys=["Current Samples"]
for key in SC.Config["Locations"].keys():
   keys.append(key)
VariableList.append(("Sample Set:",SC.Config["DefaultGroup"],"radio",keys))
VariableList.append(("Elevation Adjustment",SC.Config["ElevationDefault"],"radio",["On","Off"]))
VariableList.append(("Elevation Factor",36,"numeric"))
#
#  The actual Tool
#
class Tool (SmartScript.SmartScript):
   def __init__(self,dbss):
      self._dbss=dbss
      SmartScript.SmartScript.__init__(self,dbss)
      self.tkroot=None
      #
      #  Global variables used throughout
      #
      self.guiInfo={}
      self.guiInfo['vectedit']=0
      self.guiInfo['minvalue']=0
      self.guiInfo['maxvalue']=100
      self.guiInfo['resolution']=0
      self.guiInfo['masterlabel']="xxx"

   def preProcessTool(self,varDict):
      self.OA = ObjAnal.ObjAnal(self._dbss)
      self.setname=varDict["Sample Set:"]
      if varDict["Elevation Adjustment"]=="On":
         self.elevfactor=varDict["Elevation Factor"]
      else:
         self.elevfactor=0.0
      if self.elevfactor<1:
         self.elevfactor=0.0
#---------------------------------------------------------------------------
#
   def execute(self, Topo, variableElement,variableElement_GridInfo, WEname,
               GridTimeRange, editArea):
      #
      #  get variable type, get the vector edit mode, change the variable
      #  name if we are modifying only a part of a vector, get the min/max
      #  values for the sliders
      #
      wxType=variableElement_GridInfo.getGridType().toString()
      UpdatedName=self.getVectEdit(WEname,wxType)
      self.getResolution(variableElement_GridInfo)
      self.getMinMaxValue(variableElement_GridInfo)
      self.getMasterLabel(GridTimeRange,UpdatedName)
      #
      #  setup mask for editArea
      #
      editAreaMask=self.setupEditAreaMask(editArea)
      #
      #  if user wants the current sample set, set up the
      #  locations array with those values
      #
      if self.setname=="Current Samples":
         err=self.addCurrentSamples(Topo)
         if (err==1):
            self.statusBarMsg("No sample points defined","U")
            self.cancel()
      #
      #  setup sliders with current value at each point
      #  and save the current values for later
      #
      err=self.setInitValues(wxType,variableElement,editAreaMask,Topo)
      if (err==1):
         self.statusBarMsg("No control points defined","U")
         self.cancel()
      #
      #  Run the dialog which is a new Tkinter root window every time execute runs.
      #  The dialog will halt execution of the main processing until the dialog
      #  window is destroyed.  The values from the GUI will be in an attribute of
      #  the ControlValues instance.
      #
      dialog=ControlValues(self.guiInfo, SC.Config["MaxPointsInColumn"], "Set Control Values")
      dialog.mainloop()
      if (dialog.result!="OK"):
         self.cancel()
      #
      #  If the user pressed OK, get the changes and get the
      #  remoteness, the average min distance to other control
      #  points, and the distance weights from each control point
      #  to all other gridpoints (all values that will be used
      #  later in the serp algorithm)
      #
      self.getChangeValues(self.guiInfo['vectedit'],
                           dialog.Values,
                           self.guiInfo['InitValues'],
                           self.InitDirs,
                           self.InitSpeeds
                           )
      #
      #  Handle adding no-change points around the outside of the
      #  editArea, if the tool is not operating on the whole grid.
      #
      self.handleEditArea(editAreaMask)
      #
      #  Calculate the change grid
      #
      zval=self.OA.Serp(self.zlist,self.xloclist,self.yloclist,self.hloclist,
                        self.elevfactor,Topo)
      #
      #  add result to the original values -
      #    OR - for vectors, modify the speed/dir
      #
      absmax=variableElement_GridInfo.getMaxValue()
      absmin=variableElement_GridInfo.getMinValue()
      if wxType=='SCALAR': # scalar
         return clip(variableElement+zval,absmin,absmax)
      if wxType=='VECTOR': # vector
         speed=variableElement[0]
         direc=variableElement[1]
         if (self.guiInfo['vectedit']==1):
            newspd=speed
            newdir=direc+zval
            newdir[greater(newdir, 360)] -= 360
            newdir[less(newdir ,0)] += 360
         elif (self.guiInfo['vectedit']==0):
            newspd=clip(speed+zval,absmin,absmax)
            newdir=direc
         else:
            newspd=clip(speed+zval,absmin,absmax)
            zval=self.OA.Serp(self.ylist,self.xloclist,self.yloclist,self.hloclist,
                        self.elevfactor,Topo)
            newdir=direc+zval
            newdir[greater(newdir, 360)] -= 360
            newdir[less(newdir ,0)] += 360

         return (newspd,newdir)
#---------------------------------------------------------------------------
#
#  Make label for controlpoint dialog with timerange of grid
#
   def getMasterLabel(self,GridTimeRange,WEname):
      startday=GridTimeRange.startTime().day
      starthour=GridTimeRange.startTime().hour
      endday=GridTimeRange.endTime().day
      endhour=GridTimeRange.endTime().hour
      self.guiInfo["masterlabel"]="Set %s for %d / %2.2dZ --> %d / %2.2dZ" % (WEname,
                  startday,starthour,endday,endhour)
      return
#---------------------------------------------------------------------------
#
#  Get the vector edit mode (and modify WEname if needed),
#      vectedit=0 if a scalar or a vector modifying only magnitude
#      vectedit=1 if a vector modifying direction only
#      vectedit=2 if a vector modifying both
#
   def getVectEdit(self,WEname,wxType):
      self.guiInfo["vectedit"]=0
      if (wxType=='VECTOR'):
         vecteditstring=self.getVectorEditMode()
         if (vecteditstring=="Magnitude Only"):
            self.guiInfo["vectedit"]=0
            WEname+="Spd"
         if (vecteditstring=="Direction Only"):
            self.guiInfo["vectedit"]=1
            WEname+="Dir"
         if (vecteditstring=="Both"):
            self.guiInfo["vectedit"]=2
      return(WEname)
#---------------------------------------------------------------------------
#
#  Get the resolution of changes (i.e. 0.01 for QPF,  0.1 for SnowAmount)
#  by using the parm precision information
#
   def getResolution(self,variableElement_GridInfo):
      precision=variableElement_GridInfo.getPrecision()
      if (precision==0):
         self.guiInfo["resolution"]=1.0
      else:
         self.guiInfo["resolution"]=1.0/(10**precision)
      return
#---------------------------------------------------------------------------
#
#  Get the minimum/maximum value for the sliders from the variable
#  max/min limits
#
   def getMinMaxValue(self,variableElement_GridInfo):
      self.guiInfo["minvalue"]=variableElement_GridInfo.getMinValue()
      self.guiInfo["maxvalue"]=variableElement_GridInfo.getMaxValue()
      if (self.guiInfo["vectedit"]==1):
         self.guiInfo["minvalue"]=0
         self.guiInfo["maxvalue"]=360
      return
#---------------------------------------------------------------------------
#
#  Add the current sample point lat/lon to the Locations array
#    return an err of 1 if no sample points are currently specified
#    return an err of 0 if some sample points were found
#
   def addCurrentSamples(self,Topo):
       shape1=Topo.shape
       ymax=shape1[0]-1
       xmax=shape1[1]-1
       self.samplePoints = self.getSamplePoints(None)
       curpoints=[]
       for sample in self.samplePoints:
          (x,y)=sample
          if (x<0)or(x>xmax)or(y<0)or(y>ymax):
             LogStream.logEvent("serp:sample point at %d,%d is off GFE grid - ignored"%(x,y))
             continue
          (lat,lon)=self.getLatLon(x,y)
          label="%5.2f %7.2f" % (lat,lon)
          curpoints.append((label,lat,lon))
       if (len(curpoints)<1):
          return 1
       SC.Config["Locations"]["Current Samples"]=curpoints
       return 0
#---------------------------------------------------------------------------
#
#  Limit direction changes to +/- 180 degrees
#
   def limitDirChange(self,dirchg):
      while dirchg>180:
         dirchg=dirchg-360
      while dirchg<-180:
         dirchg=dirchg+360
      return dirchg
#---------------------------------------------------------------------------
#
#  setup InitValues array with current values at points,
#  as well as xloclist, yloclist, hloclist with location/elevation at points
#
   def setInitValues(self,wxType,variableElement,editAreaMask,Topo):

      self.xloclist=[]
      self.yloclist=[]
      self.hloclist=[]
      self.guiInfo['InitValues']=[]
      self.guiInfo['Labels']=[]
      self.InitSpeeds=[]
      self.InitDirs=[]
      for i in range(len(SC.Config["Locations"][self.setname])):
         (name,lat,lon)=SC.Config["Locations"][self.setname][i]
         (x,y)=self.getGridCell(lat,lon)
         if ((x is None)or(y is None)):
            msg="serp:point %s ignored because it is off the GFE grid"%name
            LogStream.logEvent(msg)
            continue
         #
         #  Ignore sites not on the GFE grid
         #
         xint=int(round(x,0)+0.5)
         yint=int(round(y,0)+0.5)
         if (editAreaMask[yint,xint]<0.5):
            LogStream.logEvent("serp:point %s ignored because it is not in editArea"%name)
            continue
         #
         #  ignore sites at a gridpoint already included
         #
         if ((xint in self.xloclist) and (yint in self.yloclist)):
            skip=0
            for j in range(len(self.xloclist)):
               if ((xint==self.xloclist[j])and(yint==self.yloclist[j])):
                  skip=1
                  continue
            if (skip==1):
               LogStream.logEvent("serp:point %s ignored because gridpoint is already a control point"%name)
               continue
         #
         #  append location to control point list
         #
         self.guiInfo['Labels'].append(name)
         elev=Topo[yint,xint]
         self.hloclist.append(elev)
         self.xloclist.append(xint)
         self.yloclist.append(yint)
         #
         #  get initial value at control points
         #
         if wxType=='SCALAR':
            current=self.round(variableElement[yint,xint],"Nearest",self.guiInfo['resolution'])
         else:
            if (self.guiInfo['vectedit']==0):
               current=self.round(variableElement[0][yint,xint],"Nearest",self.guiInfo['resolution'])
            elif (self.guiInfo['vectedit']==1):
               current=self.round(variableElement[1][yint,xint],"Nearest",self.guiInfo['resolution'])
            else:
               curspd=variableElement[0][yint,xint]
               curdir=variableElement[1][yint,xint]
               self.InitSpeeds.append(curspd)
               self.InitDirs.append(curdir)
               current="%3d@%-3d" % (int(curdir+0.5),int(curspd+0.5))
         self.guiInfo['InitValues'].append(current)
      #
      #  return error if no points in control point list
      #
      if (len(self.xloclist)<1):
         return 1
      return 0
#---------------------------------------------------------------------------
#
#  get change values at every point (zlist), if a vector change - also get ylist
#
   def getChangeValues(self,vectedit,Values,InitValues,InitDirs,InitSpeeds):

      self.zlist=[];
      self.ylist=[];
      for i in range(len(InitValues)):
         if (vectedit==2):
            valreturn=Values[i]
            (dirstr,spdstr)=valreturn.split("@")
            dir1=int(dirstr)
            spd1=int(spdstr)
            dirchg=self.limitDirChange(dir1-InitDirs[i])
            spdchg=spd1-InitSpeeds[i]
            self.zlist.append(spdchg)
            self.ylist.append(dirchg)
         else:
            change=Values[i]-InitValues[i]
            if (vectedit==1):
               change=self.limitDirChange(change)
            self.zlist.append(change)
#------------------------------------------------------------------------------
#
#  setupEditAreaMask - sets up a mask for gridpoints inside the editArea
#
   def setupEditAreaMask(self,editArea):
      if editArea is None:
         mask=self.getTopo()*0
      else:
         mask=self.encodeEditArea(editArea)
      return mask
#------------------------------------------------------------------------------
#
#  handleEditArea - if an editArea is specified, then it adds in "bogus"
#    control points that specify "no change" just outside the border of 
#    the editArea
#
   def handleEditArea(self,editAreaMask):
      #
      #  If editArea include all gridpoints - then no bogus points are 
      #  needed
      #
      Topo=self.getTopo()
      allpts=add.reduce(add.reduce(less(Topo*0.0,5)))
      numpts=add.reduce(add.reduce(editAreaMask))
      if numpts==allpts:
          return
      #
      #  make out1 a grid that is 1 for all pixels just outside the
      #  editArea
      #
      mask=editAreaMask*100
      smooth1=self.smoothpm(mask,1)
      out1=logical_and(greater(smooth1,0),less(mask,50))
      #
      #  get list of all x,y coords that are on the edge
      #
      xl=[]
      yl=[]
      for iy in range(Topo.shape[0]):
          for ix in range(Topo.shape[1]):
              if out1[iy,ix]>0.5:
                  xl.append(ix)
                  yl.append(iy)
      #
      #  Thin the points (if needed)
      #
      roughMax=250
      if len(xl)>roughMax:
          thinamt=float(len(xl))/float(roughMax)
          (xpts,ypts)=self.thinpts(xl,yl,thinamt)
      else:
          xpts=xl
          ypts=yl
      #
      #  We can simply add these points to the list of points.
      #  Normally, we would have to be careful to make sure that
      #  a duplicate point did not exist.  But here, all the normal
      #  control points are inside the editArea, and all these 
      #  added "bogus" points are outside the editArea, so they are
      #  guaranteed to not be a duplicate of the others
      #
      for i in range(len(xpts)):
         elev=Topo[ypts[i],xpts[i]]
         self.hloclist.append(elev)
         self.xloclist.append(xpts[i])
         self.yloclist.append(ypts[i])
         self.zlist.append(0.0)
         self.ylist.append(0.0)
      #
      #
      #
      return
   #-------------------------------------------------------------------
   #  Given a list of x,y coordinates of points - thin the list
   #  so that no points are closer than "num" gridpoints to another
   #
   def thinpts(self,xl,yl,num):
      xc=copy.copy(xl)
      yc=copy.copy(yl)
      xpts=[]
      ypts=[]
      xp=xc[0]
      yp=yc[0]
      xpts.append(xp)
      ypts.append(yp)
      while len(xc)>0:
         dlist=self.within(xp,yp,xc,yc,num)
         dlist.sort()
         dlist.reverse()
         for i in range(len(dlist)):
            del xc[dlist[i]]
            del yc[dlist[i]]
         del dlist
         if len(xc)>0:
            (xnear,ynear)=self.nearest(xp,yp,xc,yc)
            xp=xnear
            yp=ynear
            xpts.append(xp)
            ypts.append(yp)
      return(xpts,ypts)
   #-------------------------------------------------------------------
   #  Return x,y of point nearest xp,yp
   #
   def nearest(self,xp,yp,xc,yc):
      dist=9.0e10
      for i in range(len(xc)):
         dif2=((xc[i]-xp)**2)+((yc[i]-yp)**2)
         if dif2<dist:
            xnear=xc[i]
            ynear=yc[i]
      return(xnear,ynear)
   #-------------------------------------------------------------------
   #  Return list of point indices that are within num points of xp,yp
   #
   def within(self,xp,yp,xc,yc,num):
      num2=num**2
      clist=[]
      for i in range(len(xc)):
         dif2=((xc[i]-xp)**2)+((yc[i]-yp)**2)
         if dif2<num2:
            clist.append(i)
      return clist 
   #=======================================================================
   #
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
   #             Much faster by using the cumsum function in numpy.
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
         #
         #  The really fast part
         #
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
         #
         #  The really fast part
         #
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
         gridmin1=where(mask,gridmin,float32(0))
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
#
#  Custom dialog that provides sliders or entries for each control point
#
import AppDialog
class ControlValues(AppDialog.AppDialog):
    # Use custom __init__ method to get the guiInfo with the labels and initial
    # values for the sliders.
    def __init__(self, guiInfo, MaxPointsInColumn, title="Tk", **kwargs):
       self.guiInfo=guiInfo
       self.MaxPointsInColumn=MaxPointsInColumn
       self.result=None
       AppDialog.AppDialog.__init__(self, **kwargs)
       self.title(title)

    def setChanges(self,master):
        decimal=abs(int(math.log10(self.guiInfo['resolution'])))
        places=int(math.log10(
                       max(self.guiInfo['maxvalue'], abs(self.guiInfo['minvalue']))
                             )
                  )+1
        if decimal==0:
           for i in range(len(self.ChangeVals)):
              self.ChangeVals[i].set("(%+*d)"%(places,
                            self.Values[i].get()-self.guiInfo['InitValues'][i]))
        else:
           for i in range(len(self.ChangeVals)):
              self.ChangeVals[i].set("(%+*.*f)"%(places+decimal+1,
                            decimal,self.Values[i].get()-self.guiInfo['InitValues'][i]))

    def body(self,master):
        masterlabel=self.guiInfo['masterlabel']

        #
        #  get number of rows/columns for dialog
        #  based on number of points - trying to balance it as much as
        #  possible without going over the MaxPointsInColumn value
        #
        numpoints=len(self.guiInfo['Labels'])
        numcols=(int((numpoints-1)/self.MaxPointsInColumn))+1
        numrows=(int((numpoints-1)/numcols))+1
        #
        #  Masterlabel at top of dialog
        #
        frame =Tkinter.Frame(master) 
        frame.pack(side=Tkinter.TOP)
        Tkinter.Label(frame,text=masterlabel).pack(side=Tkinter.TOP,fill=Tkinter.X,expand=1)
        #
        #  grid of scale or entry widgets
        #
        num=0
        self.ScaleIDs=[]
        self.ChangeVals=[]
        self.Values=[]
        for col in range(numcols):
            fc=Tkinter.Frame(frame)
            for row in range(numrows):
                if (num>=len(self.guiInfo['Labels'])):
                    continue
                fr=Tkinter.Frame(fc,relief=Tkinter.GROOVE,borderwidth=1)
                lab=Tkinter.Label(fr,text=self.guiInfo['Labels'][num])
                # Make Tkinter variables for use as widget variables
                # textvar to show delta from original value
                tkStrVar=Tkinter.StringVar()
                tkStrVar.set('(0)')
                self.ChangeVals.append(tkStrVar)
                # The slider values
                tkDblVar=Tkinter.DoubleVar()
                tkDblVar.set(self.guiInfo['InitValues'][num])
                self.Values.append(tkDblVar)

                if (self.guiInfo['vectedit']==2):
                    lab.grid(row=0,column=0,sticky=Tkinter.EW)
                    self.ScaleIDs.append(Tkinter.Entry(fr,width=7))
                    self.ScaleIDs[num].delete(0,Tkinter.END)
                    self.ScaleIDs[num].insert(Tkinter.END,self.guiInfo['InitValues'][num])
                    self.ScaleIDs[num].grid(row=1,column=0)
                else:
                    lab.grid(row=0,column=0,columnspan=2,sticky=Tkinter.EW)
                    self.ScaleIDs.append(
                             Tkinter.Scale(fr, orient=Tkinter.HORIZONTAL,
                                           from_=self.guiInfo['minvalue'],
                                           to=self.guiInfo['maxvalue'],
                                           resolution=self.guiInfo['resolution'],
                                           variable=self.Values[num],
                                           command=self.setChanges,
                                           length=175
                                           ))
                    val=self.guiInfo['InitValues'][num]
                    self.ScaleIDs[num].set(val)
                    self.ScaleIDs[num].grid(row=1,column=0,sticky=Tkinter.EW)
                    chg=Tkinter.Label(fr,textvariable=self.ChangeVals[num])
                    chg.grid(row=1,column=1,sticky=Tkinter.S)
                    fr.columnconfigure(1,minsize=60)
                fr.pack(side=Tkinter.TOP,fill=Tkinter.X)
                num=num+1
            fc.pack(side=Tkinter.LEFT,fill=Tkinter.Y,expand=0)
        # AppDialog wants a widget returned from body to set the focus to.
        return frame

    def ok(self, event=None):
        """Process the Ok button. The ok method in AppDialog destroys the window
        before running the apply method.  Need to run apply first to get the
        data from the slider widgets."""
        if self.validate():
            self.apply()
            self.result="OK"
            self.destroy()

    def apply(self):
        """Retrieve the values from the scale widgets into attribute Values."""
        self.Values=[]
        for num in range(len(self.guiInfo['Labels'])):
            self.Values.append(self.ScaleIDs[num].get())
