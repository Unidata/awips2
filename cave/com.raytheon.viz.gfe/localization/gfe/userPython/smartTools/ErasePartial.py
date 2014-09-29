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
# ErasePartial - version 1.1
#
#   Erase a feature - by "deleting" what is inside the editArea, and "filling
#   in" with something "reasonable" based on the data outside the editArea.
#   The "filling in" is done by performing an objective analysis using the
#   "points around the outside of the editArea" as the "data points", and a
#   a first guess of a "flat field".  The results of the analysis are then
#   returned INSIDE the editArea, with the data outside the editArea unchanged.
#
#   Uses the serp routine of the ObjAnal utility to perform the analysis.
#   Automatically "thins" the number of control points if the editArea is
#   so large that it would affect performance.
#
#   This version allows the user to specify the topography influence and
#   the "percentage" of erasing that is desired.  The "quick" version Erase
#   sets the topography influence at 36.0 and percentage to 100%, so that
#   the tool can run without user interaction.  The EraseSmooth sets the
#   topography value at 0, and the percentage at 100%, and also runs without
#   user interaction.
#
# Author: Tim Barker - SOO Boise, ID
#  2011-01-13 - version 1.0 - Original Implementation
#  2011-02-18 - version 1.1 - AWIPS-2 Port
#
# ----------------------------------------------------------------------------
ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList=["SCALAR","VECTOR"]
#
#  Ask user to set topography factor and percentage to erase
#
VariableList=[("Topography Factor:",36.0,"scale",[0.0,100.0]),
              ("Percentage Erase:",100.0,"scale",[0.0,100.0]),
             ]
#
#
#
from numpy import *
import SmartScript,ObjAnal,copy

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss=dbss
        SmartScript.SmartScript.__init__(self, dbss)
    def preProcessTool(self,varDict):
        self.OA = ObjAnal.ObjAnal(self._dbss)
        return
    def execute(self, editArea, Topo, variableElement, variableElement_GridInfo, varDict):
        "Erase Feature with Topo effects"
        elevFactor=varDict["Topography Factor:"]
        percent=varDict["Percentage Erase:"]
        #
        #  Get a grid containing all points within 1 pixel of
        #  editArea (out1)
        #
        mask=self.encodeEditArea(editArea)*100
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
        #  Figure out if vector, and if so, which piece (or pieces)
        #  of vector need to be modified.  Use the doAnal routine
        #  to do the analysis.
        #
        wxType=variableElement_GridInfo.getGridType().toString()
        #
        #  For SCALAR elements - just use doAnal to do the analysis
        #
        if wxType=="SCALAR":
            finalGrid=self.doAnal(variableElement,xpts,ypts,elevFactor,Topo,mask,percent)
        #
        #  For VECTOR elements - split apart the mag/dir of the incoming grid
        #
        elif wxType=="VECTOR":
            (origMag,origDir)=variableElement
            vecteditstring=self.getVectorEditMode()
            #
            #  If only magnitude - use doAnal to do a scalar analysis on
            #  the magnitude, and use the original direction
            #
            if (vecteditstring=="Magnitude Only"):
                finalMag=self.doAnal(origMag,xpts,ypts,elevFactor,Topo,mask,percent)
                finalGrid=(finalMag,origDir)
            #
            #  For "Dir Only", or "Both Mag/Dir" - do TWO analyses (one for
            #  U component, other for V component)
            #
            else:
                (origU,origV)=self.MagDirToUV(origMag,origDir)
                finalU=self.doAnal(origU,xpts,ypts,elevFactor,Topo,mask,percent)
                finalV=self.doAnal(origV,xpts,ypts,elevFactor,Topo,mask,percent)
                (finalMag,finalDir)=self.UVToMagDir(finalU,finalV)
                #
                #  If "Dir Only", then return the new dir with the original
                #  magnitude
                #
                if (vecteditstring=="Direction Only"):
                    finalGrid=(origMag,finalDir)
                #
                #  If "Both Mag/Dir", then return the full result of the
                #  combined U/V analyses
                #
                else:
                    finalGrid=(finalMag,finalDir)
        #
        #  Return finalGrid
        #
        return finalGrid
    #-----------------------------------------------------------------
    #
    #  Do the scalar analysis - only replacing values inside the
    #  mask editArea
    #
    def doAnal(self,origGrid,xpts,ypts,elevFactor,Topo,mask,percent):
        #
        #  Get values of the current grid on the points
        #
        xlist=[]
        ylist=[]
        zlist=[]
        hlist=[]
        for i in range(len(xpts)):
            xp=xpts[i]
            yp=ypts[i]
            xlist.append(xp)
            ylist.append(yp)
            zlist.append(origGrid[yp,xp])
            hlist.append(Topo[yp,xp])
        #
        #  Do the analysis
        #
        analGrid=self.OA.Serp(zlist,xlist,ylist,hlist,elevFactor,Topo)
        #
        #  Substitude the analysis values inside the editArea
        #
        pct=percent/100.0
        pctold=1.0-pct
        new=(analGrid*pct)+(origGrid*pctold)
        finalGrid=where(mask,new,origGrid)
        #
        #  Return the modified grid
        #
        return finalGrid
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
        #return
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
