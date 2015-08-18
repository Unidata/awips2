# ----------------------------------------------------------------------------
# SVN: $Revision: 134 $  $Date: 2010-08-26 17:32:30 +0000 (Thu, 26 Aug 2010) $
#
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ObjAnal - version 2.12 - various Objective Analysis routines
#
# Author: Tim Barker - SOO Boise, ID
#
# 2014/10/06 - Version 2.12.  Fix typo with timetupe in logtime which handles
#              when running in simulations.
# 2014/08/31 - Version 2.11.  Get rid of debug print statement that shouldn't
#              have been there in the first place.
# 2014/07/28 - Version 2.10.  Fix issues when ActualElev=1 and landMask is
#              used, and a control point near the edge of the landMask has
#              an elevation that is wildly different than the grid elevation
#              at at that location.  Also introduce the concept of a 'missing'
#              elevation value for the point obs.  If the elevation is missing
#              the code will use the grid elevation - regardless of the 
#              setting of ActualElev.   Defaults to -500ft.  Can be changed
#              with new setMissingElevThreshold routine (but doubt anybody will)
# 2014/03/20 - Version 2.8.  Better import of numpy.  Used SmartScript for
#              _gmtime instead of time module (for more effective playback)
# 2014/01/10 - Version 2.7.  Fixed copy of self._empty
# 2013/12/03 - Version 2.6.  Fixed a typo in the ActualElev code, and made
#              using ActualElev the default.
# 2013/05/04 - Version 2.5.  Tweaked the code a bit more when using Serp
#              and actual_elev=1.  Does a better job of estimating what 
#              the grid WOULD have at the ob elevation - by finding a best
#              match among surrounding gridpoints, rather than a value
#              vs. elevation regression.
# 2012/09/11 - Version 2.4.  Fixed a bug where consecutive calls to Serp 
#              using different points incorrectly tried to use the cached
#              point data the second time through - and could crash the
#              calculations.
# 2012/08/15 - Version 2.3.  Added configuration element to control size of
#              cache for Serp distance grids.  Trimmed memory usage in Serp 
#              a little more. Changed sense of Verbose logging.  Changed to 
#              CamelCase for config parameters.
# 2012/06/02 - Version 2.2 - Added code to produce better analyses when
#              using ActualElev=1.  Now estimates what the grid "would"
#              have at that elevation at that gridpoint.  This makes the
#              magnitude of changes needed much more reasonable. In Serp 
#              routine, a final step to match the point obs exactly was 
#              added at the end.  Added some memory enhancements in Serp.
# 2011/03/11 - Handle AWIPS-2 vector grids now being LISTS, instead of Tuples.
# 2010/07/30 - AWIPS 2 port by Paul Jendrowski
# 2007/07/10 - Add code for Barnes and Inverse Distance Squared (most of
#              the code came from Ken Pomeroy and Chris Gibson).
# 2007/06/17 - Add code for handling a land/sea mask. Essentially just
#              makes gridpoints not on the same (land or sea) appear to
#              be MUCH further apart.
# 2006/10/10 - Reduce memory in the Serp routines
# ----------------------------------------------------------------------------
import numpy as np
import SmartScript
import sys,types,math,os,gc
import numpy.linalg as LinearAlgebra

class ObjAnal(SmartScript.SmartScript):
   def __init__(self, dataMgr, mdMode=None, toolType="numeric"):
      SmartScript.SmartScript.__init__(self,dataMgr)
      self.verbose=0
      #
      #  speed up distance calculations with vectors of I/J coords
      #
      gridsize=self.getGridShape()
      ij=np.indices(gridsize,dtype=np.float32)
      i=ij[1]
      self.Irow=i[0,:]
      j=ij[0]
      self.Jrow=j[:,0]
      #
      #  Size of output grid is based on GFE gridsize
      #
      self.ymax=self.getGridShape()[0]
      self.xmax=self.getGridShape()[1]
      self.gridres=self.getGridSpacing()
      #
      #  If ActualElev=1...then use the station elevation for elevation
      #                     related calculations.
      #     otherwise.......use the elevation of the gridpoint that
      #                     contains the station for elevation related
      #                     calculations
      #     However...if the station elevation is lower than the missing
      #               elevation Threshold, then use the grid elevation
      #               even if ActualElev is equal to 1.
      #
      self.ActualElev=1
      self.MissingElevThreshold=-500
      #
      #  Default Serp parameters
      #    Cache (500 by default) (between 0 and 1000) amount of memory
      #        (in MB) allowed for saving distance grids between Serp
      #        calls. 
      #    Elevfactor - the elevation factor used in the previous Serp
      #        analysis
      #    SerpXYgrids - the cache of distance grids saved between Serp
      #        runs
      #
      self.SerpLastPoints=0
      self.SerpCache=500
      self.SerpXYgrids={}
      self.SerpElevfactor=-1.0
      #
      #  Default Barnes parameters
      #    Gamma (0.3 by default) (should be between 0.1 and 1.0)
      #    Spacing (calculated by default) wavelength below which
      #             data will be filtered.
      #
      self.BarnesGamma=0.3
      self.BarnesSpacing=-1 # negative value forces calculation
      #
      #  Default DSquared parameters
      #     Dist    --- minimum radius around a gridpoint to search for
      #                 station data to use in the weighted average
      #     MaxPoints - maximum number of stations to use in the
      #                 weighted average for a gridpoint.
      #
      self.DSquaredDist=-1
      self.DSquaredMaxPoints=-1
      
      return

   #---------------------------------------------------------------------------
   #  ObjectiveAnalysis - perform an objective analysis of the point values,
   #      using the specified guess grid.  If the guess grid is a vector type 
   #      then both the point values and grids are handled differently.
   #
   def ObjectiveAnalysis(self,values,guessGrid,analysisType,
                         elevfactor=0.0,topoGrid=None,landMask=None):
      self.logtime("Performing %s analysis"%analysisType,1)
      self.logtime("Mem usage at start of ObjectiveAnalysis: %d"%memory(),5)
      if topoGrid is None:
         topoGrid=self.getTopo()
      if landMask is None:
         landMask=self.newGrid(True, bool)
      values=self.removeDuplicates(values)
      gridType=type(guessGrid)
      if ((gridType is not types.TupleType)and(gridType is not types.ListType)):
         new=self.ObjectiveAnalysisScalar(values,guessGrid,analysisType,
                                          elevfactor,topoGrid,
                                          landMask)
         self.logtime("Mem usage at end of ObjectiveAnalysis: %d"%memory(),5)
         return new
      else: # vector
         uvalues=[]
         vvalues=[]
         for i in range(len(values)):
            (name,x,y,elev,spd,direc)=values[i]
            (u,v)=self.MagDirToUV(spd,direc)
            uvalues.append((name,x,y,elev,u))
            vvalues.append((name,x,y,elev,v))
         (spdgrid,dirgrid)=guessGrid
         (uguess,vguess)=self.MagDirToUV(spdgrid,dirgrid)
         #
         unew=self.ObjectiveAnalysisScalar(uvalues,uguess,analysisType,
                                           elevfactor,topoGrid,
                                           landMask=landMask)
         vnew=self.ObjectiveAnalysisScalar(vvalues,vguess,analysisType,
                                           elevfactor,topoGrid,
                                           landMask)
         (newspd,newdir)=self.UVToMagDir(unew,vnew)
         self.logtime("Mem usage at end of ObjectiveAnalysis (vector): %d"%memory(),5)
         self.logtime("%s analysis complete"%analysisType,1)
         return(newspd,newdir)
   #---------------------------------------------------------------------------
   #  ObjectiveAnalysisScalar - perform an objective analysis of the point
   #      values, using the specified guess grid.  Point values are a list of
   #      tuples. Each tuple contains: name,x,y,elev,val
   #
   def ObjectiveAnalysisScalar(self,values,guessGrid,analysisType,
                               elevfactor,topoGrid,landMask=None):
      self.logtime("Mem usage at start of ObjectiveAnalysisScalar: %d"%memory(),5)
      #
      #  Make lists of x,y,h,value-guess - and get rid of points
      #  that are off the grid
      #
      xloclist=[]
      yloclist=[]
      hloclist=[]
      zlist=[]
      if landMask is None:
          newlandMask=self.newGrid(True, bool)
      else:
          newLandMask=landMask
      self.logtime("Point values used in analysis:",4)
      for i in range(len(values)):
         (name,x,y,elev,val)=values[i]
         if (x>(self.xmax-1))or(x<0)or(y>(self.ymax-1))or(y<0):
            continue
         #
         #  If the ob point elevation is missing - always use
         #  the gridpoint elevation
         #
         if elev>self.MissingElevThreshold:
            hloclist.append(elev)
         else:
            hloclist.append(topoGrid[y,x])
         xloclist.append(x)
         yloclist.append(y)
         #
         #  If using the grid elevation at the point, then the
         #  z value (change)is simply the observed value minus the guess
         #  grid value.
         #
         if self.ActualElev!=1:
            self.logtime("  %12s %3d,%3d %5d Val:%5.1f -- grid:%5.1f -- change:%5.1f"%(name,x,y,elev,val,guessGrid[y,x],val-guessGrid[y,x]),4)
            zlist.append(val-guessGrid[y,x])
             
         #
         # If using actual elevations - then need to make the z value the
         # difference between what the guess grid WOULD have at the ob elevation
         # rather than the guess grid value itself.   Searches outward until
         # it finds a guess grid point with an elevation less than 100 feet
         # from the ob's elevation.
         #
         else:
            pt=topoGrid[y,x]
            obLandMask=newLandMask[y,x]
            desiredDiff=100
            bestval=guessGrid[y,x]
            if elev>self.MissingElevThreshold:
               bestdif=abs(elev-pt)
            else:
               bestdif=0
            bestele=pt
            wid=1
            #
            #  Spiral out from the point - looking for nearby gridpoints
            #  that are closer to the actual observation elevation
            #  than the gridpoint elevation.  When we find one within
            #  100ft of the observation - stop searching and use the
            #  grid value at that point to determine how much we need
            #  to change the grid at the observation gridpoint.
            #
            while ((bestdif>desiredDiff)and(wid<10)):
               #print "  searching with wid=%d"%wid
               if ((y+wid)<self.ymax):
                  for ii in range(max(0,x-wid),min(x+wid+1,self.xmax)):
                     if obLandMask==newLandMask[y+wid,ii]:
                        gelev=topoGrid[y+wid,ii]
                        dif=abs(elev-gelev)
                        if dif<bestdif:
                           bestdif=dif
                           bestele=gelev
                           bestval=guessGrid[y+wid,ii]
               if ((y-wid)>=0):
                  for ii in range(max(0,x-wid),min(x+wid+1,self.xmax)):
                     if obLandMask==newLandMask[y-wid,ii]:
                        gelev=topoGrid[y-wid,ii]
                        dif=abs(elev-gelev)
                        if dif<bestdif:
                           bestdif=dif
                           bestele=gelev
                           bestval=guessGrid[y-wid,ii]
               if ((x+wid)<self.xmax):
                  for jj in range(max(0,y-wid),min(y+wid+1,self.ymax)):
                     if obLandMask==newLandMask[jj,x+wid]:
                        gelev=topoGrid[jj,x+wid]
                        dif=abs(elev-gelev)
                        if dif<bestdif:
                           bestdif=dif
                           bestele=gelev
                           bestval=guessGrid[jj,x+wid]
               if ((x-wid)>=0):
                  for jj in range(max(0,y-wid),min(y+wid+1,self.ymax)):
                     if obLandMask==newLandMask[jj,x-wid]:
                        gelev=topoGrid[jj,x-wid]
                        dif=abs(elev-gelev)
                        if dif<bestdif:
                           bestdif=dif
                           bestele=gelev
                           bestval=guessGrid[jj,x-wid]
               if bestdif>desiredDiff:
                  wid+=1
            estval=bestval   
            self.logtime("  %12s %3d,%3d, est at %5d:%5.1f --- grid at %5d:%5.1f --- (%5d diff) -- Val:%5.1f -- Change:%5.1f"%(name,x,y,elev,estval,pt,guessGrid[y,x],pt-elev,val,val-estval),4)
            zlist.append(val-estval)
      #
      #  Do the requested analysis
      #
      if analysisType=="serp":
         zval=self.Serp(zlist,xloclist,yloclist,hloclist,elevfactor,
                        topoGrid,landMask=landMask)
         finalGrid=(guessGrid+zval).astype(np.float32)
         if self.ActualElev==1:
            for i in range(len(values)):
               (name,x,y,elev,val)=values[i]
               if (x>(self.xmax-1))or(x<0)or(y>(self.ymax-1))or(y<0):
                  continue
               finalGrid[y,x]=val
      elif analysisType=="barnes":
         zval=self.Barnes(zlist,xloclist,yloclist,hloclist,elevfactor,
                          topoGrid,landMask=landMask)
         finalGrid=(guessGrid+zval).astype(np.float32)
      elif analysisType=="dsquared":
         zval=self.Dsquared(zlist,xloclist,yloclist,hloclist,elevfactor,
                               topoGrid,landMask=landMask)
         finalGrid=(guessGrid+zval).astype(np.float32)
      else:
         self.logtime("Unknown analysisType:%s"%analysisType)
         zval=self.empty()
         finalGrid=(guessGrid+zval).astype(np.float32)
      self.logtime("Mem usage at end of ObjectiveAnalysisScalar: %d"%memory(),5)
      return finalGrid
   #---------------------------------------------------------------------------
   #  removeDuplicates(stationlist) - find any stations in the same x,y gridbox
   #      and average the data for those stations, returning a new stationlist.  
   #      The stationlist is a list of tuples.  For vectors the tuples have 6 
   #      values: name,x,y,elev,speed,direc  For scalars the tuples have 5
   #      values: name,x,y,elev,value
   #
   def removeDuplicates(self,values):
      if len(values)<1:
         return values
      test=values[0]
      numpieces=len(test)
      if len(test)==6:
         type="VECTOR"
      elif len(test)==5:
         type="SCALAR"
      else:
         return values
      #
      newvalues=[]
      hash={}
      for stn in values:
         x=stn[1]
         y=stn[2]
         key="%4.4d%4.4d"%(x,y)
         if key in hash:
            list=hash[key]
            list.append(stn)
            hash[key]=list
         else:
            list=[]
            list.append(stn)
            hash[key]=list

      hkeys=hash.keys()
      hkeys.sort()
      for key in hkeys:
         stnlist=hash[key]
         if (len(stnlist)==1):
            newvalues.append(stnlist[0])
         else:
            valsum=0
            usum=0
            vsum=0
            valnum=0
            avgnames=""
            for stn in stnlist:
               if type=="VECTOR":
                  (name,x,y,elev,spd,direc)=stn
                  (u,v)=self.MagDirToUV(spd,direc)
                  usum=usum+u
                  vsum=vsum+v
               else:
                  (name,x,y,elev,val)=stn
                  valsum=valsum+val
               valnum=valnum+1
               avgnames=avgnames+name+"+"
            avgname=avgnames[:-1]
            if type=="VECTOR":
               uavg=float(usum)/float(valnum)
               vavg=float(vsum)/float(valnum)
               (spd,direc)=self.UVToMagDir(uavg,vavg)
               stn=(avgname,x,y,elev,spd,direc)
            else:
               valavg=int(float(valsum)/float(valnum))
               stn=(avgname,x,y,elev,valavg)
            newvalues.append(stn)
      return newvalues
   #---------------------------------------------------------------------------
   #  Serp - Given a list of values (zlist) at points (xlist, ylist, hlist 
   #      lists) and topography weighting factor (elevfactor) calculate a grid 
   #      that fits the values exactly, using a curve-fitting algorithm using 
   #      'serpentine' curves.
   #
   #      To save time, this routine carefully checks to see if it has been 
   #      recently called with the same set of gridpoint locations and 
   #      elevation factor - and then skips all the calculations based on 
   #      location - and only applies the code based on the zlist values.
   #
   def Serp(self,zlist,xlist,ylist,hlist,elevfactor,Topo,landMask=None):
      #
      #  Check for case of cbig array being bigger than 2GB.  If so,
      #  likely to have memory problems.  Thus, write an error message
      #  and return with no change.
      #
      mem=((self.xmax*self.ymax)*len(zlist))*8
      self.logtime("Serp memory usage estimate: %d"%mem,5)
      if mem>2147000000:
          self.logtime("  Combination of size of grid (%d x %d) and"%(self.xmax,self.ymax))
          self.logtime("  number of control points (%d) will take up too"%len(zlist))
          self.logtime("  much memory for Serp.  Either use smaller grid, fewer")
          self.logtime("  control points, or use a different analysis scheme")
          chg=Topo*0.0
          return chg
      self.logtime("Mem usage at start of serp: %d"%memory(),5)
      #
      #  Determine if we need to do setup again
      #     first are the number of points different
      #     second is the elevation factor different
      #     third (if still OK) check that each point is in the
      #          distance arrays Disq
      #
      setup=0
      if (len(xlist)!=self.SerpLastPoints):
         setup=1
      if (elevfactor!=self.SerpElevfactor):
         setup=1
      if (setup==0):
         for i in range(len(xlist)):
            x=xlist[i]
            y=ylist[i]
            xy=(y*self.xmax)+x
            if (xy not in self.SerpXYgrids):
               setup=1
               break
      #
      #  Now we know if we need to do the setup stuff again
      #
      if (setup==0):
         self.logtime("Skipping SerpSetup - same points",2)
      else:
         self.logtime("Running SerpSetup",2)
         if elevfactor!=self.SerpElevfactor:
            self.SerpXYgrids={}
            self.SerpElevfactor=elevfactor
         #
         (numpts,xarr,yarr,harr,larr,scaledtopo,newlandMask)=self.setupScaling(xlist,
                 ylist,hlist,elevfactor,Topo,landMask)
         #
         #
         #
         totDistSquared=self.getTotDistSquared(xarr,yarr,harr,larr)
         totDist=np.sqrt(totDistSquared)
         #
         newsize=(numpts,self.ymax,self.xmax)
         #
         #  Get the "remoteness" values which modify the weights
         #
         self.logtime("Calculating Remoteness",3)
         rem=self.getSerpRemoteness(totDist)
         #
         #  For each control point, get the distance to the
         #  next nearest control point
         #
         self.logtime("Calculating MinDist",3)
         dmin=self.getMinDist(totDist)
         dmin2=np.square(dmin)
         del dmin
         del totDist
         #
         #  make a new total distance
         #
         self.SerpDisq=np.zeros(newsize,np.float32)
         #
         #  zero out the avary-array, which varies for every control point
         #
         avary=np.zeros((numpts,numpts),np.float32)
         #
         #  Get maximum number of distance grids to save for quick
         #  recall (dont let it use more than SerpCache MB of space)
         #
         ngrid=self.xmax*self.ymax
         maxsave=int((self.SerpCache*1000000)/(ngrid*8))
         self.logtime("calculated max points to save:%d"%maxsave,4)
         #
         #  Get the factor that relates every control point to
         #  every gridpoint, as well as the sum of those factors
         #
         self.logtime("Calculating SerpDisq",3)
         newcount=0
         dcount=0
         for k in range(numpts):
            x=int(xarr[k])
            y=int(yarr[k])
            avary[k]=dmin2[k]
            xy=(y*self.xmax)+x

            if xy in self.SerpXYgrids:
               tempdist=self.SerpXYgrids[xy]
            else:
               newcount=newcount+1
               xs=np.square(self.Irow-x)
               ys=np.square(self.Jrow-y)
               b=np.add.outer(ys,xs)
               if self.ActualElev==0:
                  elev=scaledtopo[y,x]
               else:
                  elev=harr[k]
               ed=scaledtopo-elev
               land=newlandMask[y,x]
               ld=np.square(land-newlandMask)
               ed=ed+(ld*10000.0)
               tempdist=b+np.square(ed)
               if (len(self.SerpXYgrids)<maxsave):
                  self.SerpXYgrids[xy]=tempdist
                  dcount=dcount+1
                  if dcount>=10:
                     self.logtime("Points saved  so far:%d"%len(self.SerpXYgrids),4)
                     self.logtime("Mem used      so far:%d"%memory(),5)
                     dcount=0
            self.SerpDisq[k]=(rem[k]/(tempdist+dmin2[k])).astype(np.float32)
         self.logtime("Mem after all points in:%d"%memory(),5)
         self.SerpDsum=np.add.reduce(self.SerpDisq)
         #
         #  The coefficients for each control point
         #
         rej=np.transpose(np.resize(rem,(numpts,numpts)))
         SerpWeights=rej/(totDistSquared+avary)
         del rej
         del rem
         del totDistSquared
         del avary
         self.SerpWsum=np.add.reduce(SerpWeights)
         #  
         #  Solve Matrix of weights
         #
         self.SerpCc=LinearAlgebra.inv(SerpWeights).astype(np.float32)
         #
         #  Free up some memory
         #
         del SerpWeights
         self.logtime("Mem before serp setup gc.collect: %d"%memory(),5)
         gc.collect()
         self.SerpLastPoints=numpts
      self.logtime("Mem after serp setup: %d"%memory(),5)
      #
      #  Now do the Serp calculations
      #
      self.logtime("Running Serp calculations",2)
      numpts=len(zlist)
      zarr=np.array(zlist,np.float32)
      #
      #
      #
      nearzero=np.logical_and(np.less(zarr,0.001),np.greater(zarr,-0.001))
      zarr[nearzero] = 0.001
      del nearzero
      zw=zarr*self.SerpWsum
      del zarr
      rjt=np.resize(zw,(numpts,numpts))
      del zw
      rj=np.transpose(rjt)
      del rjt
      self.logtime("Mem usage after rj: %d"%memory(),5)
      #
      #  fastest way I could come up with to expand c array
      #  out into grids that have the same value for every
      #  gridpoint and every control point
      #
      tshape=(self.SerpDisq.shape[1],self.SerpDisq.shape[2],self.SerpDisq.shape[0])
      a1=self.SerpCc*rj
      del rj
      a2=np.add.reduce(a1)
      del a1
      a3=np.resize(a2,tshape)
      del a2
      cbig=np.transpose(a3,(2,0,1))
      del a3
      gc.collect()
      self.logtime("Mem usage after cbig calculation: %d"%memory(),5)
      #
      #  calculate change grid by multiplying each gridpoint by the
      #  weight of each change point (and considering the distance
      #  squared between each gridpoint and the change point)  
      #
      a1=cbig*self.SerpDisq
      del cbig
      a2=np.add.reduce(a1)
      del a1
      gc.collect()
      chg=a2/self.SerpDsum
      del a2
      self.logtime("Mem usage after the chg calculation: %d"%memory(),5)
      self.logtime("Done with serp calculations",2)
      return chg
   #---------------------------------------------------------------------------
   #  setSerpCache - set size of the serp distance grids cache (in MB). The
   #      default value of 500MB allows for a significant speedup in the serp 
   #      routines - by saving and re-using expensive distance calculations 
   #      between runs. However, these are kept in memory and can cause the 
   #      calculations to fail with 'out of memory' errors. You can set this 
   #      value to 0 to NOT use any cache - but expect the analysis to run 20% 
   #      slower each time.
   #
   def setSerpCache(self,value):
      if ((value>=0) and (value<=1000)):
         self.SerpCache=value
      else:
         self.logtime("SerpCache must be between 0 and 1000")
      return
   #---------------------------------------------------------------------------
   #  Dsquared - An inverse distance squared weighting scheme.
   #
   def Dsquared(self,zlist,xlist,ylist,hlist,elevfactor,Topo,
                   landMask=None):
      self.logtime("Running Distance Squared Calculations",2)
      #
      #  Setup elevation and land/sea scaling
      #
      (numpts,xarr,yarr,harr,larr,scaledtopo,newlandMask)=self.setupScaling(xlist,
             ylist,hlist,elevfactor,Topo,landMask)
      #
      #  turn lists into numeric python arrays
      #
      zarr=np.array(zlist,np.float32)
      #
      nearzero=np.logical_and(np.less(zarr,0.001),np.greater(zarr,-0.001))
      zarr[nearzero] = 0.001

      newsize=(numpts,self.ymax,self.xmax)
         
      dsquared=np.zeros(newsize,np.float32)
      dists=np.zeros(newsize,np.float32)
      
      self.logtime("Getting distances",3)
      for k in range(numpts):
         dist=self.getDistance(xarr[k],yarr[k],harr[k],scaledtopo,newlandMask)
         dist[np.less(dist,0.000001)] = 0.000001
         dsquared[k]=(dist*dist).astype(np.float32)
         dists[k]=dist.astype(np.float32)
      self.logtime("Done getting distances",3)   

      if self.DSquaredMaxPoints>0:
         usePoints = min(int(self.DSquaredMaxPoints)-1,numpts-1)
         sortdists=np.sort(dists,0)
         finalDist=sortdists[usePoints]

      totweight=self.empty()
      totsum=self.empty()
      for k in range(numpts):
         w=1.0/dsquared[k]
         if self.DSquaredMaxPoints>0:
            if self.DSquaredDist>0:
               dd=self.DSquaredDist/self.gridres
               finalDist=np.where(np.greater(dd,finalDist),dd,finalDist)
            w[np.greater(dists[k],finalDist)] = 0.0
         elif self.DSquaredDist>0:
            w[np.greater(dists[k],self.DSquaredDist/self.gridres)] = 0.0
         totweight=totweight+w
         totsum=totsum+(zarr[k]*w)

      totweight[np.less(totweight,1.0e-200)] = 1.0
      chg=totsum/totweight
      self.logtime("Done with Distance Squared calculations",2)
      return chg
   #---------------------------------------------------------------------------
   #  setDSquaredDist - set the minimum distance used by the Distance Squared
   #      weighting algorithm.  Only control points within this distance of a 
   #      gridpoint will be used in calculating the weighted average.  If set 
   #      negative then the distance is calculated such that the nearest 5 
   #      control points are used at each gridpoint.
   #
   def setDSquaredDist(self,value):
      self.DSquaredDist=value
      if value<0.0:
         self.logtime("Distance Squared distance will be infinite",1)
      else:
         self.logtime("Distance Squared distance will be %f"%value,1)
      return
   def setDSquaredMaxPoints(self,value):
      self.DSquaredMaxPoints=value
      if value>0:
         self.logtime("Distance Squared number of points will now be %d"%value,1)
      else:
         self.logtime("Distance Squared number of points will now be infinite",1)
      return
   #-----------------------------------------------------------------------
   #  Barnes - A Barnes analysis routine
   #
   def Barnes(self,zlist,xlist,ylist,hlist,elevfactor,
              Topo,landMask=None):
      self.logtime("Running barnes calculations",2)
      #
      #  Setup elevation and land/sea scaling
      #
      (numpts,xarr,yarr,harr,larr,scaledtopo,newlandMask)=self.setupScaling(xlist,
             ylist,hlist,elevfactor,Topo,landMask)
      totDistSquared=self.getTotDistSquared(xarr,yarr,harr,larr)
      totDist=np.sqrt(totDistSquared)
      #
      #  Get distance squared of control points to every gridpoint
      #
      self.logtime("Getting distance squared between control points and gridpoints",3)
      dists=np.zeros((numpts,self.ymax,self.xmax),np.float32)
      for k in range(numpts):
         d=self.getDistance(xarr[k],yarr[k],harr[k],scaledtopo,newlandMask)*self.gridres
         dists[k]=(d*d).astype(np.float32)
      #
      #  If BarnesSpacing is negative...they want it calculated
      #
      if self.BarnesSpacing<0:
         self.logtime("Calculating Barnes Station Spacing",3)
         if len(xlist)>1:
            #
            #  get min distance of control points to each other
            #
            minDist=self.getMinDist(totDist)
            #
            #  If <-50...Get average distance to closest neighbor 
            #
            if self.BarnesSpacing<-50:
               self.logtime("  using average distance of 'closest neighbor'",3)
               total=np.add.reduce(minDist)
               c=(total/len(xlist))*self.gridres
            #
            #  otherwise...get maximum distance to closest neighbor
            #
            else:
               self.logtime("  using furthest 'closest neighbor' for all control points",3)
               c=np.maximum.reduce(minDist)*self.gridres
         else:
            c=50
         self.logtime("Calculated Barnes Station Spacing = %.2f km"%c,3)
      else:
         c=self.BarnesSpacing
      self.logtime("Using a Barnes Station Spacing of %.2f km"%c,3)
      #
      #  The Barnes 'kappa' value depends on twice the barnes distance
      #
      kappa=5.052*(((2.0*c)/math.pi)**2)
      self.logtime("Barnes kappa value= %f"%kappa,3)
      #
      #  Barnes PASS 1
      #
      self.logtime("Barnes Pass 1",3)
      totweights=np.zeros((self.ymax,self.xmax),np.float32)
      totsum=np.zeros((self.ymax,self.xmax),np.float32)
      for k in range(numpts):
         #
         #  get scaled distance squared divided by kappa 
         #
         xx=dists[k]/kappa
         #
         #  Barnes weight is e taken to the negative xx power -
         #  but make sure it isn't huge - which would return a zero weight
         #
         xx[np.greater(xx,200.0)] = 200.0
         w=(np.exp(xx*-1.0)).astype(np.float32)         
         totweights=totweights+w
         #
         #  Calculate weight * point k value
         #
         z=zlist[k]
         totsum = totsum + (w*z).astype(np.float32)
      #
      #  Calculate weighted average.  Sum of (weights * values) divided by
      #  the sum of weights (make sure sum of weights is non-zero)
      #
      totweights[np.less(totweights,1.0e-200)] = 1.0e-200
      chg=totsum/totweights
      #
      #  Barnes PASS 2
      #
      self.logtime("Barnes Pass 2",3)
      totweights=np.zeros((self.ymax,self.xmax),np.float32)
      totsum=np.zeros((self.ymax,self.xmax),np.float32)
      for k in range(numpts):
         #
         #  get scaled distance squared divided by gamma *kappa 
         #
         xx=dists[k]/(self.BarnesGamma*kappa)
         #
         #  Barnes weight is e taken to the negative xx power -
         #  but make sure it isn't huge - which would return a zero weight
         #
         xx[np.greater(xx,200.0)] = 200.0
         w=(np.exp(xx*-1.0)).astype(np.float32)
         totweights=totweights+w
         #
         #  In second pass...weighting the difference between the
         #  point k value, and the change calcuated in the first pass
         #
         x=int(xarr[k])
         y=int(yarr[k])
         zdiff=zlist[k]-chg[y,x]
         totsum = totsum + (w*zdiff).astype(np.float32)
      #
      #  Calculate weighted average.  Sum of (weights * values) divided by
      #  the sum of weights (make sure sum of weights is non-zero)
      #
      totweights[np.less(totweights,1.0e-200)] = 1.0e-200
      chg2=totsum/totweights
      #
      #  Add the adjustment from PASS 2 to PASS 1
      #
      chg=chg+chg2
      #
      #  Return the adjustment
      #
      self.logtime("Done with Barnes calculations",2)
      return chg
   #---------------------------------------------------------------------------
   #  setBarnesGamma - set the gamma values used in the second pass of Barnes
   #      algorithm.  By default it is 0.3, but the user can set it to anything 
   #      between 0.0 and 1.0
   #
   def setBarnesGamma(self,value):
      if ((value>=0.0) and (value<=1.0)):
         self.BarnesGamma=value
      else:
         self.logtime("Barnes Gamma must be between 0.0 and 1.0")
      return
   #---------------------------------------------------------------------------
   #  setBarnesSpacing - set the station spacing used by the Barnes algorithm.
   #      Basically data for wavelengths less than 2 times this distance are 
   #      removed by the analysis.  If set to a negative value, the Barnes 
   #      routine will calculate this by finding the distance to the nearest 
   #      neighbor for each control point...and then finding the maximum (the 
   #      'furthest closest neighbor').  If less than -50, it will take the 
   #      average of the distances to the closest neighbors (the more 
   #      traditional Barnes value).
   #
   def setBarnesSpacing(self,value):
      self.BarnesSpacing=value
      if value<0.0:
         self.logtime("Barnes Station Spacing will be calculated",1)
      return
   #---------------------------------------------------------------------------
   #  setupScaling - setup all the numeric arrays for the control point
   #      locations...based on any elevation and land/sea scaling
   #
   def setupScaling(self,xlist,ylist,hlist,elevfactor,Topo,landMask):
      #
      #  Number of control points
      #
      numpts=len(xlist)
      #
      #  scaling topo
      #
      (halist,scaledtopo)=self.setupElev(xlist,ylist,hlist,elevfactor,Topo)
      #
      #  setup the land/water mask
      #
      if landMask is None:
         newlandMask=(Topo*0.0)+1.0
      else:
         newlandMask=landMask
      llist=self.setupLandWater(xlist,ylist,newlandMask)
      #
      #  setup arrays
      #
      xarr=np.array(xlist,np.float32)
      yarr=np.array(ylist,np.float32)
      harr=np.array(halist,np.float32)
      larr=np.array(llist,np.float32)
      #
      #
      #
      return(numpts,xarr,yarr,harr,larr,scaledtopo,newlandMask)
   #---------------------------------------------------------------------------
   #  getTotDistSquared - get "total" distance between each point and every 
   #      other point.  This includes the elevation distance, and the 
   #      land/water.
   #
   def getTotDistSquared(self,xarr,yarr,harr,larr):
         xd=np.square(self.getComponentDiff(xarr))
         yd=np.square(self.getComponentDiff(yarr))
         ld=np.square(self.getComponentDiff(larr))
         hd=np.square(self.getComponentDiff(harr)+(ld*10000.0))
         return(xd+yd+hd)
   #---------------------------------------------------------------------------
   #  useActualElev - set options so that actual station elevation will be used
   #      when calculating "distance" of a gridpoint from the observation.
   #
   def useActualElev(self):
      self.ActualElev=1
      return
   #--------------------------------------------------------------------------
   #  useGridElev - set options so that elevation of the gridpoint that
   #      contains an observation will be used when calculating the "distance" 
   #      of a gridpoint from the observation
   #
   def useGridElev(self):
      self.ActualElev=0
      return
   #---------------------------------------------------------------------------
   #  getDistance - get a grid of distance from a single point with coordinates
   #      xval,yval and elevation hval. This distance is in terms of 
   #      grid-spacing - not physical distance units like km.  The distance 
   #      includes difference between the hval elevation and the topography 
   #      grid passed in via scaledtopo. Also differences in the land/water 
   #      mask between the point and each gridpoint count strongly in the 
   #      distance calculation).
   #
   def getDistance(self,xval,yval,hval,scaledtopo,landMask):
      ix=int(xval)
      iy=int(yval)
      xs=np.square(self.Irow-ix)
      ys=np.square(self.Jrow-iy)
      horizdist=np.add.outer(ys,xs)
      #
      #
      #
      if self.ActualElev==0:
         elev=scaledtopo[iy,ix]
      else:
         elev=hval
      ed=scaledtopo-elev
      #
      #  A land/water difference counts as 10000 in scaled elevation
      #  units. 
      #
      land=landMask[iy,ix]
      ld=np.square(land-landMask)
      ed2=np.square(ed+(ld*10000.0))
      #
      #
      #
      dist=np.sqrt(horizdist+ed2)
      return dist
   #---------------------------------------------------------------------------
   #  getMinDist - the minimum distance between a control point and all other
   #      control points (elevation and land/water is considered) - but this is 
   #      in terms of gridpoints - not km
   #
   def getMinDist(self,totDist):
      d=np.where(np.less(totDist,0.001),2*self.xmax,totDist)
      dmin=np.minimum.reduce(d)
      return dmin
   #---------------------------------------------------------------------------
   #  getSerpRemoteness - a multiplier for the serp weight - such that "remote"
   #      points (ones without many neighbors) are weighted more strongly than 
   #      points that are very near other points.  This keeps 'clustered'
   #      control points from dominating the analysis - since there might be 
   #      many clustered points giving basically the same info.
   #
   def getSerpRemoteness(self,totDist):
      numpts=totDist.shape[0]
      #
      #  special cases:
      #     only 1 point: remoteness is 1.0
      #
      if (numpts==1):
         ren=np.array([1.0]).astype(np.float32)
         return ren
      #
      #  two points is easy - remoteness is 0.5
      #
      if (numpts==2):
         ren=np.array([0.5,0.5]).astype(np.float32)
         return ren
      #
      #  sort the distances...so for each point we have the
      #  distances to its neighbors in sorted order
      #
      dsort=np.sort(totDist,0)
      #
      #  The distance of each point to its nearest neighbor is now
      #  in dsort[1,:]
      #
      dmax=dsort[:,:]
      mostremote=np.maximum.reduce(dmax)
      #
      #  add up distances from each point to each neighbor point
      #
      dsums=np.add.accumulate(dsort)
      dsumsflat=dsums.flat
      #
      #  get rid of all accumulated distances greater than most remote
      #  that way maximum value in each column will be the one where
      #  distance is less or equal to mostremote distance
      #
      dloc=np.where(np.greater(dsums,mostremote),np.float32(0),dsums)
      #
      #  get total distance up to the point where it is less than mostremote
      #
      dint=np.argmax(dloc,0)
      dintindex=(dint*numpts)+np.arange(numpts)
      valuebefore=np.take(dsumsflat,dintindex)
      #
      #  get total distance at point where it is more than most remote
      #
      dnext=dint+1
      dnextindex=(dnext*numpts)+np.arange(numpts)
      valueafter=np.take(dsumsflat,dnextindex)
      #
      #  get fractional part of points
      #
      frac=(mostremote-valuebefore)/(valueafter-valuebefore)
      #
      #  get total number of points to make the most remote distance
      #  and take reciprocal
      #
      npt=dint+frac
      factor=1.0/npt
      #
      #  divide by sum of all factors - so they add to 1.0
      #
      factorsum=np.add.reduce(factor)
      ren=(factor/factorsum).astype(np.float32)
      #
      #
      #
      return ren
   #---------------------------------------------------------------------------
   #  setupElev - use the elevfactor to change real Topo into a 'scaled topo',
   #      as well as changing actual station elevations in hlist into 'scaled 
   #      elevations' in scaledhlist.
   #
   #      elevfactor should be in units of feet/km.  If you set it to 1, then 
   #      1 foot of elevation difference is equivalent to 1km of horizontal 
   #      distance (this means that elevation is VERY important in the 
   #      analysis). If you set it to 1000, then 1000 feet of elevation
   #      difference is equal to 1 km of horizontal distance (this means that 
   #      elevation is NOT important to the analysis).  To turn off elevation 
   #      completely - set the elevfactor to zero.
   #
   def setupElev(self,xlist,ylist,hlist,elevfactor,Topo):

      scaledhlist=[]
      if elevfactor>0.001:
         factor=elevfactor*self.gridres
         scaledtopo=Topo/factor
         for i in range(len(hlist)):
            h=hlist[i]
            if self.ActualElev==0:
               scaledhlist.append(scaledtopo[ylist[i],xlist[i]])
            else:
               scaledhlist.append(h/factor)
      else:
         scaledtopo=Topo*0.0
         for h in hlist:
            scaledhlist.append(0.0)
      return(scaledhlist,scaledtopo)
   #---------------------------------------------------------------------------
   #  setupLandWater - setup a list that contains the value of the landMask
   #      grid for every point in the xlist,ylist locations.  It doesn't really 
   #      matter - but the convention is that land=1 and water=0
   #
   def setupLandWater(self,xlist,ylist,landMask):
      llist=[]
      for i in range(len(xlist)):
         x=xlist[i]
         y=ylist[i]
         if landMask is None:
            llist.append(1)
         else:
            llist.append(landMask[y,x])
      return llist
   #---------------------------------------------------------------------------
   #  getComponentDiff - get difference between all control points
   #
   def getComponentDiff(self,xloc):
      xd=-(np.subtract.outer(xloc,xloc))
      return xd
   #---------------------------------------------------------------------------
   #  getGridSpacing - get 'rough grid spacing' by getting the distance between
   #      the corners of the GFE grid and dividing by the number of points.
   #
   def getGridSpacing(self):
      (lat1,lon1)=self.getLatLon(0.0, 0.0)
      (lat2,lon2)=self.getLatLon(self.xmax-1.0, self.ymax-1.0)
      hypot=math.hypot(self.xmax-1.0, self.ymax-1.0)
      spacing1=self.getCircleDistance(lat1,lon1,lat2,lon2)/hypot
      (lat1,lon1)=self.getLatLon(0.0, self.ymax-1.0)
      (lat2,lon2)=self.getLatLon(self.xmax-1.0, 0.0)
      spacing2=self.getCircleDistance(lat1,lon1,lat2,lon2)/hypot
      avgspacing=(spacing1+spacing2)/2.0
      return avgspacing
   #---------------------------------------------------------------------------
   #  getCircleDistance - get the 'great circle distance' between two lat lon
   #      points (in km) 
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
   #---------------------------------------------------------------------------
   #  setVerbose - set 'verbosity' of logging.  By default sets to 1, but
   #      can set higher to see even more detailed messages.
   #          0=no messages (only errors)
   #          1=simple message saying doing analysis
   #          2=add messages about pieces of analysis being done
   #          3=add messages with more timing information
   #          4=add listing of all point obs used in analysis
   #          5=add memory usage messages
   #
   def setVerbose(self,value=1):
      self.verbose=value
      return
   #---------------------------------------------------------------------------
   #  setQuiet - set 'verbosity' to zero so that only required (level=0) 
   #      log messages are output.
   #
   def setQuiet(self):
      self.verbose=0
      return
   #---------------------------------------------------------------------------
   #  setMissingElevThreshold - set the MissingElevThreshold value
   #     Obs with elevation values less than or equal to this threshold
   #     will use the topo grid elevation instead, even if ActualElev is set
   #     to 1.
   #
   def setMissingElevThreshold(self,value):
      self.MissingElevThreshold=value
      return
   #---------------------------------------------------------------------------
   #  logtime - write a string with date/time stamp.  Can dynamically control 
   #      which get printed by using the importance and verbosity settings.  
   #      Will only print message with importance less or equal to verbosity 
   #      setting. (in other words, importance=0 are VERY IMPORTANT messages 
   #      that are always printed.  importance=1 are only shown when Verbose
   #      is 1 or greater, etc.).
   #
   def logtime(self,string,importance=0):
      if importance<=self.verbose:
         tt=self._gmtime().timetuple()
         ts="%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d"%(tt[0],tt[1],tt[2],tt[3],tt[4],tt[5])
         print "%s|ObjAnal - %s" % (ts,string)
         sys.stdout.flush()
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

