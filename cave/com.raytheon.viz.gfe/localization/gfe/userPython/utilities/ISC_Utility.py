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
# ISC_Utility - Version 3.05 (Tim Barker - SOO Boise, ID)
#
#   Supports new routines that mimic NDFD algorithms that were changed
#   in late 2005 or early 2006.  Algorithms now have potentially different
#   thresholds for every 'border pair', based on topography, and the values
#   of the grids themselves (i.e. large values of waveheight have more
#   leniant' thresholds).   Algorithms now consider a border as 'discrepant'
#   if the average absolute difference along the border is larger than the
#   average threshold along that border.  Some tools/algorithms will also show
#   which individual pairs violate their particular threshold.
#
# Author: barker
#  2008-11-19 - Barker - Version 3.05.  Added code to check for 'office type'
#               of editAreas, so that it checks only ISC_xxxx areas for wfos -
#               not rfcs.  Also added a check for ISC_xxxx editAreas without
#               corresponding xxxx editArea.
#  2006-01-23 - Barker - Version 3.04.  Add thresholds for PoP12hr, QPF12hr
#               SnowAmt12hr (standard in ER), WindChill, HeatIndex (instead
#               of AppT), and PoP6, PoP12 (common in SR).
#  2006-01-19 - Barker - Version 3.03.  Another typo for non-square grids.
#  2006-01-17 - Barker - Version 3.02.  Fix problem for non-square grids.
#  2006-01-13 - Barker - Version 3.01.  Changed for new NDFD algorithm
#               Thresholds now vary at each gridpoint - overall average
#               difference along border must be less than average threshold
#               along that border (a much better algorithm!).
#
# ----------------------------------------------------------------------------

import numpy
import SmartScript
import time
import TimeRange
from com.raytheon.uf.common.dataplugin.gfe.db.objects import GFERecord_GridType as GridType

class ISC_Utility(SmartScript.SmartScript):
    def __init__(self, dbss, eaMgr, mdMode=None, toolType="numeric"):
        SmartScript.SmartScript.__init__(self, dbss)
        self.setToolType(toolType)
        self._dbss = dbss
       # self.setUp(eaMgr, mdMode, toolType)
        self.configuration()
        #
        #  Always check to see if BorderPairs are current
        #
        self.Topo=self.getTopo()
        self._empty = self.Topo * 0.0
        refresh=7200  # seconds between refresh of borders
        self._debug=0 # set to 1 or 5 or 10 for increasing info
        self.list=""
        #
        #  get Border Pair info - either from cache, or by calculating
        #
        self.pairInfo=self._getCachedPairs("BorderPairs","ISCPairs",refresh)
        if self.pairInfo is None:
            if self._debug>=1:
                self.statusBarMsg("Calculating Pairs","R")
            self.pairInfo=self._getPairInfo(self.Topo)
            if self._debug>=1:
                self.statusBarMsg("Calculating Pairs Done","R")
            self._cachePairs("BorderPairs",self.pairInfo,"ISCPairs")
        else:
            if self._debug>=1:
                self.statusBarMsg("Pair info obtained from cache","R")
                
        
    #-------------------------------------------------------------------------
    #        
    #  C O N F I G U R A T I O N   S E C T I O N   F O R   A L L   I S C   TOOLS
    #
    # this function is intended to be overridden and sets up the default
    # configuration for the set of ISC tools.  Copy and place into the
    # ISC_Utility_Local_New and modify as needed.
    def configuration(self):
        #
        #  points which have an elevation difference greater than this will NOT
        #  be considered in ISC statistics (in feet).  NDFD sets this to 1000ft.
        #
        self.MAXTOPODIFF=1000.0
        #
        #  NDFD checks are not performed when one side of a border is a land
        #  point and the other side is an ocean point.  To do this, an EditArea
        #  with land/sea points needs to be calculated.  With LANDEDITAREA set
        #  to None - the code will calculate the land area by a 'union' of all
        #  points found in the CWA editareas named XXX, where the XXX values
        #  are taken from all the editareas name ISC_XXX.  If you have not
        #  overridden the ISC_XXX editarea or XXX edit areas, then this will
        #  work fine.  If you HAVE overridden these edit area - use the
        #  LANDEDITAREA to specify the name of an editarea that contains just
        #  land points (all others are assumed to be ocean points).
        #
        self.LANDEDITAREA=None # or string with name of EditArea containing land
        #
        #--------------------------------------------------------------------
        #  These configuration items for Show_ISC_Info and Show_ISC_Highlights.
        #
        #  If you want the check for a particular parm to ACTUALLY check other
        #  parms, then list them here.  Vector parms need not be listed - but
        #  the threshold for Vector parms in GFE is assumed to be the threshold
        #  for the magnitude part - and the threshold for the direction part is
        #  hard-coded below
        #
        self.MultiParms={"MaxRH":("MinT","TdMrn","MaxRH"),
                    "MinRH":("MaxT","TdAft","MinRH"),
                    "RH":   ("T","Td","RH"),
                   }

        #  Minimum number of points along a border before it considers a
        #  failing average threshold "significant" (to get rid of short borders)
        #
        self.MINPOINTS=10
        #
        #------------------------------------------------------------------
        #
        #  NDFD thresholds - should not need to be modified.
        #
        #  Each entry in THRESHOLDS contains a tuple ( parmnames, thresholdinfo)
        #     parmnames can be a tuple with many parms listed that use the
        #               same threshold
        #     thresholdinfo contains (thresholdtype,thresholdvalues,
        #                             conditions,dirflag) where:
        #         thresholdtype="contant","topo" or "graduated"
        #         thresholdvalues=
        #               for "constant" type:  value
        #
        #                   differences greater than value are considered
        #                   discrepant
        #
        #               for "topo" type: (elev,lowvalue,highvalue)
        #
        #                   if the elevation difference between points is
        #                   less than elev, then the lowvalue is used as
        #                   the threshold value.  Otherwise the highvalue
        #                   is used for the threshold value
        #
        #               for "graduated" type: (bigvalue,(lessthan,value),(lessthan,value),...)
        #
        #                   bigvalue is the default threshold value. However
        #                   if the lowest of the two pair points is less than the
        #                   'lessthan', then that 'value' is used for the
        #                   threshold instead.  All 'lessthan' values are checked,
        #                   so they should be listed in decreasing order.
        #
        self.DEFAULT_THRESHOLD=("constant",5,("none",0,0),0)
        self.THRESHOLDS=[
            (("T","Td","MaxT","MinT","TdAft","TdMrn"),
             ("topo",(500,5,7),("none",0,0),0)),
            (("HeatIndex","WindChill"),
             ("topo",(500,7,9),("none",0,0),0)),
            (("PoP","PoP12","PoP6","PoP12hr","PoP6hr"),
             ("constant",20,("none",0,0),0)),
            (("WindSpd","TransWindSpd","WindGust"),
             ("graduated",(15,(20,10)),("greater_equal",12,0),0)),
            (("WindDirec","TransWindDirec"),
             ("topo",(500,45,90),("greater_equal",12,1),1)),
            (("Sky"),
             ("topo",(500,25,35),("none",0,0),0)),
            (("QPF","QPF6hr"),
             ("graduated",(1.0,(3.0,0.5),(1.5,0.25)),("greater",0.25,0),0)),
            (("SnowAmt","SnowAmt6hr"),
             ("graduated",(6,(12,4),(6,2)),("greater",2,0),0)),
            (("SnowLevel","FzLevel","MixHgt"),
             ("constant",1000,("none",0,0),0)),
            (("RH","MaxRH","MinRH"),
             ("graduated",(25,(75,20),(50,15),(25,10)),("none",0,0),0)),
            (("WaveHeight"),
             ("graduated",(10,(36,9),(32,8),(28,7),(24,6),(20,5),(16,4),(12,3),(6,2)),("greater",0,1),0)),
            (("CWR"),
             ("constant",10,("none",0,0),0)),
            (("Haines"),
             ("constant",1,("none",0,0),0)),
            ]
# ---------  E N D   C O N F I G U R A T I O N   S E C T I O N  ----------

        
        
    #=================================================================
    #  _getPairInfo - calculate pairInfo data structure from edit areas
    #
    #      each entry in pairInfo is a tuple with (label,pairlist)
    #          where label=string with CWA neighbor name
    #                pairlist=list of tuples
    #          where each entry in pairlist is a tuple with: (insidex,insidey,outsidex,outsidey,TopoDiff)
    #              where insidex,insidey = pair coordinates inside CWA
    #                    outsidex,outsidey = pair coordinates outside CWA
    #                    absTopoDiff = topography difference (always positive)
    #
    def _getPairInfo(self,Topo):
        pairInfo=[]
        #
        #  Find coordinates of neighboring pairs
        #  first setup eah=home edit area
        #
        homeSite=self.getSiteID()
        name="ISC_"+homeSite
        eah=self.encodeEditArea(self.getEditArea(name))
        homeType=self.myOfficeType()
        #
        #  Get grids with home edit area shifted 1 pixel
        #  each direction...and topo difference when shifted
        #  1 pixel in each direction
        #
        xshift=( 1,-1, 0, 0)
        yshift=( 0, 0, 1,-1)
        homeshifts=[]
        topodiffs=[]
        for i in range(4):
           homeshifts.append(self.offset(eah,xshift[i],yshift[i]))
           topodiffs.append(Topo-self.offset(Topo,xshift[i],yshift[i]))
        landGrid=self._getLandEditArea()
        #
        #  Loop through other ISC_xxx edit areas - except the one
        #  for the home edit area
        #
        eanames=self.editAreaList()
        
        eaTime = time.time()
        for eaname in eanames:
            iterationTime = time.time()
            if (len(eaname)==7)and(eaname[0:4]=="ISC_")and(eaname[4:]!=homeSite):
                siteName=eaname[4:]
                sType=self.officeType(siteName)
                if sType is None:
                   continue
                if sType==homeType:
                    ean=self.encodeEditArea(self.getEditArea(eaname))
                    #
                    #  Compare edit area to the shifted home edit areas
                    #  looking for common points.  Also check that topo
                    #  difference is less than MAXTOPODIFF
                    #
                    pairs=[]
                    for k in range(4):
                        pair=numpy.logical_and(homeshifts[k],ean)
                        topodiff=topodiffs[k]
                        
                        for x in range(eah.shape[1]):
                           for y in range(eah.shape[0]):
                              if pair[y,x]==1:  # common point
                                  tdiff=numpy.abs(topodiff[y,x])
                                  homepointx=x-xshift[k]
                                  homepointy=y-yshift[k]
                                  land1=landGrid[homepointy,homepointx]
                                  land2=landGrid[y,x]
                                  coast=numpy.not_equal(land1,land2) # 0 if both land or both water
                                  pairs.append((homepointx,homepointy,x,y,tdiff,coast))

                    if len(pairs)>0:
                        label=siteName
                        pairInfo.append((label,pairs))

                           
        return pairInfo
    #=================================================================
    #  _getCachedPairs - See if cached pairInfo structure is still
    #                    valid.  If so, return it, otherwise return
    #                    None.
    #
    def _getCachedPairs(self, name, category, timeLimit):
          try:
              object = self.getObject(name+"_"+self._dbss.getSiteID(), category)
              pairInfo, timeWritten, geoInfo = object
              if timeLimit != 0 and time.time() - timeWritten > timeLimit:
                  return None        #too old

              # validate geoinfo
              geo = self._dbss.getSiteID()
              if not geoInfo == geo:
                  return None        #different geoinfo
              return pairInfo
          except:
              return None
    #=================================================================
    #  _cachePairs - save pairInfo structure data, along with time and
    #                grid location info to IFPS database - so it can be
    #                grabbed quickly later without re-calculating
    #
    def _cachePairs(self, name, pairInfo, category):
          #geo = self._dbss.getParmManager().compositeGridLocation()
          object = pairInfo, time.time(), self._dbss.getSiteID()
          self.saveObject(name+"_"+self._dbss.getSiteID(), object, category)
    #=================================================================
    #  _getLandEditArea - get Land editArea grid, calculating from
    #                     cwa edit areas if not specified via
    #                     configuration
    #
    #  Returns grid of 0/1 for land.  Everything not land is assumed
    #  to be water.
    #
    def _getLandEditArea(self):
        #
        #  Get points that are land - either by the specified edit
        #  area in configuration, or by looking for ISC_xxx edit
        #  areas, and adding up all the corresponding xxx areas.
        #
        #  This does NOT work if the default CWA edit areas named 'xxx'
        #  have been overridden with other names or different areas, or
        #  if new ISC_xxx edit areas have been added to the system and
        #  have a corresponding xxx edit area.
        #
        landGrid=None
        if (self.LANDEDITAREA is not None):
            landea=self.getEditArea(self.LANDEDITAREA)
            if (landea is not None):
                landGrid=self.encodeEditArea(landea)
        if landGrid is None:
            landGrid=self.getTopo() * 0.0
            eanames=self.editAreaList()
            for eaname in eanames:
                if ((len(eaname)==7)and(eaname[0:4]=="ISC_")):
                    name=eaname[4:]
                    if name in eanames:
                        ea=self.getEditArea(name)
                        if ea is not None:
                            grid=self.encodeEditArea(ea)
                            landGrid=numpy.where(grid,1,landGrid)
        return landGrid
    #========================================================================
    #  _getThresholdInfo - return thresholdInfo structure for the
    #                      specified parm.
    #
    def _getThresholdInfo(self,parmName):
        thresholdInfo=self.DEFAULT_THRESHOLD
        for (names,threshold) in self.THRESHOLDS:
            if parmName in names:
                thresholdInfo=threshold
                break
        return thresholdInfo
    #=================================================================
    #  _getListing - get text detailing most recent checks
    #
    def _getListing(self):
        return self.list
    #=================================================================
    #  _checkParmBorders - check ISC borders for this parm - which may
    #                      mean checking more than one grid (see the
    #                      MultiParms configuration section)
    #
    def _checkParmBorders(self,WEname,GridTimeRange,listing=0):
        timetext=self.makeTimeMsg(GridTimeRange)
        if listing==1:
            self.list="ISC Discrepancies Check for %s %s:\n\n"%(WEname,timetext)
        #
        #  Get list of parms to check - which might be more than one
        #
        if (WEname in self.MultiParms.keys()):
            parmlist=self.MultiParms[WEname]
        else:
            parmlist=(WEname,)
        #
        #  Loop over each parm - put status messages for reading problems
        #
        totalviolate=0
        totalwarning=0
        totalchecked=0
        for parmname in parmlist:
            results=self._checkGridBorders(parmname, GridTimeRange, listing=listing)
            (status,numchecked,violate,warning)=results
            if status==1:
                msg="No %s GridInfo for %s"%(WEname,timetext)
                self.statusBarMsg(msg,"A")
                continue
            if status==2:
                msg="No %s ISC data for %s"%(WEname,timetext)
                self.statusBarMsg(msg,"A")
                continue
            if status==3:
                msg="No border checks for weather or discrete elements: %s"%WEname
                self.statusBarMsg(msg,"A")
                continue
            totalviolate=totalviolate+violate
            totalwarning=totalwarning+warning
            totalchecked=totalchecked+numchecked
        return(totalchecked,totalviolate,totalwarning)
    #=================================================================
    #  _checkGridBorders - check Borders for a single grid (which
    #                      might be a vector - so it might actually
    #                      check two grids)
    #
    #  returns (status,numchecked,violate,warning)
    #
    #  if status is non-zero, then there was a problem reading the
    #  grid and the values are meaningless).  numchecked is the total
    #  number of pairs checked - if zero - then no checks were done
    #  because no points met the conditions.  That is different than
    #  no violations/warning when lots of points were checked.
    #
    def _checkGridBorders(self,parmName,GridTimeRange,listing=0):
        #
        #  Get grid info - return status=1 if no gridInfo
        #
        try:
            gridInfoList=self.getGridInfo(self.mutableID(), parmName, "SFC",
                                          GridTimeRange)
        except:
            return(1,0,0,0)
        #
        #  Return status=1 if gridInfo is empty
        #
        if (len(gridInfoList)<1):
            return (1,0,0,0)
        gridInfo=gridInfoList[0]
        #
        #  get the ISC data - return status=2 if could not read ISC data
        #
        bits,isc=self._getBitsAndISC(parmName, gridInfo, GridTimeRange)
        if ((bits is None) or (isc is None)):
           return (2,0,0,0)
        #
        #  Only know how to check for Scalar or Vector grids
        #
        WEtype=gridInfo.type()
        if (not GridType.SCALAR.equals(WEtype)) and (not GridType.VECTOR.equals(WEtype)):
            return (3,0,0,0)
        
        #
        #  If a vector - then check each part seperately - but the
        #  condition always depends on speed.  For scalar grids the
        #  condition is always the same as the grid itself.
        #
        gridList=[]
        if GridType.SCALAR.equals(WEtype):
            mag=isc
            gridList.append((parmName,mag,mag))
        else:
            name1=parmName+"Spd"
            (mag,direc)=isc
            gridList.append((name1,mag,mag))
            name2=parmName+"Direc"
            gridList.append((name2,direc,mag))
        #
        #  Loop over grids (usually one, but possibly two for vectors)
        #
        totalchecked=0
        totalviolate=0
        totalwarning=0
        for (pname,grid,condgrid) in gridList:
            if self._debug>=10:
                print "checking %s grid"%pname
            if listing==1:
                self.list=self.list+"For %s:\n"%pname
            (numchecked,violate,warning)=self._checkAllBorders(pname,grid,condgrid,bits,listing=listing)
            totalchecked=totalchecked+numchecked
            totalviolate=totalviolate+violate
            totalwarning=totalwarning+warning
            if listing==1:
                self.list=self.list+"\n"
        return(0,totalchecked,totalviolate,totalwarning)
    #=================================================================
    #  _checkAllBorders - check all borders for a single scalar grid
    #                     that has a condition grid of congrid and bits
    #                     indicates where ISC data was available.
    #
    #                     if listing=1, then adds text to self.list
    #                        that shows some stats for each border
    #                     
    #                     returns:
    #                       numchecked - total number of pairs checked
    #                                    over all borders
    #                       numviolations - total number of borders that
    #                                       were in violation
    #                       numwarnings - total number of borders that
    #                                     were in warning (violating but
    #                                     having less than MINPOINTS
    #                                     pairs)
    #
    def _checkAllBorders(self,parmName,grid,condgrid,bits,listing=0):
        #
        #  get Threshold info for this parm
        #
        thresholdInfo=self._getThresholdInfo(parmName)
        if self._debug>=10:
            print "thresholdInfo=",thresholdInfo
        (thresholdType,thresholdValues,conditions,dirflag)=thresholdInfo
        #
        #  Loop over each neighbors border
        #
        violate=0
        warning=0
        totalchecks=0
        for (label,pairList) in self.pairInfo:
            if self._debug>=5:
                print "Checking borders with %s"%label
            results=self._checkOneBorder(grid,condgrid,bits,pairList,thresholdInfo)
            (returnvalue,totalnum,numchecked,numviolate,
             avgbias,avgdiff,avgthresh)=results
            if self._debug>=5:
                print "  totalpoints  :%d"%totalnum
                print "  numberchecked:%d"%numchecked
                print "  numviolate   :%d"%numviolate
                if (numchecked>0):
                    print "  bias         :%f"%(avgbias)
                    print "  diff         :%f"%(avgdiff)
                    print "  threshold    :%f"%(avgthresh)
            if listing==1:
                self.list=self.list+"  Avg Diff for %s is %7.2f (limit %7.2f) [%4d pairs - %4d failed] - "%(label,avgdiff,avgthresh,numchecked,numviolate)
                if avgdiff>avgthresh:
                    if numchecked<self.MINPOINTS:
                        self.list=self.list+"IGNORED"
                    else:
                        self.list=self.list+"FAILED"
                else:
                    self.list=self.list+"OK"
                self.list=self.list+"\n"
            totalchecks=totalchecks+numchecked
            if returnvalue==1:
                warning=warning+1
            if returnvalue==2:
                violate=violate+1
        return (numchecked,violate,warning)
    #=================================================================
    #  _meetCondiditions - returns 1 if conditions for checking against
    #                      the threshold are met - or 0 if the conditions
    #                      are not met.
    #
    def _meetConditions(self, conditions, x1,y1,x2,y2,condgrid):
        (conditionType,conditionValue,bothFlag)=conditions
        if conditionType=="none":
            return 1
        value1=condgrid[y1,x1]
        value2=condgrid[y2,x2]
        if conditionType=="greater":
            cond1=(value1>conditionValue)
            cond2=(value2>conditionValue)
        elif conditionType=="greater_equal":
            cond1=(value1>=conditionValue)
            cond2=(value2>=conditionValue)
        elif conditionType=="less":
            cond1=(value1<conditionValue)
            cond2=(value2<conditionValue)
        elif conditionType=="less_equal":
            cond1=(value1<=conditionValue)
            cond1=(value2<=conditionValue)
        if bothFlag==1:
            return (cond1 and cond2)
        else:
            return (cond1 or cond2)
    #=================================================================
    #  _getDiffGrid - check Borders for a single grid - and return
    #                 a grid with the max difference between the
    #                 point and any neighboring points.  If vectordir
    #                 =1, and grid is a vector - then make grid for
    #                 direction - otherwise get grid for magnitude part.
    #                 If MaskNonViolators=1 then do not set the grid
    #                 value unless it violates the threshold for these
    #                 points.
    #
    #  returns grid of non-zero values, or None if there were problems
    #  reading the grids.
    #
    #
    def _getDiffGrid(self,parmName,GridTimeRange,vectordir=0,maskNonViolators=0):
        #
        #  Get grid info - return None if no gridInfo
        #
        diffGrid=self._empty

        try:
            gridInfoList=self.getGridInfo(self.mutableID(), parmName, "SFC",
                                          GridTimeRange)
        except:
            return None
        #
        #  Return None if an empty list of gridInfos
        #
        if (len(gridInfoList)<1):
            return None
        gridInfo=gridInfoList[0]
        #
        #  get the ISC data - return None if it could not read data
        #
        bits,isc=self._getBitsAndISC(parmName, gridInfo, GridTimeRange)
        if ((bits is None) or (isc is None)):
           return None
        #
        #  Only know how to check for Scalar or Vector grids
        #
        WEtype=gridInfo.type()
        if (not GridType.SCALAR.equals(WEtype)) and (not GridType.VECTOR.equals(WEtype)):
            return None
        #
        #  If a vector - then check only magnitude part
        #
        if GridType.SCALAR.equals(WEtype):
            grid=isc
            condgrid=isc
            checkName=parmName
        else:
            (mag,direc)=isc
            if vectordir==0:
               checkName=parmName+"Spd"
               grid=mag
               condgrid=mag
            else:
               checkName=parmName+"Direc"
               grid=direc
               condgrid=mag
        #
        #  get Threshold info for this parm
        #
        thresholdInfo=self._getThresholdInfo(checkName)
        if self._debug>=10:
            print "thresholdInfo=",thresholdInfo
        (thresholdType,thresholdValues,conditions,dirflag)=thresholdInfo
        #
        #  Loop over each neighbors border
        #
        for (label,pairList) in self.pairInfo:
            if self._debug>=5:
                print "Checking borders with %s"%label
            for pair in pairList:
                (x1,y1,x2,y2,topodiff,coast)=pair
                if self._debug>=10:
                    print "  point %3d,%3d-->%3d,%3d"%(x1,y1,x2,y2)
                #
                #  get values across the border
                #
                value1=grid[y1,x1]
                value2=grid[y2,x2]
                #
                #  Get the difference across the border
                #
                diff=value1-value2
                if dirflag==1:
                    if diff>180.0:
                        diff=diff-360.0
                    if diff<-180.0:
                        diff=diff+360.0
                absdiff=abs(diff)
                #
                #  Get the threshold (which might depend on the values)
                #
                if thresholdType=="constant":
                    thresh=thresholdValues
                elif thresholdType=="topo":
                    (elevation,low,high)=thresholdValues
                    if (topodiff<=elevation):
                        thresh=low
                    else:
                        thresh=high
                elif thresholdType=="graduated":
                    minvalue=min(value1,value2)
                    thresh=thresholdValues[0]
                    for i in range(1,len(thresholdValues)):
                        (lessthan,newthresh)=thresholdValues[i]
                        if minvalue<lessthan:
                            thresh=newthresh
                #
                #  Calculate whether this particular point violates the threshold
                #
                if absdiff<=thresh:
                    itPasses=1
                else:
                    itPasses=0
                if self._debug>=10:
                    print "  %f %f diff:%f threshold:%f itPasses:%1d"%(value1,value2,diff,thresh,itPasses)
                #
                #  If too big a topodiff - or a coast - then it passes anyway
                #
                if ((topodiff>self.MAXTOPODIFF) or (coast==1)):
                    itPasses=1
                #
                #  If no ISC data - then it passes anyway
                #  available in ISC
                #
                if (bits[y2,x2]<0.5):
                    itPasses=1
                #
                #  Make sure conditions for checking are met
                #
                if (not self._meetConditions(conditions,x1,y1,x2,y2,condgrid)):
                    itPasses=1
                #
                #  Do nothing if this point passes and configuration
                #  indicates that nonViolators will not be displayed
                #
                if (maskNonViolators and itPasses):
                    continue
                #
                if absdiff>abs(diffGrid[y1,x1]):
                    diffGrid[y1,x1]=diff
        return diffGrid
    #=================================================================
    #
    #  checkOneBorder - given a list of points in pairList, the threshold
    #                   info, the grid and condition grid, and the bits
    #                   that indicates where ISC data is available:
    #                   then for each point:
    #                     get the grid values,
    #                     check that conditions are met,
    #                     get difference,
    #                     get threshold,
    #                     check difference comapred to threshold and
    #                       add to total differences, biases, etc.
    #
    #                   return:
    #                     code = 0 if border passed (even if no points)
    #                            1 if border violated but had MINPOINTS
    #                                 or fewer pairs
    #                            2 if border violated and had more than
    #                                 MINPOINTS pairs
    #                     totalnum = total number of pairs along the
    #                                border
    #                     numchecked = total pairs checked along the
    #                                  border after skipping ones without
    #                                  ISC data (bits=0), topo difference
    #                                  higher than limit, or condgrid
    #                                  values not meeting condition
    #                     numviolate = number of individual pairs that
    #                                  violated their threshold
    #                     avgbias = average difference among pairs
    #                     avgdiff = average absolute difference among pairs
    #                     avgthresh = average threshold along border
    #                     
    #
    def _checkOneBorder(self,grid,condgrid,bits,pairList,thresholdInfo):
        (thresholdType,thresholdValues,conditions,dirflag)=thresholdInfo
        totalnum=len(pairList)
        numchecked=0
        numviolate=0
        biastotal=0.0
        difftotal=0.0
        threshtotal=0.0
        
        for pair in pairList:
            (x1,y1,x2,y2,topodiff,coast)=pair
            if self._debug>=10:
                print "  point %3d,%3d-->%3d,%3d"%(x1,y1,x2,y2)
            #
            #  no tests if too big of topodiff, or if a coastline pair
            #
            if ((topodiff>self.MAXTOPODIFF) or (coast==1)):
                if self._debug>=10:
                    print "    skipped because of elevation or coastline"
                continue
            #
            #  no tests if bits of outside point indicate it is NOT
            #  available in ISC
            #
            if (bits[y2,x2]<0.5):
                if self._debug>=10:
                    print "    skipped because ISC data not available"
                continue
            #
            #  get values across the border
            #
            value1=grid[y1,x1]
            value2=grid[y2,x2]
            #
            #  Make sure conditions for checking are met
            #
            if (not self._meetConditions(conditions,x1,y1,x2,y2,condgrid)):
                if self._debug>=10:
                    print "    skipped because %f and %f did not meet the conditions:%s"%(value1,value2,conditions)
                continue
            numchecked=numchecked+1
            #
            #  Get the difference across the border - add it to the
            #  total difference (biastotal) and total of absolute
            #  value of differences (difftotal)
            #
            diff=value1-value2
            if dirflag==1:
                if diff>180.0:
                    diff=diff-360.0
                if diff<-180.0:
                    diff=diff+360.0
            absdiff=abs(diff)
            biastotal=biastotal+diff
            difftotal=difftotal+absdiff   
            #
            #  Get the threshold (which might depend on the values)
            #  Add it to the total of thresholds
            #
            if thresholdType=="constant":
                thresh=thresholdValues
            elif thresholdType=="topo":
                (elevation,low,high)=thresholdValues
                if (topodiff<=elevation):
                    thresh=low
                else:
                    thresh=high
            elif thresholdType=="graduated":
                minvalue=min(value1,value2)
                thresh=thresholdValues[0]
                for i in range(1,len(thresholdValues)):
                    (lessthan,newthresh)=thresholdValues[i]
                    if minvalue<lessthan:
                        thresh=newthresh
            threshtotal=threshtotal+float(thresh)
            #
            #  Calculate whether this particular point violates the threshold
            #
            if absdiff<=thresh:
                itPasses=1
            else:
                itPasses=0
                numviolate=numviolate+1
            if self._debug>=10:
                print "  %f %f diff:%f threshold:%f itPasses:%1d"%(value1,value2,diff,thresh,itPasses)
        #
        #  Calculate average bias, average abs difference, average threshold
        #
        if numchecked>0:
            avgbias=biastotal/numchecked
            avgdiff=difftotal/numchecked
            avgthresh=threshtotal/numchecked
        else:
            avgbias=0.0
            avgdiff=0.0
            avgthresh=0.0
        #
        #  Setup return value (0 if OK, 1 if failed but only a few points, 2 if failed)
        #
        returnvalue=0
        if ((avgdiff>avgthresh) and (avgthresh>0.0)):
            if numchecked>self.MINPOINTS:
                returnvalue=2
            else:
                returnvalue=1
        return(returnvalue,totalnum,numchecked,numviolate,avgbias,avgdiff,avgthresh)
    #=========================================================================
    #
    #  _getBitsAndISC - a routine to get the ISC composite - broken out
    #  from the neighboringPoints routine so that it need not be called
    #  several times when looping over different edit areas (for each
    #  of the neighboring CWAs).
    #
    def _getBitsAndISC(self,WEname,GridInfo,GridTimeRange):
       isc = self._getBetterComposite(WEname, GridTimeRange)    
       if isc is None:
           return None, None
       #
       # See if we are working with a Scalar or Vector element
       #
       wxType = GridInfo.type()
       if GridType.SCALAR.equals(wxType):
          bits, isc = isc
          return bits,isc
       elif GridType.VECTOR.equals(wxType):
          bits, isc, direc = isc
          return bits,(isc,direc)
       else:
          return None, None
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
    def _getBetterComposite(self,parmName, timeRange):
        #
        #  Get the type, rateParm flag, and limits
        #  for the parameter name passed in.
        #
        mutableID=self.mutableID()
        baseGrid=self.getGrids(mutableID,parmName,"SFC",timeRange,noDataError=0)
        if baseGrid is None:
            return None
        gridInfoList=self.getGridInfo(mutableID,parmName,"SFC",timeRange)
        if (len(gridInfoList)<1):
           return None
        for gridInfo in gridInfoList:
           wxType=gridInfo.type()
           rateParm=gridInfo.rateParm()
           minlimit=gridInfo.minLimit()
           maxlimit=gridInfo.maxLimit()
        #
        #  Make sure ISC grids exist for this parm
        #
        parm=self.getParm("ISC",parmName,"SFC")
        if parm is None:
            return None
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
        if ((parmName=="MaxT")or(parmName=="PoP")):
            sum=self._empty-150.0
        elif (parmName=="MinT"):
            sum=self._empty+150.0
        else:
            sum=self._empty
            if GridType.VECTOR.equals(wxType):
                sumv=self._empty
        cnt=self._empty
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
            if GridType.SCALAR.equals(wxType):
                bits,isc=comp
                #isc=self.getGrids("ISC",parmName,"SFC",tr)
                if ((parmName=="MaxT")or(parmName=="PoP")):
                    sum=numpy.where(bits,numpy.maximum(isc,sum),sum)
                    cnt=numpy.where(bits,1,cnt)
                elif (parmName=="MinT"):
                    sum=numpy.where(bits,numpy.minimum(isc,sum),sum)
                    cnt=numpy.where(bits,1,cnt)
                else:
                    sum=numpy.where(bits,sum+isc,sum)
                    cnt=numpy.where(bits,cnt+1,cnt)
            if GridType.VECTOR.equals(wxType):
                bits,mag,direc = comp
                #(mag,direc)=self.getGrids("ISC",parmName,"SFC",tr)
                (u,v)=self.MagDirToUV(mag,direc)
                sum=numpy.where(bits,sum+u,sum)
                sumv=numpy.where(bits,sumv+v,sumv)
                cnt=numpy.where(bits,cnt+1,cnt)
            if GridType.WEATHER.equals(wxType):
                bits = comp
                bits,keys,strings=comp
                #(keys,strings)=self.getGrids("ISC",parmName,"SFC",tr)
        #
        #  now calculate average/max/min, etc.
        #  (count is always 1 for max/min)
        #
        noISC=numpy.less(cnt,0.5)
        bits=numpy.greater(cnt,0.5)
        if GridType.SCALAR.equals(wxType) or GridType.VECTOR.equals(wxType):
            cnt=numpy.where(numpy.less(cnt,1),1,cnt)
            if GridType.VECTOR.equals(wxType):
                sum=numpy.where(noISC,minlimit,sum/cnt)
                sumv=numpy.where(noISC,minlimit,sumv/cnt)
                (mag,direc)=self.UVToMagDir(sum,sumv)
                (baseMag,baseDir)=baseGrid
                mag=numpy.where(noISC,baseMag,mag)
                direc=numpy.where(noISC,baseDir,direc)
                return bits,mag,direc
            else:
                sum=numpy.where(noISC,baseGrid,sum/cnt)
                return bits,sum
        else:
            return bits,keys,strings
    #=================================================================
    #  makeTimeMsg - Make short string with time of this grid, usually
    #                something like "Mon (6/5) 12-18Z:", but can get
    #                complicated like "Mon (6/5) 18Z - Tue (6/6) 03Z:"
    #
    def makeTimeMsg(self,tr):
        DAYS=("Monday","Tuesday","Wednesday","Thursday",
              "Friday","Saturday","Sunday")
        swdy=DAYS[time.gmtime(tr.startTime().unixTime())[6]]
        sday=tr.startTime().day
        smon=tr.startTime().month
        sdate="%s (%d/%d)"%(swdy,smon,sday)
        shou=tr.startTime().hour
        ewdy=DAYS[time.gmtime(tr.endTime().unixTime())[6]]
        eday=tr.endTime().day
        emon=tr.endTime().month
        edate="%s (%d/%d)"%(ewdy,emon,eday)
        ehou=tr.endTime().hour
        if (sdate==edate):
            msg="%s %2.2d-%2.2dZ:"%(sdate,shou,ehou)
        else:
            msg="%s %2.2dZ - %s %2.2dZ:"%(sdate,shou,edate,ehou)
        return msg
    #================================================================
    #  _getElementList - get sorted list of currently displayed mutable
    #       model elements (by default it excludes nonSCALAR and non-
    #       VECTOR elements - but you can include them with the flag)
    #
    def _getElementList(self,excludeWxDiscrete=1):
        mutableModel=self.mutableID().modelName()
        parmList = self._dbss.getParmManager().getDisplayedParms()
        elementList = []
        for parm in parmList:
            name = parm.expressionName()
            model = parm.getParmID().getDbId().getModelName()
            if model == mutableModel:
                wxType = parm.getGridInfo().getGridType()
                if ((excludeWxDiscrete == 1) and (GridType.SCALAR.equals(wxType) or \
                    GridType.VECTOR.equals(wxType))):
                    elementList.append(parm.expressionName())
        elementList.sort()
        return elementList
    #================================================================
    #  _getTimeRangeList - get list of all PublishTimes, 
    #       plus "All Grids" and "Selected Time" added at the top of
    #       the list
    # 
    def _getTimeRangeList(self):
        trList = ["All Grids", "Selected Time"]
        publishTimes = self.getConfigItem("PublishTimes",[])
        #inv = self._dbss.dataManager().selectTRMgr().inventory()
        inv = self._dbss.getSelectTimeRangeManager().inventory()
        for t in publishTimes:
            if t in inv:
                trList.append(t)
        return trList
    #================================================================
    #  _convertTimeRange - given a timeRangeName - return the 
    #      timeRange (including the bogus names "All Grids" and
    #      "Selected Time").
    # 
    def _convertTimeRange(self, trName):
        if trName == "All Grids":
            timeRange = TimeRange.allTimes()
           # timeRange = AFPS.TimeRange(AFPS.AbsTime(0),
           #      AFPS.AbsTime_maxFutureTime())
        elif trName == "Selected Time":
            selectedTime = self._dbss.getParmOp().getSelectionTimeRange()
            if selectedTime is None:
                return None
            else:
                tr = self._dbss.getParmOp().getSelectionTimeRange()
                return TimeRange.TimeRange(tr.getStart(), tr.getEnd())
        else:
            timeRange = self.getTimeRange(trName)
        return timeRange

    def _getCachedGrid(self, name, category, timeLimit):
        try:
            object = self.getObject(name, category)
            grid, timeWritten, geoInfo = object
            if timeLimit != 0 and time.time() - timeWritten > timeLimit:
                return None        #too old
            
            # validate geoinfo
            geo = self._dbss.getParmManager().compositeGridLocation()
            if geoInfo != `geo`:
                return None        #different geoinfo
            return grid
        except:
            return None

    def _cacheGrid(self, name, grid, category):
          geo = self._dbss.getParmManager().compositeGridLocation()
          object = grid, time.time(), `geo`
          self.saveObject(name, object, category)

    #========================================================================
    #
    #  _checkViolate makes a mask of points INSIDE the SITE_EDITAREA that
    #  have a difference with a neighbors gridpoints (defined by areamask)
    #  that have a magnitude greater than threshold).  Uses bits and criteria
    #  just like other calcuations
    #
    def _checkViolate(self, bits, criteria, areamask, discGrid, threshold):
       violate=self._empty
       for i in range(4): # range(8) to consider diagonal neighbors
           #
           #  make sure data exists for both points
           #
           bitshift=self.offset(bits,self._ishift[i],self._jshift[i])
           exist=logical_and(bitshift,bits)
           #
           #  Make sure at least one of the points meets the criteria
           #
           critshift=self.offset(criteria,self._ishift[i],self._jshift[i])
           meetcrit=logical_or(critshift,criteria)
           #
           #  Make sure it borders the specified area
           #
           areashift=self.offset(areamask,self._ishift[i],self._jshift[i])
           onborder=logical_and(areashift,self._siteAreaMask)
           #
           #  Make sure it meets all criteria: exist, meetcrit, onborder
           #  and checkNDFD (meets topo thresholds)
           #
           mask=logical_and(logical_and(logical_and(exist,meetcrit),onborder),less(self._topodiff[i],self.MAXTOPODIFF))
           #
           violate=where(logical_and(mask,greater(abs(discGrid),threshold)),1,violate)
       return violate


    #========================================================================
    #
    #  _nbmask
    #
    #  Replicate the NDFD neighboring points algorithm.  It checks 'border
    #  pairs' where one point is 'inside' the CWA and the immediate
    #  neighbor (east-west-north-south) is 'outside' the CWA.  A point
    #  inside the CWA might be used in several 'border pairs' - since it
    #  might have a neighbor outside the CWA to both the north and the west,
    #  etc. 
    #
    #  A 'border pair' is compared if:
    #    .  The elevation difference between the two points is less
    #       than 1000 feet.
    #    .  Both points have ISC data available.
    #    .  The point 'outside' must be inside the area specified by
    #       areamask (usually the mask of the CWA you are checking against
    #
    #  After finding the differences for each of the remaining border pairs,
    #  It averages them, and returns the average border pair difference, the
    #  average elevation difference for the pairs, the overall average of the
    #  values, the minimum of the average of the values 'inside' or the values
    #  'outside', and the number of pairs used in this average.
    #
    def _nbmask(self, bits, isc, criteria, areamask, dirtype=0):
       totalpts=0
       totaldiff=0
       totaltopo=0
       totalvalue=0
       totalvaluein=0
       totalvalueout=0
       for i in range(4): # range(8) to consider diagonal neighbors
           #
           #  make sure data exists for both points
           #
           bitshift=self.offset(bits,self._ishift[i],self._jshift[i])
           exist=logical_and(bitshift,bits)
           #
           #  Make sure at least one of the points meets the criteria
           #
           critshift=self.offset(criteria,self._ishift[i],self._jshift[i])
           meetcrit=logical_or(critshift,criteria)
           #
           #  Make sure it borders the specified area
           #
           areashift=self.offset(areamask,self._ishift[i],self._jshift[i])
           onborder=logical_and(areashift,self._siteAreaMask)
           #
           #  Make sure it meets all criteria: exist, meetcrit, onborder
           #  and elevation difference less than MAXTOPODIFF  (meets topo thresholds)
           #
           mask=logical_and(logical_and(logical_and(exist,meetcrit),onborder),less(self._topodiff[i],self.MAXTOPODIFF))
           #
           #  Shift the data the directions
           #
           shift=self.offset(isc,self._ishift[i],self._jshift[i])
           #
           #  Get the difference (different for directions)
           #
           if dirtype==1:
               d1=self._dirdiff(isc,shift)
           else:
               d1=abs(isc-shift)
           #
           #  Get number of points
           #
           pts=add.reduce(add.reduce(mask))
           #
           #  get sum of differences for each shift direction
           #
           diff=sum(compress(mask.flat,d1.flat))
           #
           #  get sum of topo differences
           #
           topo=sum(compress(mask.flat,self._topodiff[i].flat))
           #
           #  get sums for points inside/outside
           #
           totalout=sum(compress(mask.flat,shift.flat))
           totalin=sum(compress(mask.flat,isc.flat))
           totalpts=totalpts+pts
           totaldiff=totaldiff+diff
           totaltopo=totaltopo+topo
           totalvalue=totalvalue+totalout+totalin
           totalvaluein=totalvaluein+totalin
           totalvalueout=totalvalueout+totalout
       #
       #  calculate total average
       #
       if (totalpts>0):
           avg=totaldiff/totalpts
           topoavg=totaltopo/totalpts
           avgvalue=totalvalue/(totalpts*2)
           avgvaluein=totalvaluein/totalpts
           avgvalueout=totalvalueout/totalpts
           minavgvalue=min(avgvaluein,avgvalueout)
       else:
           avg=0
           topoavg=0
           avgvalue=0
           minavgvalue=0
       return(avg,topoavg,avgvalue,minavgvalue,totalpts)

    #========================================================================
    #
    #  Get the smallest direction difference between two directions
    #
    def _dirdiff(self,dir1,dir2):
        diff=abs(dir1-dir2)
        less1=less(dir1,dir2)
        diff=where(greater(diff,180.0),where(less1,dir1+360.0-dir2,dir2+360.0-dir1),diff)
        return diff

