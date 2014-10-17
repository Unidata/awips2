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
# serpFile - version 2.0
#
#    Does the equivalent of the plain 'serp' tool - but gets the location
#    and control point information from a file.  Thus, this tool needs no
#    user input and can be run as part of a cronjob, etc.
#
#    The file is a comma delimited file where each data line contains a
#    station ID (ignored), a latitude, a longitude, and a data value.
#    Typical data lines might look like this:
#
#    BOI,43.57,-116.22,50.5
#    TWF,42.48,-114.48,43 # comment about this line
#
#    To make the file more readable, you can have comment lines which
#    start with a # character or are simply whitespace.
#
#    Any lines with less than 4 comma delimited values are ignored.  Lines
#    with more than 4 comma delimited values are potentially used - but
#    fields after the first 4 are ignored. 
#
#    Stations located off the GFE grid are ignored.  
#
#    Multiple sites lying on the same GFE gridpoint  are ignored (only 
#    the first one is used - and a status bar message is produced 
#    which tells you that the second (or more) station is being ignored).
#
#    No timeRange checking is done - the tool simply operates on the
#    current grid, using the values supplied in the file and stores the
#    results back into the same grid.  Clipping is performed so that the
#    values of the new grid do not exceed the allowable values for the
#    grid.
#
#    This works for SCALAR grids only - not vectors or weather/discrete 
#    elements
#
# Author: Tim Barker - SOO BOI (serp tool is from Les Colin)
#   2014/06/11 - Modified a couple of things to make it cleaner in A2
#   2010/08/05 - updated to use ObjAnal utility
#   2003/10/16 - original implementation based on serp tool
#=======================================================================
#  START OF CONFIGURATION SECTION
#
#  The filename to read
#
FILENAME="/tmp/lsrinfo.dat"
#
#  If you wish to include elevation adjustment (so that adjustments
#  are based on elevation differences as well as horizontal distance
#  from the point) then set elevation_factor to a non-zero value.
#
#  elevation_factor should be in units of feet/km.
#
#  If you set it to 1, then 1 foot of elevation difference is
#  equivalent to 1km of horizontal distance (this means
#  that elevation is VERY important in the analysis).
#
#  if you set it to 1000, then 1000 feet of elevation
#  difference is equal to 1 km of horizontal distance
#  (this means that elevation is NOT important to the
#  analysis).  
#
#  To turn off elevation completely - set the elevation_factor to zero.
#  which is the default  
#
#  A value of 36 feet/km seems work reasonably well for including SOME
#  influence of elevation - but not too much.
#
elevation_factor=0.0
#
#  END OF CONFIGURATION SECTION
#=======================================================================
ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList = ["SCALAR"]

import numpy as np
import SmartScript
import ObjAnal
import os,re

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss=dbss
        
    def execute(self, Topo, variableElement, variableElement_GridInfo, varDict):
        "Match specified points to values in file using objective analysis"
        print "Tool serp_file starting"
        #
        #  Setup the utility
        #
        self.OA=ObjAnal.ObjAnal(self._dbss)
        #
        #  value limits for grid being edited
        #
        self.valmax=variableElement_GridInfo.getMaxValue()
        self.valmin=variableElement_GridInfo.getMinValue()
        #
        #  read data lines from file
        #
        filename=FILENAME
        datalines=self.readFILE(filename)
        if len(datalines)<1:
           msg="No data in file %s, so grid left unchanged"%(filename)
           self.statusBarMsg(msg,"S")
           print msg
           self.cancel()
        #
        #  setup data locations from file
        #
        valuelist=self.getDataLocations(datalines,variableElement,Topo)
        if (len(valuelist)<1):
            msg="No valid data in file %s, so grid left unchanged"%(filename)
            self.statusBarMsg(msg,"S")
            print msg
            self.cancel()
        #
        #
        #
        new=self.OA.ObjectiveAnalysis(valuelist,variableElement,"serp",
                                      elevfactor=elevation_factor)
        #
        #  clip to grid min/max
        #
        newclip=np.clip(new,self.valmin,self.valmax)
        print "Tool serp_file complete"
        return newclip
    #=================================================================
    #
    #  Read data values from the data lines
    #
    def getDataLocations(self,datalines,variableElement,Topo):
        #
        #  setup storage for location info
        #
        valuelist=[]
        self.xloclist=[]
        self.yloclist=[]
        #
        #  decode data lines into location info
        #
        for line in datalines:
            (id,latstr,lonstr,valuestr)=line.split(",",3)
            latstr=re.sub('[^-0123456789.]','',latstr)
            lonstr=re.sub('[^-0123456789.]','',lonstr)
            valuestr=re.sub(',.*$','',valuestr) # get rid of any more comma-delimited things at end of line
            valuestr=re.sub('#.*$','',valuestr) # get rid of any inline comments at end of field
            valuestr=re.sub('[^-0123456789.]','',valuestr) # get rid of non-numeric characters in remaining value
            latf=float(latstr)
            lonf=float(lonstr)
            if (latf<-90.0)or(latf>90.0)or(lonf<-180.0)or(lonf>180.0):
                msg="Invalid lat/lon ignored: %s"%line
                self.statusBarMsg(msg,"S")
                print msg
                continue
            #
            #   make sure point is on grid
            #
            (x,y)=self.getGridCell(latf,lonf)
            if ((x is None)or(y is None)):
                msg="Data for %s ignored (%6.3f,%8.3f) - location not on GFE grid" % (id,latf,lonf)
                self.statusBarMsg(msg,"S")
                print msg
                continue
            xint=int(x)
            yint=int(y)
            #
            #   Make sure point has not already been specified
            #
            if len(self.xloclist)>0:
                skip=0
                for i in range(len(self.xloclist)):
                    if ((self.xloclist[i]==xint) and (self.yloclist[i]==yint)):
                        msg="Data for %s ignored - data for this GFE gridpoint already specified"%(id)
                        self.statusBarMsg(msg,"S")
                        print msg
                        skip=1
                        break
                if skip==1:
                    continue
            #
            #  Make sure value is valid
            #
            valf=float(valuestr)
            if (valf<self.valmin):
                msg="%s value of %.3f clipped to allowable range of %f-%f"%(id,valf,self.valmin,self.valmax)
                self.statusBarMsg(msg,"S")
                print msg
                valf=float(self.valmin)
            if (valf>self.valmax):
                msg="%s value of %.3f clipped to allowable range of %f-%f"%(id,valf,self.valmin,self.valmax)
                self.statusBarMsg(msg,"S")
                print msg
                valf=float(self.valmax)
            #
            #  add it to list
            #
            valuelist.append((id,xint,yint,Topo[yint,xint],valf))
            self.xloclist.append(xint)
            self.yloclist.append(yint)
        return valuelist
    #===================================================================
    #  readFILE - read specified FILE returning only data lines where
    #             4 or more comma delimited values occur
    #
    def readFILE(self,filename):
        datalines=[]
        #
        #  make sure the file exists
        #
        if (not os.path.exists(filename)):
           msg="Could not find file %s" % (filename)
           self.statusBarMsg(msg,"S")
           print msg
           return datalines
        #
        #  read the file
        #
        filespec=file(filename,'r')
        lines=filespec.readlines()
        filespec.close()
        #
        #  get only data lines
        #
        for line in lines:
           stripline=line.strip()  # ignore whitespace at begin/end
           if len(stripline)<1:
              continue
           if line[0:1]=="#":  #  ignore comment lines
              continue
           pieces=stripline.split(",",3)
           if len(pieces)!=4:  # ignore lines with less than 4 comma fields
              continue
           datalines.append(stripline)
        return datalines
