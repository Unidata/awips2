# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# MakeHazard.py
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer     Description
# ------------ ---------- -----------  --------------------------
#  Apr 03,2012 436        randerso     Converted to Python procedure to allow some
#                                      level of site customization
#  Apr 09,2012 436        randerso     Merged RNK's MakeHazards_Elevation procedure
#  Feb 12,2014 17058      ryu          Extend converter for Collections$EmptyList objects.
#
# Author: randerso
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Hazards"]


import SmartScript
import time, string, sys
import HazardUtils
import re
import numpy
import LogStream
import JUtil

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

        self._dataManager = dbss
        self._afterInit = 0  #flag indicating init is done.

        self._tropicalHaz = ['HU.W','HU.A','HU.S','TR.W','TR.A']
        self._natlBaseETN = 1001


    def setUpUI(self):
        if sys.modules.has_key("MakeHazardConfig"):
            sys.modules.__delitem__("MakeHazardConfig")
        import MakeHazardConfig

        args = {}
        args['dataManager'] = self._dataManager
        args['selectedTimeRange'] = self.selectedTimeRange
        args['mapColor'] = MakeHazardConfig.mapColor
        args['defaultMapWidth'] = MakeHazardConfig.defaultMapWidth
        args['timeScaleEndTime'] = MakeHazardConfig.timeScaleEndTime
        args['areaThreshold'] = MakeHazardConfig.areaThreshold
        args['defaultHazardType'] = MakeHazardConfig.defaultHazardType
        args['mapNames'] = MakeHazardConfig.mapNames
        args['hazardDict'] = MakeHazardConfig.hazardDict
        args['tcmList'] = MakeHazardConfig.tcmList
        args['tropicalHaz'] = self._tropicalHaz
        args['natlBaseETN'] = self._natlBaseETN
        
        if not hasattr(MakeHazardConfig, 'localEffectAreas') or \
           MakeHazardConfig.localEffectAreas is None:
            args['localEffectAreas'] = {}
        else:
            args['localEffectAreas'] = MakeHazardConfig.localEffectAreas
        
        if not hasattr(MakeHazardConfig, 'localAreaData') or \
           MakeHazardConfig.localAreaData is None:
            args['localAreaData'] = {}
        else:
            args['localAreaData'] = MakeHazardConfig.localAreaData

        # create the Java/SWT dialog and open it
        from com.raytheon.viz.gfe.makehazard import MakeHazardDialog
        self.__dlg = MakeHazardDialog.createFromPython(
            JUtil.pyValToJavaObj(args)
        )
        self.__dlg.openFromPython()
        
        # run the Java/SWT event loop
        try:
            dismiss = False
            while not dismiss:
                args = JUtil.javaObjToPyVal(self.__dlg.runFromPython(), converter)
                dismiss = True;
                # if args is None, then Cancel was pressed
                if args is not None:
                    # dismiss is True if the Run/Dismiss button is pressed,
                    # false if Run is pressed
                    dismiss = args["dismiss"]
                    del args["dismiss"]
                    
                    if self.makeHazardGrid(**args) != 1:
                        dismiss = False
        finally:
            # close the Java/SWT dialog when Cancelled, Dismissed or exception occurs
            self.__dlg.closeFromPython()
           
    # RJM modified this routine from the HazardUtility file
    # returns a Numeric mask where each zone in zoneList is set to 1
    def _makeMask(self, zoneList, hazLocalEffect):

        #  RJM had to modify this next line to point to the hazUtils
        #  for the getGridSize routine.
        gridSize = self._hazUtils._getGridSize()
        mask = numpy.zeros(gridSize)
        eaList = self.editAreaList()

        #  Get the elevation from the GUI input.  We'll do this by clipping
        #  of any numerical digits from the local effect.
#        elevation_string = re.findall("\d+", hazLocalEffect)
#        print "re elevation=", elevation_string, "xxx"
#        try:
#            elevation = elevation_string[0]
#        except:
#            elevation = "None"
#        print "re elevation=", elevation, "xxx"
        for z in zoneList:
            print "in _makeMask processing zone ", z

            if z in eaList:
                zoneArea = self.getEditArea(z)
                zoneMask = self.encodeEditArea(zoneArea)

                #  Code added by RJM.  This checks to see if the local effect
                #  area was specified and is a valid edit area.  If so,
                #  make a mask from it, and then do an intersection with
                #  the zone mask.
                if hazLocalEffect in eaList:
                    print "Masking",z,"with",hazLocalEffect
                    localEffectArea = self.getEditArea(hazLocalEffect)
                    localEffectMask = self.encodeEditArea(localEffectArea)
                    zoneMask = numpy.logical_and(zoneMask, localEffectMask)

                mask = numpy.logical_or(mask, zoneMask)
#            else:
#                if z in eaList:
#                    zoneArea = self.getEditArea(z)
#                    zoneMask = self.encodeEditArea(zoneArea)
#                    mask = numpy.logical_or(mask, zoneMask)

        return mask

    # Creates the hazard grid based on the dialog input
    def makeHazardGrid(self, selectedHazard, timeRange, areaList, segmentNumber, 
                       selectedTimeRange, defaultAreaList, defaultHazard, defaultSegment, 
                       hazLocalEffect):
        siteID = self.getSiteID()
        usingHazLocalEffect = (hazLocalEffect != 'None')

        if len(areaList) == 0:
            editArea = self.getActiveEditArea()
            mask = self.encodeEditArea(editArea)
        else:
            # make the mask based on the list selections
            if not usingHazLocalEffect:
                mask = self._hazUtils._makeMask(areaList)
            else:
                mask = self._makeMask(areaList, hazLocalEffect)
                
        if usingHazLocalEffect:
            # get the segment number and filter for valid characters
            segNum = segmentNumber
            
            # get the hazards currently defined as temporary grids
            hazParms = self.getHazardParmNames()
            
            # look through the list of grids and create a list of
            # segment numbers (if any) that are already in use
            # for the current hazard
#            if len(hazParms) == 0:
#                self.statusBarMsg("No temporary grids to merge.", "S")
#                return 0
            segList = []
            print "selectedHazard=", selectedHazard
            selectedPhen = selectedHazard[0:2]
            selectedSig  = selectedHazard[3]
            print "selectedPhen,selectedSig=", selectedPhen, ".", selectedSig
            for hazParm in hazParms:
                print "hazParm=", hazParm
                trList = self._hazUtils._getWEInventory(hazParm)
                for tr in trList:
                    print "  tr=", tr, timeRange
                    intersect_hours = tr.intersection(timeRange).duration()
                    print "  intersect=", intersect_hours
                    intersect_percent = intersect_hours / timeRange.duration() * 100.0
                    print "  intersect %=", intersect_percent
                phen = hazParm[3:5]
                sig = hazParm[5:6]
                print "phen,sig=", phen, ".", sig
                if len(hazParm) > 6:
                    if hazParm[6:].isdigit():
                        seg = int(hazParm[6:])
                        print "  seg=", seg
                        if phen == selectedPhen and sig == selectedSig:
                            segList.append(seg)
                            print "appending ", seg
                    else:
                        seg = 0
            segList.sort()

#            print "looping through segList"
#            for seg in segList:
#                print "   seg=", seg," elev=", elevation
#                if str(elevation) == str(seg):
#                    print "adding 1 to elevation"
#                    elevation += 1 
#
#            if elevation > 400:
#                print "using elevation for segNum"
#                segNum = elevation
#                #  replace the segmentNumber field with the elevation +/- the Above/Below indicator.
#                self.__dlg.setSegmentNumber(elevation)
#                segmentNumber = str(elevation)
#                print "*** segmentNumber=", segmentNumber
            
        index = string.find(selectedHazard, " ")
        if index != -1:
            selectedHazard = selectedHazard[0:index]
        if len(segmentNumber) > 0:
            hazardKey = selectedHazard + ":" + segmentNumber
        else:
            hazardKey = selectedHazard
            
        defaultHazKey = ""
        if defaultHazard is not None:
            index = string.find(defaultHazard, " ")
            if index != -1:
                defaultHazard = defaultHazard[0:index]
            defaultHazKey = defaultHazard
            
            if len(defaultSegment) > 0:
                defaultHazKey += ":" + defaultSegment

        weName = self._hazUtils._makeTempWEName(hazardKey)        

        # if we're modifying, remove the old grid first
        if defaultAreaList != [] and hazardKey == defaultHazKey:
            self.deleteCmd([weName], self.selectedTimeRange)

        # if we have no selection prevent user from making an empty hazard
        if 1 not in mask:
            self.statusBarMsg("NO EDIT AREA SELECTED:  \n Select area from map or load edit area in GFE!", "S")
            return 0

        self._hazUtils._addHazard(weName, timeRange, hazardKey, mask)
        LogStream.logUse("Set: ", weName, 
          self._hazUtils._printTime(timeRange.startTime().unixTime()), 
          self._hazUtils._printTime(timeRange.endTime().unixTime()), hazardKey, 
          self._hazUtils._printAreas(areaList))

        return 1
    
    def getHazardParmNames(self):
        # get the list of loaded temporary hazard parms
        parms = self.loadedParms()
        hazParms = []
        for weName, level, dbID in parms:
            if "haz" in weName:
                key = self._hazUtils._tempWENameToKey(weName)
                index = string.find(key, ":")
                if index != -1:
                    mkey = key[0:index]
                    segNum = key[index+1:]
                else:
                    mkey = key
                    segNum = ""

                # append the hazard and a description
                parmName = "haz" + key
                parmName = string.replace(parmName, ".", "")
                parmName = string.replace(parmName, ":", "")
                hazParms.append(parmName)

        return hazParms

    def execute(self, timeRange):
        #self._hazUtils = HazardUtils.HazardUtils(self._dataManager, self.eaMgr())
        self._hazUtils = HazardUtils.HazardUtils(self._dataManager, None)
        # save the selected timeRange
        self.selectedTimeRange = timeRange
        
        self.setToolType("numeric")

        # see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in " + \
              "the GFE before running MakeHazard", "S")
            self.cancel()


        # always separate the Hazards grid first
        self._hazUtils._separateHazardGrids()

        self.setUpUI()

        self._afterInit = 1   #initialization done

        return

def converter(obj):
    import AbsTime
    import TimeRange
    retVal = None

    objtype = obj.jclassname
    if objtype == "java.util.Date":
        retVal = AbsTime.AbsTime(obj)
    elif objtype == "java.util.Collections$EmptyList":
        retVal = []
    elif objtype == "com.raytheon.uf.common.time.TimeRange":
        retVal = TimeRange.TimeRange(obj)
    return retVal

