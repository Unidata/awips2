# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_Finalize - Version 20181203
#
# Last Modified: 3 December 2018
#
# Author: lefebvre
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards
MenuItems = ["Populate"]

VariableList = []

import time
import SmartScript
import numpy as np
import copy
import tkinter as tk
import AbsTime, TimeRange
import Aviation_EDASConfig as FinalizeConfig
import Exceptions

# Translates words into wxTypes
# Used by the method addNewWxFromVis()
wxTypesDict = {"Fog" : "F",
               "Haze" : "H",
               "Smoke" : "K",
               }

# Tooltip class so we can specify a tooltip for any widget. Just pass in the
# widget and the text to be displayed.
class CreateToolTip(object):

    def __init__(self, widget, text='widget info'):
        self.widget = widget
        self.text = text
        self.widget.bind("<Enter>", self.enter)
        self.widget.bind("<Leave>", self.close)
        
    def enter(self, event=None):
        x = y = 0
        x, y, cx, cy = self.widget.bbox("insert")
        x += self.widget.winfo_rootx() + 25
        y += self.widget.winfo_rooty() + 20
        # creates a toplevel window
        self.tw = tk.Toplevel(self.widget)
        # Leaves only the label and removes the app window
        self.tw.wm_overrideredirect(True)
        self.tw.wm_geometry("+%d+%d" % (x, y))
        label = tk.Label(self.tw, text=self.text, justify='left',
                         background='yellow', relief='solid', borderwidth=1,
                         font=("times", "10", "normal"))
        label.pack(ipadx=1)

    def close(self, event=None):
        if self.tw:
            self.tw.destroy()

# End Tooltip class

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Fetches the inventory as a list of timeRanges of the specified model and
    # and weather element. This list will be restricted to the specified timeRange,
    # if it's provided.
    def getWEInventory(self, modelName, WEName, timeRange=None):

        allTimes = TimeRange.allTimes()
        
        if timeRange is None:
            timeRange = allTimes

        trList = []
        # getGridInfo will just die if the modelName or weName is not valid
        # so wrap it in a try block and return [] if it fails
        try:
            gridInfo = self.getGridInfo(modelName, WEName, "SFC", timeRange)
        except Exceptions.EditActionError:
            return trList

        for g in gridInfo:
            if timeRange.overlaps(g.gridTime()):
                trList.append(g.gridTime())

        return trList
    # Returns True if the specified WeName is a rate parm. 
    def cumulativeParm(self, weName, dbName):
        parm = self.getParm(dbName, weName, "SFC")
        if parm is None:
            return False

        rateParm = parm.getGridInfo().isRateParm()
        rateParm = (rateParm == 1)   # convert int to boolean

        return rateParm

    # Get the type of the specified weather element name
    def getWEType(self, weName):

        parm = self.getParm("Fcst", weName, "SFC")
        # Check for None in case the element is not configured.
        if parm is None:
            return None

        parmType = str(parm.getGridInfo().getGridType())

        return parmType
    
    def getParmMinMaxLimits(self, modelName, weName):
        
        parm = self.getParm(modelName, weName, "SFC")

        return parm.getGridInfo().getMinValue(), parm.getGridInfo().getMaxValue()

    # Creates a list of hourly timeRanges based on the specified timeRange
    def getTimeRanges(self, timeRange):
        
        start = timeRange.startTime().unixTime()
        end = timeRange.endTime().unixTime()
        
        trList = []
        for t in range(start, end, 3600):
            s = AbsTime.AbsTime(t)
            e = AbsTime.AbsTime(t + 3600)
            tr = TimeRange.TimeRange(s, e)
            
            trList.append(tr)
        
        return trList
    
    # Fetches every grid within the specified timeRange for the elements in weList
    def fetchAllGrids(self, weList, selectedTR):

        weDict = {}
        emptyMask = self.empty(np.bool)
        for weName in weList:
            trList = self.getWEInventory("Fcst", weName, selectedTR)
            weDict[weName] = {}
            
            cumParm = self.cumulativeParm(weName, "Fcst")
            
            for tr in trList:
                
                grid = self.getGrids("Fcst", weName, "SFC", tr)
                if tr.duration() ==  3600:
                    weDict[weName][tr] = grid, emptyMask
                    continue
                
                # Parm of different duration needs to be split
                hourlyTRList = self.getTimeRanges(tr)
                # If it's a cumulative parm divide by the duration
                if cumParm:
                    grid = grid / (tr.duration() // 3600)
                    
                for hourlyTR in hourlyTRList:
                    weDict[weName][hourlyTR] = grid, emptyMask

        return weDict

    # Returns a mask where the Wx grid matches the specified Wx type and intensity.
    # if wxIntens is None, the mask matches the Wx type and all intensities.
    def getWxMask(self, wxGrid, wxType, wxIntens=None):
    
        byteGrid, wxKeys = wxGrid
        
        gridMask = np.zeros(byteGrid.shape, np.bool)
        for wxKey in wxKeys:
            subKeys = wxKey.split("^")
            for subKey in subKeys:
                subParts = subKey.split(":")
                if subParts[1] == wxType and subParts[2] == wxIntens or \
                   subParts[1] == wxType and wxIntens is None:
                    wxIndex = self.getIndex(wxKey, wxKeys)
                    gridMask = gridMask | (byteGrid == wxIndex)
                    
        return gridMask
    
    # Returns the three Wx components if both type are obstructions and "" otherwise
    def bothTypesObstructions(self, newSubType, wxKey):
        
        subKeys = wxKey.split("^")
        for subKey in subKeys:
            subCov, subType, subInten = self.parseCovTypeIntens(subKey)
            # if both are obstructions, return True
            if subType in self._obstructionTypes and newSubType in self._obstructionTypes:
                return subType
        
        return ""


    # Called when we want to replace a subkey within and existing wx key
    def replaceWxSubKey(self, wxKey, wxTypeToReplace, newWxCov, newWxType, newWxIntens):
        newWxKey = ""
        
        subKeys = wxKey.split("^")
        for subKey in subKeys:
            wxCov, wxType, wxIntens = self.parseCovTypeIntens(subKey)
            
            # Add the separator if needed
            if newWxKey != "":
                newWxKey = newWxKey + "^"
            
            if wxType == wxTypeToReplace:  # Replace this subKey
                newWxKey = newWxKey + newWxCov + ":" + newWxType + ":" + newWxIntens + ":<NoVis>:"
            else:  # Add to the existing key
                newWxKey = newWxKey + subKey
        
        return newWxKey

    def removeWxType(self, wxGrid, removeWxType):
                
        byteGrid, wxKeys = wxGrid
        initialWxKeys = copy.copy(wxKeys)
        noWxUglyStr = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"

        for wxKey in initialWxKeys:
            newKey = ""
            
            subKeys = wxKey.split("^")
            
            for subKey in subKeys:
                wxCov, wxType, wxInten = self.parseCovTypeIntens(subKey)
                
                if wxType != removeWxType:
                    if newKey == "":
                        newKey = subKey
                    else:
                        newKey = newKey + "^" + subKey

            # Don't bother changing the WxGrid, if the key is the same                
            if newKey == wxKey:
                continue
            # If the key was only the specified wxType the newKey will be empty
            if newKey == "":
                newKey = noWxUglyStr
            
            oldIndex = self.getIndex(wxKey, wxKeys)
            mask = byteGrid == oldIndex
            newIndex = self.getIndex(newKey, wxKeys)
            byteGrid[mask] = newIndex
        
        return byteGrid, wxKeys


    # Adds the new Wx coverage, type, and intensity to the specified wx grid and keys.
    # If the wx type is the same as any type found in the wxKeys, the type is preserved
    # and the specified coverage and intensity is used.
    def addNewWx(self, wxGrid, wxKeys, newWxCov, newWxType, newWxIntens, mask):
        
        initialWxKeys = copy.copy(wxKeys)
                
        for wxKey in initialWxKeys:
            newType = newWxType
            
            wxIndex = self.getIndex(wxKey, wxKeys)
            wxMask = wxGrid == wxIndex
            
            # See if there's any overlap and if not skip this wxKey
            overlap = wxMask & mask
            if not overlap.any():
                continue

            # Check to see if we're trying to add the same Wx type.
            # If so, build a brand new key and add that to the Wx keys
            # Trying to combine two keys of the same type is not allowed
            wxParts = self.wxCovTypeIntensMatchType(wxKey, newType) # Returns None if not the same wx type
            if wxParts is not None:  # We found matching wxTypes somewhere in the full key
                if len(wxKey.split("^")) == 1:  # only one subKey
                    # Make a brand new subkey
                    newWxKey = newWxCov + ":" + newType + ":" + newWxIntens + ":<NoVis>:"
                else:  # This wxKey has more subKeys
                    # Make a new key that includes the key we want plus the other subKeys
                    subKeyList = wxKey.split("^")
                    newWxKey = ""
                    for subKey in subKeyList:
                        subCov, subType, subInten = self.parseCovTypeIntens(subKey)
                        if self.wxTypeMatches(subType, newType):
                            if newWxKey != "":  # Add the leading caret 
                                newWxKey = newWxKey + "^"
                            newWxKey = newWxKey + newWxCov + ":" + newType + ":" + newWxIntens + ":<NoVis>:"
                        else:
                            newWxKey = newWxKey + "^" + subKey
                            
            else:  # No wxType match, so make a new key
            # If both types are obstructions, replace whatever is there
                replaceWxType = self.bothTypesObstructions(newWxType, wxKey)
                if replaceWxType != "":
                    newWxKey = self.replaceWxSubKey(wxKey, replaceWxType, newWxCov, newWxType, newWxIntens)
                    #print "replacing:", wxKey, "with:", newWxCov, newWxType, newWxIntens
                else: # just add it in to the existing key
                    newWxKey = self.makeNewWxKey(wxKey, newWxCov, newType, newWxIntens)
                        
            newWxIndex = self.getIndex(newWxKey, wxKeys)
            wxGrid[overlap] = newWxIndex

        return wxGrid, wxKeys
    
    # Makes and returns a new Wx sub key based on the specified old key and new Wx components.
    # Old and new weather types should not match.
    def makeNewWxKey(self, oldKey, newCov, newType, newIntens):
                
        keyToAdd = newCov + ":" + newType + ":" + newIntens + ":<NoVis>:"

        if oldKey == self._noWxStr:
            return keyToAdd
        
        newKey = ""
        
        # See if a subKey already has the F or ZF and replace it with the new attributes
        subKeys = oldKey.split("^")
        for subKey in subKeys:
            wxCov, wxType, wxIntens = self.parseCovTypeIntens(subKey)
            if wxCov == "":
                continue
                
            # If old type is F and we want FZ or vice versa, replace the subKey
            elif wxType in ["F", "ZF"] and newType in ["F", "ZF"]:
                keyPart = keyToAdd
            else:
                keyPart = subKey + "^" + keyToAdd
                
            if newKey == "":
                newKey = newKey + keyPart
            else:
                newKey = newKey + "^" + keyPart
                     
        return newKey
  
    # Caps the CBP height based on different ranges of PoP values
    def popAssignsCBP(self, PoP, CloudBasePrimary, Sky, Visibility):
        changeMask = self.empty(np.bool)
        mask = (PoP >= 15.0) & (PoP < 25.0) & (CloudBasePrimary > 110.0) & self._editAreaMask
        CloudBasePrimary[mask] = 110.0   # 11000 feet
        changeMask = changeMask | mask
        mask = (PoP >= 25.0) & (PoP < 55.0) & (CloudBasePrimary > 100.0)
        CloudBasePrimary[mask] = 100.0   # 10000 feet
        changeMask = changeMask | mask
        mask = (PoP >= 55.0) & (CloudBasePrimary > 90.0)
        CloudBasePrimary[mask] = 90.0   # 9000 feet
        changeMask = changeMask | mask
        
        # Make sure we have no "zero" ceilings if SKy < OVC and good Visibility
        mask = (Sky <= 87.5) & (Visibility >= 7.0) & (CloudBasePrimary < 1.0)
        CloudBasePrimary[mask] = 1.0
        changeMask = changeMask | mask
        
        return CloudBasePrimary, changeMask
    
    # Returns a mask where any precipitation type is found. 
    def getPrecipNonPrecipMask(self, Wx, wxTypeList):
        
        mask = self.empty(np.bool)  # mask of zeros
        wxGrid, wxKeys = Wx
        # Check each key
        for wxKey in wxKeys:
            # and each subkey within each key
            subKeys = wxKey.split("^")
            for subKey in subKeys:
                wxCov, wxType, wxIntens = self.parseCovTypeIntens(subKey)
                
                if wxType in wxTypeList:
                    wxIndex = self.getIndex(wxKey, wxKeys)
                    mask = mask | (wxGrid == wxIndex)
        
        return mask

    # Rule that ensures the Visibility remains consistent with the Wx and PoP.
    def lowPoPNoWxVis10(self, PoP, Wx, Visibility):
        changeMask = self.empty(np.bool)
        
        # Get the mask of places where there are obstructions.
        obstructionMask = self.getPrecipNonPrecipMask(Wx, self._obstructionTypes)
        # But we really want the place where there is no precip.
        noPrecipMask = ~obstructionMask
        # Calculated the other masks
        lowVisMask = Visibility < self._config["visPopThreshold"]
        popMask = (PoP < 55.0) & self._editAreaMask
        # Set the visibility
        mask = popMask & noPrecipMask & lowVisMask    
        Visibility[mask] = self._config["visPopThreshold"]
        changeMask = changeMask | mask

        highPoPMask = PoP >= 55.0
        visMask = Visibility > 6.0

        # Set the visibility
        mask = highPoPMask & visMask & self._editAreaMask 
        Visibility[mask] = 6.0
        changeMask = changeMask | mask

        return Visibility, changeMask

    # Ensures that Sky is not less than the PoP value
    def popAssignsSky(self, PoP, Sky):
        changeMask = self.empty(np.bool)
        mask = (PoP > Sky) & self._editAreaMask
        Sky[mask] = PoP[mask]
        changeMask = changeMask | mask

        return Sky, changeMask
    
    # Removes zero CBP values where Sky is not OVC and Visibility > 0.5
    def noZeroCeiling(self, CloudBasePrimary, Sky, Visibility):
        changeMask = self.empty(np.bool)
        
        zeroCeilingMask = CloudBasePrimary == 0.0
        noOVCMask = Sky < 93.5
        visMask = Visibility >= 0.5
        
        mask = zeroCeilingMask & noOVCMask & visMask
        CloudBasePrimary[mask] = 1.0
        changeMask = changeMask | mask

        return CloudBasePrimary, changeMask
    
    # Internal method to extract the Visibility limits based on wxType and coverage.
    def calcCoverageLimits(self, wxType, coverage):
        covList = copy.copy(self._wxTypeCovDict[wxType]) # need to copy so we don't mess with the original copy.
        covList.reverse()
        visList = [0.0]
        for i in range(len(covList)):
            visList.append(self._scales[covList[i]].get())

        covIndex = covList.index(coverage)
        visMin = visList[covIndex]
        visMax = visList[covIndex + 1]

        return visMin, visMax


    # Modify PoTFog based on Visibility 
    def makePoTGridFromVis(self, Visibility, T, Td, wxType, TMinusTd, tr):                
        # Make masks of interesting places
        tBelow32 = (T <= 32.0)
        # Places where we want Fog
        fogMask = ((T - Td) <= TMinusTd) & self._editAreaMask
        
        covList = self._wxTypeCovDict[wxType] # Fetch coverages for Fog
        
        covDict = {"Patchy" : 20.0,
                   "Areas" : 40.0,
                   "Wide" : 70.0,
                   "Def" : 100,
                   }
        
        if wxType == "Fog":
            potFogGrid = self.empty()  # grid of zeros
            potFreezingFogGrid = self.empty()  # grid of zeros
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(wxType, cov)
                
                mask = (Visibility >= covMin) & (Visibility < covMax) & fogMask
                
                if not mask.any(): # skip if no point are found
                    continue
                
                if cov not in covDict:
                    print("ERROR!! coverage:", cov, " not found in covDict")
                    continue
                
                potValue = covDict[cov] # Fetch the potFog value from the dict
                
                # Update the potFogGrid grid
                potFogGrid[mask] = potValue
    
                # Update the potFreezingFogGrid grid
                potFreezingFogGrid[mask & tBelow32] = potValue
                
                # Create the grids
                self.createGrid("Fcst", "PotFog", "SCALAR", potFogGrid, tr)
                self.createGrid("Fcst", "PotFreezingFog", "SCALAR", potFreezingFogGrid, tr)

        # Add Haze
        elif wxType == "Haze":
            potHazeGrid = self.empty()  # grid of zeros
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(wxType, cov)                
                mask = (Visibility >= covMin) & (Visibility < covMax) & self._editAreaMask
                if not mask.any(): # skip if no point are found
                    continue
                
                if cov not in covDict:
                    print("ERROR!! coverage:", cov, " not found in covDict")
                    continue
                
                potValue = covDict[cov]
                potHazeGrid[mask] = potValue
                self.createGrid("Fcst", "PotHaze", "SCALAR", potHazeGrid, tr)

        # Add Smoke 
        elif wxType == "Smoke":
            potSmokeGrid = self.empty()  # grid of zeros
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(wxType, cov)                
                mask = (Visibility >= covMin) & (Visibility < covMax) & self._editAreaMask

                if not mask.any(): # skip if no point are found
                    continue
                
                if cov not in covDict:
                    print("ERROR!! coverage:", cov, " not found in covDict")
                    continue
        
                potValue = covDict[cov]
                potSmokeGrid[mask] = potValue
                self.createGrid("Fcst", "PotSmoke", "SCALAR", potSmokeGrid, tr)
        
        return
    
    # Add new weather (F, K, H) where Visibility values dictate. 
    def addNewWxFromVis(self, Visibility, T, Td, Wx, newWxType, TMinusTd, tr):

        oldByteGrid, oldKeys = Wx
        # First remove the weather from the grid we will be changing
        # wxTypesDict is defined at the top of this module.
        wxGrid, wxKeys = self.removeWxType(Wx, wxTypesDict[newWxType])
        
        changeMask = self.empty(np.bool)
        
        noWxIntensity = "<NoInten>"
        # Make masks of interesting places
        visBelowPoint25 = Visibility <= 0.25
        visBelowFiveEighths = Visibility < 0.625
        tBelow32 = T <= 32.0
        tAbove32 = T > 32.0

        # Define the criteria for Obstruction Wx based on the above masks
        fogMask = ((T - Td) <= TMinusTd) & self._editAreaMask
        
        covList = self._wxTypeCovDict[newWxType]
        # Add Fog to Wx
        if newWxType == "Fog":
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(newWxType, cov)
                mask = (Visibility >= covMin) & (Visibility <= covMax) & fogMask
                changeMask = changeMask | mask

                # Add Fog
                newWxGrid, newWxKeys = self.addNewWx(wxGrid, wxKeys, cov, "F", noWxIntensity, mask & tAbove32)
                changeMask = changeMask | (mask & tAbove32)
    
                # Add Freezing Fog - Use the Wx grid and keys from above.
                newWxGrid, newWxKeys = self.addNewWx(newWxGrid, newWxKeys, cov, "ZF", noWxIntensity, mask & tBelow32 & visBelowFiveEighths)
                changeMask = changeMask | (mask & tBelow32 & visBelowFiveEighths)

                # Check for dense fog conditions
                newWxGrid, newWxKeys = self.addNewWx(newWxGrid, newWxKeys, cov, "F", "+", mask & tAbove32 & visBelowPoint25)
                changeMask = changeMask | (mask & tAbove32 & visBelowPoint25)
                
                # Check for freezing Dense Fog conditions
                newWxGrid, newWxKeys = self.addNewWx(newWxGrid, newWxKeys, cov, "ZF", "+", mask & tBelow32 & visBelowPoint25)
                changeMask = changeMask | (mask & tBelow32 & visBelowPoint25)
                
            if self._config["MakePotFog"] == "Yes":
                # Make the PoTFog grid
                self.makePoTGridFromVis(Visibility, T, Td, newWxType, TMinusTd, tr)
                
        # Add Haze to Wx
        elif newWxType == "Haze":  # Haze has only one coverage - "Def"
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(newWxType, cov)                
                mask = (Visibility >= covMin) & (Visibility < covMax) & self._editAreaMask
                newWxGrid, newWxKeys = self.addNewWx(wxGrid, wxKeys, cov, "H", noWxIntensity, mask)
                changeMask = changeMask | mask

            if self._config["MakePotFog"] == "Yes":
                # Make the PoTFog grid
                self.makePoTGridFromVis(Visibility, T, Td, newWxType, TMinusTd, tr)
            
        # Add Smoke to Wx
        elif newWxType == "Smoke":
            for cov in covList:
                covMin, covMax = self.calcCoverageLimits(newWxType, cov)                
                mask = (Visibility >= covMin) & (Visibility < covMax) & self._editAreaMask
                newWxGrid, newWxKeys = self.addNewWx(wxGrid, wxKeys, cov, "K", noWxIntensity, mask)
                changeMask = changeMask | mask
                
            if self._config["MakePotFog"] == "Yes":
                # Make the PoTFog grid
                self.makePoTGridFromVis(Visibility, T, Td, newWxType, TMinusTd, tr)
        
        return (newWxGrid, newWxKeys), changeMask

    # Modifies the Visibility base on the Wx coverage and intensity.
    def updateVisFromWx(self, Wx, Visibility):
        changeMask = self.empty(np.bool)
        
        # Set Vis to 7SM where <7 and NoWx
        wxMask = self.getWxMask(Wx, "<NoWx>", "<NoInten>") 
        visMask = (Visibility < 7.0) & self._editAreaMask
        Visibility[wxMask & visMask] = 7.0
        changeMask = changeMask | (wxMask & visMask)

        # Set Vis to 6 if regular F
        wxMask = self.getWxMask(Wx, "F", "<NoInten>")  
        visMask = (Visibility > 6.0) & self._editAreaMask
        Visibility[wxMask & visMask] = 6.0
        changeMask = changeMask | (wxMask & visMask)
        
        # Set Vis to 0.25 if F+
        wxMask = self.getWxMask(Wx, "F", "+") 
        visMask = (Visibility > 0.25) & self._editAreaMask
        Visibility[wxMask & visMask] = 0.25
        changeMask = changeMask | (wxMask & visMask)
        
        # Set Vis based on Snow intensity        
        wxMask = self.getWxMask(Wx, "S", "m")
        visMask = (Visibility > 0.5) & self._editAreaMask
        Visibility[wxMask & visMask] = 0.5
        changeMask = changeMask | (wxMask & visMask)
        
        wxMask = self.getWxMask(Wx, "S", "+")
        visMask = (Visibility > 0.25) & self._editAreaMask
        Visibility[wxMask & visMask] = 0.25
        changeMask = changeMask | (wxMask & visMask)
        
        # Make masks and set the Vis to 6.0 for random wxTypes. Add more to this list
        # in order to have them considered.
        wxTypes = ["K"]  # make this a list for now in case new type need to be added later
        for wxType in wxTypes:
            wxMask = self.getWxMask(Wx, wxType, "<NoInten>")
            visMask = (Visibility > 6.0) & self._editAreaMask
            Visibility[wxMask & visMask] = 6.0
            changeMask = changeMask | (wxMask & visMask)
            
        return Visibility, changeMask
    
    # Simple method to parse a Wx string
    def parseCovTypeIntens(self, wxSubKey):
        keyParts = wxSubKey.split(":")
        # Just return the first three coverage, type, and intensity
        return keyParts[0:3]
    
    # Returns the list of weather types in the proper order based on configuration.
    def getWxTypeOrder(self, wxKey):
        
        # Get the choice from the GUI
        wxCat = self._intVar.get()

        if wxCat == "Precip":
            wxTypeList = self._config["IntensPrecipList"] + \
                         self._config["IntensNonPrecipList"]
        elif wxCat == "Non-Precip":
            wxTypeList = self._config["IntensNonPrecipList"] + \
                         self._config["IntensPrecipList"]
        else:
            print("ERROR! Invalid Intensity choice from GUI!!!!!!")
            
        return wxTypeList
    
    # Determines which wx type to change based on configuration.
    def determineWxTypeToAdjust(self, wxKey):
        
        # Get the order of wx types based on GUI choices and configuration
        wxTypeList = self.getWxTypeOrder(wxKey)
                
        # Now find this wx type in the wxKey
        subKeys = wxKey.split("^")
        for wxType in wxTypeList:
            for subKey in subKeys:
                subKeyWxCov, subKeyWxType, subKeyWxInt = self.parseCovTypeIntens(subKey)
                if wxType == subKeyWxType:
                    return wxType
        
        # If we didn't find a matching wx type, return None
        return None
    
    # Returns True if the weather types match. Snow and snow showers are treated
    # as a match as are rain and rain showers.
    def wxTypeMatches(self, wxType1, wxType2):
        
        if wxType1 == wxType2:
            return True
        elif wxType1 in ["S", "SW"] and wxType2 in ["S", "SW"]:
            return True
        elif wxType1 in ["R", "RW"] and wxType2 in ["R", "RW"]:
            return True
    
        return False

    # Returns the wx coverage, type, and intensity if the type matches wxType.
    # Otherwise returns None
    def wxCovTypeIntensMatchType(self, wxKey, wxType):
        
        parts = wxKey.split("^")
        for p in parts:
            coverage, wxKeyType, intensity = self.parseCovTypeIntens(wxKey)
            
            if self.wxTypeMatches(wxKeyType, wxType):    
                return coverage, wxKeyType, intensity
            
        return None
    
    # Updates the Wx Intensity grid based on visibility.
    def updateWxIntensWithVis(self, Visibility, Wx):
        changeMask = self.empty(np.bool)

        # The intensities of these Wx types only are allowed to be updated
        wxTypes = ["S", "SW", "L", "ZL", "F", "ZF"]
        # Get the area masks based on Vis
        visHeavy = Visibility <= 0.25
        visMod = (Visibility > 0.25) & (Visibility <= 0.5)
        visLight = Visibility > 0.5
        # Make a list of the intensity to add and mask that goes with those conditions
        maskInfo = [("m", visMod), ("+", visHeavy), ("-", visLight)]
        
        wxGrid, wxKeys = Wx
        
        # Make a copy of the keys so they won't get modified
        # If we don't do this, an infinite loop occurs.
        initWxKeys = copy.copy(wxKeys)

        for wxKey in initWxKeys:
            
            if wxKey == self._noWxStr:
                continue
            
            # Adjust the intensity of a Wx type determined by the configuration
            # and the choice on the GUI. Wx type adjusted depends on the other
            # Wx sub keys.
            targetWxType = self.determineWxTypeToAdjust(wxKey)
                        
            subKeys = wxKey.split("^")
            
            # Examine each subKey
            for subKey in subKeys:
                subCov, subType, subIntens = self.parseCovTypeIntens(subKey)
                
                # If it's not one of the special types, skip it
                if subType not in wxTypes:
                    continue
                # If it's not the type we're looking to adjust, skip it
                if subType != targetWxType: 
                    continue
                 
                # Now check each intensity to see if any changes are needed
                for intens, visMask in maskInfo:
                    intenToAdd = intens
                    # If no points in the mask skip it.
                    if not visMask.any():
                        continue
                    # Fog intensity behaves differently 
                    if targetWxType in ["F", "ZF"] and intens in ["-", "m"]:
                        intenToAdd = "<NoInten>"
                     
                    if intens == subIntens:  # this subKey already has the right intensity
                        continue
                     
                    # At this point, we have the type and intensity that we're looking for
                    # so add/change to the new intensity
                    oldIndex = self.getIndex(wxKey, wxKeys)
                    mask = (wxGrid == oldIndex) & visMask & self._editAreaMask
                     
                    # Skip if no points in the mask
                    if not mask.any():
                        continue
                    
                    wxGrid, wxKeys = self.addNewWx(wxGrid, wxKeys, subCov, subType, intenToAdd, mask)
                    changeMask = changeMask | mask
                
        return (wxGrid, wxKeys), changeMask

    # Sets the WindGust to the WindSpeed value where gust < speed
    def updateWindGustWithWind(self, Wind, WindGust):
        changeMask = self.empty(np.bool)

        speed, direction = Wind
        mask = (speed > WindGust) & self._editAreaMask
        WindGust[mask] = speed[mask]
        changeMask = changeMask | mask
        
        return WindGust, changeMask
    
    # Adds the specified Vis to the specified Wx key.
    def addVisToWxKey(self, oldKey, visStr):
        # Don't add Vis to <NoWx>
        if oldKey == self._noWxStr:
            return oldKey
        
        subKeys = oldKey.split("^")
        newKey = ""
        
        for i in range(0, len(subKeys)):
            
            # if it's the first subKey, set the new Vis, otherwise set to <NoVis>
            if i == 0:
                newVis = visStr
            else:
                newVis = "<NoVis>"
            
            # Split the subKey into components and reassemble
            subParts = subKeys[i].split(":")

            if len(subParts) >= 3:
                newSubKey = subParts[0] + ":" + subParts[1] + ":" + subParts[2] + ":" + newVis + ":"
            
            if len(subParts) == 5 and subParts[4] != "":
                newSubKey = newSubKey + subParts[4] + ":"
                                   
            # Append to the new subKey
            newKey = newKey + newSubKey + "^"
            
         # strip off the last "^"
        if newKey[-1] == "^":
            newKey = newKey[0:-1] 

        return newKey
    
    # Calculates the Ceiling grid based on the specified CBP and Sky grids.
    def makeCeiling(self, CloudBasePrimary, Sky):

        
        # Initialize the Ceiling grid to -30000
        ceiling = np.zeros(CloudBasePrimary.shape, np.float32) + -30000.0

        if CloudBasePrimary is None or Sky is None:
            return ceiling
        
        # Find points above the sky threshold
        mask = (Sky > self._ceilingSkyThreshold)
        
        # Assign the CPB value at these points
        ceiling[mask] = CloudBasePrimary[mask] * 100  # convert to feet from 100s of feet

        changeMask = np.ones(ceiling.shape, np.bool) # Always reset all the Ceiling values
        
        return ceiling, changeMask
    
    # Removes all Visibility components from the Wx ugly string
    def removeVis(self, wxGrid):
        
        byteGrid, wxKeys = wxGrid
        
        for i in range(len(wxKeys)):
            
            subKeys = wxKeys[i].split("^")
            
            newWxKey = ""
            for subKey in subKeys:
                wxParts = subKey.split(":")
                
                if len(wxParts) >= 4:
                    visStr = wxParts[3]
                    if visStr not in ["<NoVis>", ""]:
                        newSubKey = wxParts[0] + ":" + wxParts[1] + ":" + wxParts[2] + ":" + \
                                    "<NoVis>" + ":"
                        if len(wxParts) == 5 and wxParts[4] != "":
                            newSubKey = newSubKey + wxParts[4] 
                    else:
                        newSubKey = subKey
                                                                        
                    newWxKey = newWxKey + newSubKey + "^"
                else:
                    newWxKey = newWxKey + subKey + "^"
            
            if newWxKey[-1] == "^":
                newWxKey = newWxKey[0:-1]
    
            wxKeys[i] = newWxKey
            
        return byteGrid, wxKeys

    
    # Returns a new Wx grid that contains the corresponding Visibility string
    # based on the Visibility at that same point for all values less than
    # the specified visThreshold.
    def setWxVis(self, Wx, Visibility, visThreshold):
        changeMask = self.empty(np.bool)
        
        # Define the ranges for each visibility category
        # Note that the first value in the range is inclusive and the last
        # value is exclusive, thus the last value matches the next first value.
        visDict = {
                   "0SM" :      (0.0, 0.125),
                   "1/4SM" :    (0.125, 0.375), 
                   "1/2SM" :    (0.375, 0.675), 
                   "3/4SM" :    (0.675, 0.875), 
                   "1SM" :      (0.875, 1.25), 
                   "11/2SM" :   (1.25, 1.75),
                   "2SM" :      (1.75, 2.25),
                   "21/2SM" :   (2.25, 2.75),
                   "3SM" :      (2.75, 3.50),
                   "4SM" :      (3.50, 4.50),
                   "5SM" :      (4.50, 5.50),
                   "6SM" :      (5.50, 6.50),
                   "P6SM" :     (6.50, 99.00),
                   }
        
        # For each Vis category modify the Wx grid and save it.
        Wx = self.removeVis(Wx)
        
        wxGrid, wxKeys = Wx  # Decompose the weather grid
        for visStr in visDict:
            visMin, visMax = visDict[visStr]
            # make a mask for the point we plan to modify
            visMask = (Visibility >= visMin) & (Visibility < visMax) & (Visibility <= visThreshold) & self._editAreaMask
            if sum(sum(visMask)) == 0:  # don't bother with empty masks
                continue
               
            for wxKey in wxKeys:
                wxIndex = self.getIndex(wxKey, wxKeys)
                wxMask = wxGrid == wxIndex
                
                overlap = wxMask & visMask
                   
                if sum(sum(overlap)) == 0:  # don't bother with empty masks
                    continue
                   
                newKey = self.addVisToWxKey(wxKey, visStr)
                newWxIndex = self.getIndex(newKey, wxKeys)
                   
                wxGrid[overlap] = newWxIndex
                changeMask = changeMask | overlap
        
        return (wxGrid, wxKeys), changeMask
    
    # Check to determine if the grid forecast has no gaps
    def inventoryIsComplete(self, weList, timeRange):
        for weName in weList:
            # Skip Ceiling since the tool will derive that element
            if weName in self._notRequiredWEs:
                continue
             
            trList = self.getWEInventory("Fcst", weName, timeRange)
            # Check for any gaps for this WE
            if len(trList) == 0:
                #self.statusBarMsg("Some " + weName + " grids missing. Inventory check failed.", "S")
                return False
             
            # Check for the first grid. Start time must be at or before the timeRange
            if trList[0].startTime() > timeRange.startTime():
                #self.statusBarMsg("Some " + weName + " grids missing. Inventory check failed.", "S")
                return False    
             
            endTime = trList[0].endTime()
            for i in range(1, len(trList)):
                if trList[i].startTime() != endTime:
                    #self.statusBarMsg("Some " + weName + " grids missing. Inventory check failed.", "S")
                    return False
                endTime = trList[i].endTime()
 
            # Check for the last grid
            if trList[-1].endTime().unixTime() < timeRange.endTime().unixTime():
                #self.statusBarMsg("Some " + weName + " grids missing. Inventory check failed.", "S")
                return False
             
             
        return True
    
    # Returns true if the tool is configured to offer the Save and Publich option.
    def saveAndPublish(self):
        if self._config["OfferSavePublish"] != "Yes":
            return False
        if self._savePublishVar.get() == "Yes":
            return True
        
        return False
    
    # Check to ensure that all the data specified are available
    def allDataAvailable(self, weDict, weList, tr):
        for we in weList:
            if we in weDict:
                if tr not in weDict[we]:
                    return False
        
        return True
    
    # Reads the Ceiling and visibility grids and copies them into the CigHgt and Vsby
    # grids, respectively.
    def copyCeilingVisToCigHgtVsby(self):
        
        weTuples = [("Ceiling", "CigHgt"), ("Visibility", "Vsby")]
        
        
        cigMin, cigMax = self.getParmMinMaxLimits("Fcst", "CigHgt")
        
        for sourceWE, targetWE in weTuples:
            
            minValue, maxValue = self.getParmMinMaxLimits("Fcst", targetWE)
            
            trList = self.getWEInventory("Fcst", sourceWE)
            for tr in trList:
                sourceGrid = self.getGrids("Fcst", sourceWE, "SFC", tr)
           
                # clip the grid in case the target has different min, max allowed values
                sourceGrid = sourceGrid.clip(minValue, maxValue)
                
                self.createGrid("Fcst", targetWE, "SCALAR", sourceGrid, tr)
        
        return
        
    # Main method that calls all of the selected rules.
    def applyFinalizeRules(self, editArea):
        selectedTR = TimeRange.TimeRange(AbsTime.AbsTime(self._editStartTime),
                                         AbsTime.AbsTime(self._editEndTime))

        # Get the GUI variables
        wxToAdd = self._selectedWxType.get()

        coverageDict = {}
        covList = self._wxTypeCovDict[wxToAdd]
        for cov in covList:
            coverageDict[cov] = self._scales[cov].get()
            
        TMinusTd = self._scales[self._TMinusTdScaleName].get()
        self._addVisBelow = self._addVisToWxScale.get()
        
        trList = self.getTimeRanges(selectedTR)
        # Make a mask over the edit are and restrict changes to that
        self._editAreaMask = self.encodeEditArea(editArea)
        if np.sum(self._editAreaMask) == 0:
            self._editAreaMask = np.ones(self.getGridShape(), np.bool)
        
        # Check the inventory for gaps and Stop or interpolate based on config setting
        inventoryComplete = self.inventoryIsComplete(self._weList, selectedTR)
        if not inventoryComplete:
            if self._config["GapAction"] == "Stop":
                self.statusBarMsg("Fcst gaps found but configured GapAction is set to Stop. Stopping Aviation_Finalize", "S")
                return
            
        if self._config["GapAction"] == "Interpolate":
            # Make a list of WEs. Start with the total set and remove those not required.
            weList = []
            for we in self._weList:
                if we not in self._notRequiredWEs:
                    weList.append(we)
            self.interpolateCmd(weList, selectedTR, "GAPS", "SYNC", interval=1, duration=1)

        # Fetch all the grids for the defined elements over the selected timeRange
        weDict = self.fetchAllGrids(self._weList, selectedTR)
        
        # Remove Pot grids if configured
        if self._config["MakePotFog"] == "Yes" and "Vis Adds New Wx" in self._userSelectedRules:
            self.deleteCmd(["PotFog", "PotFreezingFog", "PotHaze", "PotSmoke"], selectedTR)

        # Run the rules
        for tr in trList:
            if "PoP Adjusts Cloud Base" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["PoP", "CloudBasePrimary"], tr):
                    weDict["CloudBasePrimary"][tr] = self.popAssignsCBP(weDict["PoP"][tr][0], weDict["CloudBasePrimary"][tr][0],
                                                                 weDict["Sky"][tr][0], weDict["Visibility"][tr][0])
                  
            if "PoP Defines Sky" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["PoP", "Sky"], tr):
                    weDict["Sky"][tr] = self.popAssignsSky(weDict["PoP"][tr][0], weDict["Sky"][tr][0])
                
            if "Vis Adds New Wx" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["Visibility", "T", "Td", "Wx"], tr):
                    grid, mask = self.addNewWxFromVis(weDict["Visibility"][tr][0], weDict["T"][tr][0], weDict["Td"][tr][0],
                                                            weDict["Wx"][tr][0], wxToAdd, TMinusTd, tr)
                    finalMask = weDict["Wx"][tr][1] | mask  # Accumulate the points
                    weDict["Wx"][tr] = grid, finalMask 

            if "Wx Adjusts Vis" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["Wx", "Visibility"], tr):
                    grid, mask = self.updateVisFromWx(weDict["Wx"][tr][0], weDict["Visibility"][tr][0])                
                    finalMask = weDict["Visibility"][tr][1] | mask  # Accumulate the points
                    weDict["Visibility"][tr] = grid, finalMask 
                        
            if "QC Vis with PoP & Wx" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["PoP", "Wx", "Visibility"], tr):
                    grid, mask = self.lowPoPNoWxVis10(weDict["PoP"][tr][0], weDict["Wx"][tr][0], weDict["Visibility"][tr][0])
                    finalMask = weDict["Visibility"][tr][1] | mask  # Accumulate the points
                    weDict["Visibility"][tr] = grid, finalMask 
    
            if "Update Wx Intensity Using Vis" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["Visibility", "Wx"], tr):
                    grid, mask = self.updateWxIntensWithVis(weDict["Visibility"][tr][0], weDict["Wx"][tr][0])
                    finalMask = weDict["Wx"][tr][1] | mask  # Accumulate the points
                    weDict["Wx"][tr] = grid, finalMask 

            if "Adjust WindGust Using Wind" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["Wind", "WindGust"], tr):
                    weDict["WindGust"][tr] = self.updateWindGustWithWind(weDict["Wind"][tr][0], weDict["WindGust"][tr][0])

            if "Add Vis to Wx String" in self._userSelectedRules:
                if self.allDataAvailable(weDict, ["Wx", "Visibility"], tr):
                    grid, mask = self.setWxVis(weDict["Wx"][tr][0], weDict["Visibility"][tr][0], self._addVisBelow)
                    finalMask = weDict["Wx"][tr][1] | mask  # Accumulate the points
                    weDict["Wx"][tr] = grid, finalMask 
            
            # Ensure there are no zero ceiling values if Sky is not OVC and Vis > 0.5 miles
            # Note this is not an optional rule like the others.
            if self.allDataAvailable(weDict, ["CloudBasePrimary", "Sky", "Visibility"], tr):
                grid, mask = self.noZeroCeiling(weDict["CloudBasePrimary"][tr][0], weDict["Sky"][tr][0],
                                                weDict["Visibility"][tr][0])
                finalMask = weDict["CloudBasePrimary"][tr][1] | mask  # Accumulate the points
                weDict["CloudBasePrimary"][tr] = grid, finalMask

            if self.allDataAvailable(weDict, ["CloudBasePrimary", "Sky"], tr):
                weDict["Ceiling"][tr] = self.makeCeiling(weDict["CloudBasePrimary"][tr][0],
                                                         weDict["Sky"][tr][0])                   

            for weName in self._weList:
                # No need to update every element
                if weName in self._immutableWEs:
                    continue
                
                weType = self.getWEType(weName)
                if weType is None:
                    continue
                # Fetch the grid and mask
                if tr in weDict[weName]:
                    grid, mask = weDict[weName][tr]
                    if mask.sum() == 0:  # don't modify grids that don't need it
                        continue
                    else:  # save the grid
                        self.createGrid("Fcst", weName, weType, grid, tr)
                        
        # Copy the Celing and Visibility grids to the legact elements.
        self.copyCeilingVisToCigHgtVsby()
        
        # Save and Publish these elements
        if self.saveAndPublish():
            #print "Saving and Publishing Elements:", self._publishWEList
            self.saveElements(self._publishWEList)
            self.publishElements(self._publishWEList, selectedTR)
        # Save and publish the PoWt grids if configured
        if self._config["MakePotFog"] == "Yes":
            #print "Saving and Publishing PoT Elements:", self._potElements
            self.saveElements(self._potElements)
            self.publishElements(self._potElements, selectedTR)

                 
        return
                
    # GUI code  ################################################################################################
    
    # Called when the cancel button is clicked
    def cancelCommand(self):
        self._tkmaster.destroy()
        return

    # Called when the Run button is clicked
    def runCommand(self):
        self.applyFinalizeRules(self._editArea)
        return

    # Called when the Run and Dismiss button is clicked
    def runDismissCommand(self):
        self.applyFinalizeRules(self._editArea)
        self.cancelCommand()
        return

    # Create the Execute and Cancel buttons
    def makeBottomButtons(self, frame, row):

        # Make the frame
        self._bottomButtonFrame = tk.Frame(frame, bg=self._bgColor)
        self._bottomButtonFrame.grid(row=row, column = 0, columnspan=6, pady=20)

        # Run button
        self._runButton = tk.Button(self._bottomButtonFrame, text="Run",
                                            command=self.runCommand)
        self._runButton.grid(row=0, column=0, padx=20)

        # Run/Dismiss buttom
        self._runDismissButton = tk.Button(self._bottomButtonFrame, text="Run/Dismiss",
                                            command=self.runDismissCommand)
        self._runDismissButton.grid(row=0, column=1, padx=20)
        # Cancel button
        self._cancelButton = tk.Button(self._bottomButtonFrame, text="Cancel",
                                            command=self.cancelCommand)
        self._cancelButton.grid(row=0, column=2, padx=20)

        return

    # Called when the cursor enter the GUI. Just fetches the current timeRange
    def enterEvent(self, event=None):        
        return
    
    # Toggles on the recently selected rule button
    def ruleSelected(self, event, ruleName):
        
        # toggle on
        if ruleName not in self._userSelectedRules and \
            self._ruleDict[ruleName].cget("state") == tk.ACTIVE:
            self._userSelectedRules.append(ruleName)
            self._ruleDict[ruleName].select()
            if ruleName == "Vis Adds New Wx":
                self.displayAddWxFrame()
            elif ruleName == "Add Vis to Wx String":
                self.displayAddVisToWxFrame()
            elif ruleName == "Update Wx Intensity Using Vis":
                self.displayIntensityFrame()
        elif ruleName in self._userSelectedRules and \
            self._ruleDict[ruleName].cget("state") == tk.ACTIVE:
            if ruleName in self._userSelectedRules:
                self._userSelectedRules.remove(ruleName)
                self._ruleDict[ruleName].deselect()
            if ruleName == "Vis Adds New Wx":
                self.removeAddWxFrame()
            elif ruleName == "Add Vis to Wx String":
                self.removeAddVisToWxFrame()
            elif ruleName == "Update Wx Intensity Using Vis":
                self.removeIntensityFrame()

        return
    
    # Makes the rule buttons on the GUI
    def makeRuleCheckButtons(self, frame):
        
        # Make the element frame for selecting the source
        ruleFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        ruleFrame.grid(row=0, column=0, sticky=tk.N+tk.E+tk.S+tk.W)
        
        # make a label
        label = tk.Label(ruleFrame, text="Rules To Apply:")
        label.grid(row=0, column=0)
        
        self._ruleList = self._config["AllChecks"]
        
        # Make each source button
        row = 1
        for i in range(len(self._ruleList)):
            ruleName = self._ruleList[i]
            def cbHandler(event, self=self, buttonName=ruleName):
                return self.ruleSelected(event, buttonName)
            self._ruleDict[ruleName] = tk.Checkbutton(ruleFrame, text=ruleName)
            
            self._ruleDict[ruleName].grid(row=row, sticky=tk.W, pady=10)
            self._ruleDict[ruleName].bind("<ButtonRelease-1>", cbHandler)
            self._ruleDict[ruleName].bind("<ButtonRelease-2>", cbHandler)
            if ruleName in self._defaultOnRules:
                self._ruleDict[ruleName].select()
            
            if ruleName in self._immutableRules:
                self._ruleDict[ruleName].config(state=tk.DISABLED)
                
            button_ttp = CreateToolTip(self._ruleDict[ruleName],
                                       self._config["toolTips"][ruleName])

            row = row + 1
        
        if "Vis Adds New Wx" not in self._defaultOnRules:
            self.removeAddWxFrame()
        if "Add Vis to Wx String" not in self._defaultOnRules:
            self.removeAddVisToWxFrame()
        if "Update Wx Intensity Using Vis" not in self._defaultOnRules:
            self.removeIntensityFrame()
            
    # Called when the Wx type button is selected
    def wxTypeSelected(self):
        if self._prevWxType is not None:
            self.removeCovSliders(self._prevWxType)
            
        self.makeCovSliders(self._selectedWxType.get())
            
        self._prevWxType = self._selectedWxType.get()
        
        return
    
    # Called when any slider is moved
    def sliderChanged(self, sliderValue):

        wxType = self._selectedWxType.get()
        covList = self._wxTypeCovDict[self._selectedWxType.get()]
        for cov in covList:            
            # Update the label
            label = cov + " " + wxType + " where Vis < " + "%.2f" % self._scales[cov].get()
            if cov == covList[0]: # last item
                label = cov + " " + wxType + " where Vis < " + "%.2f" % self._scales[cov].get()
                
            self._scales[cov].config(label=label)

        # Update the T-Td Slider label
        if wxType == "Fog":
            #scale = self._scales[self._TMinusTdScaleName]
            label = "Fog where T - Td <= " + str(self._scales[self._TMinusTdScaleName].get())
            self._scales[self._TMinusTdScaleName].config(label=label)
            self._scales[self._TMinusTdScaleName].grid_propagate(0)
        
        # Adjust the other sliders so they stay ordered
        activeSliderValue = float(sliderValue)
        activeIndex = 0
        for cov in covList:
            if self._scales[cov].get() == activeSliderValue:
                activeIndex = covList.index(cov)
             
        for i, cov in enumerate(covList):
            if i == activeIndex:
                continue
            if i > activeIndex and self._scales[cov].get() > activeSliderValue:
                self._scales[cov].set(activeSliderValue + 0.01)
            elif i < activeIndex and self._scales[cov].get() < activeSliderValue:
                self._scales[cov].set(activeSliderValue - 0.01)
        return
    
    # Removes Coverage sliders
    def removeCovSliders(self, wxType):
        
        self._sliderFrame.destroy()
        
        return
    # Makes the Coverage sliders for the GUI
    def makeCovSliders(self, wxType):
        
        covList = self._wxTypeCovDict[wxType]
        self._scaleVar = {}
        row = self._startCovRow
        
        self._sliderFrame = tk.Frame(self._addWxFrame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        self._sliderFrame.grid(row=self._startCovRow, column=0, sticky=tk.N+tk.E+tk.S+tk.W)
    
        # Add the Dew point depression scale
        if wxType == "Fog":
            self._TMinusTdScaleName = "TMinusTd"
            self._scaleVar[self._TMinusTdScaleName] = tk.DoubleVar()
            self._scaleVar[self._TMinusTdScaleName].set(self._config["T-TdDefault"])
            self._scales[self._TMinusTdScaleName] = tk.Scale(self._sliderFrame, from_= 0, to=10,
                                                orient=tk.HORIZONTAL,
                                                command=self.sliderChanged,
                                                resolution=1, length=200,
                                                sliderlength=20,
                                                variable=self._scaleVar[self._TMinusTdScaleName])
            self._scales[self._TMinusTdScaleName].grid(row=row, column=1, sticky=tk.E+tk.W)
            self._scales[self._TMinusTdScaleName].grid_propagate(0)
            self._scales[self._TMinusTdScaleName].name = self._TMinusTdScaleName

            row = row + 1
            
        for cov in covList:
            self._scaleVar[cov] = tk.DoubleVar()
            covKey = wxType + "_" + cov
            defaultValue = self._config[covKey]
            self._scaleVar[cov].set(defaultValue)

            self._scales[cov] = tk.Scale(self._sliderFrame, from_= 0, to=6,
                                         orient=tk.HORIZONTAL,
                                         command=self.sliderChanged,
                                         resolution=0.01, length=200,
                                         sliderlength=20,
                                         variable=self._scaleVar[cov])
            self._scales[cov].grid(row=row, column=1, sticky=tk.E+tk.W)
            self._scales[cov].grid_propagate(0)
            self._scales[cov].name = cov
            row = row + 1
            
        return

    # Method that draws the widgets for adding new Wx
    def makeAddNewWxWidgets(self, frame):
        
        self._addWxFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3, bg=self._bgColor)
        self._addWxFrame.grid(row=0, column=1, sticky=tk.N+tk.E+tk.S+tk.W, columnspan=2)    
        
        # Make the Wx type buttons
        label = tk.Label(self._addWxFrame, text="Add Wx Type", font=self._activeFont)
        label.grid(row=1, column=0)
        # Make each source button
        row = 2
        
        wxTypeList = sorted(self._wxTypeCovDict.keys())
        self._selectedWxType = tk.StringVar()
        self._selectedWxType.set(self._defaultWxType)
        self._prevWxType = None
        for i in range(len(wxTypeList)):
            name = wxTypeList[i]            
            self._wxTypeDict[name] = tk.Radiobutton(self._addWxFrame, text=wxTypeList[i], padx=10, pady=2,
                                                    command=self.wxTypeSelected, variable=self._selectedWxType,
                                                    value = wxTypeList[i])
            self._wxTypeDict[name].grid(row=row, sticky=tk.W)               

            row = row + 1
        
        self._startCovRow = row
        self.makeCovSliders(self._defaultWxType)
        
        return

    # These methods draw or remove their respective frames
    def removeAddWxFrame(self):
        self._addWxFrame.grid_remove()
        return
    
    def displayAddWxFrame(self):
        self._addWxFrame.grid(row=0, column=1, sticky=tk.N+tk.E+tk.S+tk.W)
        return
    
    def removeAddVisToWxFrame(self):
        self._addVisToWxFrame.grid_remove()
        return
    
    def displayAddVisToWxFrame(self):
        self._addVisToWxFrame.grid(row=self._addVisToWxRow, column=0, sticky=tk.N+tk.E+tk.S+tk.W)
        return
    
    def removeIntensityFrame(self):
        self._intFrame.grid_remove()
        return
    
    def displayIntensityFrame(self):
        self._intFrame.grid(row=self._addVisToWxRow, column=1, sticky=tk.N+tk.E+tk.S+tk.W)
        return
    
    ##########################################################################################
    # Time Scale methods 
    
    # Define the coordinates for the time scale objects
    def defineGeometry(self):

        
        self._xSize = 500  # width of each canvas
        #self._ySize = 150  # height of each canvas
        self._xOffset = 10 # horizontal room for y-scale on left
       
        # Time Scale geometry
        self._markerTopY = 10
        self._markerBottomY = 25
        self._markerWidth = 14
        self._markerHeight = 14
        self._adjStartMarker = False
        self._adjEndMarker = False
        self._timeCanvasHeight = 40
        self._tickHeight = 7
        self._timeLabelOffset = 7
        self._labelFreq = 6 # hours
        self._deltaTime = self._endTime - self._startTime
        self._pixPerHour = (self._xSize - self._xOffset - 20) // (self._deltaTime // 3600)
        
        return 
    
    # Converts a time in seconds to the corresponding x-coordinate
    def timeToX(self, timeInSec):

        timeOffset = float(timeInSec - self._startTime) / 3600

        #xCoord = self._xOffset + (timeOffset * self._pixPerHour) + 1
        xCoord = self._xOffset + (timeOffset * self._pixPerHour) + 3

        return xCoord
    
    # Converts the x-coordinate to  time in seconds
    def xToTime(self, xCoord):

        coordTime = self._startTime + (((xCoord - self._xOffset - 3) * 3600) // self._pixPerHour)
        
        return coordTime
    
    # Returns the coordinate of the marker associated with the specifiedTime
    def markerCoords(self, markerTime):
        
        startX = self.timeToX(markerTime)

        x1 = startX - self._markerWidth // 2
        y1 = self._markerBottomY + self._markerHeight
        x2 = startX + self._markerWidth // 2
        y2 = self._markerBottomY + self._markerHeight
        x3 = startX
        y3 = self._markerBottomY
        
        return x1, y1, x2, y2, x3, y3
        
    # Draws the time marker at the specified time
    def drawTimeMarker(self, markerTime):

        startX = self.timeToX(markerTime)

        if markerTime == self._editStartTime:
            color = "green"
        elif markerTime == self._editEndTime:
            color = "red"
        
        self._timeCanvas.create_line(startX, self._markerTopY,
                                     startX, self._markerBottomY, fill=color)

        x1, y1, x2, y2, x3, y3 = self.markerCoords(markerTime)

        self._timeCanvas.create_polygon(x1, y1, x2, y2, x3, y3, fill=color,
                                        outline="black")

        return
    
    # Called when any time marker is pressed
    def markerPress(self, event):
        # Figure out if the start marker was pressed
        self._pressTime = self.xToTime(event.x)

        closeEnough = 600
        if abs(self._editStartTime - self._pressTime) < closeEnough:
            self._adjStartMarker = True
        elif abs(self._editEndTime - self._pressTime) < closeEnough:
            self._adjEndMarker = True
        return

    # Called when any time marker is moved or draged
    def markerMotion(self, event):
        markerTime = self.xToTime(event.x)
        # Round time to nearest hour
        markerTime = int((markerTime + 1800) / 3600) * 3600

        if self._adjStartMarker:
            if markerTime != self._editStartTime:
                if markerTime < self._startTime:
                    markerTime = self._startTime
                # Can't go past end time    
                if markerTime > self._editEndTime - 3600:
                    markerTime = self._editEndTime - 3600
                else:
                    self._editStartTime = markerTime

                self.makeTimeScale()
                return
        elif self._adjEndMarker:
            if markerTime != self._editEndTime:
                if markerTime > self._endTime:
                    markerTime = self._endTime
                # Can't go past start time
                if markerTime < self._editStartTime + 3600:
                    markerTime = self._editStartTime + 3600
                else:
                    self._editEndTime = markerTime

                self.makeTimeScale()
                return

        return

    # Called when any time marker is released
    def markerRelease(self, event):

        # If it's a click snap the closest time marker
        releaseTime = self.xToTime(event.x)

        # If it's a click, snap the closest time marker
        if releaseTime == self._pressTime:
            startDiff = abs(self._editStartTime - releaseTime)
            endDiff = abs(self._editEndTime - releaseTime)
            if releaseTime < self._editStartTime or startDiff < endDiff:
                self._adjStartMarker = True
                self.markerMotion(event)
            elif releaseTime > self._editEndTime or startDiff >= endDiff:
                self._adjEndMarker = True
                self.markerMotion(event)

        self._adjStartMarker = False
        self._adjEndMarker = False

        return

    # Creates the timeScale GUI
    def makeTimeScale(self):
        
        wList = self._timeFrame.winfo_children()
        for w in wList:
            w.destroy()
        
        hours = int((self._editEndTime - self._editStartTime) / 3600)
        if hours > 1:
            labelText = "Select Time Period -----  " + str(hours) + " hours selected"
        else:
            labelText = "Select Time Period -----  " + str(hours) + " hour selected"

        label = tk.Label(self._timeFrame, text=labelText)
        label.grid(row=0, column=0)
        

        self._timeCanvas = tk.Canvas(self._timeFrame,width=self._xSize,
                                          height=self._timeCanvasHeight,
                                          bg='gray')
        self._timeCanvas.grid(row=1)

        self._timeCanvas.bind("<ButtonPress-" + self._mouseButton + ">", self.markerPress)
        self._timeCanvas.bind("<ButtonRelease-" + self._mouseButton + ">", self.markerRelease)
        #self._timeCanvas.bind("<Leave>", self.markerRelease)
        self._timeCanvas.bind("<Motion>", self.markerMotion)          

        # Make a horizontal line
        x0 = self.timeToX(self._startTime)
        x1 = self.timeToX(self._endTime)
        self._timeCanvas.create_line(x0, 1, x1, 1, fill='black')
           
        self.drawTimeMarker(self._editStartTime)
        self.drawTimeMarker(self._editEndTime)
        for t in range(self._startTime, self._endTime + 1, 3600):                
            tupleTime = time.gmtime(t)
            dayStr = str(tupleTime.tm_mday).zfill(2)
            hourStr = str(tupleTime.tm_hour).zfill(2)
            timeStr = dayStr + "." + hourStr
            x0 = self.timeToX(t)
            y0 = 0
            tickHeight = self._tickHeight
            if tupleTime.tm_hour % self._labelFreq == 0:
                tickHeight = tickHeight * 2 

            y1 = y0 + tickHeight
            self._timeCanvas.create_line(x0, y0, x0, y1, fill='black')


            if tupleTime.tm_hour % self._labelFreq == 0: 
                self._timeCanvas.create_text(x0, y1 + self._timeLabelOffset,
                                             text=timeStr, fill='black')
        return

    # Adds wx slider
    def addVisToWxSliderChanged(self, event):
    
        valueStr = "%.2f" % self._addVisToWx.get()
        
        label = "Add Vis to Wx where Vis <= " + valueStr
        self._addVisToWxScale.config(label=label)
        self._addVisToWxScale.grid_propagate(0)
    
        return
    
    # Draws Vis scales
    def makeAddVisToWxScale(self, frame, row):
                   
        # make a label
        self._addVisToWxRow = row
        self._addVisToWxFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        self._addVisToWxFrame.grid(row=self._addVisToWxRow, column=0, sticky=tk.N+tk.S+tk.E+tk.W)
           
#         label = tk.Label(addVisToWxFrame, text="")
#         label.grid(row=0, column=0)
           
        self._addVisToWx = tk.DoubleVar()
        self._addVisToWx.set(self._config["AddVisToWxValue"])
        self._addVisToWxScale = tk.Scale(self._addVisToWxFrame, from_= 0, to=10,
                                         orient=tk.HORIZONTAL,
                                         command=self.addVisToWxSliderChanged,
                                         resolution=0.01, length=220,
                                         sliderlength=20,
                                         variable=self._addVisToWx)
        self._addVisToWxScale.grid(row=1, column=0)
           
        return
    
    # No-opt callback
    def intensitySelected(self):
        return
    
    # Creates the Wx Intensity buttons
    def makeIntensityButtons(self, frame, row):
        self._intFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        self._intFrame.grid(row=row, column=1, sticky=tk.N+tk.S+tk.E+tk.W)
        
        label = tk.Label(self._intFrame, text="Vis Updates\nWx Intensity of:")
        label.grid(row=0, column=0)
        # Make Intensity Buttons
        intensityDefault = self._config["IntensityDefault"]
        self._intVar = tk.StringVar()
        self._intVar.set(intensityDefault)
        choiceList = ["Precip", "Non-Precip"]
        row = 1
        for choice in choiceList:            
            button = tk.Radiobutton(self._intFrame, text=choice, padx=10, pady=2,
                                    command=self.intensitySelected, variable=self._intVar,
                                    value = choice)
            button.grid(row=row)
            row = row + 1
        return               
    
    # Call back when Save/Pub selected. Nothing to do.
    def savePubSelected(self):
        return

    # Makes the Save and Publish buttons
    def makeSavePublishButtons(self, frame, row):
        if self._config["OfferSavePublish"] != "Yes":
            return
        
        savePubFrame = tk.Frame(frame, relief=tk.GROOVE, bd=3)
        savePubFrame.grid(row=row, column=2, sticky=tk.N+tk.S+tk.E+tk.W, columnspan=3)
        
        label = tk.Label(savePubFrame, text="Save and\nPublish?")
        label.grid(row=0, column=0)
        # Make Save/Publish Buttons
        savePubDefault = self._config["SavePublishDefault"]
        self._savePublishVar = tk.StringVar()
        self._savePublishVar.set(savePubDefault)
        choiceList = ["Yes", "No"]
        row = 1
        for choice in choiceList:            
            button = tk.Radiobutton(savePubFrame, text=choice, padx=10, pady=2,
                                    command=self.savePubSelected, variable=self._savePublishVar,
                                    value = choice)
            button.grid(row=row)
            row = row + 1
        return               
    
    # This code displays makes the window display at the cursor location
    def displayWindowOnCursor(self):
        self._master.update_idletasks()
        wh= self._master.winfo_height()
        ww= self._master.winfo_width()
        px, py =self._master.winfo_pointerxy()
        self._master.geometry("%dx%d+%d+%d" % (ww, wh, px - (ww // 2), py - (wh // 2)))
        return

    # High-level method that calls other methods to build the GUI a widget at a time
    def setUpUI(self):

        self._tkmaster = tk.Tk()
        self._master = tk.Toplevel(self._tkmaster)

        self._master.title("Aviation_Finalize")

        # Capture the "x" click to close the GUI
        self._master.protocol('WM_DELETE_WINDOW', self.cancelCommand)

#         self._topFrame = tk.Frame(self._master, bg=self._bgColor)
        self._topFrame = tk.Frame(self._master, width=450)
        self._topFrame.grid()
        self._tkmaster.withdraw() # remove the master from the display
        self._topFrame.bind("<Enter>", self.enterEvent)

        # Make the GUI widgets
        row = 1
        self.makeAddVisToWxScale(self._topFrame, row)
        self.makeSavePublishButtons(self._topFrame, row)
        self.makeIntensityButtons(self._topFrame, row)
        row = row + 1
        self.makeAddNewWxWidgets(self._topFrame)
        self.makeRuleCheckButtons(self._topFrame)
                
#         row = 1
#         self.makeAddVisToWxScale(self._topFrame, row)
#         self.makeSavePublishButtons(self._topFrame, row)
#         row = row + 1
        
        self._timeScaleRow = row
        self._timeFrame = tk.Frame(self._topFrame, pady=5, relief=tk.GROOVE, bd=3)
        self._timeFrame.grid(row=self._timeScaleRow, columnspan=3, sticky=tk.N)
        self.makeTimeScale()
 
        
        row = row + 1
        self.makeBottomButtons(self._topFrame, row)
        
        self.displayWindowOnCursor()
        
        return
    
    # Algorithm to determine the start and end edit times, courtesy of Marcia Cronce.
    def determineTAFTimes(self):
        
        # Find the time rounded up to the next three hours
        start = (int(time.time() / (3600 * 3)) * (3600 * 3)) + (3600 * 3)
        hour = time.gmtime(start).tm_hour
        
        if hour % 6 == 0:
            end = start + (31 * 3600)
        else:
            end = start + (37 * 3600)
        
        return start, end
    
    def filterWeatherElements(self, weList):
        goodWEs = []
        
        for weName in weList:
            parm = self.getParm("Fcst", weName, "SFC")
            if parm is not None:
                goodWEs.append(weName)
        
        return goodWEs
    
    # Main method called when the tool starts.
    def execute(self, editArea, timeRange):
        # Get the config dictionary for this tool
        self._config = FinalizeConfig.config["Finalize"]
        
        self._timeRange = timeRange
        self._editArea = editArea

        # set up some constants for this tool
        self._activeFont = "Helvetica 12 bold"
        self._disabledFont = "Helvetica 12 normal"
        self._bgColor = "#d9d9d9"
        
        self._defaultOnRules = copy.copy(self._config["DefaultOnChecks"])
        self._userSelectedRules = copy.copy(self._config["DefaultOnChecks"])
        self._immutableRules = copy.copy(self._config["ImmutableChecks"])
        
        self._noWxStr = "<NoCov>:<NoWx>:<NoInten>:<NoVis>:"

        self._ruleDict = {}
        
        self._wxTypeDict = {}
        self._scales = {}
        self._defaultWxType = "Fog"
        
        self._ceilingSkyThreshold = 57.0 # percentage above which we can get a Ceiling

        # Comprehensive list of elements we need.
        self._weList = ["PoP", "CloudBasePrimary", "CloudBaseSecondary", "CloudBaseConditional",
                        "Wx", "Sky", "T", "Td", "Visibility", "VisibilityConditional", "Wind",
                        "WindGust", "Ceiling", "LLWS", "LLWSHgt", "SkySecondary"]
        # List of elements we will publish no matter what
        self._publishWEList = ["PoP", "CloudBasePrimary", "CloudBaseSecondary", "CloudBaseConditional",
                               "Wx", "Sky", "Visibility", "VisibilityConditional", "SkySecondary",
                               "Wind", "WindGust", "Ceiling", "LLWS", "LLWSHgt", "CigHgt", "Vsby"]
        
        # Filter out elements that are not configured, otherwise saving and publishing
        # throw errors.
        self._publishWEList = self.filterWeatherElements(self._publishWEList)
        
#         # Define the Pot elements if we're making those
#         if self._config["MakePotFog"] == "Y":
        self._potElements = ["PotFog", "PotFreezingFog", "PotHaze", "PotSmoke"]
        self._potElements = self.filterWeatherElements(self._potElements)

        # These elements are not required
        self._notRequiredWEs = ["Ceiling", "CloudBaseSecondary", "CloudBaseConditional", "VisibilityConditional",
                                "SkySecondary"]
        # These are elements used to keep others consistent and are not modified
        self._immutableWEs = ["PoP", "T", "Td", "Wind"]

        self._wxTypeCovDict = {
                               "Fog" : ["Patchy", "Areas", "Wide"],
                               "Haze" : ["Def"],
                               "Smoke" : ["Patchy", "Areas", "Def"],
                               }
        
        # List of all precipitation types
        self._obstructionTypes = ["F", "H", "K", "ZF"]
        
        # Initialize time variables
        startHour = self._config["TAFStartHour"]
        endHour = self._config["TAFEndHour"]
        self._startTime = int(self._gmtime().unixTime() / 3600) * 3600 + (startHour * 3600)
        self._endTime = self._startTime + (endHour * 3600)
        
        # Get the initial GFE selected start and end times
        gfeStart = int(timeRange.startTime().unixTime() / 3600) * 3600
        gfeEnd = int(timeRange.endTime().unixTime() / 3600) * 3600
        
        # Set the edit start and end times. These may be changed by the user.
        self._editStartTime = self._startTime
        self._editEndTime = self._endTime

        if self._config["AlwaysUseFullTAFPeriod"] != "Yes":
            # If no time was selected, employ the Marcia method for determining the start and end.
            if gfeStart == 0 and gfeEnd == 0:
                self._editStartTime, self._editEndTime = self.determineTAFTimes()
                if self._editEndTime > self._endTime:  # reset the display end time so everything fits
                    self._endTime = self._editEndTime
            # Reset the start and end times to what is selected in the GFE, if reasonable
            if gfeStart >= self._editStartTime and gfeStart < self._editEndTime:
                self._editStartTime = gfeStart
            if gfeEnd > self._editStartTime and gfeEnd <= self._editEndTime:
                self._editEndTime = gfeEnd
            
        self._mouseButton = self._config["editButton"]
        # Define the geometry variables for the time scale
        self.defineGeometry()

        self.setUpUI()
        
        tk.mainloop()

