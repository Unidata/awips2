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
# ViewWCL
#
# Author: Matt Davis/ARX, Tom LeFebvre - Modified to fetch inventory
# 
# ----------------------------------------------------------------------------

# The MenuItems list defines the GFE menu item(s) under which the
# Procedure is to appear.
# Possible items are: Populate, Edit, Consistency, Verify, Hazards

MenuItems = ["Hazards"]

import os
import SmartScript, re, time
import AbsTime
import TimeRange 
import Tkinter
import HazardUtils
from numpy import *
            
class Procedure (SmartScript.SmartScript):
    
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        self._dbss = dbss

    # creates the various UI objects
    def setUpUI(self):

        self.__inventoryDict = self.getWCLInventoryDict()

        # first check to see if there's any data we can plot
        if len(self.__inventoryDict.keys()) == 0:  # no entries in the dict
            self.statusBarMsg("There are no current WCLs to view.", "S")
            self.cancel()
            return

        # make the frames
        self.__master = Tkinter.Tk()
        self.__master.title('ViewWCL')
        self.__topFrame = Tkinter.Frame(self.__master)
        
        self.__topFrame.pack(side=Tkinter.TOP, expand=Tkinter.YES,
                             anchor=Tkinter.N, fill=Tkinter.BOTH)

        self.__listFrame = Tkinter.Frame(self.__topFrame, borderwidth=3,
                                         relief=Tkinter.GROOVE)
        self.__buttonFrame = Tkinter.Frame(self.__master)


        self.makeInventoryButtons(self.__inventoryDict)

        # make the buttons at the bottom of the dialog
        self.makeRunButton()
        self.makeRunDismissButton()
        self.makeCancelButton()
        
        self.__buttonFrame.pack(side=Tkinter.TOP)

    ##
    # Get the directory in which decoded WCLs are stored from GFE localization.
    # 
    # @return: the WCL directory
    # @rtype: string
    def getWclDir(self):
        # get the path manager
        from com.raytheon.uf.common.localization import PathManagerFactory
        pathManager = PathManagerFactory.getPathManager()
        
        # get the proper localization context
        from com.raytheon.uf.common.localization import LocalizationContext
        from com.raytheon.uf.common.localization import LocalizationContext_LocalizationType as LocalizationType
        CAVE_STATIC = LocalizationType.valueOf("CAVE_STATIC")
        from com.raytheon.uf.common.localization import LocalizationContext_LocalizationLevel as LocalizationLevel
        SITE = LocalizationLevel.valueOf("SITE")
        ctx = pathManager.getContext(CAVE_STATIC, SITE)
        
        # use localization to get the full path
        wclName = os.path.join("gfe", "wcl")
        wclDir = pathManager.getFile(ctx, wclName)
        return wclDir.getAbsolutePath()
        
    # gets the text inventory for WCL products and stores in a dictionary
    def getWCLInventoryDict(self):

        invDict = {}
        wclDir = self.getWclDir();
        
        if os.path.exists(wclDir):
            for name in os.listdir(wclDir):
                if re.search(r'^WCL[A-J]$', name):
                    absName = os.path.join(wclDir, name)
                    file = None
                    text = None
                    try:
                        file = open(absName, "r")
                        text = file.read(0xffff)
                    finally:
                        if file is not None:
                            file.close()
                    if text is not None:
                        dataDict = {}
                        exec text in dataDict
                        dataDict.setdefault("issueTime", 0)
                        dataDict.setdefault("expTime", 0)
                        issueTime = dataDict["issueTime"]
                        expTime = dataDict["expTime"]
                        timeDiff = (AbsTime.current().unixTime() - issueTime)
                        if timeDiff < 6 * 3600 and issueTime < expTime: 
                            invDict[name] = dataDict
            
        return invDict

    # Make a button for each entry in the inventory
    def makeInventoryButtons(self, invDict):

        labelStr = Tkinter.StringVar()
        labelStr.set("Name        Issuance Time")
        label = Tkinter.Label(self.__listFrame, textvariable=labelStr)
        label.pack(side=Tkinter.TOP, fill=Tkinter.X, expand=Tkinter.NO, padx=10,
                   pady=10)
        
        invList = []
        for name in invDict:
            invList.append((invDict[name]['issueTime'], name))
        invList.sort()
        invList.reverse()
        firstName = invList[0][1]
        self.prodSelected = Tkinter.StringVar()
        for issueTime, name in invList:
            timeStr = time.strftime("%a %d %b %H%MZ", time.gmtime(issueTime))
            buttonStr = name + "   " + timeStr

            button = Tkinter.Radiobutton(self.__listFrame, highlightthickness = 0,
                                         text=buttonStr, value=name,
                                         variable=self.prodSelected,
                                         command=self.WCLSelected)
            if name == firstName:
                button.select()
                
            button.pack(side=Tkinter.TOP, anchor=Tkinter.W)

        self.__listFrame.pack(side=Tkinter.TOP, expand=Tkinter.NO,
                              fill=Tkinter.Y, anchor=Tkinter.N)
        return
        
    # called when a selection is made (does nothing)
    def WCLSelected(self):
        return

    def _deleteWCLGrids(self):
        tr = TimeRange.allTimes()
        gridInfo = self.getGridInfo("WCL", "ProposedWatches", "SFC", tr)
        trList = []
        for g in gridInfo:
            self.deleteGrid("WCL", "ProposedWatches", "SFC", g.gridTime())

    def makeRunButton(self):
        # create the Run button
        Tkinter.Button(self.__buttonFrame, text="Run",
          width=10, command=self.runCommand, state=Tkinter.NORMAL).pack(\
          side=Tkinter.LEFT, pady=5, padx=10)

    ### Makes the Run/Dismiss buttom
    def makeRunDismissButton(self):
        # create the Combine button
        Tkinter.Button(self.__buttonFrame, text="Run/Dismiss",
          width=10, command=self.runDismissCommand,
          state=Tkinter.NORMAL).pack(side=Tkinter.LEFT, pady=5, padx=10)
    
    ### Makes the Cancel buttom
    def makeCancelButton(self):
        # create the Combine button
        Tkinter.Button(self.__buttonFrame, text="Cancel",
          width=10, command=self.cancelCommand, state=Tkinter.NORMAL).pack(\
          side=Tkinter.LEFT, pady=5, padx=10)

    ### called when the Run button is selected
    def runCommand(self):
        prodName = self.prodSelected.get()
        self.plotWCL(prodName)
        return
    
    ### called when the Run/Dismiss button is selected
    def runDismissCommand(self):
        prodName = self.prodSelected.get()
        self.plotWCL(prodName)
        self.cancelCommand()

    ### called when the Cancel button is selected        
    def cancelCommand(self):
        # unregister the maps
        self.__master.destroy()

    # Main block of the tool.  Sets up the UI.
    def execute(self, timeRange):
        self._hazUtils = HazardUtils.HazardUtils(self._dbss, None)
        
        self.setToolType("numeric")

        # see if the Hazards WE is loaded in the GFE, if not abort the tool
        if not self._hazUtils._hazardsLoaded():
            self.statusBarMsg("Hazards Weather Element must be loaded in " + \
              "the GFE before running ViewWCL", "S")
            self.cancel()

        self.setUpUI()
        self.__master.mainloop()                

        return

    # Does the work of plotting the watch areas.  Fetches the specified text
    # product, deocodes it and creates a temporary grid that shows the areas
    # of the proposed watches
    def plotWCL(self, productName):
        #extract the data fro the dictionary
        watchType = self.__inventoryDict[productName]['watchType']
        expTime = self.__inventoryDict[productName]['expTime']
        issueTime = self.__inventoryDict[productName]['issueTime']
        finalUGCList = self.__inventoryDict[productName]['finalUGCList']
        
        currentTime = time.time()
        startTime = None
        endTime = None

        # Select WCL to plot
        wclVersion = productName

        # This section reads the active table and decodes current watches
        activeTable = self.vtecActiveTable()

        # Remove unwanted data
        cleanTable = []
        for each in activeTable:
            if not each.has_key('pil'):
                continue
            if not each['pil'] == 'WCN':
                continue
            if not each.has_key('endTime'):
                continue
            if each['endTime'] <= currentTime:
                continue
            if not each.has_key('act'):
                continue
            
            if each['act'] not in ['CAN', 'EXP']:
                cleanTable.append(each)
                if startTime is None:
                    startTime = each['startTime']
                elif startTime > each['startTime']:
                    startTime = each['startTime']
                if endTime is None:
                    endTime = each['endTime']
                elif endTime > each['endTime']:
                    endTime = each['endTime']

        # Adjust start/end times based on issueTime, expTime
        if endTime is None or expTime > endTime:
            endTime = expTime
        if startTime is None or issueTime < startTime:
            startTime = issueTime
        
        # Round to hour
        startTime = int(startTime / 3600) * 3600
     
        # Change keys for this procedure
        for each in cleanTable:
            if each['phensig'] == 'SV.A':
                each['phensig'] = 'sv.a'
            else:
                each['phensig'] = 'to.a'

        # Create a master list of all IDs
        watchUGCs = []
        for each in cleanTable:
            if each['id'] not in watchUGCs:
               watchUGCs.append(each['id'])
        for each in finalUGCList:
            if each not in watchUGCs:
               watchUGCs.append(each)

        # Next, loop over the master ID list to determine the final key to 
        # plot.
        finalKeys = []
        for each in watchUGCs:
            actualKey = ''
            for eachRecord in cleanTable:
                if eachRecord['id'] == each:
                    actualKey = eachRecord['phensig'] 
            if each in finalUGCList:
                if actualKey != '':
                    if actualKey == 'sv.a':
                        if watchType == 'SV.A':
                            actualKey = 'sv->SV'
                        else:
                            actualKey = 'sv->TO'
                    else:
                        if watchType == 'SV.A':
                            actualKey = 'to->SV'
                        else:
                            actualKey = 'to->TO'
                else:
                    actualKey = watchType
            finalKeys.append((each, actualKey))
               
        # Get the ending day/time to create a timeRange. Don't have to bother
        # with day groups, as watches are always < 24 hrs.
        #ensure sanity
        if abs(time.time() - startTime) > 43200:
            startTime = int(time.time() / 3600) * 3600
        if abs(time.time() - endTime) > 43200:
            endTime = int(time.time() / 3600) * 3600 + (43200)
        timeRange = TimeRange.TimeRange(AbsTime.AbsTime(startTime), 
          AbsTime.AbsTime(endTime))

        # Create a dummy grid of zeros
        gridLoc = self.getGridLoc()
        xSize = gridLoc.gridSize().x
        ySize = gridLoc.gridSize().y
        gridSize = (ySize, xSize)
        grid = zeros(gridSize, int8)

        # Define the allowed keys
        keys = ['<None>','SV.A','TO.A','sv.a','to.a','to->SV','sv->SV','to->TO','sv->TO']

        # Loop over the finalKeys list and plot
        eaList = self.editAreaList()
        
        for each in finalKeys:
            watchIndex = self.getIndex(each[1], keys)

            # Set each edit area found in the WCL to the mask value
            mask = zeros(gridSize)
            if each[0] in eaList:
                zoneArea = self.getEditArea(each[0])
                zoneMask = self.encodeEditArea(zoneArea)
                mask = logical_or(mask, zoneMask)
            grid = where(mask, watchIndex, grid)


        #remove any existing grid
        parms = self.loadedParms()
        for weName, level, dbID in parms:
            if weName == "ProposedWatches" and level == "SFC" and \
              dbID.modelName() == "WCL":
                # found parm, delete any grids
                self._deleteWCLGrids()
                break

        self.createGrid("WCL", "ProposedWatches", "DISCRETE", (grid, keys), \
          timeRange, discreteKeys=keys, discreteOverlap=0, \
          discreteAuxDataLength=0)
        self.setActiveElement("WCL", "ProposedWatches", "SFC", timeRange, \
          colorTable='GFE/WCLHazards')

        return 



