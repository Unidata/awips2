# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# DefineMaxWindGUI
#
# Author:
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

from numpy import *
AWIPS_ENVIRON = "AWIPS2"

import SmartScript
import tkinter
import tkinter.font


class DefineMaxWindGUI(SmartScript.SmartScript):
    
    def __init__(self, dbss, eaMgr=None):
        SmartScript.SmartScript.__init__(self, dbss)

        if AWIPS_ENVIRON == "AWIPS1":
            self.setUp(eaMgr)

    def reportQuadError(self):
        self.statusBarMsg("Only three quadants at a time may be reduced.\n" + \
                          "...Please toggle another quadrant off first.","S")
        return

    def toggleButton(self, buttonLabel):
        b = self._buttonLabels.index(buttonLabel)
        if self._buttonState[b]:  # button was on
            self._buttonState[b] = False
            self._buttonList[b].configure(background="gray", activebackground="gray")
        else:   # button was off
            if sum(self._buttonState) > 2: # only three at a time allowed
                self.reportQuadError()
                return
            self._buttonState[b] = True
            self._buttonList[b].configure(background="green", activebackground="green")


    def NEClick(self):
        self.toggleButton("NE")
        return
    
    def SEClick(self):
        self.toggleButton("SE")
        return
    
    def SWClick(self):
        self.toggleButton("SW")
        return
    
    def NWClick(self):
        self.toggleButton("NW")
        return

    def makeLabel(self, frame):

        label = tkinter.Label(frame, fg="red", font=self._boldFont,
                              text="Max winds will be reduced by\n 20% in selected quadrants")
        label.grid(row=0)

        return
    
    def makeBottomButtons(self, frame):
        # Create the Execute/Cancel buttons
        self._doneButton = tkinter.Button(frame, text="Done",
                                command=self.doneCommand)
        self._doneButton.grid(row=3, column=0, padx=20, pady=5, sticky=tkinter.W)
        
        self._cancelButton = tkinter.Button(frame, text="Cancel",
                                command=self.cancelCommand)
        self._cancelButton.grid(row=3, column=2, padx=20, pady=5, sticky=tkinter.E)

        frame.grid(columnspan=3, sticky=tkinter.EW)

        return

    def makeQuadButtons(self, frame):
        # Create the quadrant buttons
        commandList = [self.NWClick, self.SWClick, self.SEClick, self.NEClick]
        self._buttonLabels = ["NW", "SW", "SE", "NE"]

        # define the button position in geometric order
        buttonPos = [(0, 0), (1, 0), (1, 1), (0, 1)]
        for b in range(len(self._buttonLabels)):
            label = self._buttonLabels[b]
            
            self._buttonList[b] = tkinter.Button(frame, text=label,
                                                 command=commandList[b],
                                                 font=self._bigFont, width=3)
            rowPos, colPos = buttonPos[b]
            self._buttonList[b].grid(row=rowPos, column=colPos, padx=30, pady=10)
        
        return

    def setUpUI(self):

        self._labelFrame = tkinter.Frame(self._master)

        self._labelFrame.grid(row=0)

        
        self._buttonFrame = tkinter.Frame(self._master, borderwidth=3,
                                          relief=tkinter.RIDGE, bd=2, pady=5)
        self._buttonFrame.grid(row=1, column=0,padx=25,
                               sticky=tkinter.E+tkinter.W, pady=5)


        self._bottomFrame = tkinter.Frame(self._master, borderwidth=3,
                                      relief=tkinter.RIDGE, bd=2)
        self._bottomFrame.grid(row=2, column=0, columnspan=2, padx=25)

        self._master.title("Reduce Max Wind by Quadrant")

        self.makeLabel(self._labelFrame)

        self.makeQuadButtons(self._buttonFrame)

        self.makeBottomButtons(self._bottomFrame)

        return

    def doneCommand(self):
        self._master.quit()

        quadCount = 4
        reducePct = 0.80
        
        # Gather up the maxWind info to return to the main tool
        self._maxWindDict = {}
        for h in range(len(self._hourList)):
            windList = []
            for quad in range(quadCount):

                # Reduce the value if that quadrant was selected
                if self._buttonState[quad]:
                    windValue = self._maxWind[quad][h] * self._allTimeMaxWind * reducePct
                else:
                    windValue = self._maxWind[quad][h] * self._allTimeMaxWind

                windList.append(windValue)
            
            windList.reverse()  
            self._maxWindDict[self._hourList[h]] = windList

        return
    
    def cancelCommand(self):
        self._master.destroy()
        
        return None

    def displayGUI(self, windDict):

        self._windDict = windDict
        self._maxWindDict = None
        self._quadCount = 4

        hourKeys = sorted(self._windDict.keys())
        self._hourList = hourKeys
        self._initialMinWind = []
        self._initialMaxWind = []
        for hour in hourKeys:
            minWind, maxWind = windDict[hour]
            self._initialMinWind.append(minWind)
            self._initialMaxWind.append(maxWind)

        self._hourCount = len(hourKeys)

        if AWIPS_ENVIRON == "AWIPS1":
            self._allTimeMaxWind = max(self._initialMaxWind)
        else:  # numpy uses "amax"
            self._allTimeMaxWind = amax(self._initialMaxWind)


        
        # Make the topLevel window - different for A1 and A2
        if AWIPS_ENVIRON == 'AWIPS1':
            self._master = tkinter.Toplevel(self.eaMgr().root())
            self._master.transient(self.eaMgr().root())  # always draw on top of GFE
        else:
            self._tkmaster = tkinter.Tk()
            self._master = tkinter.Toplevel(self._tkmaster)
            self._tkmaster.withdraw()

        self._buttonLabels = ["NW", "SW", "SE", "NE"]

        self._buttonState = [False, False, False, False]
        self._buttonList = [None, None, None, None]

        self._boldFont = tkinter.font.Font(family="Helvetica", size=12, weight="bold")
        self._bigFont = tkinter.font.Font(family="Helvetica", size=16)
       
        self.setUpUI()

        self._maxWind = zeros((self._quadCount, self._hourCount)) * 1.0

        for hour in range(len(hourKeys)):
            for quad in range(self._quadCount):
                minWind, maxWind = self._windDict[hourKeys[hour]]
                self._maxWind[quad][hour] = maxWind / self._allTimeMaxWind
                

        #self.updateDisplay() # Draws everything

        
        self._master.mainloop()

        self._master.withdraw()
        self._master.destroy()

        return self._maxWindDict
