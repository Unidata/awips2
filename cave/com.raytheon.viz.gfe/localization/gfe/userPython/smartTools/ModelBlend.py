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
# Model_Blend version 2.1
#
# Make combinations of recent (last two) model runs, or the forecast or
# official grids.  Can extrapolate by using a combination of negative and
# positive weights.  Weights cannot add up to zero - and error message is
# generated if user sets them that way.
#
# Author: Tim Barker
#    2009-12-18: Rewritten to run from Java. Removed features that depend
#                on prestarted Tkinter app: dialog derived from tkSimpleDialog,
#                global-level IntVars and StringVars, IntVars and StringVars
#                using the default parent.
#    2006-01-12: Version 2.1. Fixed accumulative elements like QPF/SnowAmt
#                to add up model QPF/SnowAmt grids before doing blend.  Old
#                way averaged them - then blended.  Also fixed so that it
#                does not read grids from cache - that way changes to Fcst
#                grid in one area are reflected when the tool is run again
#                in another area.
#    2005-06-01: Version 2.0. Re-worked to be non-modal dialog box, add
#                optional edge effects when working on an edit area,
#                simplify using previous model runs, and make negative
#                weights optional.
#    2002-10-09: Original Implementation from Les Colin Idea
#---------------------------------------------------------------------
#
#  C O N F I G U R A T I O N   S E C T I O N
#
#---------------------------------------------------------------------
#
#  To keep the dialog from being too "long", you can specify the
#  maximum number of weights in a column of the dialog.  It will try
#  to balance columns if needed.
#
MAX_IN_COLUMN=15
#
#  If you do not want to allow negative weights (which can be used to
#  extrapolate trends), then set USE_NEGATIVE_WEIGHTS to zero.
#
USE_NEGATIVE_WEIGHTS=1
#
#  List of GFE model databases that you will potentially blend.
#  The name is followd by a number (separated by a colon) that
#  gives the number of versions to potentially blend.  The versions
#  is followed by a list of elements for which to add this model
#  (assumed to be for all elements if missing) or if the list starts
#  with a ^ character, then it is a list of elements where this model
#  will NOT be listed). You can add the "ISC" database with "ISC:1".
#  If ISC is specified, the list of elements reflect the destination
#  weather elements in the Fcst database, and not the actual weather element
#  names in the ISC database (since they can be renamed by the system).
#
Models=("ADJMET:2",
        "ADJMETBC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "Eta12:2",
        "Eta12BC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "ADJMAV:2",
        "ADJMAVBC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "GFS40:2",
        "GFS40BC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "ADJFWC:1",
        "ADJFWCBC:1",
        "NGM80:1",
        "NGM80BC:1",
        "ADJMEX:2",
        "ADJMEXBC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "ADJMEH:1:MaxT,MinT,PoP",
        "ADJMEN:1:MaxT,MinT,PoP",
        "ADJMEL:1:MaxT,MinT,PoP",
        "DGEX:1",
        "DGEXBC:1:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "ADJHPC:1:MaxT,MinT,PoP,Sky,Td,Wind",
        "ADJKAF:2:MaxT,MinT,Wind,T,Td,MaxRH,MinRH,RH,TdMrn,TdAft",
        "ADJKAFBC:2:MaxT,MinT,MaxRH,MinRH,TdMrn,TdAft,T,Td,RH",
        "NWHAS:3:QPF",
        )

edgestyleDefault="Flat"
#---------------------------------------------------------------------
#
#  END OF CONFIGURATION SECTION
#
#---------------------------------------------------------------------
#
#
#
ToolType = "numeric"
WeatherElementEdited = "variableElement"
ScreenList = ["SCALAR","VECTOR"]
#
#
#
from numpy import *
import Tkinter
import AppDialog
import SmartScript

edgestyles=["Flat","Edge","Taper"]
#
#
#
class ToolDialog(AppDialog.AppDialog):
    def __init__(self, title="Tk", callbackMethod=None, labels=None, **kwargs):
        self.__callbackMethod = callbackMethod
        self.dbIds = []
        self.labels = []
        self.__percents = []
        self.weights = []
        self.__weightVars = []
        self.numrows = MAX_IN_COLUMN
        self.numcolumns = 1
        if labels is not None:
            self.labels.extend(labels)
            self.numrows = min(len(labels), MAX_IN_COLUMN)
            self.numcolumns = (len(labels)-1)/MAX_IN_COLUMN + 1
        AppDialog.AppDialog.__init__(self, **kwargs)
        self.title(title)
        
    def buttonbox(self):
        buttonFrame = Tkinter.Frame(self)
        # create the buttons associated with this dialog
        Tkinter.Button(buttonFrame, text="Run",
            command=self.__runCB, width=10, state=Tkinter.NORMAL).pack(\
            side=Tkinter.LEFT, pady=5, padx=10)
        Tkinter.Button(buttonFrame, text="Run/Dismiss",
            command=self.__okCB, width=12, state=Tkinter.NORMAL).pack(\
            side=Tkinter.LEFT, pady=5, padx=10)
        Tkinter.Button(buttonFrame, text="Cancel", width=10,
            command=self.cancelCB).pack(\
            side=Tkinter.LEFT, pady=5, padx=10)
        buttonFrame.pack(side=Tkinter.BOTTOM)

    def body(self, master):
        bodyFrame = Tkinter.Frame(master)
        self.buildWeightSliders(bodyFrame)
        self.buildEdgeControl(bodyFrame)
        bodyFrame.pack(side=Tkinter.TOP)
        return bodyFrame

    ##
    # Validate the inputs.
    # Because self.destroy() is called before apply(), any Tkinter variables
    # such as StringVar or IntVar instances will be invalid by the time apply()
    # is called. Therefore, validate() must also preserve any data in such
    # variables that apply() will need.
    def validate(self):
        rtnval = True;
        self.weights = []
        for wv in self.__weightVars:
            self.weights.append(wv.get())
        self.edgestyle = self.edgestyleString.get()
        self.edgeWidth = self.edgeWidthVar.get()
        return rtnval

    ##
    # Set the percent labels based on the slider weights.
    # This is primarily a callback method invoked by the scale widgets.
    # @param weight: Weight of the scale widget that changed
    # @type weight: int
    #        
    def setPercents(self, weight):
        "Set the percent labels based on the slider weights."
        total=0
        for wv in self.__weightVars:
           total+=wv.get()
        if total==0:
            for pctVar in self.__percents:
                pctVar.set("%4d%%"%0)
        else:
            wpct = 100 / float(total)
            for i, pctVar in enumerate(self.__percents):
                pctVar.set("%4d%%"%(self.__weightVars[i].get() * wpct))
                
    def __runCB(self):
        "The callback invoked by the Run button"
        self.validate()
        self.__callbackMethod("Run")

    def __okCB(self):
        "The callback invoked by the Ok button"
        self.validate()
        self.__callbackMethod("OK")
        self.ok()

    def cancelCB(self):
        "The callback invoked by the Cancel button"
        self.__callbackMethod("Cancel")
        self.cancel()
        
    def apply(self, event=None):
        pass

    def buildWeightSliders(self, master):
        hull = Tkinter.Frame(master)
        lastColumn = len(self.labels)/MAX_IN_COLUMN
        row = 0
        column = 0
        fc = None
        if USE_NEGATIVE_WEIGHTS:
            origin = -10
        else:
            origin = 0
        for labelText in self.labels:
            if fc is None:
                fc = Tkinter.Frame(hull)
            # Create Tk variables for the weight and percent
            weightVar = Tkinter.IntVar(master)
            pctVar = Tkinter.StringVar(master)
            # Store references for other routines
            self.__weightVars.append(weightVar)
            self.__percents.append(pctVar)
            # Initialize the weight and percent variables
            weightVar.set(0)
            pctVar.set("%4d%%"%0)
            # Create labels and sliders
            lbl = Tkinter.Label(fc, text=labelText)
            slider = Tkinter.Scale(fc,orient=Tkinter.HORIZONTAL,
                                   from_=origin,to=10,resolution=1,
                                   command=self.setPercents,
                                   variable=weightVar,length=150)
            lab2=Tkinter.Label(fc,textvariable=pctVar,width=5)
            # Grid the items left-to-right in the current row
            lbl.grid(row=row, column=0, sticky=Tkinter.SE)
            slider.grid(row=row, column=1, sticky=Tkinter.SE)
            lab2.grid(row=row,column=2,sticky=Tkinter.SE)
            if column < lastColumn:
                f2=Tkinter.Frame(fc,bg="black",width=1)
                f2.grid(row=row,column=3,sticky=Tkinter.NS)
            row+=1
            if row >= MAX_IN_COLUMN:
                fc.grid(row=0, column=column, sticky=Tkinter.N)
                row=0
                column+=1
                fc = None
        if fc is not None:
            fc.grid(row=0, column=column, sticky=Tkinter.N)
        # Revise the weight of the forecast item
        self.__weightVars[0].set(1)
        self.setPercents(1)
        hull.grid(row=0,column=0, sticky=Tkinter.S)
    
    def buildEdgeControl(self, master):
        edgeFrame=Tkinter.Frame(master,relief=Tkinter.GROOVE,borderwidth=2)
        edgestyleFrame=Tkinter.Frame(edgeFrame)
        edgewidthFrame=Tkinter.Frame(edgeFrame)
        # Create the edge style radio buttons
        self.edgestyleString=Tkinter.StringVar(master)
        for edgestyle in edgestyles:
           a=Tkinter.Radiobutton(edgestyleFrame,text=edgestyle,
                               variable=self.edgestyleString,value=edgestyle)
           if edgestyle == edgestyleDefault:
              a.invoke()
           a.pack(side=Tkinter.TOP,anchor=Tkinter.W)
        edgestyleFrame.pack(side=Tkinter.LEFT,anchor=Tkinter.W)
        # Create the edge width slider
        self.edgeWidthVar=Tkinter.IntVar(master)
        self.edgeWidthVar.set(5)
        a=Tkinter.Scale(edgewidthFrame,from_=1,to=30,variable=self.edgeWidthVar,
                      showvalue=1,label="Edge Width:",orient=Tkinter.HORIZONTAL)
        a.pack(side=Tkinter.TOP,anchor=Tkinter.N,fill=Tkinter.X)
        edgewidthFrame.pack(side=Tkinter.RIGHT,anchor=Tkinter.W,fill=Tkinter.X,expand=1)
        
        # Add the edge control below the weight sliders
        edgeFrame.grid(row=self.numrows,column=0,columnspan=self.numcolumns,sticky=Tkinter.EW)

#========================================================================
class TestDialog(object):
    "A dummy object used to test the back end."
    
    def __init__(self, title="Tk", callbackMethod=None, labels=None, **kwargs):
        print "TestDialog constructor:"
        print "Title=", title
        print "labels=", labels
        print "kwargs=", kwargs
        self.__callbackMethod = callbackMethod
        self.edgestyle = "Taper"
        self.edgeWidth = ""
        self.weights = [1] * len(labels)

    def mainloop(self):
        self.__callbackMethod("Run")

#========================================================================
#
#  The real GFE Tool
#
class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        self._dbss=dbss
        SmartScript.SmartScript.__init__(self, dbss)
        
    def preProcessGrid(self,WEname):
        #
        #  Setup the arrays of information for the dialog
        #  box that sets the weights
        #
        #  The mutable database is labeled "Forecast" no matter
        #  what you do, and put in the first slot on the dialog.
        #  The "Official" database is hardcoded into the next slot,
        #  then others are added
        #

        self.labels = []
        self.dbIds = []

        db=self.mutableID()
        id=db.modelIdentifier()
        self._addModel('Forecast:', id)
        #
        db=self.findDatabase("Official")
        id=db.modelIdentifier()
        self._addModel("Official", id)
        #
        plist = None
        allOfficeTypes = None
        for modelString in Models:
            model, versions, parmlist = self.parseMS(modelString)
            if model is None:
                continue
            if not self.acceptPL(WEname, parmlist):
                continue
             
            #
            #  Make labels for each of the model runs we want.
            #  Singleton databases (like FCST or Official) that have
            #  no date (actually a 1970 date) have no date/run label.
            #
            for run in range(0,-versions,-1):
                db=self.findDatabase(model,run)
                if db is None:
                    continue
                id=db.modelIdentifier()
                if id is None or ""==id or id in self.dbIds:
                    continue
                if "ISC" == db.modelName():
                    if allOfficeTypes is None:
                        allOfficeTypes = self.knownOfficeTypes()
                    iscOfficeTypes = [self.myOfficeType()]
                    if plist is None:
                        plist = self.availableParms()
                    for pname, plevel, pdb in plist:
                        if id != pdb.modelIdentifier():
                            continue
                        for ot in allOfficeTypes:
                            if pname.endswith(ot) and \
                              ot not in iscOfficeTypes:
                                iscOfficeTypes.append(ot)
                    for otype in iscOfficeTypes:
                        ltext = "%s (%s):"%(model, otype)
                        self._addModel(ltext, id)                        
                else:
                    modtime=db.modelTime()
                    year=modtime.year
                    if year==1970:
                        lbltext="%s:"%model
                    else:
                        month=modtime.month
                        day=modtime.day
                        hour=modtime.hour
                        lbltext="%s %2.2d/%2.2d %2.2dZ:" % (model,month,day,hour)
                    self._addModel(lbltext,id)
        #
        #  Now run the dialog box to get the weights
        #  resulting weights stored in Weights array
        #
        self.dlg=ToolDialog("Set Model Weights",
                       callbackMethod=self.execWeights,
                       labels=self.labels)
#        self.dlg=TestDialog("Set Model Weights",
#                       callbackMethod=self.execWeights,
#                       labels=self.labels)
        #
        #  Cancel the tool in the first pre-processGrid routine.
        #  No Execute routine is done - and grid is not marked as
        #  edited.  Any editing will take place when they press a
        #  Button on the dialog and it calls execWeights
        #
        self.dlg.mainloop()
        self.cancel()
    
    def parseMS(self, modelstring):
        """Parse a model string into a model, versions, and parmlist."""
        model = None
        versions = None
        parmlist = None
        pieces = modelstring.split(":")
        len_pcs = len(pieces)
        if len_pcs < 4:
            model = pieces[0]
            versions = 1
            parmlist = 'ALL'
            if len_pcs > 1:
                try:
                    versions = abs(int(pieces[1]))
                except:
                    pass
            if len_pcs > 2:
                parmlist = pieces[2]
        return (model, versions, parmlist)
    
    def acceptPL(self, WEName, parmlist):
        """Check WEName against parmlist."""
        invert = False
        parms = parmlist.split(",")
        if '^'==parms[0][0]:
            parms[0] = parms[0][1:]
            invert = True
        result = ('ALL'==parms[0]) or (WEName in parms) 
        result = invert ^ result
        return result
        
    ##
    #
    #     
    def _addModel(self, text, id):
        "Add text and id to self.labels and self.dbIds, respecively."
        self.labels.append(text)
        self.dbIds.append(id)

    #=================================================================
    #
    #  Dummy execute routine.  Tool is cancelled in preProcessGrid
    #  and all the real action is accomplished in execWeights which
    #  is called when the user presses a button on the dialog
    #
    def execute(self,variableElement):
        "Specified blend of any/all model/forecast fields"
        return variableElement
    #=================================================================
    #
    #  execWeights - The main calculation routine called when a button
    #                is pressed in the dialog.  Passes in the string
    #                name of the button pressed
    #
    def execWeights(self,button):
        #
        #  If user presses cancel, do an immediate return and stop
        #
        if button=="Cancel":
            return
        
        #
        #  Get the results from the dialog
        #
        #for num in range(len(Labels)):
        #    Weights[num]=ScaleIDs[num].get()
        EdgeType=self.dlg.edgestyle
        EdgeWidth=self.dlg.edgeWidth
        #
        #  If user presses run or run/dismiss, first add up the
        #  weights (in the ScaleIDs variables) and check for
        #  common issues like all weights zero, only weights on
        #  current grid, or grids add up to zero.
        #
        totweight=0
        fcstweight=0
        someweights=0
        otherweights=0

        dbIds = self.dbIds # alias
        weights = self.dlg.weights
        maxAbsWeight = max( max(weights), abs(min(weights)) )
        someweights = (maxAbsWeight > 0.5)
        fcstweight = weights[0]
        otherweights = sum(weights[1:])
        totweight = fcstweight + otherweights
        
        if not someweights:
            self.statusBarMsg("ModelBlend has no weights","R")
            return
        if abs(fcstweight) > 0.5 and otherweights==0:
            self.statusBarMsg("ModelBlend Weights add to no change","R")
            return
        if totweight==0:
            self.statusBarMsg("Weights cannot add up to zero","A")
            return
        #
        #  Get stuff usually provided by tool code:
        #     fcst=mutable model database name
        #     selectTR=the selected timerange
        #
        fcst = self.mutableID().modelIdentifier()
        selectTR = self._dbss.getParmOp().getSelectionTimeRange()
        #
        #  get list of parms that are selected and mutable
        #
        # Making a derivation from AWIPS1's version of this script.
        # Instead of calling direct to Java's ParmManager to get the Parm
        # objects, we'll use SmartScript's selectedParms() to retrieve native
        # Python objects which should save us Java heap space which wouldn't
        # be freed otherwise until the user terminates the SmartTool
        #
        # allParms = self._dbss.getParmManager().getSelectedParms()
        allParms = self.selectedParms()
        parms = []
        for parm in allParms:
            # model = parm.getParmID().getDbId().getModelId()
            model = parm[2].modelIdentifier()
            if model == fcst:
                parms.append(parm)
                
        #
        #  loop over the mutable parms.
        #  get:  wxType - type of parm
        #        WEname - short parm name string
        #        parmlevel - parm level string
        #
        for WEname, parmlevel, dbId in parms:
            # Another AWIPS1 derivation: Use of different selectedParms()
            # call forces us to retrieve Parm to retrieve some of these 
            # pieces of information
            #
            # rateParm = parm.getGridInfo().isRateParm()
            # wxType = parm.getGridInfo().getGridType().toString()
            # WEname = parm.getGridInfo().getParmID().getParmName()
            # parmlevel = parm.getGridInfo().getParmID().getParmLevel()
            parm = self.getParm(dbId, WEname, parmlevel)
            rateParm = parm.getGridInfo().isRateParm()
            wxType = parm.getGridInfo().getGridType().toString()
            del parm
            
            #
            #  Get list of grids for this parm within the selcted time range
            #  and loop over each of those grids
            #
            gridinfos=self.getGridInfo(fcst,WEname,parmlevel,selectTR)
            for gridinfo in gridinfos:
                GridTimeRange=gridinfo.gridTime()
                #
                #  Easier when just a scalar
                #
                if 'SCALAR'==wxType:
                    #
                    #  read each 'model' grid with a non-zero weight
                    #  add up the weights again, because we cannot count
                    #  weights for grids that cannot be read.
                    #
                    gsum=self._empty.copy()
                    totweight=0
                    fcstweight=0
                    oldgrid=self.getGrids(self.dbIds[0],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                    if oldgrid==None:
                        self.statusBarMsg("ModelBlend tool could not get Fcst data for " + WEName,"A")
                    for num, label in enumerate(self.labels):
                        weight=weights[num]
                        if weight!=0:
                            modeType="TimeWtAverage"
                            if rateParm==1:
                                modeType="Sum"
                            #determine source - special if from ISC
                            idx = label.find("(")
                            idx1 = label.find(")",idx)
                            if idx == -1 or idx1 == -1:
                                WEnameSource = WEname
                            else:
                                ot = label[idx+1:idx1]
                                if ot == self.myOfficeType():
                                    WEnameSource = WEname
                                else:
                                    WEnameSource = WEname + ot
                            grid=self.getGrids(self.dbIds[num],WEnameSource,"SFC",GridTimeRange,mode=modeType,noDataError=0,cache=0)
                            if grid != None:
                                gsum+=(grid*weight)
                                totweight+=weight
                                if (num==0):
                                    fcstweight=weight
                            else:
                                errorstring="ModelBlend tool could not get data for %s" % label
                                self.statusBarMsg(errorstring,"A")
                    #
                    #  Check again for no weights, or only weights for the current
                    #  grid - in which case we make no changes and write info message
                    #  otherwise - save the grid
                    #
                    if (totweight!=0):
                        if fcstweight==totweight:
                            self.statusBarMsg("ModelBlend makes no change","R")
                        else:    
                            newgrid=gsum/totweight
                            finalgrid=self.inEditArea(newgrid,oldgrid,EdgeType,EdgeWidth)
                            self.createGrid(fcst,WEname,wxType,finalgrid,GridTimeRange)
                    else:            
                        self.statusBarMsg("ModelBlend weights ended up Zero - so cancelled","A")
                #
                #  A little more complicated when a vector
                #
                if 'VECTOR'==wxType:
                    #
                    #  read each 'model' grid with a non-zero weight
                    #  add up the weights again, because we cannot count
                    #  weights for grids that cannot be read.
                    #
                    oldgrid=self.getGrids(dbIds[0],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                    if oldgrid==None:
                        self.statusBarMsg("ModelBlend tool could not get Fcst data for " + WEName,"A")
                    (mag,direc)=oldgrid
                    (uold,vold)=self.MagDirToUV(mag,direc)
                    
                    usum=self._empty.copy()
                    vsum=self._empty.copy()
                    
                    totweight=0
                    fcstweight=0
                    for num, weight in enumerate(weights):
                        if weight!=0:
                            grid=self.getGrids(self.dbIds[num],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                            if grid != None:
                                (mag,direc)=grid
                                (u,v)=self.MagDirToUV(mag,direc)
                                usum+=(u*weight)
                                vsum+=(v*weight)
                                totweight+=weight
                                if (num==0):
                                    fcstweight=weight
                            else:
                                errorstring="ModelBlend tool could not get data for %s" % self.labels[num]
                                self.statusBarMsg(errorstring,"A")
                    #
                    #  Check again for no weights, or only weights for the current
                    #  grid - in which case we make no changes and write info message
                    #  otherwise - save the grid.
                    #
                    if (totweight!=0):
                        if fcstweight==totweight:
                            self.statusBarMsg("ModelBlend makes no change","R")
                        else:
                            unew=usum/totweight
                            vnew=vsum/totweight
                            ufinal=self.inEditArea(unew,uold,EdgeType,EdgeWidth)
                            vfinal=self.inEditArea(vnew,vold,EdgeType,EdgeWidth)
                            result=self.UVToMagDir(ufinal,vfinal)
                            self.createGrid(fcst,WEname,wxType,result,GridTimeRange)
                            #self.callSmartTool("DoNothing",WEname,None,GridTimeRange)
                    else:            
                        self.statusBarMsg("ModelBlend weights ended up Zero - so cancelled","A")

    #=====================================================================
    #  inEditArea - Take an old grid and a new grid - and return the
    #               grid with the proper weighting between the two.
    #
    #               This is where the EdgeType and EdgeWidth of the dialog
    #               box gets used.  If there are no points in the current
    #               edit area - then we assume they want the entire domain.
    #               Otherwise we use the current edit area.  If FLAT is
    #               used, then the new grid is returned in the edit area
    #               and the old grid is returned outside the edit area.
    #               If EDGE or TAPER are used - then we nudge areas inside
    #               the edit area toward the new grid - based on how close
    #               it is to the edge of the edit are.
    #
    #               Returns the final grid that should be returned.
    #
    def inEditArea(self,new,old,EdgeType,EdgeWidth):
        #
        #  Get the active editarea
        #
        editArea=self.getActiveEditArea()
        #
        #  We don't have the benefit of the usual GFE question about what
        #  to do with empty edit areas.  We assume they want to run it over
        #  the entire domain - but have to switch the edit area ourselves
        #
        editAreaMask=editArea.getGrid()
        if not editAreaMask.isAnyBitsSet():
            editArea.invert()
        #
        #  Make edgegrid 0-1 across edit area
        #
        if (EdgeType=="Flat"):
           edgegrid=editArea.getGrid().__numpy__[0]
        elif (EdgeType=="Edge"):
           edgegrid=self.taperGrid(editArea,EdgeWidth)
        else:
           edgegrid=self.taperGrid(editArea,0)
        #
        #  return the final grid
        #
        diff=new-old
        final=old+(diff*edgegrid)
        return(final)
