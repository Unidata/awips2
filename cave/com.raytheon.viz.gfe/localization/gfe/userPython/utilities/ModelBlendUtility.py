# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ModelBlendUtility - Version 20181114 
#
#  This Utility is a version of the ModelBlend SmartTool (Tim Barker, BOI)
#  which can be used directly within Procedure GUIs, which behave slightly
#  differently.  In this way the model weight dialog GUI does not have to
#  appear or be adjusted for each and every time step. It contains
#  modifications by Bill Goodman (OKX) and Matthew Belk (BOX). 
#
# Author:  Matthew H. Belk                  Created: 01/22/2010
#          WFO Taunton MA             Last Modified: 07/19/2016
# ----------------------------------------------------------------------------

import SmartScript
import tkinter, StartupDialog

#  Global variables for the dialog
#
DbaseIDs=[]
Labels=[]
ScaleIDs=[]
Weights=[]
Percents=[]
Edge="Flat"
EdgeWidth=5
#
#
class ModelBlendUtility(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)
        
    def preProcessGrid(self,WEname):
        #
        #  Setup the arrays of information for the dialog
        #  box that sets the weights
        #
        #  The mutable database is labelled "Forecast" no matter
        #  what you do, and put in the first slot on the dialog.
        #  The "Official" database is hardcoded into the next slot,
        #  then others are added
        #
        id=self.mutableID()
        DbaseIDs.append(id.modelIdentifier())
        Labels.append("Forecast:")
        place=tkinter.IntVar()
        place.set(0)
##        place.set(1)
        Weights.append(place)
        place=tkinter.StringVar()
        place.set("%4.4d%%"%100)
        Percents.append(place)
        #
        id=self.findDatabase("Official")
        DbaseIDs.append(id.modelIdentifier())
        Labels.append("Official:")
        place=tkinter.IntVar()
        place.set(0)
        Weights.append(place)
        place=tkinter.StringVar()
        place.set("%4.4d%%"%0)
        Percents.append(place)
        #
        num=2
        for modelstring in Models:
            pieces=modelstring.split(":")
            if ((len(pieces)<1)or(len(pieces)>3)):
                continue
            if len(pieces)<2:
                model=modelstring
                versions=1
                parmlist="ALL"
            elif len(pieces)<3:
                model=pieces[0]
                try:
                    versions=abs(int(pieces[1]))
                except:
                    versions=1
                parmlist="ALL"
            else:
                model=pieces[0]
                try:
                    versions=abs(int(pieces[1]))
                except:
                    versions=1
                parmlist=pieces[2]
            #
            #  Figure out if this parm is in the list of parms for
            #  this model database - if not, go on to next model
            #
            if parmlist!="ALL":
                if parmlist[0:1]=="^":
                    parms=parmlist[1:].split(",")
                    if WEname in parms:
                        continue
                else:
                    parms=parmlist.split(",")
                    if WEname not in parms:
                        continue
            #
            #  Make labels for each of the model runs we want.
            #  Singleton databases (like FCST or Official) that have
            #  no date (actually a 1970 date) have no date/run label.
            #
            for run in range(0,-versions,-1):
                db=self.findDatabase(model,run)
                id=db.modelIdentifier()
                if ((id is not "") and (id not in DbaseIDs)):
                    modtime=db.modelTime()
                    year=modtime.year()
                    month=modtime.month()
                    day=modtime.day()
                    hour=modtime.hour()
                    if year==1970:
                        Labels.append("%s:"%model)
                    else:
                        Labels.append("%s %2.2d/%2.2d %2.2dZ:" % \
                                      (model,month,day,hour))
                    DbaseIDs.append(id)
                    place=tkinter.IntVar()
                    place.set(0)
                    Weights.append(place)
                    place=tkinter.StringVar()
                    place.set("%4.4d%%"%0)
                    Percents.append(place)
                    num=num+1
        #
        #  Now run the dialog box to get the weights
        #  resulting weights stored in Weights array
        #
        self.dlg=ModelWeights(self.eaMgr().root(),"Set Model Weights",
                       callbackMethod=self.execWeights,modal=0)
        #
        #  Cancel the tool in the first pre-processGrid routine.
        #  No Execute routine is done - and grid is not marked as
        #  edited.  Any editing will take place when they press a
        #  Button on the dialog and it calls execWeights
        #
        self.cancel()
    #=================================================================
    #
    #  Dummy execute routine.  Tool is cancelled in preProcessGrid
    #  and all the real action is accomplished in execWeights which
    #  is called when the user presses a button on the dialog
    #
    def execute(self,variableElement):
        "Allows blending of any/all sources available for forecast fields."
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
        EdgeType=self.dlg.edgestyleString.get()
        EdgeWidth=self.dlg.edgeWidth.get()
##        VecType=self.dlg.vecstyleString.get()
##        BlendType=self.dlg.blendstyleString.get()
        BlendType="Weighted blend"
    
##        if VecType == "Add vector magnitude/direction separately":
        USE_VECTOR_ADDITION = 0
##        else:
##            USE_VECTOR_ADDITION = 1
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
        for num in range(len(Labels)):
            weight=Weights[num].get()
            if abs(weight)>0.5:
                someweights=1
            if num==0:
                fcstweight=weight
            else:
                otherweights=otherweights+weight
            totweight=totweight+weight
        if someweights==0:
            self.statusBarMsg("ModelBlend has no weights","R")
            return
        if (abs(fcstweight)>0.5)and(otherweights==0):
            self.statusBarMsg("ModelBlend Weights add to no change","R")
            return
        if totweight==0 and BlendType == "Weighted blend":
            self.statusBarMsg("Weights cannot add up to zero","A")
            return
        #
        #  Get stuff usually provided by tool code:
        #     fcst=mutable model database name
        #     selectTR=the selected timerange
        #
        fcst=self.mutableID().model()
        selectTR=self._dbss.dataManager().parmOp().selectionTimeRange()
        #
        #  get list of parms that are selected and mutable
        #
        allParms=self._dbss.dataManager().parmMgr().selectedParms()
        parms=[]
        for parm in allParms:
            model=parm.parmID().databaseID().model()
            if model==fcst:
                parms.append(parm)
        #
        #  loop over the mutable parms.
        #  get:  wxType - type of parm (0=scalar, 1=vector, 2=Wx, 3=discrete)
        #        WEname - short parm name string
        #        parmlevel - parm level string
        #
        for parm in parms:
            rateParm=parm.parmInfo().rateParm()
            wxType=parm.parmInfo().gridType()-1
            WEname=parm.parmInfo().parmId().parmName()
            parmlevel=parm.parmInfo().parmId().level()
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
                if wxType==0:  # SCALAR
                    #
                    #  read each 'model' grid with a non-zero weight
                    #  add up the weights again, because we cannot count
                    #  weights for grids that cannot be read.
                    #
                    gsum=self.empty()
                    totweight=0
                    fcstweight=0
                    oldgrid=self.getGrids(DbaseIDs[0],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                    if oldgrid is None:
                        self.statusBarMsg("ModelBlend tool could not get Fcst data for xxx","A")
                    for num in range(len(Labels)):
                        weight=Weights[num].get()
                        if weight!=0:
                            modeType="TimeWtAverage"
                            if rateParm==1:
                                modeType="Sum"
                            grid=self.getGrids(DbaseIDs[num],WEname,"SFC",GridTimeRange,mode=modeType,noDataError=0,cache=0)
                            if grid is not None:
                                if BlendType == "Max":
                                    if totweight == 0:
                                        gsum = grid
                                    else:
                                        gsum = maximum(gsum, grid)
                                elif BlendType == "Min":
                                    if totweight == 0:
                                        gsum = grid
                                    else:
                                        gsum = minimum(gsum, grid)
                                else:
                                    gsum = gsum + grid * weight
                                
                                totweight = totweight + weight
                                
                                if (num==0):
                                    fcstweight=weight
                            else:
                                errorstring="ModelBlend tool could not get data for %s" % Labels[num]
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
                            if BlendType == "Weighted blend":
                                newgrid=gsum/totweight
                            else:
                                newgrid=gsum
                            
                            finalgrid=self.inEditArea(newgrid,oldgrid,EdgeType,EdgeWidth)
                            self.createGrid(fcst,WEname,"SCALAR",finalgrid,GridTimeRange)
##                            self.callSmartTool("UpdateTime",WEname,None,GridTimeRange)
                    else:            
                        self.statusBarMsg("ModelBlend weights ended up Zero - so cancelled","A")
                #
                #  A little more complicated when a vector
                #
                if wxType==1:  # VECTOR
                    #
                    #  read each 'model' grid with a non-zero weight
                    #  add up the weights again, because we cannot count
                    #  weights for grids that cannot be read.
                    #
                    oldgrid=self.getGrids(DbaseIDs[0],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                    if oldgrid is None:
                        self.statusBarMsg("ModelBlend tool could not get Fcst data for xxx","A")
                    (mag,direc)=oldgrid

                    #  Added by MHB 08/03/2006
                    if USE_VECTOR_ADDITION:
                        (uold,vold)=self.MagDirToUV(mag,direc)
                    else:
                        mag1 = where(mag, 1.0, 1.0)
                        (uold,vold)=self.MagDirToUV(mag1,direc)
                    
                    magold=mag                      #  added by MHB
                    usum=self.empty()
                    vsum=self.empty()
                    magsum=self.empty()             #  added by MHB
                    totweight=0
                    fcstweight=0
                    for num in range(len(Labels)):
                        weight=Weights[num].get()
                        if weight!=0:
                            grid=self.getGrids(DbaseIDs[num],WEname,"SFC",GridTimeRange,noDataError=0,cache=0)
                            if grid is not None:
                                (mag,direc)=grid

                                #  Added by MHB 08/03/2006
                                if USE_VECTOR_ADDITION:
                                    (u,v)=self.MagDirToUV(mag,direc)
                                else:
                                    (u,v)=self.MagDirToUV(mag1,direc)
                                
                                usum=usum+(u*weight)
                                vsum=vsum+(v*weight)

                                #  Added by MHB 08/03/2006
                                if BlendType == "Max":
                                    if totweight == 0:
                                        magsum = mag
                                    else:
                                        magsum = maximum(magsum, mag)
                                elif BlendType == "Min":
                                    if totweight == 0:
                                        magsum = mag
                                    else:
                                        magsum = minimum(magsum, mag)
                                else:
                                    magsum=magsum + mag * weight
                                
                                totweight=totweight + weight
                                
                                if (num==0):
                                    fcstweight=weight
                            else:
                                errorstring="ModelBlend tool could not get data for %s" % Labels[num]
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
                            
                            #  Added by MHB 08/03/2006
                            if BlendType == "Weighted blend" and not USE_VECTOR_ADDITION:
                                magnew=magsum/totweight
                            else:
                                magnew=magsum
                            
                            ufinal=self.inEditArea(unew,uold,EdgeType,EdgeWidth)
                            vfinal=self.inEditArea(vnew,vold,EdgeType,EdgeWidth)
                            
                            #  Added by MHB 08/03/2006
                            if not USE_VECTOR_ADDITION:
                                magfinal=self.inEditArea(magnew,magold,EdgeType,EdgeWidth)
                                dirfinal=self.UVToMagDir(ufinal,vfinal)
                                newgrid=(magfinal, dirfinal[1])
                            else:
                                newgrid=self.UVToMagDir(ufinal,vfinal)
                                
                                if not BlendType == "Weighted blend":
                                    magfinal=self.inEditArea(magnew,magold,EdgeType,EdgeWidth)
                                    newgrid=(magfinal, newgrid[1])
                            
                            mode = self.getVectorEditMode()
                            
                            if mode == "Magnitude Only":
                                result = (newgrid[0], oldgrid[1])
                            elif mode == "Direction Only":
                                result = (oldgrid[0], newgrid[1])
                            else:
                                result = newgrid
                            
                            self.createGrid(fcst, WEname, "VECTOR", result, GridTimeRange)
##                            self.callSmartTool("UpdateTime",WEname,None,GridTimeRange)
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
        editAreaMask=editArea.pyGrid()
        npts=add.reduce(add.reduce(editAreaMask))
        if (npts==0):
            editArea.invert()
        #
        #  Make edgegrid 0-1 across edit area
        #
        if (EdgeType=="Flat"):
           edgegrid=editArea.pyGrid()
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


class ToolDialog(StartupDialog.Dialog):
    def __init__(self, parent=None, name="nonModal Dialog", callbackMethod=None,
                 modal=1):

        self.__parent = parent
        self.__name = name
        self.__modal = modal
        self.__callbackMethod = callbackMethod
        self.__dialog=IFPDialog.Dialog.__init__(self,
                                                parent=self.__parent,
                                                title=self.__name,
                                                modal=self.__modal)

    def buttonbox(self):
        buttonFrame = tkinter.Frame(self)
        # create the buttons associated with this dialog
        if self.__modal == 1:
            tkinter.Button(buttonFrame, text="Ok",
                command=self.__okCB, width=10, state=tkinter.NORMAL).pack(\
                side=tkinter.LEFT, pady=5, padx=10)
        else:
            tkinter.Button(buttonFrame, text="Run",
                command=self.__runCB, width=10, state=tkinter.NORMAL).pack(\
                side=tkinter.LEFT, pady=5, padx=10)
            tkinter.Button(buttonFrame, text="Run/Dismiss",
                command=self.__okCB, width=12, state=tkinter.NORMAL).pack(\
                side=tkinter.LEFT, pady=5, padx=10)
        tkinter.Button(buttonFrame, text="Cancel", width=10,
          command=self.cancelCB).pack(side=tkinter.RIGHT, pady=5, padx=10)
        buttonFrame.pack(side=tkinter.BOTTOM)

    def body(self, master):
        bodylabel=tkinter.Label(master,text="This is the body")
        bodylabel.pack(side=tkinter.TOP)

    def __runCB(self):
        self.__callbackMethod("Run")

    def __okCB(self):
        self.withdraw()
        self.__callbackMethod("OK")
        self.ok()

    def cancelCB(self):
        self.__callbackMethod("Cancel")
        self.cancel()

    def cancel(self):
        IFPDialog.Dialog.cancel(self)
#=======================================================================
#
#  Custom dialog that provides weight sliders for each model
#
class ModelWeights(ToolDialog):
    def setPercents(self,master):
        global Percents
        total=0
        for i in range(len(Weights)):
            total=total+Weights[i].get()
        if total==0:
            for i in range(len(Weights)):
                Percents[i].set("%4d%%"%0)
        else:
            for i in range(len(Weights)):
                Percents[i].set("%4d%%"%((float(Weights[i].get())/float(total))*100))

    def body(self,master):
        hull=tkinter.Frame(self)

        numpoints=len(Labels)
        numcols=(int((numpoints-1)/MAX_IN_COLUMN))+1
        numrows=(int((numpoints-1)/numcols))+1
        #
        #  grid of scale widgets
        #
        num=0
        for col in range(numcols):
            fc=tkinter.Frame(hull)
            for row in range(numrows):
                if (num>=len(Labels)):
                    continue
                lab=tkinter.Label(fc,text=Labels[num])
                lab.grid(row=row,column=0,sticky=tkinter.SE)
                if USE_NEGATIVE_WEIGHTS==0:
                    ScaleIDs.append(tkinter.Scale(fc,orient=tkinter.HORIZONTAL,
                                              from_=0,to=SCALE_MAX,resolution=SCALE_RES,
                                              command=self.setPercents,
                                              variable=Weights[num],length=150))
                else:
                    ScaleIDs.append(tkinter.Scale(fc,orient=tkinter.HORIZONTAL,
                                              from_=-SCALE_MAX,to=SCALE_MAX,resolution=SCALE_RES,
                                              command=self.setPercents,
                                              variable=Weights[num],length=150))                        
                #if Labels[num]=="Forecast:":
                #    ScaleIDs[num].set(1)
                #    Weights.append(1)
                #lse:
                #    ScaleIDs[num].set(0)
                #    Weights.append(0)
                ScaleIDs[num].grid(row=row,column=1,sticky=tkinter.SE)
                lab2=tkinter.Label(fc,textvariable=Percents[num],width=5)
                lab2.grid(row=row,column=2,sticky=tkinter.SE)
                if col!=(numcols-1):
                    f2=tkinter.Frame(fc,bg="black",width=1)
                    f2.grid(row=row,column=3,sticky=tkinter.NS)
                num=num+1
            fc.grid(row=0,column=col,sticky=tkinter.N)
        #
        #  edge effects
        #
        edgeFrame=tkinter.Frame(hull,relief=tkinter.GROOVE,borderwidth=2)
        edgestyleFrame=tkinter.Frame(edgeFrame)
        edgewidthFrame=tkinter.Frame(edgeFrame)
        edgestyleDefault="Flat"
        edgestyles=["Flat","Edge","Taper"]
        self.edgestyleString=tkinter.StringVar()
        for edgestyle in edgestyles:
            a=tkinter.Radiobutton(edgestyleFrame,text=edgestyle,
                                  variable=self.edgestyleString,value=edgestyle)
            if edgestyle is edgestyleDefault:
                a.invoke()
            a.pack(side=tkinter.TOP,anchor=tkinter.W)
        edgestyleFrame.pack(side=tkinter.LEFT,anchor=tkinter.W)
        self.edgeWidth=tkinter.IntVar()
        self.edgeWidth.set(5)
        a=tkinter.Scale(edgewidthFrame,from_=1,to=30,variable=self.edgeWidth,
                        showvalue=1,label="Edge Width:",orient=tkinter.HORIZONTAL)
        a.pack(side=tkinter.TOP,anchor=tkinter.N,fill=tkinter.X)
        edgewidthFrame.pack(side=tkinter.RIGHT,anchor=tkinter.W,fill=tkinter.X,expand=1)
        
        edgeFrame.grid(row=numrows,column=0,columnspan=numcols,sticky=tkinter.EW)

        hull.pack(side=tkinter.TOP,fill=tkinter.BOTH,expand=1)
        
        return

