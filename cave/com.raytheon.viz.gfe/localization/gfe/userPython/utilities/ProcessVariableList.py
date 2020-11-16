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
#
# Python interface to the Java runtime UI SWT component.
# 
# This interface provides a link between user created python scripts and the Java driven user interface.
# This script is to be called from within Java through a jep interface.
#
# If an invalid widget type is encountered a Label widget is generated instead with a tooltip explaining what went wrong.
#
# <pre>
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# Jun 11, 2008  1164     jelkins   Initial creation
# Jun 16, 2008  1164     jelkins   Implemented Callback Handling
# Jun 23, 2008  1164     jelkins   Support 3 argument widget tuple
# Nov 28, 2017  6540     randerso  Set default precision to 0 to 
#                                  match default resolution
# Jan 15, 2018  6684     randerso  Code restructured for improved 
#                                  maintainability/readability
# 
# </pre>
# 
# @author jelkins
##

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

from com.raytheon.viz.gfe.ui.runtimeui import ValuesDialog

import JUtil, DatabaseID, ParmID, TimeRange

from collections import namedtuple
#
# Field definition
# 
# name         name/description of field
#              Required
#
# defaultValue default/initial value
#              Required
#
# entType      type of field see Field Types below
#              Required
#
# valueList    list of values for buttons or min/max for scales
#              Default = []
#
# res          resolution/increment for scale fields
#              Default = 1.0
#
# prec         precision (number of fractional digits) for scale fields
#              Default = 0
#
# newRow       True if a new row should be started AFTER this field,
#              Default = True for button fields, False for other field types
#
#
# Field Types
#
# label
# numeric
# alphaNumeric
# compactText
# radio
# check
# scale
# scrollbar
# parm[Mutable]
# parms[Mutable]
# parm[Mutable]PlusVariable
# parms[Mutable]PlusVariable
# database
# databases
# D2D_model
# D2D_models
# model
# models
# refset
# refsets
# timeRange
# timeRanges
# map
# maps
# output file
# output directory
# startTime
# endTime
#
##
FieldDef = namedtuple("FieldDef", ['name', 'defaultValue','entType','valueList','res','prec','newRow'])
FieldDef.__new__.__defaults__ = ([], 1.0, 0, None)

class ProcessVariableList:
    
    ## Class constructor
    ##
    ## The following arguments are no longer used but remain part of the signature for compatibility reasons
    ##  * parent - not needed
    ##  * modal - similar functionality is achieved by setting or not setting a callback
    ##  * dataMgr - required for model and D2D_model widgets
    ##  * cmdLineVarDict - not needed, see TODO
    ##
    ##  TODO dbSubsytem may be needed when implementing the model and D2D_model widgets
    ##  
    ##  @param title: str() of the title of the dialog that will appear
    ##  @param varList: list() of widgets to to place on the dialog
    ##  @param varDict: dict() onto which to add widget values
    ##
    ##  @param runCB: function() pointer to a callback function which takes two arugments argList (see below) and varDict
    ##  @param argList: list() of arguments to pass to the callback function
    ## 
    def __init__(self, title, varList, varDict=None, parent=None,
                 dataMgr=None, modal=1, runCB=None, argList=[],
                 cmdLineVarDict=None):

        self.__varDict = varDict    
        if self.__varDict is None:
            self.__varDict = {}
        self.__callbackResult = None
        
        # build widgetList
        widgetList = buildWidgetList(varList)

        # Construct the dialog
        self.__dialog = ValuesDialog.openDialog(title, widgetList, dataMgr)
        #self.__dialog = ValuesDialog(title,widgetList)
        
        # since ValuesDialog blocks on open() we can set status and varDict here
        values = JUtil.javaMapToPyDict(self.__dialog.getValues(), self.__convertJavaObjToPyWrapper)
        
        self.__varDict.update(values)
        
        from com.raytheon.uf.viz.python.swt import ButtonConstant
        self.__selectionStatus = str(ButtonConstant.getButton(self.__dialog.getReturnCode()))
        
        # Handle callbacks
        #if runCB is not None:
        #    self.__callbackLoop(runCB,argList)
        #else:
        #    self.__dialog.open()
    
    ## The following methods may no longer be needed as they are mostly taken care of from within the ValuesDialog
    ## def __getCmdLineSelections(self, varList, cmdLineVarDict):
    ## def __getSelections(self, title, varList, varDict, parent):
    ## def selectionCB(self, entry, status="Ok"):
        
    # display UI until the user clicks the Cancel or Run/Dismiss button
    # when the user clicks the Run button evaluate the callback and keep the display
    #
    # TODO find a way to keep the dialog from noticably disappearing and re-appearing when the run button is selected
    #
    # @param callback: function() callback with signature (list(),dict())
    # @param callbackArguments: list() of arguments to pass to the callback
    def __callbackLoop(self,callback,callbackArguments):
        from com.raytheon.uf.viz.python.swt import ButtonConstant

        self.__dialog.setCloseAfterRun(True)
        self.__dialog.open()
            
        # poll for button press (can't use self.status() because it only returns Cancel, Ok
        button = self.status()

        # execute the callback
        if button == "Run" or button == "Run/Dismiss":
             self.__callbackResult = callback(callbackArguments,self.varDict())
        
        # break the cycle
        if button == "Cancel" or button == "Run/Dismiss":
            return
        
        # replace the initial widget values with the updated ones
        
        self.__callbackLoop(callback, callbackArguments)
    
    def __convertJavaObjToPyWrapper(self, javaObj):
        objtype = javaObj.java_name
        if objtype == "com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID":
            retVal = DatabaseID.DatabaseID(javaObj)
        elif objtype == "com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID":
            retVal = ParmID.ParmID(javaObj)
        elif objtype == "com.raytheon.uf.common.time.TimeRange":
            retVal = TimeRange.TimeRange(javaObj)
        else:
            retVal = None
        
        return retVal
    
    # @return: str() of the dialog button that was pressed
    def status(self):
        return self.__selectionStatus
    
    # @return: dict() of the values from the widgets
    def varDict(self): 
        return dict(self.__varDict)
    
    # @return: the result of the callback    
    def lastOkReturn(self):
        return self.__callbackResult

# build a Java list capable of being passed to the ValuesDialog
#
# @param widgetList: list() of widgets to add to the dialog
#
# @return: Java List of widgets
def buildWidgetList(pythonWidgetList):

    from java.util import ArrayList
    from com.raytheon.viz.gfe.smartscript import FieldDefinition
    FieldType = FieldDefinition.FieldType
    widgetList = ArrayList()
    
    for widgetDef in pythonWidgetList:
        
        if not isinstance(widgetDef, FieldDef):
            widgetDef = FieldDef(*(widgetDef))
        
        # Handle possibility of (label, variable) tuple
        if isinstance(widgetDef.name, tuple):
            desc = widgetDef.name[0] # label/description
        else:
            desc = widgetDef.name

        w = FieldDefinition(widgetDef.name, desc, 
                            FieldType.convertPythonType(widgetDef.entType),
                            widgetDef.defaultValue, 
                            widgetDef.valueList, 
                            float(widgetDef.res),int(widgetDef.prec),
                            widgetDef.newRow)
        
        
        widgetList.add(w)
        
    
    return widgetList

    