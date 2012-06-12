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

##
## Python interface to the Java runtime UI SWT component.
## 
## This interface provides a link between user created python scripts and the Java driven user interface.
## This script is to be called from within Java through a jep interface.
##
## If an invalid widget type is encountered a Label widget is generated instead with a tooltip explaining what went wrong.
##
## <pre>
## SOFTWARE HISTORY
## Date            Ticket#        Engineer    Description
## ------------    ----------    -----------    --------------------------
## Jun 11, 2008    1164           jelkins    Initial creation
## Jun 16, 2008    1164           jelkins    Implemented Callback Handling
## Jun 23, 2008    1164           jelkins    Support 3 argument widget tuple
## 
## </pre>
## 
## @author jelkins
## @version 1.0
## 
from com.raytheon.viz.gfe.ui.runtimeui import ValuesDialog
from com.raytheon.uf.common.dataplugin.gfe.db.objects import DatabaseID, ParmID
from com.raytheon.uf.common.time import TimeRange

import JUtil, DatabaseID, ParmID, TimeRange

import types

class ProcessVariableList:
    
    ## Class constructor
    ##
    ## The following arguments are no longer used but remain part of the signature for compatibility reasons
    ##  * parent - not needed
    ##  * modal - similar functionality is achieved by setting or not setting a callback
    ##  * dbSubsystem - not needed, see TODO
    ##  * cmdLineVarDict - not needed, see TODO
    ##
    ##  TODO evaluate cmdLineVarDict and determine if it is really not needed
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
                 dbSubsystem=None, modal=1, runCB=None, argList=[],
                 cmdLineVarDict=None):

        self.__varDict = varDict    
        if self.__varDict is None:
            self.__varDict = {}
        self.__callbackResult = None
        
        # build widgetList
        widgetList = buildWidgetList(varList)

        # Construct the dialog
        self.__dialog = ValuesDialog.openDialog(title, widgetList)
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
        objtype = javaObj.jclassname
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
    from com.raytheon.viz.gfe.smartscript import FieldDefinition_FieldType as FieldType
    widgetList = ArrayList()
    
    for widget in pythonWidgetList:
        
        res = 1.0   # Default resolution
        prec = 3    # Default precision
        valueList = []
        
        # unpack the tuple
        if len(widget) == 3:
            name,defaultValue,entType = widget
        if len(widget) == 4:        
            name,defaultValue,entType,valueList = widget 
        if len(widget) == 5:
            name,defaultValue,entType,valueList,res = widget
        if len(widget) == 6:
            name,defaultValue,entType,valueList,res,prec = widget
        # Handle possibility of (label, variable) tuple
        if type(name) is types.TupleType:
            desc = name[0]
        else:
            desc = name
                    
        w = FieldDefinition(JUtil.pyValToJavaObj(name),desc,FieldType.convertPythonType(entType),
                            JUtil.pyValToJavaObj(defaultValue),JUtil.pyValToJavaObj(valueList),
                            float(res),int(prec))
        widgetList.add(w)
        
    
    return widgetList

    