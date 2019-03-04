##
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Utility.py
# Class which takes a dbSubsystem or an ifpClient
#    and performs various utility functions.
#
# Author: hansen
# ----------------------------------------------------------------------------

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

import sys, traceback
import logging

class Utility:

    def __init__(self, dbSubsystem, root, ifpClient=None):
        # Utility class --
        # Provides various methods used across diverse classes
        # Can work with a full dbSubsystem or just an ifpClient

        self.__dbSubsystem = dbSubsystem
        self.__invMgr = self.__msgHand = None
        if dbSubsystem is not None:
            self.__invMgr = dbSubsystem.dataManager()
            self.__msgHand = dbSubsystem.msgHandler()
        elif ifpClient is not None:
            self.__invMgr = ifpClient
        self.__root = root
        self.__ifpClient = ifpClient
        self.log = logging.getLogger("FormatterRunner.Utility.Utility")

    def getTextInventory(self, category):
        #  Get the text products from the server
        if self.__invMgr is None:
            self.handleError("Utility: No Access to Text inventory")
            return None
        return self.__invMgr.getTextInventory(category)

    def module(self, moduleName, showError=1):        
        # Return the module with the given name
        try:
            #if sys.modules.has_key(moduleName):                
            #  del sys.modules[moduleName]          
            module = __import__(moduleName)        
        except:
            if showError:
                self.handleError("Problem importing module: " + moduleName, tracebackFlag=1)
            else:
                self.log.error("Problem importing module: " + moduleName + " " + \
                                traceback.format_exc())
            return None
        return module

    def set(self, dict, value, default=None):
        # Try to find the value in the dictionary
        try:
            val = dict[value]
        except:
            val = default
        return val

    def handleError(self, errorMsg, tracebackFlag=0):
        type, value, tb = sys.exc_info()
        if type is None or tracebackFlag == 0:
            message = errorMsg
        else:
            sys.last_type = type
            sys.last_value = value
            sys.last_traceback = tb
            tblist = traceback.extract_tb(tb)
            del tblist[:1]
            list = traceback.format_list(tblist)
            if list:
                list.insert(0, "\nTraceback (innermost last):\n")
            list[len(list):] = traceback.format_exception_only(type, value)
            message = errorMsg + "--\n"
            for item in list:
                message = message + item
        if self.__msgHand is not None:
            AFPS.UserAlertMsg_send_mh(self.__msgHand, message, "S", "GFE")
        PyErrorDialog.PyErrorDialog(message=message)

    def removeDups(self, list):
        # Return a list that has removed duplicates from the original
        # and preserves the order
        # Sort the list
        sortedList = list
        sortedList.sort()
        curItem = None
        # Create new list without duplicates
        reducedList = []
        for item in sortedList:
            if curItem is None or not item == curItem:
                reducedList.append(item)
                curItem = item
        # Go through original list eliminating duplicates,
        # but preserving the order
        orderedList = []
        for item in list:
            if item in reducedList:
                orderedList.append(item)
                reducedList.remove(item)
        return orderedList

    def findInImported(self, varName):
        # Look for the given variable within modules that
        #   are already imported
        for key in sys.modules.keys():
            module = sys.modules[key]
            try:
                var = getattr(module, varName)
                return var, module
            except:
                continue
        return None, None
