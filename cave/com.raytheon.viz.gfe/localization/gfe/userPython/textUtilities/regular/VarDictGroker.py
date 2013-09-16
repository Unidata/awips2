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
import ProcessVariableList, types

class VarDictGroker:
    def __init__(self, module, definition, name, issuedBy=None, parent=None):
        self._module = module
        self._definition = definition
        self._name = name
        self._issuedBy = issuedBy
        self._parent = parent

    def getVarDict(self):
        # Create and display the User Dialog for this product.
        #
        # If the product has a _processVariableList method,
        #   use it to get the varDict
        # Otherwise,
        #   Create the User Dialog entries from the VariableList (if present)
        #   and the _issuanceList (if present)
        # Return a text string command line argument for the User Responses
        # Return None if user cancelled

        varDict = {}

        # Look for method _processVariableList
        try: # Try to instantiate the smart text product
            product = self._module.TextProduct()
            processVariableList = product._processVariableList
        except:  # Simple table product
            processVariableList = None
            product = None
                
        if processVariableList is not None:
            print "processVariableList is not None"
            co = processVariableList.im_func.func_code
            if co.co_argcount > 2:
                argValues = [self._definition, self._parent]
            else:
                argValues = [self._definition]

            import Tkinter,sys
            sys.argv = ["FormatterRunner"]
            root=Tkinter.Tk()
            root.title("FormatterRunner")
            root.withdraw()

            varDict = processVariableList(*argValues)            

            root.destroy()

            if varDict is None:
                return None

        else:

            # Get _issuanceList entries for the User Dialog
            print "TextProduct()"
            try: # Try to instantiate the smart text product
                product = self._module.TextProduct()
                product._definition = product.Definition
            except:  # Simple table product
                product = None
            
            varList = []
            try: # Try to find an issuance list in the smart text product
                issuanceList = product._issuance_list({})
                if len(issuanceList) > 0:
                    issuanceEntry = self._createVariableListEntry(
                        ("Product Issuance", "productIssuance"), issuanceList)
                    varList = [issuanceEntry]
            except:
                pass

            # Add in module VariableList
            try:  # Try to find a VariableList in the smart text product
                productVarList = product.VariableList
                varList += productVarList
            except:
                pass

            # Look for "runTime" variables in the Definition
            varList = self._addRunTimeVariables(varList, self._definition)            

            if varList is not None and len(varList):
                # Display User Dialog
                print "ProcessVariableList.ProcessVariableList"
                processVarList = ProcessVariableList.ProcessVariableList(
                                                                         self._name, varList, varDict={}, parent=self._parent)                
                self._selectionStatus = processVarList.status()
                if not self._selectionStatus == "OK":
                    return None   # User Cancelled
                varDict = processVarList.varDict()

        # Add "issuedBy" to the varDict if applicable
        varDict[("Issued By","issuedBy")] = self._issuedBy

        # Return a text string version of the User Responses (varDict)
        return str(varDict)

    def _createVariableListEntry(self, entryName, entryList, entryType="radio",
                                 default=""):
        # Create a VariableList entry for the entryList
        # and the given entryType and default value
        # "entryList" is a list of items for a radio or checkbutton
        # VariableList entry.  If entryList items are tuples, the first
        # value of the tuple is used.
        if len(entryList) == 1:
            # Do not need to put in the dialog
            return None
        entries = []
        for entry in entryList:
            if type(entry) is types.TupleType:
                entry = entry[0]
            entries.append(entry)
        result = (entryName, default, entryType, entries)
        return result

    def _addRunTimeVariables(self, varList, definition):
        # Add entries to varList to solicit variables designated as
        # "runTime" in the product definition

        for item, default, rtItem, varType in [
            ("language","english","runTimeLanguage", ["radio",
               ["english","french","spanish"]]),
            ("outputFile","/tmp/forecast.out","runTimeOutputFile",
               ["alphaNumeric"]),
            ("appendFile",None,"runTimeAppendFile", ["alphaNumeric"]),
            ("lineLength",69,"runTimeLineLength",["alphaNumeric"]),
            ("timePeriod",3, "runTimePeriod", ["alphaNumeric"]),
            ]:
            value = definition.setdefault(item, default)
            rtValue = definition.setdefault(item, default)
            if rtValue == "yes":
                varEntry = [item, value] + varType
                varEntry = tuple(varEntry)
                varList.append(varEntry)

        # Handle Edit Areas
        rtEditAreas = definition.setdefault("runTimeEditAreas", "no")
        if rtEditAreas == "yes":
            # Add an entry to varList to solicit edit areas at run time
            # Use the defaultEdit areas as choices
            dfEditAreas = definition.setdefault("defaultEditAreas", [])
            editAreaList = []
            for name, label in dfEditAreas:
                if type(name) == types.TupleType:
                    # use label instead of lat/lon values
                    editAreaList.append(label)
                else:
                    editAreaList.append(name)
            varList.append(
                ("Choose Edit Areas", [],"check", editAreaList))

        # Handle Time Ranges
        rtRanges = definition.setdefault("runTimeRanges", "no")
        if rtRanges == "yes":
            # Add an entry to varList to solicit time ranges at run time
            # Use the defaultRanges as choices
            dfRanges = definition.setdefault("defaultRanges", [])
            varList.append(
                    ("Choose Time Ranges", [],"check", dfRanges))

        return varList
