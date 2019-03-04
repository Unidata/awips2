##
##
##
# This is a class meant to act as a placeholder for a Java
# IGridData object in unit testing of HazardUtils.py.
# It has a single field, slice, and a single method, getSlice(),
# which returns whatever was set for the slice in the constructor
# or through direct manipulation of the slice field. 
class MockGridData(object):
    "Crippled substitute for IGridData java class."
    
    def __init__(self, slice=None):
        "Constructor"
        self.slice = slice
        
    def getGridSlice(self):
        "Return the slice field."
        return self.slice
    