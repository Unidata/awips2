##
##
##
# This is a class meant to act as a placeholder for a Java
# IGridSlice object in unit testing of HazardUtils.py.
# It has a single field, validTime, and a single method, getValidTime(),
# which returns whatever was set for the slice in the constructor
# or through direct manipulation of the slice field. 
class MockSlice(object):
    def __init__(self, validTime=None):
        self.validTime = validTime
        
    def getValidTime(self):
        return self.validTime
