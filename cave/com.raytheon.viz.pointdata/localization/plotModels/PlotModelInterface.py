##
##

##
# This is a base file that is not intended to be overridden.
##

class PlotDelegate(object):
    def __init__(self):
        self.__plotModelFactory = None
    
    def init(self, plotModelFactory):
        self.__plotModelFactory = None
    
    # Returns False if the plot for rec should be discarded
    def isValid(self, rec):
        return True
    
    def getSampleText(self, rec):
        return None
