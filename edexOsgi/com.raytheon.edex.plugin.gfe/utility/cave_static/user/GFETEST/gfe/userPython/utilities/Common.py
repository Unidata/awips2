##
##

import SmartScript

class Common(SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Include your utility methods here
    def _convertFtToM(self, value):
        print "Using Utility Version of convertFtToM"
        return value/3.28084


