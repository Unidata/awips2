##
#
# nwpsCG1LIX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1LIX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1LIXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1LIX", "nwpsCG1LIX")

def main():
    forecaster = nwpsCG1LIXForecaster()
    forecaster.run()
    forecaster.notifyGFE('LIX')

if __name__ == "__main__":
    main()
