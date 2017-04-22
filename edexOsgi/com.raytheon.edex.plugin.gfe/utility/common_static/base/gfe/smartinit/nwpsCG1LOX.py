##
#
# nwpsCG1LOX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1LOX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1LOXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1LOX", "nwpsCG1LOX")

def main():
    forecaster = nwpsCG1LOXForecaster()
    forecaster.run()
    forecaster.notifyGFE('LOX')

if __name__ == "__main__":
    main()
