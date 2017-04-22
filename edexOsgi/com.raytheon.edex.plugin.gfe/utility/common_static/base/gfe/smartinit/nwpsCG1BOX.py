##
#
# nwpsCG1BOX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1BOX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1BOXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1BOX", "nwpsCG1BOX")

def main():
    forecaster = nwpsCG1BOXForecaster()
    forecaster.run()
    forecaster.notifyGFE('BOX')

if __name__ == "__main__":
    main()
