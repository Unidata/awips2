##
#
# nwpsCG1CAR - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1CAR model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1CARForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1CAR", "nwpsCG1CAR")

def main():
    forecaster = nwpsCG1CARForecaster()
    forecaster.run()
    forecaster.notifyGFE('CAR')

if __name__ == "__main__":
    main()
