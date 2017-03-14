##
#
# nwpsCG1AKQ - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1AKQ model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1AKQForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1AKQ", "nwpsCG1AKQ")

def main():
    forecaster = nwpsCG1AKQForecaster()
    forecaster.run()
    forecaster.notifyGFE('AKQ')

if __name__ == "__main__":
    main()
