##
#
# nwpsCG1TBW - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1TBW model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1TBWForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1TBW", "nwpsCG1TBW")

def main():
    forecaster = nwpsCG1TBWForecaster()
    forecaster.run()
    forecaster.notifyGFE('TBW')

if __name__ == "__main__":
    main()
