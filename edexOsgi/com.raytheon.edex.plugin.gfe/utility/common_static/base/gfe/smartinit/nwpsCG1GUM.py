##
#
# nwpsCG1GUM - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1GUM model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1GUMForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1GUM", "nwpsCG1GUM")

def main():
    forecaster = nwpsCG1GUMForecaster()
    forecaster.run()
    forecaster.notifyGFE('GUM')

if __name__ == "__main__":
    main()
