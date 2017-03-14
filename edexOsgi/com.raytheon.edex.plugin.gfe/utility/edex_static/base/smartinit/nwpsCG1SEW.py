##
#
# nwpsCG1SEW - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1SEW model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1SEWForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1SEW", "nwpsCG1SEW")

def main():
    forecaster = nwpsCG1SEWForecaster()
    forecaster.run()
    forecaster.notifyGFE('SEW')

if __name__ == "__main__":
    main()
