##
#
# nwpsCG1MOB - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1MOB model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1MOBForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1MOB", "nwpsCG1MOB")

def main():
    forecaster = nwpsCG1MOBForecaster()
    forecaster.run()
    forecaster.notifyGFE('MOB')

if __name__ == "__main__":
    main()
