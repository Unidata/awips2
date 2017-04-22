##
#
# nwpsCG1HGX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1HGX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1HGXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1HGX", "nwpsCG1HGX")

def main():
    forecaster = nwpsCG1HGXForecaster()
    forecaster.run()
    forecaster.notifyGFE('HGX')

if __name__ == "__main__":
    main()
