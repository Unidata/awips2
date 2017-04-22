##
#
# nwpsCG1TAE - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1TAE model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1TAEForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1TAE", "nwpsCG1TAE")

def main():
    forecaster = nwpsCG1TAEForecaster()
    forecaster.run()
    forecaster.notifyGFE('TAE')

if __name__ == "__main__":
    main()
