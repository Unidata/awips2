##
#
# nwpsCG1MTR - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1MTR model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1MTRForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1MTR", "nwpsCG1MTR")

def main():
    forecaster = nwpsCG1MTRForecaster()
    forecaster.run()
    forecaster.notifyGFE('MTR')

if __name__ == "__main__":
    main()
