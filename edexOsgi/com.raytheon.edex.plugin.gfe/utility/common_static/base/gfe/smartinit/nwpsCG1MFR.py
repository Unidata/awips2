##
#
# nwpsCG1MFR - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1MFR model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1MFRForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1MFR", "nwpsCG1MFR")

def main():
    forecaster = nwpsCG1MFRForecaster()
    forecaster.run()
    forecaster.notifyGFE('MFR')

if __name__ == "__main__":
    main()
