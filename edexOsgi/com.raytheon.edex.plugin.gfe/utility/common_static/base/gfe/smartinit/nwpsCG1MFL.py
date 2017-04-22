##
#
# nwpsCG1MFL - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1MFL model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1MFLForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1MFL", "nwpsCG1MFL")

def main():
    forecaster = nwpsCG1MFLForecaster()
    forecaster.run()
    forecaster.notifyGFE('MFL')

if __name__ == "__main__":
    main()
