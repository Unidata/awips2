##
#
# nwpsCG1LCH - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1LCH model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1LCHForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1LCH", "nwpsCG1LCH")

def main():
    forecaster = nwpsCG1LCHForecaster()
    forecaster.run()
    forecaster.notifyGFE('LCH')

if __name__ == "__main__":
    main()
