##
#
# nwpsCG1HFO - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1HFO model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1HFOForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1HFO", "nwpsCG1HFO")

def main():
    forecaster = nwpsCG1HFOForecaster()
    forecaster.run()
    forecaster.notifyGFE('HFO')

if __name__ == "__main__":
    main()
