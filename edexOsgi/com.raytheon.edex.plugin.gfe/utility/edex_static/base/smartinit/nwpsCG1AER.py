##
#
# nwpsCG1AER - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1AER model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1AERForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1AER", "nwpsCG1AER")

def main():
    forecaster = nwpsCG1AERForecaster()
    forecaster.run()
    forecaster.notifyGFE('AER')

if __name__ == "__main__":
    main()
