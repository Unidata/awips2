##
#
# nwpsCG1AFG - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1AFG model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1AFGForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1AFG", "nwpsCG1AFG")

def main():
    forecaster = nwpsCG1AFGForecaster()
    forecaster.run()
    forecaster.notifyGFE('AFG')

if __name__ == "__main__":
    main()
