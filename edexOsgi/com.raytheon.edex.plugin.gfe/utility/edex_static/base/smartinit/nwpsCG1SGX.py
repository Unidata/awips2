##
#
# nwpsCG1SGX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1SGX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1SGXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1SGX", "nwpsCG1SGX")

def main():
    forecaster = nwpsCG1SGXForecaster()
    forecaster.run()
    forecaster.notifyGFE('SGX')

if __name__ == "__main__":
    main()
