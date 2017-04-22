##
#
# nwpsCG1OKX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1OKX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1OKXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1OKX", "nwpsCG1OKX")

def main():
    forecaster = nwpsCG1OKXForecaster()
    forecaster.run()
    forecaster.notifyGFE('OKX')

if __name__ == "__main__":
    main()
