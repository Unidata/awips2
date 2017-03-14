##
#
# nwpsCG1MHX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1MHX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1MHXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1MHX", "nwpsCG1MHX")

def main():
    forecaster = nwpsCG1MHXForecaster()
    forecaster.run()
    forecaster.notifyGFE('MHX')

if __name__ == "__main__":
    main()
