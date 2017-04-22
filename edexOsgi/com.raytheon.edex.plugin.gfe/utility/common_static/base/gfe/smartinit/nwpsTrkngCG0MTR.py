##
#
# nwpsTrkngCG0MTR - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0MTR model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0MTRForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0MTR", "nwpsTrkngCG0MTR")

def main():
    forecaster = nwpsTrkngCG0MTRForecaster()
    forecaster.run()
    forecaster.notifyGFE('MTR')


if __name__ == "__main__":
    main()
