##
#
# nwpsTrkngCG0CAR - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0CAR model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0CARForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0CAR", "nwpsTrkngCG0CAR")

def main():
    forecaster = nwpsTrkngCG0CARForecaster()
    forecaster.run()
    forecaster.notifyGFE('CAR')


if __name__ == "__main__":
    main()
