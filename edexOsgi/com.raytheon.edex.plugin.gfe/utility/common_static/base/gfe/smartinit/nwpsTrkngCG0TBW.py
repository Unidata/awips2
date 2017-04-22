##
#
# nwpsTrkngCG0TBW - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0TBW model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0TBWForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0TBW", "nwpsTrkngCG0TBW")

def main():
    forecaster = nwpsTrkngCG0TBWForecaster()
    forecaster.run()
    forecaster.notifyGFE('TBW')


if __name__ == "__main__":
    main()
