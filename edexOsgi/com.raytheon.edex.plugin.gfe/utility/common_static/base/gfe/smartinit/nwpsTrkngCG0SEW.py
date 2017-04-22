##
#
# nwpsTrkngCG0SEW - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0SEW model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0SEWForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0SEW", "nwpsTrkngCG0SEW")

def main():
    forecaster = nwpsTrkngCG0SEWForecaster()
    forecaster.run()
    forecaster.notifyGFE('SEW')


if __name__ == "__main__":
    main()
