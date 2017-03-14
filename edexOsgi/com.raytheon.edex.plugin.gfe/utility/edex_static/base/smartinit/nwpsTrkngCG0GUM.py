##
#
# nwpsTrkngCG0GUM - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0GUM model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0GUMForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0GUM", "nwpsTrkngCG0GUM")

def main():
    forecaster = nwpsTrkngCG0GUMForecaster()
    forecaster.run()
    forecaster.notifyGFE('GUM')


if __name__ == "__main__":
    main()
