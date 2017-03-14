##
#
# nwpsTrkngCG0OKX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0OKX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0OKXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0OKX", "nwpsTrkngCG0OKX")

def main():
    forecaster = nwpsTrkngCG0OKXForecaster()
    forecaster.run()
    forecaster.notifyGFE('OKX')


if __name__ == "__main__":
    main()
