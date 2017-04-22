##
#
# nwpsTrkngCG0LIX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0LIX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0LIXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0LIX", "nwpsTrkngCG0LIX")

def main():
    forecaster = nwpsTrkngCG0LIXForecaster()
    forecaster.run()
    forecaster.notifyGFE('LIX')


if __name__ == "__main__":
    main()
