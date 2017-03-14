##
#
# nwpsTrkngCG0MHX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0MHX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0MHXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0MHX", "nwpsTrkngCG0MHX")

def main():
    forecaster = nwpsTrkngCG0MHXForecaster()
    forecaster.run()
    forecaster.notifyGFE('MHX')


if __name__ == "__main__":
    main()
