##
#
# nwpsTrkngCG0SGX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0SGX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0SGXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0SGX", "nwpsTrkngCG0SGX")

def main():
    forecaster = nwpsTrkngCG0SGXForecaster()
    forecaster.run()
    forecaster.notifyGFE('SGX')


if __name__ == "__main__":
    main()
