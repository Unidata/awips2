##
#
# nwpsTrkngCG0JAX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0JAX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0JAXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0JAX", "nwpsTrkngCG0JAX")

def main():
    forecaster = nwpsTrkngCG0JAXForecaster()
    forecaster.run()
    forecaster.notifyGFE('JAX')


if __name__ == "__main__":
    main()
