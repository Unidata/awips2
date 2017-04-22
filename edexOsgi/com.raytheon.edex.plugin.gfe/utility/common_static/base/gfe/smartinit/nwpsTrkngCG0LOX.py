##
#
# nwpsTrkngCG0LOX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0LOX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0LOXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0LOX", "nwpsTrkngCG0LOX")

def main():
    forecaster = nwpsTrkngCG0LOXForecaster()
    forecaster.run()
    forecaster.notifyGFE('LOX')


if __name__ == "__main__":
    main()
