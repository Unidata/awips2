##
#
# nwpsTrkngCG0HGX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0HGX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0HGXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0HGX", "nwpsTrkngCG0HGX")

def main():
    forecaster = nwpsTrkngCG0HGXForecaster()
    forecaster.run()
    forecaster.notifyGFE('HGX')


if __name__ == "__main__":
    main()
