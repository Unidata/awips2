##
#
# nwpsTrkngCG0BOX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0BOX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0BOXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0BOX", "nwpsTrkngCG0BOX")

def main():
    forecaster = nwpsTrkngCG0BOXForecaster()
    forecaster.run()
    forecaster.notifyGFE('BOX')


if __name__ == "__main__":
    main()
