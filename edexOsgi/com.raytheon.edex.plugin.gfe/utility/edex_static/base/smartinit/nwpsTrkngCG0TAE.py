##
#
# nwpsTrkngCG0TAE - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0TAE model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0TAEForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0TAE", "nwpsTrkngCG0TAE")

def main():
    forecaster = nwpsTrkngCG0TAEForecaster()
    forecaster.run()
    forecaster.notifyGFE('TAE')


if __name__ == "__main__":
    main()
