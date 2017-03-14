##
#
# nwpsTrkngCG0AKQ - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0AKQ model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0AKQForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0AKQ", "nwpsTrkngCG0AKQ")

def main():
    forecaster = nwpsTrkngCG0AKQForecaster()
    forecaster.run()
    forecaster.notifyGFE('AKQ')


if __name__ == "__main__":
    main()
