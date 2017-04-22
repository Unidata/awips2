##
#
# nwpsTrkngCG0LCH - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0LCH model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0LCHForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0LCH", "nwpsTrkngCG0LCH")

def main():
    forecaster = nwpsTrkngCG0LCHForecaster()
    forecaster.run()
    forecaster.notifyGFE('LCH')


if __name__ == "__main__":
    main()
