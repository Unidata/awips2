##
#
# nwpsTrkngCG0MFL - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0MFL model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0MFLForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0MFL", "nwpsTrkngCG0MFL")

def main():
    forecaster = nwpsTrkngCG0MFLForecaster()
    forecaster.run()
    forecaster.notifyGFE('MFL')


if __name__ == "__main__":
    main()
