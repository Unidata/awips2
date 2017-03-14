##
#
# nwpsTrkngCG0MFR - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0MFR model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0MFRForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0MFR", "nwpsTrkngCG0MFR")

def main():
    forecaster = nwpsTrkngCG0MFRForecaster()
    forecaster.run()
    forecaster.notifyGFE('MFR')


if __name__ == "__main__":
    main()
