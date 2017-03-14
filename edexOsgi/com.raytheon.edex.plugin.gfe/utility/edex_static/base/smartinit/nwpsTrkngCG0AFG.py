##
#
# nwpsTrkngCG0AFG - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0AFG model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0AFGForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0AFG", "nwpsTrkngCG0AFG")

def main():
    forecaster = nwpsTrkngCG0AFGForecaster()
    forecaster.run()
    forecaster.notifyGFE('AFG')


if __name__ == "__main__":
    main()
