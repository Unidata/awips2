##
#
# nwpsTrkngCG0AJK - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0AJK model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0AJKForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0AJK", "nwpsTrkngCG0AJK")

def main():
    forecaster = nwpsTrkngCG0AJKForecaster()
    forecaster.run()
    forecaster.notifyGFE('AJK')


if __name__ == "__main__":
    main()
