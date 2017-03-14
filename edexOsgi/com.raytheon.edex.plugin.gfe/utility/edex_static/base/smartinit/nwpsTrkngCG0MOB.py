##
#
# nwpsTrkngCG0MOB - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0MOB model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0MOBForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0MOB", "nwpsTrkngCG0MOB")

def main():
    forecaster = nwpsTrkngCG0MOBForecaster()
    forecaster.run()
    forecaster.notifyGFE('MOB')


if __name__ == "__main__":
    main()
