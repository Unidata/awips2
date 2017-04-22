##
#
# nwpsTrkngCG0KEY - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0KEY model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0KEYForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0KEY", "nwpsTrkngCG0KEY")

def main():
    forecaster = nwpsTrkngCG0KEYForecaster()
    forecaster.run()
    forecaster.notifyGFE('KEY')


if __name__ == "__main__":
    main()
