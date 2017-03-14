##
#
# nwpsTrkngCG0BRO - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0BRO model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0BROForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0BRO", "nwpsTrkngCG0BRO")

def main():
    forecaster = nwpsTrkngCG0BROForecaster()
    forecaster.run()
    forecaster.notifyGFE('BRO')


if __name__ == "__main__":
    main()
