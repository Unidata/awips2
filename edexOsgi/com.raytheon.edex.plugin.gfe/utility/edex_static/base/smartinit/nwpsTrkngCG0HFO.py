##
#
# nwpsTrkngCG0HFO - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0HFO model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0HFOForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0HFO", "nwpsTrkngCG0HFO")

def main():
    forecaster = nwpsTrkngCG0HFOForecaster()
    forecaster.run()
    forecaster.notifyGFE('HFO')


if __name__ == "__main__":
    main()
