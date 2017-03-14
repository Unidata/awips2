##
#
# nwpsTrkngCG0AER - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0AER model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0AERForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0AER", "nwpsTrkngCG0AER")

def main():
    forecaster = nwpsTrkngCG0AERForecaster()
    forecaster.run()
    forecaster.notifyGFE('AER')


if __name__ == "__main__":
    main()
