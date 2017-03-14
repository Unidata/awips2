##
#
# nwpsTrkngCG0PHI - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0PHI model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0PHIForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0PHI", "nwpsTrkngCG0PHI")

def main():
    forecaster = nwpsTrkngCG0PHIForecaster()
    forecaster.run()
    forecaster.notifyGFE('PHI')


if __name__ == "__main__":
    main()
