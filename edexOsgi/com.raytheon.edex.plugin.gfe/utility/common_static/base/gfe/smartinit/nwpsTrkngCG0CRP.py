##
#
# nwpsTrkngCG0CRP - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0CRP model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0CRPForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0CRP", "nwpsTrkngCG0CRP")

def main():
    forecaster = nwpsTrkngCG0CRPForecaster()
    forecaster.run()
    forecaster.notifyGFE('CRP')


if __name__ == "__main__":
    main()
