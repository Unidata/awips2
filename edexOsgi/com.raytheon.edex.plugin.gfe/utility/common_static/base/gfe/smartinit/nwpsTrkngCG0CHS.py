##
#
# nwpsTrkngCG0CHS - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0CHS model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0CHSForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0CHS", "nwpsTrkngCG0CHS")

def main():
    forecaster = nwpsTrkngCG0CHSForecaster()
    forecaster.run()
    forecaster.notifyGFE('CHS')


if __name__ == "__main__":
    main()
