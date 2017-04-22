##
#
# nwpsTrkngCG0PQR - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0PQR model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0PQRForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0PQR", "nwpsTrkngCG0PQR")

def main():
    forecaster = nwpsTrkngCG0PQRForecaster()
    forecaster.run()
    forecaster.notifyGFE('PQR')


if __name__ == "__main__":
    main()
