##
#
# nwpsCG1PQR - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1PQR model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1PQRForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1PQR", "nwpsCG1PQR")

def main():
    forecaster = nwpsCG1PQRForecaster()
    forecaster.run()
    forecaster.notifyGFE('PQR')

if __name__ == "__main__":
    main()
