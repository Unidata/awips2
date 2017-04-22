##
#
# nwpsCG1GYX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1GYX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1GYXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1GYX", "nwpsCG1GYX")

def main():
    forecaster = nwpsCG1GYXForecaster()
    forecaster.run()
    forecaster.notifyGFE('GYX')

if __name__ == "__main__":
    main()
