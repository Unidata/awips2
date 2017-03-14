##
#
# nwpsCG1CRP - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1CRP model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1CRPForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1CRP", "nwpsCG1CRP")

def main():
    forecaster = nwpsCG1CRPForecaster()
    forecaster.run()
    forecaster.notifyGFE('CRP')

if __name__ == "__main__":
    main()
