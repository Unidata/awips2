##
#
# nwpsCG1BRO - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1BRO model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1BROForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1BRO", "nwpsCG1BRO")

def main():
    forecaster = nwpsCG1BROForecaster()
    forecaster.run()
    forecaster.notifyGFE('BRO')

if __name__ == "__main__":
    main()
