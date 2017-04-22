##
#
# nwpsCG1CHS - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1CHS model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1CHSForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1CHS", "nwpsCG1CHS")

def main():
    forecaster = nwpsCG1CHSForecaster()
    forecaster.run()
    forecaster.notifyGFE('CHS')

if __name__ == "__main__":
    main()
