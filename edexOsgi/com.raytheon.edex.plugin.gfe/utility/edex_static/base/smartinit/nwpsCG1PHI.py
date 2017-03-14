##
#
# nwpsCG1PHI - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1PHI model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1PHIForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1PHI", "nwpsCG1PHI")

def main():
    forecaster = nwpsCG1PHIForecaster()
    forecaster.run()
    forecaster.notifyGFE('PHI')

if __name__ == "__main__":
    main()
