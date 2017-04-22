##
#
# nwpsCG1AJK - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1AJK model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1AJKForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1AJK", "nwpsCG1AJK")

def main():
    forecaster = nwpsCG1AJKForecaster()
    forecaster.run()
    forecaster.notifyGFE('AJK')

if __name__ == "__main__":
    main()
