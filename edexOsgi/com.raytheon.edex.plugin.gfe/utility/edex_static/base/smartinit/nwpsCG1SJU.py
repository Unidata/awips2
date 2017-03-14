##
#
# nwpsCG1SJU - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1SJU model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1SJUForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1SJU", "nwpsCG1SJU")

def main():
    forecaster = nwpsCG1SJUForecaster()
    forecaster.run()
    forecaster.notifyGFE('SJU')

if __name__ == "__main__":
    main()
