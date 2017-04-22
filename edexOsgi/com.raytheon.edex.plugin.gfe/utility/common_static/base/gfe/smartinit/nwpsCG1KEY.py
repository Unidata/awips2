##
#
# nwpsCG1KEY - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1KEY model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1KEYForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1KEY", "nwpsCG1KEY")

def main():
    forecaster = nwpsCG1KEYForecaster()
    forecaster.run()
    forecaster.notifyGFE('KEY')

if __name__ == "__main__":
    main()
