##
#
# nwpsCG1ALU - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1ALU model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1ALUForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1ALU", "nwpsCG1ALU")

def main():
    forecaster = nwpsCG1ALUForecaster()
    forecaster.run()
    forecaster.notifyGFE('ALU')

if __name__ == "__main__":
    main()
