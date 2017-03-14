##
#
# nwpsTrkngCG0ALU - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0ALU model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0ALUForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0ALU", "nwpsTrkngCG0ALU")

def main():
    forecaster = nwpsTrkngCG0ALUForecaster()
    forecaster.run()
    forecaster.notifyGFE('ALU')


if __name__ == "__main__":
    main()
