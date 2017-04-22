##
#
# nwpsTrkngCG0ILM - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0ILM model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0ILMForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0ILM", "nwpsTrkngCG0ILM")

def main():
    forecaster = nwpsTrkngCG0ILMForecaster()
    forecaster.run()
    forecaster.notifyGFE('ILM')


if __name__ == "__main__":
    main()
