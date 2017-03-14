##
#
# nwpsTrkngCG0SJU - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0SJU model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0SJUForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0SJU", "nwpsTrkngCG0SJU")

def main():
    forecaster = nwpsTrkngCG0SJUForecaster()
    forecaster.run()
    forecaster.notifyGFE('SJU')


if __name__ == "__main__":
    main()
