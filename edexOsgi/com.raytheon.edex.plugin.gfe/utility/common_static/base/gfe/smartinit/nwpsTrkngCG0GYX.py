##
#
# nwpsTrkngCG0GYX - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0GYX model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0GYXForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0GYX", "nwpsTrkngCG0GYX")

def main():
    forecaster = nwpsTrkngCG0GYXForecaster()
    forecaster.run()
    forecaster.notifyGFE('GYX')


if __name__ == "__main__":
    main()
