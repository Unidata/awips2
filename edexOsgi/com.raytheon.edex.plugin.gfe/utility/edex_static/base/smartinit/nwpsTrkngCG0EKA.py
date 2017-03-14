##
#
# nwpsTrkngCG0EKA - Joe Maloney  2016-07-08
#
#   Smart Init for nwpsTrkngCG0EKA model.
#
##

from Init import *
from nwpsTrkngCG0 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsTrkngCG0 model
## output.
##--------------------------------------------------------------------------
class nwpsTrkngCG0EKAForecaster(nwpsTrkngCG0Forecaster):
    def __init__(self):
        nwpsTrkngCG0Forecaster.__init__(self, "nwpsTrkngCG0EKA", "nwpsTrkngCG0EKA")

def main():
    forecaster = nwpsTrkngCG0EKAForecaster()
    forecaster.run()
    forecaster.notifyGFE('EKA')


if __name__ == "__main__":
    main()
