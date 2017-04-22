##
#
# nwpsCG1EKA - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1EKA model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1EKAForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1EKA", "nwpsCG1EKA")

def main():
    forecaster = nwpsCG1EKAForecaster()
    forecaster.run()
    forecaster.notifyGFE('EKA')

if __name__ == "__main__":
    main()
