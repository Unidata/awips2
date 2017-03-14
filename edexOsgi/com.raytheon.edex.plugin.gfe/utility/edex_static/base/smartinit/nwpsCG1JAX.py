##
#
# nwpsCG1JAX - Joe Maloney  2016-07-18
#
#   Smart Init for nwpsCG1JAX model.
#
##

from Init import *
from nwpsCG1 import *

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from nwpsCG1 model
## output.
##--------------------------------------------------------------------------
class nwpsCG1JAXForecaster(nwpsCG1Forecaster):
    def __init__(self):
        nwpsCG1Forecaster.__init__(self, "nwpsCG1JAX", "nwpsCG1JAX")

def main():
    forecaster = nwpsCG1JAXForecaster()
    forecaster.run()
    forecaster.notifyGFE('JAX')

if __name__ == "__main__":
    main()
