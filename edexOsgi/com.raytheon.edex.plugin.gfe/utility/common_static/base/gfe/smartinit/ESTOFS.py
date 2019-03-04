##
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

## ESTOFS

from Init import *

##--------------------------------------------------------------------------
class ESTOFSForecaster(Forecaster):
    def __init__(self):
            Forecaster.__init__(self, "ESTOFS","ESTOFS")


    #===========================================================================
    #  Ingest the gridded ESTOFS storm surge guidance
    #===========================================================================
    def calcStormSurge(self, ETSRG_SFC):

        return ETSRG_SFC * 3.2808

    #===========================================================================
    #  Ingest the gridded ESTOFS storm surge guidance
    #===========================================================================
    def calcAstroTide(self, ELEV_SFC):

        return ELEV_SFC * 3.2808


def main():
    ESTOFSForecaster().run()

if __name__ == "__main__":
    main()

