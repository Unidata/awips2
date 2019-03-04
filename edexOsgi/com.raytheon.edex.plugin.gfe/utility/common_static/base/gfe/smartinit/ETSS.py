##
##

##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##

## ETSS smart init

from Init import *

##--------------------------------------------------------------------------
class ETSSForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "ETSS","ETSS")

    #===========================================================================
    #  Ingest the gridded ETSS storm surge guidance
    #===========================================================================
    def calcStormSurge(self, ETSRG_SFC):
        return ETSRG_SFC / 0.3048
    
    def calcSurgeTide(self, ETCWL_SFC):
        return ETCWL_SFC / 0.3048

def main():
    ETSSForecaster().run()

if __name__ == "__main__":
    main()

