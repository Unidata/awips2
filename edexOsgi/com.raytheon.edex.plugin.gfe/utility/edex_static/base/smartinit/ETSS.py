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
        return ETSRG_SFC * 3.2808


def main():
    ETSSForecaster().run()

if __name__ == "__main__":
    main()

