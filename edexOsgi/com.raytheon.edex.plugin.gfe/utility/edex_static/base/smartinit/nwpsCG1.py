from Init import *
import os

class nwpsCG1Forecaster(Forecaster):
    def __init__(self):
       Forecaster.__init__(self, "nwpsCG1", "nwpsCG1")

    def calcWaveHeight(self, htsgw_SFC):
        grid = htsgw_SFC / 0.3048
        return grid
    def calcSwanSwell(self, swell_SFC):
        grid = swell_SFC / 0.3048)
        return grid
    def calcPeriod(self, perpw_SFC):
        period = clip(perpw_SFC, 0, 25)
        return period

def main():
    nwpsCG1Forecaster().run()

    os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "SWAN WAVE GRIDS ARE NOW IN GFE"')

if __name__ == "__main__":
    main()
