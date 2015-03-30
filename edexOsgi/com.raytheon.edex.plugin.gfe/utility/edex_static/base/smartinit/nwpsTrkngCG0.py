from Init import *
import os

class nwpsTrkngCG0Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG0", "nwpsTrkngCG0")
 
    def calcPeriod1(self, SWPER_OSEQD1):
        period = SWPER_OSEQD1
        return period
    def calcPeriod2(self, SWPER_OSEQD2):
        period = SWPER_OSEQD2
        return period
    def calcPeriod3(self, SWPER_OSEQD3):
        period = SWPER_OSEQD3
        return period
    def calcPeriod4(self, SWPER_OSEQD4):
        period = SWPER_OSEQD4
        return period
    def calcPeriod5(self, SWPER_OSEQD5):
        period = SWPER_OSEQD5
        return period
    def calcPeriod6(self, SWPER_OSEQD6):
        period = SWPER_OSEQD6
        return period
    def calcPeriod7(self, SWPER_OSEQD7):
        period = SWPER_OSEQD7
        return period
    def calcPeriod8(self, SWPER_OSEQD8):
        period = SWPER_OSEQD8
        return period
    def calcPeriod9(self, SWPER_OSEQD9):
        period = SWPER_OSEQD9
        return period
    def calcPeriod10(self, SWPER_OSEQD10):
        period = SWPER_OSEQD10
        return period

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
       mag = SWELL_OSEQD1 * 3.28
       dir = clip(SWDIR_OSEQD1, 0, 359.5)
       return (mag, dir)
    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        mag = SWELL_OSEQD2 * 3.28
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        return (mag, dir)
    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        mag = SWELL_OSEQD3 * 3.28
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        return (mag, dir)
    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        mag = SWELL_OSEQD4 * 3.28
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        return (mag, dir)
    def calcWave5(self, SWELL_OSEQD5, SWDIR_OSEQD5):
        mag = SWELL_OSEQD5 * 3.28
        dir = clip(SWDIR_OSEQD5, 0, 359.5)
        return (mag, dir)
    def calcWave6(self, SWELL_OSEQD6, SWDIR_OSEQD6):
        mag = SWELL_OSEQD6 * 3.28
        dir = clip(SWDIR_OSEQD6, 0, 359.5)
        return (mag, dir)
    def calcWave7(self, SWELL_OSEQD7, SWDIR_OSEQD7):
        mag = SWELL_OSEQD7 * 3.28
        dir = clip(SWDIR_OSEQD7, 0, 359.5)
        return (mag, dir)
    def calcWave8(self, SWELL_OSEQD8, SWDIR_OSEQD8):
        mag = SWELL_OSEQD8 * 3.28
        dir = clip(SWDIR_OSEQD8, 0, 359.5)
        return (mag, dir)
    def calcWave9(self, SWELL_OSEQD9, SWDIR_OSEQD9):
        mag = SWELL_OSEQD9 * 3.28
        dir = clip(SWDIR_OSEQD9, 0, 359.5)
        return (mag, dir)
    def calcWave10(self, SWELL_OSEQD10, SWDIR_OSEQD10):
        mag = SWELL_OSEQD10 * 3.28
        dir = clip(SWDIR_OSEQD10, 0, 359.5)
        return (mag, dir)

def main():
    nwpsTrkngCG0Forecaster().run()

    os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "TRACKING WAVE GRIDS ARE NOW IN GFE"')

if __name__ == "__main__":
    main()
