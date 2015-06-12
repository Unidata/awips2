from Init import *
import os

class nwpsTrkngCG0Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG0", "nwpsTrkngCG0")
        
    def _calcPeriodN(self, swper):
        return swper
    
    def _calcWaveN(self, swell, swdir):
       mag = swell / 0.3048
       dir = clip(swdir, 0, 359.5)
       return (mag, dir)
 
    def calcPeriod1(self, swper_OSEQD1):
        return self._calcPeriodN(swper_OSEQD1)
    def calcPeriod2(self, swper_OSEQD2):
        return self._calcPeriodN(swper_OSEQD2)
    def calcPeriod3(self, swper_OSEQD3):
        return self._calcPeriodN(swper_OSEQD3)
    def calcPeriod4(self, swper_OSEQD4):
        return self._calcPeriodN(swper_OSEQD4)
    def calcPeriod5(self, swper_OSEQD5):
        return self._calcPeriodN(swper_OSEQD5)
    def calcPeriod6(self, swper_OSEQD6):
        return self._calcPeriodN(swper_OSEQD6)
    def calcPeriod7(self, swper_OSEQD7):
        return self._calcPeriodN(swper_OSEQD7)
    def calcPeriod8(self, swper_OSEQD8):
        return self._calcPeriodN(swper_OSEQD8)
    def calcPeriod9(self, swper_OSEQD9):
        return self._calcPeriodN(swper_OSEQD9)
    def calcPeriod10(self, swper_OSEQD10):
        return self._calcPeriodN(swper_OSEQD10)

    def calcWave1(self, swell_OSEQD1, swdir_OSEQD1):
       return self._calcWaveN(swell_OSEQD1, swdir_OSEQD1)
    def calcWave2(self, swell_OSEQD2, swdir_OSEQD2):
        return self._calcWaveN(swell_OSEQD2, swdir_OSEQD2)
    def calcWave3(self, swell_OSEQD3, swdir_OSEQD3):
        return self._calcWaveN(swell_OSEQD3, swdir_OSEQD3)
    def calcWave4(self, swell_OSEQD4, swdir_OSEQD4):
        return self._calcWaveN(swell_OSEQD4, swdir_OSEQD4)
    def calcWave5(self, swell_OSEQD5, swdir_OSEQD5):
        return self._calcWaveN(swell_OSEQD5, swdir_OSEQD5)
    def calcWave6(self, swell_OSEQD6, swdir_OSEQD6):
        return self._calcWaveN(swell_OSEQD6, swdir_OSEQD6)
    def calcWave7(self, swell_OSEQD7, swdir_OSEQD7):
        return self._calcWaveN(swell_OSEQD7, swdir_OSEQD7)
    def calcWave8(self, swell_OSEQD8, swdir_OSEQD8):
        return self._calcWaveN(swell_OSEQD8, swdir_OSEQD8)
    def calcWave9(self, swell_OSEQD9, swdir_OSEQD9):
        return self._calcWaveN(swell_OSEQD9, swdir_OSEQD9)
    def calcWave10(self, swell_OSEQD10, swdir_OSEQD10):
        return self._calcWaveN(swell_OSEQD10, swdir_OSEQD10)

def main():
    nwpsTrkngCG0Forecaster().run()
    chkfile = "/tmp/nwps/CGTrack"
    chkfiledir = "/tmp/nwps"
    try:
        os.makedirs(chkfiledir)
    except OSError:
        pass
    if not os.path.isfile(chkfile):
        open(chkfile, 'a').close()
        os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "TRACKING WAVE GRIDS ARE NOW IN GFE"')
    filemodtime = os.stat(chkfile).st_mtime
    twominutesago = time.time() - 120
    if (twominutesago - filemodtime) > 0:
        os.utime(chkfile, None)
        os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "TRACKING WAVE GRIDS ARE NOW IN GFE"')

if __name__ == "__main__":
    main()
