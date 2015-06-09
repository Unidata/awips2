from Init import *
import os

class nwpsCG1Forecaster(Forecaster):
    def __init__(self):
       Forecaster.__init__(self, "nwpsCG1", "nwpsCG1")

    def calcWaveHeight(self, htsgw_SFC):
        grid = htsgw_SFC / 0.3048
        return grid
    def calcSwanSwell(self, swell_SFC):
        grid = swell_SFC / 0.3048
        return grid
    def calcPeriod(self, perpw_SFC):
        period = clip(perpw_SFC, 0, 25)
        return period

def main():
    nwpsCG1Forecaster().run()
    chkfile = "/tmp/nwps/CG1"
    chkfiledir = "/tmp/nwps"
    try:
        os.makedirs(chkfiledir)
    except OSError:
        pass
    if not os.path.isfile(chkfile):
        open(chkfile, 'a').close()
        os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "SWAN WAVE GRIDS ARE NOW IN GFE"')
    filemodtime = os.stat(chkfile).st_mtime
    twominutesago = time.time() - 120
    if (twominutesago - filemodtime) > 0:
        os.utime(chkfile, None)
        os.system('/awips2/GFESuite/bin/sendGfeMessage -s -m "SWAN WAVE GRIDS ARE NOW IN GFE"')


if __name__ == "__main__":
    main()
