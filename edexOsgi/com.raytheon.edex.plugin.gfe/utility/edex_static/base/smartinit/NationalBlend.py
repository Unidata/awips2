from Init import *
#import LogStream

##--------------------------------------------------------------------------
## Module that calculates surface weather elements from SPC model
## output.
##
##--------------------------------------------------------------------------
class NationalBlendForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "NationalBlend", "NationalBlend")

    def calcWind(self, wind_FHAG10):
        (mag,direc)=wind_FHAG10
        newmag=self.convertMsecToKts(mag)
        return (newmag,direc)
##--------------------------------------------------------------------------
    def calcWindGust(self, wgs_FHAG10):
        return self.convertMsecToKts(wgs_FHAG10)
##--------------------------------------------------------------------------
##  QPF - change mm to inches and clip greater than 1000mm
##--------------------------------------------------------------------------
    def calcQPF(self, tp_SFC):
        grid = where(greater(tp_SFC, 1000), 0.0, tp_SFC / 25.4)
        return clip(grid, 0, 10)  # clip at zero and 10 inches

    def calcSnowAmt(self, totsn_SFC):
        return totsn_SFC

    def calcPoP(self, pop_SFC):
        return pop_SFC

    #==========================================================================
    #
    #  QPF6 - sums up all QPF grids within each 6 hour period
    #
##    def calcQPF06(self, QPF, QPF06, ctime):
##        modelhour = time.gmtime(ctime[0])
##        forecastHR = modelhour[3]
##        if forecastHR in [0, 6, 12, 18]:
##           QPF06 = 0
##        if QPF06 is None:
##           QPF06 = 0
##        QPF06 = QPF06 + QPF
##        return QPF06
##--------------------------------------------------------------------------
##  Sky
##--------------------------------------------------------------------------
    def calcSky(self, tcc_SFC):
        grid = tcc_SFC
        return clip(grid, 0, 100)
##--------------------------------------------------------------------------
##  T - change K to F
##--------------------------------------------------------------------------
    def calcT(self, t_FHAG2):
        return self.KtoF(t_FHAG2)
##--------------------------------------------------------------------------
##  Td - change K to F
##--------------------------------------------------------------------------
    def calcTd(self,dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)
##--------------------------------------------------------------------------
##  MaxT and MinT - max and min of hourly Ts
##--------------------------------------------------------------------------
    def calcMaxT(self, mxt_FHAG2):
        return self.KtoF(mxt_FHAG2)

    def calcMinT(self, mnt_FHAG2):
        return self.KtoF(mnt_FHAG2)
##--------------------------------------------------------------------------
##  RH - calculated from T and Td, rather than from input specific humidity
##--------------------------------------------------------------------------
    def calcRH(self, T, Td):
        Vt=self.vaprtf(T)
        Vd=self.vaprtf(Td)
        return ( (Vd / Vt) * 100.0)

    def vaprtf(self,tf):
        tc=(tf-32.0)*5.0/9.0
        vapr=6.112*exp((17.67*tc)/(tc+243.5))
        return vapr
##--------------------------------------------------------------------------
##  MaxRH and MinRH - max and min of hourly RHs
##--------------------------------------------------------------------------
##  def calcMaxRH(self, RH, MaxRH):
##      if MaxRH is None:
##          return RH
##      return maximum(MaxRH, RH)

##  def calcMinRH(self, RH, MinRH):
##      if MinRH is None:
##          return RH
##      return minimum(MinRH, RH)

    def calcApparentT(self, T, RH, Wind):
        ApparentT = where(less(T, 51), self.windChillCalc(T, Wind),
                          where(greater(T, 71), self.heatIndexCalc(T, RH),
                                T))
        return ApparentT


    def windChillCalc(self, T, Wind):
        mag = Wind[0] * 1.15077945
        WindChill = where(less_equal(mag, 1), T, 35.74 + (0.6215 * T) - (35.75 * (mag ** 0.16)) + (0.4275 * T * (mag ** 0.16)))

        # clip values where WindChill > T
        WindChill = where(greater(WindChill, T), T, WindChill)

        # substitute the temperature if WindChill >= 51 degrees
        WindChill = where(greater_equal(T, 51), T, WindChill)
        return WindChill

    def heatIndexCalc(self, T, RH):

    
        A = -42.379
        B =  2.04901523 * T
        C = 10.14333127 * RH
        D = -0.22475541 * T * RH
        E = -0.00683783 * pow(T, 2)
        F = -0.05481717 * pow(RH, 2)
        G =  0.00122874 * pow(T, 2) * RH
        H =  0.00085282 * T * pow(RH, 2)
        I = -0.00000199 * pow(T, 2) * pow(RH, 2)
    
        ApparentT = A + B + C + D + E + F + G + H + I

        # make the adjustments for low humidity
        rhLessThan13 = less(RH, 13.0)
        T80to112 = logical_and(greater_equal(T, 80), less_equal(T, 112))
        downMask = logical_and(rhLessThan13, T80to112)

        # make array that is T where conditions are true and 100, otherwise
        adjustT = where(downMask, T, 80.0)
        ApparentT = where(downMask, ApparentT - (((13.0 - RH) / 4.0) * \
                              sqrt((17.0 - abs(adjustT - 95.0)) / 17)),
                              ApparentT)
 
        # make the adjustments for high humidity
        rhGreater85 = greater(RH, 85.0)
        T80to87 = logical_and(greater_equal(T, 80.0), less_equal(T, 87.0))
        HeatIndexValue = where(logical_and(rhGreater85, T80to87),
                          ApparentT + (((RH - 85.0) / 10.0) * ((87 - T) / 5.0)),
                          ApparentT)

        return HeatIndexValue


def main():
    NationalBlendForecaster().run()

if __name__ == "__main__":
    main()