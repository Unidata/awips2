
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ApparentTemperature.py
#
# Author: amoore
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "ApparentT"
HideTool = 0

ScreenList = ["ApparentT"]

from numpy import *
import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, T, Td, Wind):
        "Apparent Temperature based on T, Td, Wind"

        ApparentT = where(less(T, 51), self._windChillCalc(T, Wind),
                          where(greater(T, 79), self._heatIndexCalc(T, Td),
                                T))
        
        # Return the new value
        return ApparentT


    def _heatIndexCalc(self, T, Td):
        
        #----------------------
        # Heat Index Equation
        #----------------------
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10,(Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10,(Tdc * 7.5 / (Tdc + 237.3)))   
        RH = (Vd / Vt) * 100.0
    
        A = -42.379
        B =  2.04901523 * T
        C = 10.14333127 * RH
        D = -0.22475541 * T * RH
        E = -0.00683783 * pow(T, 2)
        F = -0.05481717 * pow(RH, 2)
        G =  0.00122874 * pow(T, 2) * RH
        H =  0.00085282 * T * pow(RH, 2)
        I = -0.00000199 * pow(T, 2) * pow(RH, 2)
    
        HeatIndexValue = A + B + C + D + E + F + G + H + I

        # make the adjustments for low humidity
        rhLessThan13 = less(RH, 13.0)
        T80to112 = logical_and(greater_equal(T, 80), less_equal(T, 112))
        downMask = logical_and(rhLessThan13, T80to112)

        # make array that is T where conditions are true and 100, otherwise
        adjustT = where(downMask, T, 80.0)
        HeatIndexValue[downMask] = HeatIndexValue[downMask] - (((13.0 - RH[downMask]) / 4.0) * sqrt((17.0 - abs(adjustT[downMask] - 95.0)) / 17))
 
        # make the adjustments for high humidity
        rhGreater85 = greater(RH, 85.0)
        T80to87 = logical_and(greater_equal(T, 80.0), less_equal(T, 87.0))
        upMask = logical_and(rhGreater85, T80to87)
        HeatIndexValue[upMask] = HeatIndexValue[upMask] + (((RH[upMask] - 85.0) / 10.0) * ((87 - T[upMask]) / 5.0))

        return HeatIndexValue



    def _windChillCalc(self, T, Wind):

        #---------------------
        # Wind Chill Equation
        #---------------------
        mag = Wind[0] * 1.15
        WindChillValue = where(less_equal(mag, 3), T, 35.74 + (0.6215 * T) - 
                   (35.75 * (mag ** 0.16)) + (0.4275 * T * (mag ** 0.16)))
        mask = greater(WindChillValue, T)
        WindChillValue[mask] = T[mask]

        return WindChillValue
        
