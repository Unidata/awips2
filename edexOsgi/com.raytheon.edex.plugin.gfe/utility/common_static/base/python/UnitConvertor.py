##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# UnitConverter.py
#
# This class returns a method for converting units.
#
# The arguments are the input units, the output units and the "element string"
# (i.e. "Wind")
#
# Author: dmiller
# ----------------------------------------------------------------------------

class UnitConvertor:
    def __init__(self):
        pass

    def getConvertMethod(self, inUnits, outUnits, elementString):
        if inUnits == outUnits:
            convertMethod = float
        elif inUnits == "kts" and outUnits == "mph":
            convertMethod = self.ktToMph
        elif inUnits == "mph" and outUnits == "kts":
            convertMethod = self.mphToKt

        elif inUnits == "ft" and outUnits == "m":
            convertMethod = self.ftToM
        elif inUnits == "m" and outUnits == "ft":
            convertMethod = self.mToFt

        elif inUnits == "mm" and outUnits == "in":
            convertMethod = self.mmToIn
        elif inUnits == "in" and outUnits == "mm":
            convertMethod = self.inToMm

        elif inUnits == "kt" and outUnits == "m/s":
            convertMethod = self.ktToMps
        elif inUnits == "m/s" and outUnits == "kt":
            convertMethod = self.mpsToKt
        elif inUnits == "m/s" and outUnits == "mph":
            convertMethod = self.mpsToMph

        elif inUnits == "kt-ft" and outUnits == "m^2/s":
            convertMethod = self.ktftToM2ps
        elif inUnits == "m^2/s" and outUnits == "kt-ft":
            convertMethod = self.m2psToKtft

        elif inUnits == "F" and outUnits == "C":
            convertMethod = self.FtoC
        elif inUnits == "C" and outUnits == "F":
            convertMethod = self.CtoF

        elif inUnits == "K" and outUnits == "C":
            convertMethod = self.KtoC
        elif inUnits == "C" and outUnits == "K":
            convertMethod = self.CtoK

        elif inUnits == "K" and outUnits == "F":
            convertMethod = self.KtoF
        elif inUnits == "F" and outUnits == "K":
            convertMethod = self.FtoK

        else:
            raise TypeError, "Invalid input or output Units for " + \
            elementString+" :" + inUnits + " " + outUnits
        return convertMethod

    def ktToMph(self, value):
        "Convert from knots to mph"
        return value * 1.151

    def mphToKt(self, value):
        "Convert from mph to knots"
        return value * 0.868

    def ktToMps(self, value):
        "Convert from knots to m/s"
        return value * 0.515

    def mpsToKt(self, value):
        "Convert from m/s to knots"
        return value * 1.944

    def mpsToMph(self, value):
        "Convert from m/s to mph"
        return value * 2.237

    def mToFt(self, value):
        "Convert from meters to feet"
        return value * 3.28084

    def ftToM(self, value):
        "Convert from feet to meters"
        return value / 3.28084

    def mmToIn(self, value):
        "Convert from millimeters to inches"
        return value * 25.4

    def inToMm(self, value):
        "Convert from inches to millimeters"
        return value * 0.3937

    def ktftToM2ps(self, value):
        "Convert from kt-ft to m^2/sec"
        return value * 0.157

    def m2psToKtft(self, value):
        "Convert from m^2/sec to kt-ft"
        return value * 6.371

    def FtoC(self, value):
        "Convert from degrees Fahrenheit to degrees Celcius"
        return (value - 32) * 0.5556

    def CtoF(self, value):
        "Convert from degrees Celcius to degrees Fahrenheit"
        return (value * 1.8) + 32

    def KtoF(self, value):
        "Convert from Kelvin to degrees Fahrenheit"
        #return (value * 1.8) - 459.67
        return self.CtoF(self.KtoC(value))

    def FtoK(self, value):
        "Convert from degrees Fahrenheit to Kelvin"
        #return (value - 32)*0.5556 + 273.15
        return self.CtoK(self.FtoC(value))

    def KtoC(self, value):
        "Convert from Kelvin to degrees Celcius"
        return value - 273.15

    def CtoK(self, value):
        "Convert from degrees Celcius to Kelvin"
        return value + 273.15
