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
# HeatIndexTool
#
# Author: Steve Nelson
#         Tom LeFebvre - 8/2/2002 - added adjustments for low, high humidity
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "HeatIndex"
from numpy import *

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, T, Td):
        "New Heat Index Equation"

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
    
        HeatIndex = A + B + C + D + E + F + G + H + I

        # make the adjustments for low humidity
        rhLessThan13 = less(RH, 13.0)
        T80to112 = logical_and(greater_equal(T, 80), less_equal(T, 112))
        downMask = logical_and(rhLessThan13, T80to112)

        # make array that is T where conditions are true and 100, otherwise
        adjustT = where(downMask, T, 80.0)
        HeatIndex = where(downMask, HeatIndex - (((13.0 - RH) / 4.0) * \
                              sqrt((17.0 - abs(adjustT - 95.0)) / 17)),
                              HeatIndex)
 
        # make the adjustments for high humidity
        rhGreater85 = greater(RH, 85.0)
        T80to87 = logical_and(greater_equal(T, 80.0), less_equal(T, 87.0))
        HeatIndex = where(logical_and(rhGreater85, T80to87),
                          HeatIndex + (((RH - 85.0) / 10.0) * ((87 - T) / 5.0)),
                          HeatIndex)
                                   
        HeatIndex = where(less(T, 80), T, HeatIndex)
        return HeatIndex


