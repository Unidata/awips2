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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- --------------------------------------------
# May 02, 2018  7277     randerso  Updated calcWindGust
##
##
# This is a base file that is not intended to be overridden.
#
# This file can be subclassed to override behavior. Please see the 
# Configuration Guides->Smart Initialization Configuration section of the GFE 
# Online Help for guidance on creating a new smart init 
##


from Init import *
##--------------------------------------------------------------------------
class HRRRForecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "HRRR","HRRR")

#    def calcClgHgt(self, cc_CCL):
#        ceil = cc_CCL * .03280839
#        ceil[less_equal(ceil, 0.0)] = 250.0
#        return ceil

    def calcVis(self, vis_SFC):
        return (vis_SFC * 3.2808) / 5279.85564

    def calcT(self, t_FHAG2):
        return self.KtoF(t_FHAG2)

    def calcTd(self, dpt_FHAG2):
        return self.KtoF(dpt_FHAG2)

##--------------------------------------------------------------------------
## Returns the maximum of the specified MaxT and the T grids
##--------------------------------------------------------------------------
##    def calcMaxT(self, T, MaxT):
##        if MaxT is None:
##            return T
##        return maximum(MaxT, T)

##--------------------------------------------------------------------------
## Returns the minimum of the specified MinT and T grids
##--------------------------------------------------------------------------
##    def calcMinT(self, T, MinT):
##        if MinT is None:
##            return T
##        return minimum(MinT, T)


    def calcRH(self, T, Td):
        Tc = .556 * (T - 32.0)
        Tdc = .556 * (Td - 32.0)
        Vt = 6.11 * pow(10, (Tc * 7.5 / (Tc + 237.3)))
        Vd = 6.11 * pow(10, (Tdc * 7.5 / (Tdc + 237.3)))
        RH = (Vd / Vt) * 100.0
        # return the new value
        return RH

    def dewFromTandRH(self,T,RH):
        tc=(T-32.0)*(5.0/9.0)
        rh=clip(RH,0.001,99.999)/100.0
        x=(log(rh)/17.67)+(tc/(tc+243.5))
        tdc=(243.5*x)/(1.0-x)
        td=(tdc*9.0/5.0)+32.0
        return td

##--------------------------------------------------------------------------
# Calculates QPF from the total precip field out of the model
##--------------------------------------------------------------------------
    def calcQPF(self, tp_SFC):
        return tp_SFC / 25.4   # convert from millimeters to inches

    def calcQPF6(self, QPF, QPF6):
        if QPF6 is None:
           QPF6=QPF
        else:
           QPF6=QPF6+QPF
        return QPF6

    def calcQPF12(self, QPF6, QPF12):
        if QPF12 is None:
           QPF12=QPF6
        else:
           QPF12=QPF12+QPF6
        return QPF12

##--------------------------------------------------------------------------
##  Converts the lowest available wind level from m/s to knots
##--------------------------------------------------------------------------
    def calcWind(self, wind_FHAG10):
        return (wind_FHAG10[0] * 1.94,clip(wind_FHAG10[1], 0, 359.5))
#
#  Return the max of the max wind or wind gust
# 
    def calcWindGust(self, wgs_SFC, wind_FHAG10):
        (magnitude, direction) = wind_FHAG10
        return (maximum(wgs_SFC, magnitude) * 1.94)
#=========================================================================
#  SnowAmt - simple snow ratio based on surface temperature - multiplied
#            times the model QPF amount
#-------------------------------------------------------------------------

    def calcSnowAmt(self,T,QPF):
        m1=less(T,9.0)
        m2=greater_equal(T,30.0)
        snowr=(T*-0.5)+22.5
        snowr[m1] = 20
        snowr[m2] = 0
        snowamt=QPF*snowr
        return snowamt

##--------------------------------------------------------------------------
##  Use cloud base and cloud top to get sky cover 
##--------------------------------------------------------------------------

#    def calcSky(self, gh_CBL, gh_CTL):
#        depth=gh_CTL-gh_CBL
#        c100=greater_equal(depth, 1000)
#        partialcloudy=depth/10
#        sky=0
#        sky=where(depth, c100, sky)
##        sky=where(depth, partialcloudy, sky)
#        return sky


    def calcSky(self,tcc_EA):
        return tcc_EA

#--------------------------------------------------------------------------
# PoP - based strongly on QPF (since when model has one inch of precip the
#   chance of getting 0.01 is pretty high).  However, there is a big
#   difference between a place that model has 0.00 precip and is very
#   close to precipitating - and those where model has 0.00 and is a
#   thousand miles from the nearest cloud.  Thus, uses the average
#
#   Uses hyperbolic tangent of QPF, so that it rises quickly as model
#   QPF increases - but tapers out to nearly 100% as QPF gets high.
#
#   Adjustable parameters:
#     topQPF is QPF amount that would give 75% PoP if nothing else
#       considered at half this amount, PoP is 45%, at double this
#       amount PoP is 96%.  Default set at 0.40.
#
#--------------------------------------------------------------------------
#    def calcPoP(self, QPF12): 
#
#        topQPF=0.40    # QPF value where raw PoP would be 75%
#        factor=tanh(QPF12*(1.0/topQPF))
#        factor2=tanh(QPF12*(2.0/topQPF))
#        pop=(factor*100.0)+(factor2*100.0)
#        pop=clip(pop,0,100)
#        return pop

##--------------------------------------------------------------------------
##  Use sky, reflecivity, qpf, vis, categoricals to get weather
##--------------------------------------------------------------------------

    def calcWx(self, T, QPF, crain_SFC, csnow_SFC, cicep_SFC, bli_BL0180, cfrzr_SFC, refc_EA):

        # Now apply a different algorithm for each type
        key = ['<NoCov>:<NoWx>:<NoInten>:<NoVis>:',
               "Wide:S:-:<NoVis>:", "Wide:R:-:<NoVis>:",
               "Wide:S:-:<NoVis>:^Wide:R:-:<NoVis>:",
               'Wide:ZR:-:<NoVis>:', 'Wide:IP:-:<NoVis>:',
               'Wide:ZR:-:<NoVis>:^Wide:IP:-:<NoVis>:',
               "Sct:SW:-:<NoVis>:", "Sct:RW:-:<NoVis>:",
               "Sct:SW:-:<NoVis>:^Sct:RW:-:<NoVis>:",
               "Chc:ZR:-:<NoVis>:", 'Chc:IP:-:<NoVis>:',
               'Chc:ZR:-:<NoVis>:^Chc:IP:-:<NoVis>:']

        wx = self.empty(int8)
        wx[logical_and(greater(QPF, 0.02), greater(T, 35))] = 2
        wx[equal(crain_SFC, 1)] = 2
        wx[equal(cfrzr_SFC, 1)] = 4
        wx[equal(cicep_SFC, 1)] = 5
        wx[equal(csnow_SFC, 1)] = 1 

        # Make showers (scattered/Chc)
        convecMask = less(refc_EA, 35)
        wx[logical_and(not_equal(wx, 0), convecMask)] += 6

        # Thunder
        for i in range(len(key)):
            tcov = key[i].split(":")[0]
            if tcov == "Chc" or tcov == "<NoCov>":
                tcov = "Sct"
            key.append(key[i] + "^" + tcov
                       + ":T:<NoInten>:<NoVis>:")
        wx[logical_and(greater_equal(bli_BL0180, -3), greater_equal(refc_EA, 35))] += 13

        # No wx where no qpf
        wx[less(QPF, 0.01)] = 0
        return(wx, key)

def main():
    HRRRForecaster().run()
