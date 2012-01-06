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
# Populate_SkyTool -- Version 1.0 
#
# Author: Pete Banacos, WFO BTV (Started: 9/20/06)
# Last update: 1/23/07
# ----------------------------------------------------------------------------

ToolType = "numeric"
WeatherElementEdited = "Sky"

from numpy import *

### Solicite variables from the forecaster:
VariableList = [
    ("Populate SkyTool Version 1.0","","label"),
    ("Model:", "NAM12", "radio", ["GFS40", "NAM12"]),
    ("Model Run:", "Current", "radio", ["Current", "Previous"]),
    ("Layer depth:", "50mb", "radio", ["50mb", "25mb"]),
    ("Use RH w.r.t. ICE @ T < -25C?", "No", "radio", ["Yes", "No"]), 
    ("", "", "label"),
    ("Include high clouds (500-300mb)?", "No", "radio", ["Yes", "No"]),
    ("Include clouds below 925mb?", "Yes", "radio", ["Yes", "No"]),                                               
    ("5% Sky Cover threshold at RH percentage:", 60., "scale", [44., 74.],2.0),
    ("Above value sets RH threshold for CLR skies.", "", "label"),    
    ("Calibration:", 1.00, "scale", [1.00, 1.50],0.02),
    ("Raise calibration to get more sky cover for a given RH.", "", "label"),    
    (" ---   Limit Values Section   --- ", "", "label"),
    ("Don't give me sky cover above (percent):", 100, "scale", [0, 100], 1),
    ("Don't give me sky cover below (percent):", 0, "scale", [0, 100], 1),
    ]

import SmartScript

class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def execute(self, Sky, GridTimeRange, varDict):
        "Determine Sky Cover based on exponential function where layer RH is the dependent variable."

        SITE = self.getSiteID()

        layer_depth = varDict["Layer depth:"]
        lowPBL = varDict["Include clouds below 925mb?"]
        hiCLDS = varDict["Include high clouds (500-300mb)?"]
        UseRHi = varDict["Use RH w.r.t. ICE @ T < -25C?"]
        limit_MAX = varDict["Don't give me sky cover above (percent):"]
        limit_MIN = varDict["Don't give me sky cover below (percent):"]
        model1 = varDict["Model:"]
        modelrun = varDict["Model Run:"]
        modeltemp = "D2D_" + model1

        if modelrun == "Current":
            model = self.findDatabase(modeltemp, 0)
        else:
            model = self.findDatabase(modeltemp, -1)

# Grab RH values from the numerical model

        print 'GridTimeRange = ', GridTimeRange
        RHPBL030 = self.getGrids(model, "rh", "BL030", GridTimeRange)
        RHPBL3060 = self.getGrids(model, "rh", "BL3060", GridTimeRange)
        RHPBL6090 = self.getGrids(model, "rh", "BL6090", GridTimeRange)
        RHPBL90120 = self.getGrids(model, "rh", "BL90120", GridTimeRange)
            
# TESTING SECTION

        lvl = ["BL030", "BL3060", "BL6090", "BL90120",
               "MB925","MB900","MB875","MB850",
               "MB825","MB800","MB775","MB750",
               "MB725","MB700","MB675","MB650",
               "MB625","MB600","MB575","MB550",
               "MB525","MB500","MB450","MB400",
               "MB350","MB300"]
        
# Populate Temperature list with default of freezing        
        T_lvl = [273,273,273,273,273,273,273,273,273,
                 273,273,273,273,273,273,273,273,273,
                 273,273,273,273,273,273,273,273]
# Populate RH list with default of 50%
        RH_lvl = [50,50,50,50,50,50,50,50,50,
                  50,50,50,50,50,50,50,50,50,
                  50,50,50,50,50,50,50,50]
        e_lvl = [50,50,50,50,50,50,50,50,50,
                 50,50,50,50,50,50,50,50,50,
                 50,50,50,50,50,50,50,50]
        es_lvl = [50,50,50,50,50,50,50,50,50,
                  50,50,50,50,50,50,50,50,50,
                  50,50,50,50,50,50,50,50]
        esi_lvl = [50,50,50,50,50,50,50,50,50,
                   50,50,50,50,50,50,50,50,50,
                   50,50,50,50,50,50,50,50]
        RHi_lvl = [50,50,50,50,50,50,50,50,50,
                   50,50,50,50,50,50,50,50,50,
                   50,50,50,50,50,50,50,50]
        
        for x in range(len(lvl)):
            T_lvl[x] = self.getGrids(model, "t", lvl[x], GridTimeRange)
            RH_lvl[x] = self.getGrids(model, "rh", lvl[x], GridTimeRange)
            es_lvl[x] = 6.11 * exp(5412.*((1./273.)-(1./T_lvl[x])))           
            e_lvl[x] = (RH_lvl[x] * es_lvl[x]) / 100.
# compute RH with respect to ice, using latent heat of sublimation:
            esi_lvl[x] = 6.11 * exp(6133.*((1./273.)-(1./T_lvl[x])))
            RHi_lvl[x] = (e_lvl[x] / esi_lvl[x]) * 100.

# If grid pt. temperature is less than -25C, use RH over ice...
            if UseRHi == "Yes":
                RH_lvl[x][less(T_lvl[x], 248)] = RHi_lvl[x]

# Populate 30mb near-sfc AGL layers:

        RHPBL030 = RH_lvl[0]
        RHPBL3060 = RH_lvl[1]
        RHPBL6090 = RH_lvl[2]
        RHPBL90120 = RH_lvl[3]
              
# compute layer-averaged RH values (50mb)       

        if layer_depth == "50mb":
            RHavg925_875 = ((RH_lvl[4]+RH_lvl[5]+RH_lvl[6])/3)
            RHavg900_850 = ((RH_lvl[5]+RH_lvl[6]+RH_lvl[7])/3)
            RHavg875_825 = ((RH_lvl[6]+RH_lvl[7]+RH_lvl[8])/3)
            RHavg850_800 = ((RH_lvl[7]+RH_lvl[8]+RH_lvl[9])/3)
            RHavg825_775 = ((RH_lvl[8]+RH_lvl[9]+RH_lvl[10])/3)
            RHavg800_750 = ((RH_lvl[9]+RH_lvl[10]+RH_lvl[11])/3)
            RHavg775_725 = ((RH_lvl[10]+RH_lvl[11]+RH_lvl[12])/3)
            RHavg750_700 = ((RH_lvl[11]+RH_lvl[12]+RH_lvl[13])/3)
            RHavg725_675 = ((RH_lvl[12]+RH_lvl[13]+RH_lvl[14])/3)
            RHavg700_650 = ((RH_lvl[13]+RH_lvl[14]+RH_lvl[15])/3)
            RHavg675_625 = ((RH_lvl[14]+RH_lvl[15]+RH_lvl[16])/3)
            RHavg650_600 = ((RH_lvl[15]+RH_lvl[16]+RH_lvl[17])/3)
            RHavg625_575 = ((RH_lvl[16]+RH_lvl[17]+RH_lvl[18])/3)
            RHavg600_550 = ((RH_lvl[17]+RH_lvl[18]+RH_lvl[19])/3)
            RHavg575_525 = ((RH_lvl[18]+RH_lvl[19]+RH_lvl[20])/3)
            RHavg550_500 = ((RH_lvl[19]+RH_lvl[20]+RH_lvl[21])/3)
        else:
#           depth is in 25mb layers
            RHavg925_900 = ((RH_lvl[4]+RH_lvl[5])/2)
            RHavg900_875 = ((RH_lvl[5]+RH_lvl[6])/2)
            RHavg875_850 = ((RH_lvl[6]+RH_lvl[7])/2)
            RHavg850_825 = ((RH_lvl[7]+RH_lvl[8])/2)
            RHavg825_800 = ((RH_lvl[8]+RH_lvl[9])/2)
            RHavg800_775 = ((RH_lvl[9]+RH_lvl[10])/2)
            RHavg775_750 = ((RH_lvl[10]+RH_lvl[11])/2)
            RHavg750_725 = ((RH_lvl[11]+RH_lvl[12])/2)
            RHavg725_700 = ((RH_lvl[12]+RH_lvl[13])/2)
            RHavg700_675 = ((RH_lvl[13]+RH_lvl[14])/2)
            RHavg675_650 = ((RH_lvl[14]+RH_lvl[15])/2)
            RHavg650_625 = ((RH_lvl[15]+RH_lvl[16])/2)
            RHavg625_600 = ((RH_lvl[16]+RH_lvl[17])/2)
            RHavg600_575 = ((RH_lvl[17]+RH_lvl[18])/2)
            RHavg575_550 = ((RH_lvl[18]+RH_lvl[19])/2)
            RHavg550_525 = ((RH_lvl[19]+RH_lvl[20])/2)
            RHavg525_500 = ((RH_lvl[20]+RH_lvl[21])/2)
            
# Layer depth above 500mb is always 50mb...
        RHavg500_450 = ((RH_lvl[21]+RH_lvl[22])/2)
        RHavg450_400 = ((RH_lvl[22]+RH_lvl[23])/2)
        RHavg400_350 = ((RH_lvl[23]+RH_lvl[24])/2)
        RHavg350_300 = ((RH_lvl[24]+RH_lvl[25])/2)
      
# Generate List of layers to check.

        my_PBLlist = [RHPBL030, RHPBL3060, RHPBL6090, RHPBL90120]
        my_upr_trop_list = [RHavg500_450,RHavg450_400, RHavg400_350,
                            RHavg350_300]

        if layer_depth == "50mb":
            my_list = [RHavg925_875, RHavg900_850, RHavg875_825,
                       RHavg850_800, RHavg825_775, RHavg800_750, RHavg775_725,
                       RHavg750_700, RHavg725_675, RHavg700_650, RHavg675_625,
                       RHavg650_600, RHavg625_575, RHavg600_550, RHavg575_525,
                       RHavg550_500]

        else: 
            my_list25 = [RHavg925_900, RHavg900_875, RHavg875_850, RHavg850_825,
                         RHavg825_800, RHavg800_775, RHavg775_750, RHavg750_725,
                         RHavg725_700, RHavg700_675, RHavg675_650, RHavg650_625,
                         RHavg625_600, RHavg600_575, RHavg575_550, RHavg550_525,
                         RHavg525_500]
                     

# Put lowest RH layer being used into place holder...
        if lowPBL == "Yes":
            holder = RHPBL030
        elif layer_depth == "50mb":
            holder = RHavg925_875
        else:
            holder = RHavg925_900

# check Ground Relative layers first at low-levels, as selected by user. 
        if lowPBL == "Yes":
            for layerRH in my_PBLlist:
                holder = where(greater(layerRH, holder), layerRH, holder)

# Check Layers incrementally
        if layer_depth == "50mb":
            for layerRH in my_list:
                holder= where(greater(layerRH, holder), layerRH, holder)
        else:
            for layerRH in my_list25:
                holder= where(greater(layerRH, holder), layerRH, holder)
                
# If user wants high clouds (above 500mb), continue layer checking...
        if hiCLDS == "Yes":
           for layerRH in my_upr_trop_list:
               holder = where(greater(layerRH, holder), layerRH, holder)	

# Compute Cloud Amount
        Calib = varDict["Calibration:"]
        clr_threshold = varDict["5% Sky Cover threshold at RH percentage:"]
#        cloudamt = 5. * (exp(.106*(holder-70.)))
        cloudamt = 5. * (exp(3.*Calib*((holder-clr_threshold)/(100.-clr_threshold))))
        
# Apply Limit Values Portion as input by User:
# The limit values takes precedence over the PoP QC tool since it is the
# final check done. 
        cloudamt[greater(cloudamt, limit_MAX)] = limit_MAX
        cloudamt[less(cloudamt, limit_MIN)] = limit_MIN

# Warn user if Min cloud Limit exceeds Max Cloud Limit:         
        if limit_MIN > limit_MAX:
           self.statusBarMsg("Warning: Limit value discrpency noted (MIN cloud amount > MAX cloud amount). Verify settings", ".")            

# Return Value to for Sky grid            
        Sky = cloudamt

        return Sky
