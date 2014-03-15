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
## @file presWeather.py

from numpy import zeros
from numpy import where
from numpy import greater_equal

precipDict1 = {-9999:'RA',0:'',1:'FZRA',2:'SN',3:'RA'} # Precip type (always rain in summer)
precipDict2 = {-9999:'',0:'',1:'FZRA',2:'SN',3:'RA',4:'RASN'}
precipDict3 = {-9999:'P',0:'',1:'FZRA',2:'SN',3:'RA'}
obsStructDict1 = {-9999:'',1:'BL',2:'HZ',3:'FG',4:'',5:'BR'} # Obstruction code
obsStructDict3 = {-9999:'',1:'SS',2:'HZ',3:'FG',4:'',5:'BR'} # Obstruction code

def execute1(QPF6hr_bestCat, obVis_bestCat, precipType,tstorm6hr, severe6hr):
    "Calculate present weather."
    
    sixHourPlotCheck = QPF6hr_bestCat >= 0
    qpfYes = QPF6hr_bestCat >= 1
    
    obsTypeAtAll = zeros(obVis_bestCat.shape, dtype='|S10')
    wxTypeAtAll = zeros(precipType.shape, dtype='|S10')
    
    precipTransform = typeTransform(precipType, precipDict1)
    obsTransform = typeTransform(obVis_bestCat, obsStructDict1)
    
    tstormYes = tstorm6hr >= 20
    severeYes = severe6hr >= 10
    
    storms = zeros(severeYes.shape,dtype='|S10')
    storms[tstormYes] = 'TS'
    storms[severeYes] = '+TS'
    
    wxTypeAtAll[qpfYes] = precipTransform[qpfYes]
    obsTypeAtAll[sixHourPlotCheck] = obsTransform[sixHourPlotCheck]
    return concatArrays([obsTypeAtAll, wxTypeAtAll, storms])

def execute2(precipType, QPF12hr_bestCat, POP12hr, tstorm12hr):
    "Calculate present weather."
    
    qpfYes = QPF12hr_bestCat >= 1
    popYes = POP12hr >= 30
            
    precipTransform = typeTransform(precipType, precipDict2)
    
    wxType = zeros(precipType.shape, dtype='|S10')
    wxType[qpfYes] = precipTransform[qpfYes]
    wxType[popYes] = precipTransform[popYes]
    
    tstormYes = tstorm12hr >= 20
    storms = zeros(tstormYes.shape,dtype='|S10')
    storms[tstormYes] = 'TS'
    
    storms = where(greater_equal(precipType, 0), storms, '')
    
    return concatArrays([wxType, storms])

def execute3(obVis_bestCat, precipType, POP_hour_bestCat):
    "Calculate present weather."
    
    popYes = POP_hour_bestCat == 1
    obsTransform = typeTransform(obVis_bestCat, obsStructDict3)

    precipTransform = typeTransform(precipType, precipDict3)
    
    wxType = zeros(precipType.shape, dtype='|S10')
    wxType[popYes] = precipTransform[popYes]

    
    return concatArrays([wxType, obsTransform])

def typeTransform(dataArray, lookup):
    sDataArray = dataArray.astype('|S10')
    for value in lookup:
        search = dataArray == value
        sDataArray[search] = lookup[value]
    return sDataArray

def concatArrays(stringArrays, delimiter=" "):
    finalStringArray = list()
    for num in xrange(len(stringArrays)):
        flatArray = list(stringArrays[num])
        for stringLoc in xrange(len(flatArray)):
            if num == 0:
                finalStringArray.append('');
            if flatArray[stringLoc].itemsize > 0:
                if num > 0:
                    finalStringArray[stringLoc] += delimiter
                finalStringArray[stringLoc] += flatArray[stringLoc]
    return finalStringArray
