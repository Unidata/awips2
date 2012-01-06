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

# determine the location of the python ported meteolib
# TODO put the meteolib location in the java MasterDerivScript instead of having
#      to manually determine its location like this
import sys
from numpy import zeros

def execute(*args):
    return __execute(*args)[0]

def __execute(*args):
    """ Calculate Convective Available Potential Energy. 
        
    @param temperature
                 if there are five or six input arguments the leading argument is
                 3D (virtual) temperature. 
                 
                 Otherwise, the leading inputs are a list of pressure values and a 
                 list of corresponding (virtual) temperature values.
    @param pressure
    @param potentialTemperature
    @param specificHumidity
                 of the starting parcel
    @param useVirtualTemp
                 if true (1) use virtual temperatures, otherwise (0) use plain
                 temperatures
    @param upperTerminationPressure
                 optional argument designating the upper pressure at which the CAPE
                 computation terminates
                 
    @return a scalar
    
    Temperature must be in Kelvins
    
    """
    
    argLength = len(args)
    
    upperTerminationPressure = None
    useVirtualTemp = args[argLength - 1]
    specificHumidity = args[argLength - 2]
    potentialTemperature = args[argLength - 3]
    pressure = args[argLength - 4]
    temperatureListLengthOffset = 4
    
    if type(useVirtualTemp) != float:
        # an upperTerminationPressure has been supplied
        upperTerminationPressure = useVirtualTemp
        useVirtualTemp = args[argLength - 2]
        specificHumidity = args[argLength - 3]
        potentialTemperature = args[argLength - 4]
        pressure = args[argLength - 5]
        temperatureListLengthOffset = 5
    
    pressureValues = []
    temperatureValues = []
    
    if type(args[0]) == list:
        # we have 3D temperature data
        temperatureValues = args[0][0]
        pressureValues = zeros(temperatureValues.shape, temperatureValues.dtype)
    else:
        # we have a list of pressures followed by temperatures
        temperatureListLength = (argLength - temperatureListLengthOffset) / 2 
        for i in range(0, temperatureListLength):
            pressureValues.append(args[i])
            temperatureValues.append(args[i + temperatureListLength])
    
    # expand each pressure value to a full grid size
    for pressureLevel in range(0, pressureValues.shape[0]):
        gridLevelValues = pressureValues[pressureLevel]
        gridLevelValues[:] = args[0][1][0][pressureLevel]
    
    pressureValue = pressure
    pressure = zeros(temperatureValues.shape, temperatureValues.dtype)
    pressure[:] = pressureValue

    return capeFunc(useVirtualTemp, pressureValues, temperatureValues, pressure, potentialTemperature, specificHumidity, upperTerminationPressure)

import numpy as np
import ctypes as ct
from com.raytheon.edex.meteoLib import MeteoLibUtil

def capeFunc(usetv, p_dat, tve_dat, p0, th0, sh0, ptop = None):
    """ Use the native capeFunc function
    """
    
    # define the c_float_ptr type
    c_float_ptr = ct.POINTER(ct.c_float)
    
    # determine the input dimensions
    dataShape = tve_dat.shape
    
    nx = dataShape[2] if len(dataShape) > 2 else None
    ny = dataShape[1] if len(dataShape) > 1 else None
    nz = dataShape[0]
    
    gridArea = nx * ny 
    
    # flatten all input arrays
    p_dat = np.copy(p_dat)
    tve_dat = np.copy(tve_dat)
    p0 = np.copy(p0)
    th0 = np.copy(th0)
    sh0 = np.copy(sh0)
    p_dat.resize((nz, nx * ny,))
    tve_dat.resize((nz, nx * ny,))
    p0.resize((p0.size,))
    th0.resize((th0.size,))
    sh0.resize((sh0.size,))
    
    if ptop != None:
        ptop.resize((ptop.size,))
    
    
    # load the library
    meteoLibPath = MeteoLibUtil.getSoPath()
    meteoLib =  np.ctypeslib.load_library(meteoLibPath,"")
    capeFunc = meteoLib.capeFunc if ptop == None else meteoLib.capeFuncTop 
    
    # "define" the capeFunc signature
    capeFunc.restype = ct.c_int # return type
    capeFunc.argtypes = [ct.c_float,
                         ct.POINTER(c_float_ptr),
                         ct.POINTER(c_float_ptr),
                         c_float_ptr,
                         c_float_ptr,
                         c_float_ptr,
                         ct.c_int,
                         ct.c_int,
                         ct.c_int,
                         ct.c_int,
                         c_float_ptr,
                         c_float_ptr]
    
    if ptop != None:
        capeFunc.argtypes.append(c_float_ptr)
    
    # result arrays
    cape_dat = np.zeros(gridArea,p_dat.dtype)
    cin_dat = np.zeros(gridArea,p_dat.dtype)
    
    capeFuncArgs = [ct.c_float(usetv),
             
             # get c_style pointers to the 2D input arrays
             (c_float_ptr*len(p_dat))(*[row.ctypes.data_as(c_float_ptr) for row in p_dat]),
             (c_float_ptr*len(tve_dat))(*[row.ctypes.data_as(c_float_ptr) for row in tve_dat]),
             
             p0.ctypes.data_as(c_float_ptr), 
             th0.ctypes.data_as(c_float_ptr),
             sh0.ctypes.data_as(c_float_ptr), 
             ct.c_int(nx),
             ct.c_int(nx),
             ct.c_int(ny),
             ct.c_int(nz),
             cape_dat.ctypes.data_as(c_float_ptr), 
             cin_dat.ctypes.data_as(c_float_ptr)]
    
    if ptop != None:
        capeFuncArgs.append(ptop.ctypes.data_as(c_float_ptr))
    
    val = capeFunc(*capeFuncArgs)
    if val == 1 :
        raise MemoryError('Cape derived parameter ran out of memory')
        exit
    # resize the cape and cin data to the appropriate grid size
    cape_dat.resize((ny,nx))
    cin_dat.resize((ny,nx))
    
    return cape_dat, cin_dat
