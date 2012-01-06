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

#    SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    17 Feb 2010     #4502         jelkins        Initial Creation.

from com.raytheon.edex.meteoLib import MeteoLibUtil
import numpy as np
import ctypes as ct

def dcapeFunc(usetv, p_dat, t_dat, td_dat, p0, th0, sh0, max_evap, max_rh):
    """ Use the native dcapeFunc function
    """
    
    # define the c_float_ptr type
    c_float_ptr = ct.POINTER(ct.c_float)
    
    # determine the input dimensions
    dataShape = t_dat.shape
    
    nx = dataShape[2] if len(dataShape) > 2 else None
    ny = dataShape[1] if len(dataShape) > 1 else None
    nz = dataShape[0]
    
    gridArea = nx * ny 
    
    # flatten all input arrays
    p_dat.resize((nz, nx * ny,))
    t_dat.resize((nz, nx * ny,))
    td_dat.resize((nz, nx * ny,))
    p0.resize((p0.size,))
    th0.resize((th0.size,))
    sh0.resize((sh0.size,))
    
    # load the library
    meteoLibPath = MeteoLibUtil.getSoPath()
    meteoLib =  np.ctypeslib.load_library(meteoLibPath,"")
    dcapeFunc = meteoLib.dcapeFunc
    
    # "define" the capeFunc signature
    dcapeFunc.restype = None # return type
    dcapeFunc.argtypes = [ct.c_float,
                         ct.POINTER(c_float_ptr),
                         ct.POINTER(c_float_ptr),
                         ct.POINTER(c_float_ptr),
                         c_float_ptr,
                         c_float_ptr,
                         c_float_ptr,
                         ct.c_int,
                         ct.c_int,
                         ct.c_int,
                         ct.c_int,
                         ct.c_float,
                         ct.c_float,
                         c_float_ptr]
    
    # result arrays
    dcape_dat = np.zeros(gridArea,p_dat.dtype)
    
    dcapeFuncArgs = [ct.c_float(usetv),
             
             # get c_style pointers to the 2D input arrays
             (c_float_ptr*len(p_dat))(*[row.ctypes.data_as(c_float_ptr) for row in p_dat]),
             (c_float_ptr*len(t_dat))(*[row.ctypes.data_as(c_float_ptr) for row in t_dat]),
             (c_float_ptr*len(td_dat))(*[row.ctypes.data_as(c_float_ptr) for row in td_dat]),
             
             p0.ctypes.data_as(c_float_ptr), 
             th0.ctypes.data_as(c_float_ptr),
             sh0.ctypes.data_as(c_float_ptr), 
             ct.c_int(nx),
             ct.c_int(nx),
             ct.c_int(ny),
             ct.c_int(nz),
             ct.c_float(max_evap),
             ct.c_float(max_rh),
             dcape_dat.ctypes.data_as(c_float_ptr)]
    
    dcapeFunc(*dcapeFuncArgs)
    
    # resize the cape data to the appropriate grid size
    dcape_dat.resize((ny,nx))
    
    return dcape_dat