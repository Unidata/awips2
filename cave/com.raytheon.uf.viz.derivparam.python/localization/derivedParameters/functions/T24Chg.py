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
###

# ----------------------------------------------------------------
# 
# 
# ----------------------------------------------------------------
import numpy
from numpy import isnan

def execute(temperature, tempFromTenths, accum_temperature24, accum_tempFromTenths24):
    T0 = numpy.where(isnan(tempFromTenths), temperature, tempFromTenths)
    T0 = (T0 * 1.8) + 32
    T24 = numpy.where(isnan(accum_tempFromTenths24), accum_temperature24, accum_tempFromTenths24)
    T24 =(T24 * 1.8) + 32
    return T0 - T24