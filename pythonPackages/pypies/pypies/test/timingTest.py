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


#
# Timing tests for various storage plugins for the
# Python Process Isolated Enhanced Storage
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/20/10                      njensen       Initial Creation.
#    
# 
#

import numpy, os

ntrials = 10

from timeit import Timer
import random

import TablesTiming
import H5pyTiming
import Netcdf4Timing

sampleIndex = []
for i in range(200):
    sampleIndex.append(random.randint(0, 1499))
oneDimData = numpy.random.random((1000))
twoDimData = numpy.random.random((642, 320))


fname = "/home/njensen/pypies/testfiles/test.h5" 
                    

def createAndAppendTest(interface, array):
    g = interface.createGroup("abc")
    ds = interface.createDataset(g, "Data", array)
    for i in range(500):
        interface.appendValue(ds, random.random())
    
def test1D(clz):
    try:
        t = clz(fname, "w")
        createAndAppendTest(t, oneDimData)
        t.close()
        t = clz(fname, "r")
        sampleTest(t)
    finally:
        t.close()
        os.remove(fname)

def test2D(clz):
    try:
        t = clz(fname, "w")
        create2DTest(t, twoDimData)
        t.close()
        t = clz(fname, "r")
        retrieve2DTest(t)
    finally:
        t.close()
        os.remove(fname)
        
def create2DTest(interface, twoDimData):
    g = interface.createGroup('abc2')
    ds = interface.createDataset(g, "Data", twoDimData, 2)

def sampleTest(interface):
    ds = interface.getDataset('/abc/Data')
    for index in sampleIndex:
        interface.sampleValue(ds, index)

def retrieve2DTest(interface):
    ds = interface.getDataset('/abc2/Data')
    
    
def main():        
    timer = Timer("test1D(TablesTiming.TablesTiming)", "from __main__ import test1D; from __main__ import TablesTiming")
    print "pytables 1D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    
    timer = Timer("test1D(H5pyTiming.H5pyTiming)", "from __main__ import test1D; from __main__ import H5pyTiming")
    print "h5py 1D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    
    timer = Timer("test1D(Netcdf4Timing.Netcdf4Timing)", "from __main__ import test1D; from __main__ import Netcdf4Timing")
    print "netcdf4 1D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    
    timer = Timer("test2D(TablesTiming.TablesTiming)", "from __main__ import test2D; from __main__ import TablesTiming")
    print "pytables 2D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    
    timer = Timer("test2D(H5pyTiming.H5pyTiming)", "from __main__ import test2D; from __main__ import H5pyTiming")
    print "h5py 2D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'
    
    timer = Timer("test2D(Netcdf4Timing.Netcdf4Timing)", "from __main__ import test2D; from __main__ import Netcdf4Timing")
    print "netcdf4 2D took", sum(timer.repeat(ntrials,1))/ntrials,'seconds'

if __name__ == "__main__":
    main()