##
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# HeatRisk_daily Version:  2.5 02/7/2024
#
# Authors:
#    Paul Iniguez, PSR SOO
#    Mark Loeffelbein, WR/STID
#
# This script populates the Climo database with HeatRisk grids. Run once per day via cron.
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer       Description
# ------------ ---------- -----------    -------------------------------------
# Feb 29, 2024            mgamazaychikov Initial creation
########################################################################

MenuItems = []
import SmartScript, h5py
import datetime, sys
import numpy as np
try:
    import cPickle as pickle
except ImportError:
    import pickle

import numpy as np


h5loc = '/awips2/edex/data/share/HeatRiskIndex/data/heatrisk.hdf5'

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def loadGrid(self, varName, dateDict=None, MMDD=None):
        wfoLats, wfoLons = self.getLatLonGrids()

        # Return the new value
        with h5py.File(h5loc, 'r') as hdfid: 
            
            conusLats = hdfid['lats'][()]
            conusLons = hdfid['lons'][()]
            locs = np.maximum(np.abs(conusLons-wfoLons[0, 0]), np.abs(conusLats-wfoLats[0, 0]))
            x, y = np.where(locs == np.min(locs))
            x, y = x[0], y[0]            
            
            if varName in ['yellowLineTmax','yellowLineTmin','DRM','MaxMaxT']:
                conus = hdfid[varName][()]
            elif varName == 'p9999':
                conus = np.zeros((2,wfoLons.shape[0], wfoLons.shape[1]))
                tmp1 = hdfid[varName][0][()]
                tmp2 = hdfid[varName][1][()]
                conus[0] = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], tmp1)
                conus[1] = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], tmp2)
                return conus
            else:
                print(varName)
                i = dateDict[MMDD]
                conus = hdfid[varName][i][()]
        #

        #
        conus = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], conus)
        
        return conus

    def execute(self):
        varNames = {}
        varNames['MaxT'] = 'Tmax'
        varNames['MinT'] = 'Tmin'
        #
        # A Dict of dates to index is needed to recall the data from the hdf5.
        # The hdf5 was created in this manner to save space.
        dateDict = {}
        for i in range(366):
            mmdd = (datetime.datetime(2020,1,1)+datetime.timedelta(days=i)).strftime("%m%d")
            dateDict[mmdd] = i
        ######################################################################################
        #
        # Make sure there are no NaN in .npy files as GFE no longer supports NaN.
        #
        ######################################################################################
        lineLimits = {}
        lineLimits['yellowLineMaxT'] = 60
        lineLimits['yellowLineMinT'] = 50
        lineLimits['orangeLineMaxT'] = 72
        lineLimits['orangeLineMinT'] = 60
        lineLimits['redLineMaxT'] = 90
        lineLimits['redLineMinT'] = 83

        for parm in ['MaxT', 'MinT']:

            # Generate HeatRisk levels in GFE from npy files for next ten days
            now = datetime.datetime.now()-datetime.timedelta(days=1)

            # Obtain start time and duration of parm grid...
            fcstID = self.findDatabase('Fcst')
            tr = self.createTimeRange(0, 240, fcstID)
            times = self.getGridInfo('Fcst', parm, 'SFC', tr)
            startTime = times[0].gridTime().startTime().hour
            duration = times[0].gridTime().duration()/60./60.

            # Loop through the next 10 days...
            for d in range(-1, 10):

                # Create timerange...
                tr = self.createTimeRange(startTime+24*d, startTime+24*d+duration, 'Climo')

                # Loop through red/orange levels...
                for L in ['orange', 'red']:

                    # load data
                    if parm == 'MaxT':
                        fn = L+'LineTmax'
                    if parm == 'MinT':
                        fn = L+'LineTmin'

                    tmp = self.loadGrid(fn,dateDict,str(now.month).rjust(2, '0')+str(now.day).rjust(2, '0'))

                    heatName = L+'Line'+parm
                    lineLimit = lineLimits[heatName]
                    tmp[np.isnan(tmp)] = lineLimit

                    # Create Grid
                    self.createGrid("Climo", 'Heat'+L.capitalize()+parm, "SCALAR", tmp.astype('float32').clip(-100,140), tr)

                # Create flat yellow level
                varName = varNames[parm]

                tmp = self.loadGrid('yellowLine%s'%varName)

                heatName = 'yellowLine'+parm
                lineLimit = lineLimits[heatName]
                tmp[np.isnan(tmp)] = lineLimit

                if parm == 'MaxT':
                    self.createGrid("Climo", 'HeatYellowMaxT', "SCALAR", tmp.astype('float32').clip(-100,140), tr)
                if parm == 'MinT':
                    self.createGrid("Climo", 'HeatYellowMinT', "SCALAR", tmp.astype('float32').clip(-100,140), tr)

                # Move date Forward
                now += datetime.timedelta(days=1)

        # Save grids
        self.saveElements(['HeatYellowMaxT', 'HeatOrangeMaxT', 'HeatRedMaxT', 'HeatYellowMinT', 'HeatOrangeMinT', 'HeatRedMinT'])


    def extractWFOfromConus(self, x, y, xshape, yshape, conusData):
        wfoData = conusData[x:x+int(xshape), y:y+int(yshape)]
        return conusData[x:x+int(xshape), y:y+int(yshape)]

