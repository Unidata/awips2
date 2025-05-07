##
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# HeatRisk_calculate Version: 2.5 02/7/2024
#
# Authors:
#    Paul Iniguez, PSR SOO
#    Mark Loeffelbein, WR/STID
#
# This script calculate the daily HeatRisk based on Obs and Fcst data. It also
# recommends what WWA to issue. This should be run on-demand by forecasters.
#
# ----------------------------------------------------------------------------
#
# SOFTWARE HISTORY
#
# Date         Ticket#    Engineer       Description
# ------------ ---------- -----------    -------------------------------------
# Feb 29, 2024            mgamazaychikov Initial creation
########################################################################

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##
h5loc = '/awips2/edex/data/share/HeatRiskIndex/data/heatrisk.hdf5'

zonesExcludedFromDS=['AZZ005','AZZ006']

MenuItems = ["Populate"]
import SmartScript, datetime, numpy as np, pickle, os, re, sys, h5py
from ufpy.dataaccess import DataAccessLayer

RECOMMENDED_SITES = ["ABR", "AKQ", "ALY", "APX", "ARX", "BGM", "BIS", "BMX", 
                     "BOX", "BRO", "BTV", "BUF", "CAE", "CAR", "CHS", "CLE", 
                     "CRP", "CTP", "DLH", "DMX", "DTX", "DVN", "EAX", "EWX", 
                     "FFC", "FGF", "FSD", "FWD", "GID", "GRB", "GRR", "GSP", 
                     "GYX", "HGX", "HUN", "ICT", "ILM", "ILN", "ILX", "IND", 
                     "IWX", "JAN", "JAX", "JKL", "KEY", "LCH", "LIX", "LMK", 
                     "LOT", "LSX", "LUB", "LWX", "LZK", "MEG", "MFL", "MHX", 
                     "MKX", "MLB", "MOB", "MPX", "MQT", "MRX", "OAX", "OHX", 
                     "OKX", "OUN", "PAH", "PBZ", "PHI", "RAH", "RLX", "RNK", 
                     "SGF", "SHV", "SJT", "SJU", "TAE", "TBW", "TOP", "TSA"]

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
 
            if varName == "noAdvisory":
                return hdfid[varName].asstr()[...] 
            elif varName in ['yellowLineTmax','yellowLineTmin','DRM','MaxMaxT']:
                conus = hdfid[varName][()]
            elif varName == 'p9999':
                conus = np.zeros((2,wfoLons.shape[0], wfoLons.shape[1]))
                tmp1 = hdfid[varName][0][()]
                tmp2 = hdfid[varName][1][()]
                conus[0] = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], tmp1)
                conus[1] = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], tmp2)
                return conus
            else:
                i = dateDict[MMDD]
                conus = hdfid[varName][i][()]
        #

        #
        conus = self.extractWFOfromConus(x, y, wfoLons.shape[0], wfoLons.shape[1], conus)
        
        return conus
        
    def execute(self):
        #
        ##################################################
        #
        # Make sure HeatRisk grids are loaded and clean...
        #
        ##################################################
        #
        dbID = self.findDatabase('Fcst')
        for p in ['MaxT', 'MinT', 'Hazards', 'HeatImpactLevelsMaxT', 'HeatImpactLevelsMinT', 'HeatRisk', 'HeatRiskWWA', 'HeatRiskWWAbyZone']:
            self.loadParm(dbID, p, 'SFC')
        dbID = self.findDatabase('Climo')
        for p in ['MaxT', 'HeatRedMaxT', 'HeatOrangeMaxT', 'HeatYellowMaxT', 'MinT', 'HeatRedMinT', 'HeatOrangeMinT', 'HeatYellowMinT']:
           self.loadParm(dbID, p, 'SFC')
        #
        tr = self.createTimeRange(-100, 300)
        self.deleteGrid('Fcst', 'HeatImpactLevelsMaxT', 'SFC', tr)
        self.deleteGrid('Fcst', 'HeatImpactLevelsMinT', 'SFC', tr)
        self.deleteGrid('Fcst', 'HeatRisk', 'SFC', tr)
        self.deleteGrid('Fcst', 'HeatRiskWWA', 'SFC', tr)
        self.deleteGrid('Fcst', 'HeatRiskWWAbyZone', 'SFC', tr)
        #
        ##################################################
        #
        # Load/set various parms and arrays
        #
        ##################################################
        #
        # A Dict of dtes to index is needed to recall the data from the hdf5.
        # The hdf5 was created in this manner to save space.
        dateDict = {}
        for i in range(366):
            mmdd = (datetime.datetime(2020,1,1)+datetime.timedelta(days=i)).strftime("%m%d")
            dateDict[mmdd] = i
        #
        # Get WFO ID
        #
        wfo = self.getSiteID().strip()
        #
        # Make a mask of all the zones within the GFE domain.
        #
        states = ['AL','AK','AZ','AR','CA','CO','CT','DC','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME',
                  'MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI',
                  'SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']
        # Get a list of all edit areas and use the ones that conform to Zone names.
        zones = [z for z in self.editAreaList() if (re.search('^[A-Z]{2}Z[0-9]{3}$', z) and z[:2] in states)]
        #        
        # Make a mask of all the zones the GFE (not just CWA) has defined from the list obtained above.
        wfosMask = np.array([self.encodeEditArea(z) for z in zones]).any(axis=0)
        #
        # Make a list of zones within the CWA (via DAF).
        #
        reqParms = {'datatype' : 'maps',
                    'table' : 'mapdata.zone',
                    'locationField' : 'cwa',
                    'geomField' : 'the_geom',
                    'locationNames' : [wfo],
                    'parameters' : ['state', 'zone'],
                    }
        req = DataAccessLayer.newDataRequest(**reqParms)
        result = DataAccessLayer.getGeometryData(req)
        zones = [record.getString('state')+'Z'+record.getString('zone') for record in result]
        #        
        # Create a mask that will not apply the despeckle algorithm to.
        despeckleMask = self.empty(np.bool)
        
        tmp = []
        for z in zonesExcludedFromDS:
            if z in zones:
                tmp.append(z)
        if len(tmp)>0:
            despeckleMask = np.array([self.encodeEditArea(z) for z in tmp]).any(axis=0)
        #        
        # Get the NCEI clmate points from the install files. Then add them to the despeckleMask
        # to keep the despeckle algorithm from removing single points at NCEI locations.                
        climoPoints = self.getDataLocations()
        pointMask = self.createMaskFromPoints(climoPoints)
        despeckleMask[pointMask] = True        
        #
        # Get list of zones and those where no advisory is to be issued
        #
        noAdvisory = self.loadGrid("noAdvisory")
        #
        # Load Diurnal Range Modifier needs to be used (created during install process)
        #
        DRM = self.loadGrid('DRM')
        #
        # Load array of grid point lon values
        #
        lats, lons = self.getLatLonGrids()
        #
        # Load the 99.9th percentile grid
        #
        p9999 = self.loadGrid('p9999')
        #
        # Load the maximum MaxT Normal
        #
        MaxMaxTNormal = self.loadGrid('MaxMaxT')
        #
        # Load the red/orange base values
        #
        redBaseMaxT    = self.loadGrid('redLineTmax', dateDict, '0101' )
        orangeBaseMaxT = self.loadGrid('orangeLineTmax', dateDict, '0101')
        redBaseMinT    = self.loadGrid('redLineTmin', dateDict, '0101')
        #
        # Initiate dict to hold daily temperature, HIL, and HR arrays
        #
        Grids = {
            'MaxT': [], 'MinT': [],
            'RedMaxT': [], 'OrangeMaxT': [], 'YellowMaxT': [],
            'RedMinT': [], 'OrangeMinT': [], 'YellowMinT': [],
            'HILMaxT': [], 'HILMinT': [], 'HR': [],
        }
        #
        ##################################################
        #
        # Obtain Observed/Forecast MaxT/MinT Grids and calculate HIL
        #
        ##################################################
        #
        # Set a variable to the current time
        #
        now = datetime.datetime.now()
        #
        # Go through MaxT and MinT
        #
        for parm in ['MinT', 'MaxT']:
            #
            # Set necessary parameters
            #
            if parm == 'MaxT': mode = 'Max'
            if parm == 'MinT': mode = 'Min'
            #
            # Find start time and duration of grid (can vary by office)
            #
            tr = self.createTimeRange(0, 240, 'Fcst')
            gridTimes = self.getGridInfo('Fcst', parm, 'SFC', tr)
            startTime = gridTimes[0].gridTime().startTime().hour
            duration = gridTimes[0].gridTime().duration()/60./60.
            #
            # Loop through the past 3 days and next 9/10 (allows for full calculations)
            #
            for d in range(-2, [9 if parm == 'MaxT' else 10][0]):
                if parm == 'MaxT' and len(Grids['MaxT'])+1 >= len(Grids['MinT']):
                    print(len(Grids['MaxT']),len(Grids['MinT']))
                    break
                #
                # Access in descending order Obs/URMA/Fcst/NBM
                #
                for model in ['Obs','URMA','Fcst','NBM']:
                    #
                    # Build timerange, pull grid
                    #
                    tr = self.createTimeRange(d*24+startTime, d*24+startTime+duration, model)
                    grid = self.getGrids(model, parm, "SFC", tr, noDataError=0, mode=mode)
                    #
                    # If a valid grid was obtained...
                    #
                    if grid is not None:
                        #
                        # If it is an observation grid (Obs/URMA), check
                        # to make sure the current time is beyond the end of the grid.
                        #
                        gridhistory = self.getGridHistory(model, parm, 'SFC', tr)
                        endTime = gridhistory[0][0][2].endTime().timetuple()
                        endTime = datetime.datetime(endTime.tm_year, endTime.tm_mon, endTime.tm_mday, endTime.tm_hour) 
                        if model in ['Obs','URMA'] and now > endTime:
                            break
                        elif model in ['Fcst','NBM']:
                            break
                if grid is None:
                    print('Could not find data for day', d)
                    break 
                #
                # Store the ob/forecast grid
                #

                Grids[parm].append(grid)
                #
                # Pull the levels for the corresponding timerange
                #
                if parm == 'MaxT':
                    red = self.loadGrid('redLineTmax',dateDict, '{}'.format(tr.startTime().strftime('%m%d')))
                    orange = self.loadGrid('orangeLineTmax', dateDict, '{}'.format(tr.startTime().strftime('%m%d')))
                    yellow = self.loadGrid('yellowLineTmax')
                else:
                    red = self.loadGrid('redLineTmin',dateDict,'{}'.format(tr.endTime().strftime('%m%d')))
                    orange = self.loadGrid('orangeLineTmin',dateDict,'{}'.format(tr.endTime().strftime('%m%d')))
                    yellow = self.loadGrid('yellowLineTmin')
                #
                # Add to Grids array for later use
                #
                Grids['Red'+parm].append(red)
                Grids['Orange'+parm].append(orange)
                Grids['Yellow'+parm].append(yellow)
                #
                # Compute the HIL grid
                #
                HIL = red * 0
                HIL[grid>=yellow] = 1
                HIL[grid>=orange] = 2
                HIL[grid>=red] = 3
                #
                # Round the HIL output and mask to WFOs area
                #
                HIL = np.round(HIL+0.005, 0)
                HIL = np.where(wfosMask, HIL, 0)
                #
                # Create the grid, store the array
                #
                tr = self.createTimeRange(d*24+startTime, d*24+startTime+duration, 'Fcst')
                self.createGrid("Fcst", "HeatImpactLevels"+parm, "SCALAR", HIL.astype("float32"), tr)
                Grids['HIL'+parm].append(HIL)
            #
            # Save grids
            #
            self.saveElements(['HeatImpactLevels'+parm])
        #
        ###############################################################
        #
        #  Calculate Initial HeatRisk
        #
        ###############################################################
        #
        # Iterate through the number of MaxT grids
        #
        for d in range(len(Grids['MaxT'])):
            #
            # Initiate empty HeatRisk grid for today
            #
            HR = Grids['MaxT'][d].copy() * 0
            #
            # Equation for locations west of 104 deg...
            #
            HR = np.where(lons<-104, (2.00*Grids['HILMaxT'][d] + 0.35*Grids['HILMinT'][d] + 0.65*Grids['HILMinT'][d+1])/3.0, HR)
            #
            # Equation for locations east of 97 deg
            #
            HR = np.where(lons>-97, (1.80*Grids['HILMaxT'][d] + 1.10*Grids['HILMinT'][d] + 1.10*Grids['HILMinT'][d+1])/4.0, HR)
            #
            # Equation for central areas with DRM
            #
            HR = np.where( (lons>=-104)&(lons<=-97)&(DRM), (1.80*Grids['HILMaxT'][d] + 1.10*Grids['HILMinT'][d] + 1.10*Grids['HILMinT'][d+1])/4.0, HR)
            #
            # Equation for central areas without DRM
            #
            HR = np.where( (lons>=-104)&(lons<=-97)&(~DRM), (2.00*Grids['HILMaxT'][d] + 0.35*Grids['HILMinT'][d] + 0.65*Grids['HILMinT'][d+1])/3.0, HR)
            #
            # Round output
            #
            HR = HR.round(2)
            #
            # Identify periods of approaching next higher category (moist areas only),
            # done in consideration of humid/moist conditions.
            #
            # Apply only to places where the DRM applies and east of 104 deg
            #
            mask = np.where( (lons>-104) & (DRM), True, False)
            #
            # Define the upper part of the red and orange distributions ("hump")
            #
            hump_red = np.round((Grids['RedMaxT'][d] + redBaseMaxT)/2 + 0.005, 0)
            hump_orange = np.round((Grids['OrangeMaxT'][d] + orangeBaseMaxT)/2 + 0.005, 0)
            #
            # For identified locations, apply following adjustments...
            #
            # Where MaxT values are in the "red hump"...
            #
            # ...and the Max/Min/Min is 2/3/3 add 0.2 (elevates ADVY to WARN)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==3), HR+0.2, HR)
            #
            # ...and the Max/Min/Min is 2/2/3 add 0.35 (elevates ADVY to WARN)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==2) & (Grids['HILMinT'][d+1]==3), HR+0.35, HR)
            #
            # ...and the Max/Min/Min is 2/3/2 add 0.35 (elevates CONS to ADVY)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==2), HR+0.35, HR)
            #
            # ...and the Max/Min/Min is 2/1/3 add 0.4 (elevates CONS to ADVY)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==1) & (Grids['HILMinT'][d+1]==3), HR+0.4, HR)
            #
            # ...and the Max/Min/Min is 2/3/1 add 0.4 (elevates CONS to ADVY)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==1), HR+0.4, HR)
            #
            # For areas in dry and the Max/Min/Min is 2/3/3 add 0.10 (elevates COND to ADVY)
            HR = np.where( (~mask) & (Grids['MaxT'][d]<Grids['RedMaxT'][d]) & (Grids['MaxT'][d]>=hump_red) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==3), HR+0.1, HR)
            #
            # Where MaxT values are in the "orange hump"...
            #
            # ...and the Max/Min/Min is 1/3/3 add 0.35 (elevates CONS to ADVY)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['OrangeMaxT'][d]) & (Grids['MaxT'][d]>=hump_orange) & (Grids['HILMaxT'][d]==1) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==3), HR+0.35, HR)
            #
            # ...and the Max/Min/Min is 1/2/3 add 0.25 (elevates NONE to CONS)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['OrangeMaxT'][d]) & (Grids['MaxT'][d]>=hump_orange) & (Grids['HILMaxT'][d]==1) & (Grids['HILMinT'][d]==2) & (Grids['HILMinT'][d+1]==3), HR+0.25, HR)
            #
            # ...and the Max/Min/Min is 1/3/2 add 0.25 (elevates NONE to CONS)
            HR = np.where( (mask) & (Grids['MaxT'][d]<Grids['OrangeMaxT'][d]) & (Grids['MaxT'][d]>=hump_orange) & (Grids['HILMaxT'][d]==1) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==2), HR+0.25, HR)
            #
            # Where both MinT values are above the red base and Max/Min/Min is 2/2/2 add 0.4 (elevated CONS to ADVY)
            HR = np.where( (mask) & (Grids['MinT'][d]>=redBaseMinT) & (Grids['MinT'][d+1]>=redBaseMinT) & (Grids['HILMaxT'][d]==2) & (Grids['HILMinT'][d]==2) & (Grids['HILMinT'][d+1]==2), HR+0.4, HR)
            #
            # Where both MinT values are above the red base and Max/Min/Min is 3/2/2 add 0.2 (elevated ADVY to WARN)
            HR = np.where( (mask) & (Grids['MinT'][d]>=redBaseMinT) & (Grids['MinT'][d+1]>=redBaseMinT) & (Grids['HILMaxT'][d]==3) & (Grids['HILMinT'][d]==2) & (Grids['HILMinT'][d+1]==2), HR+0.2, HR)
            #
            #  Store initial HeatRisk array
            Grids['HR'].append(HR)
            #
        ###############################################################
        #
        #  Calculate Second-Pass HeatRisk
        #
        ###############################################################
        #
        # Make a second pass through for calculations that need HR from
        # yesterday/tomorrow (truncated by one day front/back)
        #
        for d in range(1, len(Grids['MaxT'])):
            #
            # Adjust to avoid not have warning period end with advisory due to warm/humid night...
            #
            # ...where Max/Min/Min is 3/3/1 and yesterday's HeatRisk is >2.61 add 0.2 (elevated ADVY to WARN)
            Grids['HR'][d] = np.where( (mask) & (Grids['HR'][d-1]>2.61) & (Grids['HILMaxT'][d]==3) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==1), Grids['HR'][d]+0.2, Grids['HR'][d])
            #
            # ...where Max/Min/Min is 3/1/3 and yesterday's HeatRisk is >2.61 add 0.2 (elevated ADVY to WARN)
            Grids['HR'][d] = np.where( (mask) & (Grids['HR'][d-1]>2.61) & (Grids['HILMaxT'][d]==3) & (Grids['HILMinT'][d]==1) & (Grids['HILMinT'][d+1]==3), Grids['HR'][d]+0.2, Grids['HR'][d])
            #
            # For non-DRM areas, check for dry scenario when one low MinT terminates warning early add 0.1 (elevated ADVY to WARN)
            Grids['HR'][d] = np.where( (~mask) & (Grids['HR'][d-1]>2.61) & (Grids['HILMaxT'][d]==3) & (Grids['HILMinT'][d]==3) & (Grids['HILMinT'][d+1]==1), Grids['HR'][d]+0.1, Grids['HR'][d])
            #
            # Identify areas where the HR yesterday was 3 and today is 3, then assign as 4 (magenta).
            #
            Grids['HR'][d] = np.where( (Grids['HR'][d-1]>=3.0) & (Grids['HR'][d]==3.0), 4, Grids['HR'][d])
            #
        ###############################################################
        #
        #  Calculate Third-Pass HeatRisk
        #
        ###############################################################
        #
        # Remove single magenta days (note not run for first/last day)
        #
        for d in range(1, len(Grids['MaxT'])-1):
            #
            # Adjust for "warm" climates (max normal 93+) to remove single magenta days
            #
            Grids['HR'][d] = np.where( (MaxMaxTNormal>=93) & (Grids['HR'][d-1]<4) & (Grids['HR'][d]==4) & (Grids['HR'][d+1]<4), 3, Grids['HR'][d])
            #
        ###############################################################
        #
        #  Calculate Fourth-Pass HeatRisk
        #
        ###############################################################
        #
        # Nudge near-all-time-record days up. (note not run for first/last day)
        #
        for d in range(1, len(Grids['MaxT'])-1):
            #
            # Adjust to ensure if MinT AOB 99.99th percentile (and at least 80F) and MaxT is red, day is red
            #
            Grids['HR'][d] = np.where( (Grids['MinT'][d]>=p9999[1]) & (Grids['MinT'][d]>=80) & (Grids['HILMaxT'][d]==3) & (Grids['HR'][d]<4), 3.0, Grids['HR'][d])
            #
            # Adjust to ensure if MinT AOB 99.99th percentile (and at least 80F) and MaxT is orange, day is red
            #
            Grids['HR'][d] = np.where( (Grids['MinT'][d]>=p9999[1]) & (Grids['MinT'][d]>=80) & (Grids['HILMaxT'][d]==2), np.maximum(Grids['HR'][d], 3.0), Grids['HR'][d])
            #
            # Adjust to ensure days with MaxT AOB the 99.99th percentile but below 98F and at or above 90F are (high) orange
            #
            Grids['HR'][d] = np.where( (Grids['MaxT'][d]>=p9999[0]) & (Grids['MaxT'][d]<98) & (Grids['MaxT'][d]>=90), np.maximum(Grids['HR'][d], 2.36), Grids['HR'][d])
            #
            # Adjust to ensure days with MaxT AOB 99.99th percentile and 98F+ are red
            #
            Grids['HR'][d] = np.where( (Grids['MaxT'][d]>=p9999[0]) & (Grids['MaxT'][d]>=98), np.maximum(Grids['HR'][d], 3.0), Grids['HR'][d])
            #
            #  Adjust to ensure days with MaxT AOB 99.99th percentile and 102F+ are magenta
            #
            Grids['HR'][d] = np.where( (Grids['MaxT'][d]>=p9999[0]) & (Grids['MaxT'][d]>=102), 4.0, Grids['HR'][d])
            #
            # Adjust to push near-magenta days to magenta following a magenta
            #
            # Removed with version 2.3
            #
            #Grids['HR'][d] = np.where( (Grids['HR'][d-1]==4) & (Grids['HR'][d]>2.61), 4.0, Grids['HR'][d])
            #
            # Apply the WFOs mask
            #
            Grids['HR'][d] = np.where(wfosMask, Grids['HR'][d], 0)
            #
        ###############################################################
        #
        #  Create HeatRisk Grids
        #
        ###############################################################
        #
        # Loop through two less (won't plot first/last).
        #
        for d in range(1,len(Grids['HR'])-1):
            #
            # Create the final HeatRnp.round(Grids['HR'][d]+0.005)isk grid. The timerange is adjusted to correctly plot
            # starting with yesterday's HeatRisk. Note the rounding.
            # 
            tr = self.createTimeRange((d-2)*24+6, (d-2)*24+6+24, 'Fcst')
            #
            # Apply process to remove single pixel
            #
            HR = self.despeckle(np.where(wfosMask, np.round(Grids['HR'][d]+0.005), 0), excludeMask=despeckleMask)
            #HR = np.where(wfosMask, np.round(Grids['HR'][d]+0.005), 0)
            
            self.createGrid('Fcst', 'HeatRisk', 'SCALAR', HR.astype('float32'), tr)
            self.saveElements(['HeatRisk'])
            #
            # Now create the recommended product grid
            #
            HRWWA = np.where(Grids['HR'][d] >= 2.00, 1, 0)
            HRWWA = np.where(Grids['HR'][d] >= 2.35, 2, HRWWA)
            HRWWA = np.where(Grids['HR'][d] >= 2.62, 3, HRWWA)
            #
            # For WFOs generally east of 100 deg, use "Consider/Recommended" approach.
            #
            if np.mean(lons) > -100 or wfo in ['ABR','BIS','EWX','GID','LUB','OUN','SJT']:
                HRWWA = np.where( (lons>-104) & (HRWWA>=2), 4, HRWWA)
            #
            # Now check each zone and determine the median value, then set the whole zone to that median value
            #
            # Start by initializing an empty grid
            HeatRiskWWA_Median = self.empty()
            #
            # Loop through each zone
            #
            for zone in zones:
                #
                # Make a zone mask
                #
                mask = self.getEditArea(zone)
                #
                # This is needed to make sure zone is a defined edit area
                # encodeEditArea throws an error rather than returning None
                #
                if mask is None:
                    continue
                mask = self.encodeEditArea(zone)
                #
                # Define the zone as its median value.
                #
                med = np.median(HRWWA[mask])
                #
                # If a HT.Y is suggested but it is in the list of No Advisory zones,
                # do not assign the value. Otherwise assign the suggested product to
                # the masked area.
                #
                if med < 2.35 and zone in noAdvisory:
                    pass
                else:
                    HeatRiskWWA_Median = np.where(mask, med, HeatRiskWWA_Median)

            #
            # Set the color table based on the type of wfo
            #
            if wfo in RECOMMENDED_SITES:
                definedColorTable = "GFE/HeatRiskWWA_Rec"
            else:
                definedColorTable = "GFE/HeatRiskWWA"
            
            #
            # Create Grids
            #

            keys = ["None", "Consider", "Advisory", "Warning", "Recommended"]
            self.createGrid("Fcst", "HeatRiskWWA", "DISCRETE", (HRWWA.astype(np.int8), keys), tr,
                     discreteKeys=keys, discreteAuxDataLength=0, discreteOverlap=0,
                     defaultColorTable=definedColorTable)
            self.createGrid("Fcst", "HeatRiskWWAbyZone", "DISCRETE", (HeatRiskWWA_Median.astype(np.int8), keys), tr,
                     discreteKeys=keys, discreteAuxDataLength=0, discreteOverlap=0,
                     defaultColorTable=definedColorTable)
        #
        # Save the Output
        self.saveElements(['HeatRisk','HeatRiskWWA','HeatRiskWWAbyZone'])

    def despeckle(self, grid,excludeMask=None):
        #
        pixelMask = np.ones(grid.shape, bool)
        pixelMask[:, 1:] = np.logical_and(pixelMask[:, 1:], grid[:, :-1] != grid[:, 1:])# left
        pixelMask[:, :-1] = np.logical_and(pixelMask[:, :-1], grid[:, 1:] != grid[:, :-1])# right
        pixelMask[1:, :] = np.logical_and(pixelMask[1:, :], grid[:-1, :] != grid[1:, :])# above
        pixelMask[:-1, :] = np.logical_and(pixelMask[:-1, :], grid[1:, :] != grid[:-1, :])# below
        pixelMask2 = np.ones(grid.shape, bool)
        pixelMask2[:-1, 1:] = np.logical_and(pixelMask2[:-1, 1:], grid[1:, :-1] != grid[:-1, 1:])# right/below
        pixelMask2[:-1, :-1] = np.logical_and(pixelMask2[:-1, :-1], grid[1:, 1:] != grid[:-1, :-1])# right/below
        pixelMask2[1:, 1:] = np.logical_and(pixelMask2[1:, 1:], grid[:-1, :-1] != grid[1:, 1:])# left/above
        pixelMask2[1:, :-1] = np.logical_and(pixelMask2[1:, :-1], grid[:-1, 1:] != grid[1:, :-1])# right/above
        pixelMask2 = np.logical_not(pixelMask2) # Reverse True/False so values can be combined with pixelMask
        pixelMask[pixelMask2] = False # Create single mask of all single pixels.
        gridOriginal = grid.copy()
        #
        '''Adapted from SmoothLots tool '''
        k = 3 # has to be integer number of gridpoints
        (ny, nx)=grid.shape
        k2=k*2
        #
        #  Remove the minimum from the grid so that cumsum over a full
        #  row or column of the grid doesn't get so big that precision
        #  might be lost.
        #
        fullmin=np.minimum.reduce(np.minimum.reduce(grid))
        gridmin=grid-fullmin
        #
        #  Average over the first (y) dimension - making the 'mid' grid
        #
        mid=grid*0.0
        c=np.cumsum(gridmin, 0)
        nym1=ny-1
        midy=int((ny-1.0)/2.0)
        ymax=min(k+1, midy+1)
        for j in range(ymax): # handle edges
            jk=min(j+k, nym1)
            jk2=max(nym1-j-k-1, -1)
            mid[j,:]=c[jk,:]/float(jk+1)
            if jk2==-1:
                mid[nym1-j,:]=c[nym1,:]/float(jk+1)
            else:
                mid[nym1-j,:]=(c[nym1,:]-c[jk2,:])/float(jk+1)
        #
        #  The really fast part
        #
        if ((k+1)<=(ny-k)): # middle
            mid[k+1:ny-k,:]=(c[k2+1:,:]-c[:-k2-1,:])/float(k2+1)
        #
        #  Average over the second (x) dimension - making the 'out' grid
        #
        c=np.cumsum(mid, 1)
        out=grid*0.0
        nxm1=nx-1
        midx=int((nx-1.0)/2.0)
        xmax=min(k+1, midx+1)
        for j in range(xmax): # handle edges
            jk=min(j+k, nxm1)
            jk2=max(nxm1-j-k-1, -1)
            out[:, j]=c[:, jk]/float(jk+1)
            if jk2==-1:
                out[:, nxm1-j]=c[:, nxm1]/float(jk+1)
            else:
                out[:, nxm1-j]=(c[:, nxm1]-c[:, jk2])/float(jk+1)
        #
        #  The really fast part
        #
        if ((k+1)<=(nx-k)): # middle
            out[:, k+1:nx-k]=(c[:, k2+1:]-c[:, :-k2-1])/float(k2+1)
        #
        #  Add the minimum back in
        #
        out += fullmin
        #
        gridOriginal[pixelMask] = out[pixelMask]
        #
        if excludeMask is not None:
            gridOriginal[excludeMask] = grid[excludeMask]
        return gridOriginal
    
    def getDataLocations(self):
        #
        valuelist=[]

        with h5py.File(h5loc, 'r') as hdfid:
            datalines = hdfid['pointList'][()]
        
        #
        for line in datalines:
            #
            # Get values
            #

            latf, lonf = line
            x, y = self.getGridCell(latf, lonf)

            #
            # Finally, add it to the list
            #
            if x is not None and y is not None:
                valuelist.append((x, y))
        #
        return valuelist
    
    def createMaskFromPoints(self,climoPoints):
        mask = self.empty(np.bool)
        for (x,y) in climoPoints:
            mask[y][x] = True
        return mask

    def extractWFOfromConus(self, x, y, xshape, yshape, conusData):
        wfoData = conusData[x:x+int(xshape), y:y+int(yshape)]
        return conusData[x:x+int(xshape), y:y+int(yshape)]

