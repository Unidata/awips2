The [python-awips](https://python-awips.readthedocs.io/en/latest/) package provides a data access framework for requesting grid and geometry datasets from an EDEX server.

!!! info "For a more detailed look at the python-awips package, refer to the [full documentation site](https://python-awips.readthedocs.io) which includes a number of [plotting examples for different data types](https://python-awips.readthedocs.io/en/latest/examples/index.html)."

---

## Install

    pip install python-awips

---

## Requirements

* Python 2.7+
* Numpy 1.7+
* Shapely 1.4+
* MetPy and enum34 to run Jupyter Notebook examples

---

## Example

This examples covers the callable methods of the Python AWIPS DAF when working with gridded data.  We start with a connection to an EDEX server, then query data types, then grid names, parameters, levels, and other information. Finally the gridded data is plotted for its domain using Matplotlib and Cartopy.

### DataAccessLayer.changeEDEXHost()

After DataAccessLayer is imported from the package `awips.dataaccess`, the first step is to define the EDEX data server hostname (`edex-cloud.unidata.ucar.edu` for these examples)




```python
from awips.dataaccess import DataAccessLayer
DataAccessLayer.changeEDEXHost("edex-cloud.unidata.ucar.edu")
```

### DataAccessLayer.getSupportedDatatypes()

getSupportedDatatypes() returns a list of available data types offered by the EDEX server defined above. 


```python
dataTypes = DataAccessLayer.getSupportedDatatypes()
list(dataTypes)
```




    ['acars',
     'airep',
     'binlightning',
     'bufrmosavn',
     'bufrmoseta',
     'bufrmosgfs',
     'bufrmoshpc',
     'bufrmoslamp',
     'bufrmosmrf',
     'bufrua',
     'climate',
     'common_obs_spatial',
     'ffmp',
     'gfe',
     'grid',
     'hydro',
     'ldadmesonet',
     'maps',
     'modelsounding',
     'obs',
     'pirep',
     'practicewarning',
     'profiler',
     'radar',
     'radar_spatial',
     'satellite',
     'sfcobs',
     'topo',
     'warning']



### DataAccessLayer.newDataRequest()

Now create a new data request, and set the data type to **grid** and "locationName" to **RAP40** with **setDataType()** and **setLocationNames()**


```python
request = DataAccessLayer.newDataRequest()
request.setDatatype("grid")
```

### DataAccessLayer.getAvailableLocationNames()

With datatype set to "grid", we can query all available grid names with **getAvailableLocationNames()**


```python
available_grids = DataAccessLayer.getAvailableLocationNames(request)
available_grids.sort()
list(available_grids)
```




    ['CMC',
     'EKDMOS',
     'EKDMOS-AK',
     'ESTOFS',
     'ETSS',
     'FFG-ALR',
     'FFG-FWR',
     'FFG-KRF',
     'FFG-MSR',
     'FFG-ORN',
     'FFG-PTR',
     'FFG-RHA',
     'FFG-RSA',
     'FFG-STR',
     'FFG-TAR',
     'FFG-TIR',
     'FFG-TUA',
     'FNMOC-FAROP',
     'FNMOC-NCODA',
     'FNMOC-WW3',
     'FNMOC-WW3-Europe',
     'GFS',
     'GFS20',
     'GFSLAMP5',
     'GribModel:9:151:172',
     'HFR-EAST_6KM',
     'HFR-EAST_PR_6KM',
     'HFR-US_EAST_DELAWARE_1KM',
     'HFR-US_EAST_FLORIDA_2KM',
     'HFR-US_EAST_NORTH_2KM',
     'HFR-US_EAST_SOUTH_2KM',
     'HFR-US_EAST_VIRGINIA_1KM',
     'HFR-US_HAWAII_1KM',
     'HFR-US_HAWAII_2KM',
     'HFR-US_HAWAII_6KM',
     'HFR-US_WEST_500M',
     'HFR-US_WEST_CENCAL_2KM',
     'HFR-US_WEST_LOSANGELES_1KM',
     'HFR-US_WEST_LOSOSOS_1KM',
     'HFR-US_WEST_NORTH_2KM',
     'HFR-US_WEST_SANFRAN_1KM',
     'HFR-US_WEST_SOCAL_2KM',
     'HFR-US_WEST_WASHINGTON_1KM',
     'HFR-WEST_6KM',
     'HPCGuide',
     'HPCqpf',
     'HPCqpfNDFD',
     'HRRR',
     'LAMP2p5',
     'MOSGuide',
     'MPE-Local-ALR',
     'MPE-Local-FWR',
     'MPE-Local-MSR',
     'MPE-Local-ORN',
     'MPE-Local-RHA',
     'MPE-Local-SJU',
     'MPE-Local-STR',
     'MPE-Local-TAR',
     'MPE-Local-TIR',
     'MPE-Mosaic-ALR',
     'MPE-Mosaic-FWR',
     'MPE-Mosaic-MSR',
     'MPE-Mosaic-ORN',
     'MPE-Mosaic-RHA',
     'MPE-Mosaic-SJU',
     'MPE-Mosaic-TAR',
     'MPE-Mosaic-TIR',
     'NAM12',
     'NAM40',
     'NAVGEM',
     'NCWF',
     'NDFD',
     'NOHRSC-SNOW',
     'NamDNG',
     'PROB3HR',
     'QPE-ALR',
     'QPE-Auto-TUA',
     'QPE-FWR',
     'QPE-KRF',
     'QPE-MSR',
     'QPE-Manual-KRF',
     'QPE-ORN',
     'QPE-RFC-PTR',
     'QPE-RFC-RSA',
     'QPE-RFC-STR',
     'QPE-TIR',
     'QPE-TUA',
     'QPE-XNAV-ALR',
     'QPE-XNAV-FWR',
     'QPE-XNAV-KRF',
     'QPE-XNAV-MSR',
     'QPE-XNAV-ORN',
     'QPE-XNAV-SJU',
     'QPE-XNAV-TAR',
     'QPE-XNAV-TIR',
     'QPE-XNAV-TUA',
     'RAP13',
     'RAP20',
     'RAP40',
     'RFCqpf',
     'RTMA',
     'RTOFS-Now-WestAtl',
     'RTOFS-Now-WestConus',
     'RTOFS-WestAtl',
     'RTOFS-WestConus',
     'SeaIce',
     'TPCWindProb',
     'UKMET-MODEL1',
     'URMA25',
     'WaveWatch']



### Set grid name with `setLocationNames()`


```python
request.setLocationNames("RAP13")
```

# List Available Parameters for a Grid

### DataAccessLayer.getAvailableParameters()

After datatype and model name (locationName) are set, you can query all available parameters with **getAvailableParameters()**


```python
availableParms = DataAccessLayer.getAvailableParameters(request)
availableParms.sort()
list(availableParms)
```




    ['0to5',
     '2xTP6hr',
     'AV',
     'Along',
     'AppT',
     'BLI',
     'BlkMag',
     'BlkShr',
     'CAPE',
     'CFRZR',
     'CICEP',
     'CIn',
     'CP',
     'CP1hr',
     'CPr',
     'CPrD',
     'CRAIN',
     'CSNOW',
     'CURU',
     'CXR',
     'CapeStk',
     'Corf',
     'CorfF',
     'CorfFM',
     'CorfM',
     'CritT1',
     'DIABi',
     'DivF',
     'DivFn',
     'DivFs',
     'DpD',
     'DpDt',
     'DpT',
     'Dpress',
     'DthDt',
     'EHI',
     'EHI01',
     'EHIi',
     'EPT',
     'EPTA',
     'EPTC',
     'EPTGrd',
     'EPTGrdM',
     'EPTs',
     'EPVg',
     'EPVs',
     'EPVt1',
     'EPVt2',
     'FVecs',
     'FeatMot',
     'FnVecs',
     'FsVecs',
     'Fzra1',
     'Fzra2',
     'GH',
     'GHxSM',
     'GHxSM2',
     'Gust',
     'HI',
     'HI1',
     'HI3',
     'HI4',
     'HIdx',
     'HPBL',
     'Heli',
     'Into',
     'KI',
     'L-I',
     'LIsfc2x',
     'LgSP1hr',
     'MAdv',
     'MCon',
     'MCon2',
     'MMSP',
     'MSFDi',
     'MSFi',
     'MSFmi',
     'MSG',
     'MTV',
     'Mix1',
     'Mix2',
     'Mmag',
     'MnT',
     'MpV',
     'MxT',
     'NBE',
     'NetIO',
     'OmDiff',
     'P',
     'PAdv',
     'PBE',
     'PFrnt',
     'PGrd',
     'PGrd1',
     'PGrdM',
     'PIVA',
     'PR',
     'PTvA',
     'PTyp',
     'PVV',
     'PW',
     'PW2',
     'PoT',
     'PoTA',
     'QPV1',
     'QPV2',
     'QPV3',
     'QPV4',
     'REFC',
     'RH',
     'RH_001_bin',
     'RH_002_bin',
     'RM5',
     'RMGH2',
     'RRtype',
     'RV',
     'Rain1',
     'Rain2',
     'Rain3',
     'Ro',
     'SH',
     'SHx',
     'SLI',
     'SNW',
     'SNWA',
     'SRMm',
     'SRMmM',
     'SSi',
     'Shear',
     'ShrMag',
     'SnD',
     'Snow1',
     'Snow2',
     'Snow3',
     'SnowT',
     'St-Pr',
     'St-Pr1hr',
     'StrTP',
     'StrmMot',
     'T',
     'TAdv',
     'TGrd',
     'TGrdM',
     'TP',
     'TP12hr',
     'TP168hr',
     'TP1hr',
     'TP24hr',
     'TP36hr',
     'TP3hr',
     'TP48hr',
     'TP6hr',
     'TP72hr',
     'TPrun',
     'TPx12x6',
     'TPx1x3',
     'TQIND',
     'TV',
     'TW',
     'T_001_bin',
     'Tdef',
     'Tdend',
     'ThGrd',
     'TmDpD',
     'Tmax',
     'Tmin',
     'TotQi',
     'Tstk',
     'TwMax',
     'TwMin',
     'Twstk',
     'TxSM',
     'USTM',
     'VAdv',
     'VAdvAdvection',
     'VSTM',
     'Vis',
     'WD',
     'WEASD',
     'WEASD1hr',
     'WGS',
     'Wind',
     'WndChl',
     'ageoVC',
     'ageoW',
     'ageoWM',
     'cCape',
     'cCin',
     'cTOT',
     'capeToLvl',
     'dCape',
     'dGH12',
     'dP',
     'dP1hr',
     'dP3hr',
     'dP6hr',
     'dPW1hr',
     'dPW3hr',
     'dPW6hr',
     'dT',
     'dVAdv',
     'dZ',
     'defV',
     'del2gH',
     'df',
     'fGen',
     'fnD',
     'fsD',
     'gamma',
     'gammaE',
     'geoVort',
     'geoW',
     'geoWM',
     'mixRat',
     'msl-P',
     'muCape',
     'pV',
     'pVeq',
     'qDiv',
     'qVec',
     'qnVec',
     'qsVec',
     'shWlt',
     'snoRatCrocus',
     'snoRatEMCSREF',
     'snoRatSPC',
     'snoRatSPCdeep',
     'snoRatSPCsurface',
     'swtIdx',
     'tTOT',
     'tWind',
     'tWindU',
     'tWindV',
     'uFX',
     'uW',
     'vSmthW',
     'vTOT',
     'vW',
     'vertCirc',
     'wDiv',
     'wSp',
     'wSp_001_bin',
     'wSp_002_bin',
     'wSp_003_bin',
     'wSp_004_bin',
     'zAGL']



### setParameters()


set the request parameter


```python
request.setParameters("T")
```

## List Available Levels for Parameter

Using **DataAccessLayer.getAvailableLevels()**


```python
availableLevels = DataAccessLayer.getAvailableLevels(request)
for level in availableLevels:
    print(level)
```

    0.0SFC
    350.0MB
    475.0MB
    225.0MB
    120.0_150.0BL
    900.0MB
    125.0MB
    450.0MB
    575.0MB
    325.0MB
    100.0MB
    1000.0MB
    60.0_90.0BL
    275.0MB
    1.0PV
    950.0MB
    150.0MB
    1.5PV
    700.0MB
    825.0MB
    150.0_180.0BL
    250.0MB
    375.0MB
    1000.0_500.0MB
    800.0MB
    925.0MB
    2.0PV
    0.5PV
    0.0TROP
    750.0MB
    500.0MB
    625.0MB
    400.0MB
    0.0FHAG
    2.0FHAG
    875.0MB
    175.0MB
    850.0MB
    600.0MB
    725.0MB
    975.0MB
    550.0MB
    675.0MB
    425.0MB
    200.0MB
    0.0_30.0BL
    30.0_60.0BL
    650.0MB
    525.0MB
    300.0MB
    90.0_120.0BL
    775.0MB
    0.0TILT
    0.5TILT
    340.0_350.0K
    290.0_300.0K
    700.0_600.0MB
    700.0_300.0MB
    320.0Ke
    800.0_750.0MB
    60.0TILT
    5.3TILT
    1000.0_900.0MB
    340.0K
    255.0K
    255.0_265.0K
    25.0TILT
    1000.0_850.0MB
    850.0_250.0MB
    280.0_290.0Ke
    320.0_330.0K
    310.0_320.0Ke
    310.0Ke
    330.0K
    900.0_800.0MB
    550.0_500.0MB
    2.4TILT
    50.0TILT
    35.0TILT
    12.0TILT
    300.0_310.0K
    0.9TILT
    320.0K
    400.0_350.0MB
    750.0_700.0MB
    345.0K
    250.0_260.0K
    300.0Ke
    290.0Ke
    950.0_900.0MB
    275.0_285.0Ke
    335.0Ke
    295.0_305.0Ke
    275.0_285.0K
    600.0_550.0MB
    310.0K
    335.0K
    700.0_500.0MB
    325.0_335.0K
    300.0K
    0.0MAXOMEGA
    315.0_325.0K
    325.0K
    340.0Ke
    300.0_250.0MB
    1.5TILT
    335.0_345.0K
    315.0K
    3.4TILT
    330.0Ke
    500.0_400.0MB
    305.0K
    285.0_295.0Ke
    14.0TILT
    325.0_335.0Ke
    850.0_800.0MB
    295.0Ke
    305.0Ke
    265.0_275.0K
    700.0_650.0MB
    450.0_400.0MB
    1.8TILT
    330.0_340.0K
    800.0_700.0MB
    850.0_300.0MB
    6.0TILT
    900.0_850.0MB
    320.0_330.0Ke
    8.7TILT
    650.0_600.0MB
    600.0_400.0MB
    55.0TILT
    270.0_280.0Ke
    30.0TILT
    310.0_320.0K
    1000.0_950.0MB
    250.0_200.0MB
    400.0_300.0MB
    500.0_100.0MB
    285.0Ke
    290.0K
    305.0_315.0K
    285.0_295.0K
    925.0_850.0MB
    275.0Ke
    300.0_200.0MB
    260.0_270.0K
    315.0_325.0Ke
    600.0_500.0MB
    16.7TILT
    280.0K
    500.0_250.0MB
    40.0TILT
    400.0_200.0MB
    300.0_310.0Ke
    270.0_280.0K
    1000.0_700.0MB
    45.0TILT
    850.0_500.0MB
    295.0K
    4.3TILT
    295.0_305.0K
    330.0_340.0Ke
    270.0K
    280.0_290.0K
    925.0_700.0MB
    260.0K
    10.0TILT
    325.0Ke
    285.0K
    290.0_300.0Ke
    7.5TILT
    280.0Ke
    500.0_450.0MB
    305.0_315.0Ke
    250.0K
    250.0_350.0K
    270.0Ke
    275.0K
    315.0Ke
    500.0_300.0MB
    350.0_300.0MB
    19.5TILT
    850.0_700.0MB
    350.0K
    265.0K
    0.0_0.0SFC


* **0.0SFC** is the Surface level
* **FHAG** stands for Fixed Height Above Ground (in meters)
* **NTAT** stands for Nominal Top of the ATmosphere
* **BL** stands for Boundary Layer, where **0.0_30.0BL** reads as *0-30 mb above ground level*  
* **TROP** is the Tropopause level

### request.setLevels()

For this example we will use Surface Temperature


```python
request.setLevels("2.0FHAG")
```

### DataAccessLayer.getAvailableTimes()

* **getAvailableTimes(request, True)** will return an object of *run times* - formatted as `YYYY-MM-DD HH:MM:SS`
* **getAvailableTimes(request)** will return an object of all times - formatted as `YYYY-MM-DD HH:MM:SS (F:ff)`
* **getForecastRun(cycle, times)** will return a DataTime array for a single forecast cycle.

# Request a Grid

### DataAccessLayer.getGridData()

Now that we have our `request` and DataTime `fcstRun` arrays ready, it's time to request the data array from EDEX.


```python
cycles = DataAccessLayer.getAvailableTimes(request, True)
times = DataAccessLayer.getAvailableTimes(request)
fcstRun = DataAccessLayer.getForecastRun(cycles[-1], times)

response = DataAccessLayer.getGridData(request, [fcstRun[-1]])
```


```python
for grid in response:
    data = grid.getRawData()
    lons, lats = grid.getLatLonCoords()
    print('Time :', str(grid.getDataTime()))

print('Model:', str(grid.getLocationName()))
print('Parm :', str(grid.getParameter()))
print('Unit :', str(grid.getUnit()))
print(data.shape)
print(data.min(), data.max())
```

    ('Time :', '2017-08-14 14:00:00 (21)')
    ('Model:', 'RAP13')
    ('Parm :', 'T')
    ('Unit :', 'K')
    (337, 451)
    (271.21939, 306.71939)

