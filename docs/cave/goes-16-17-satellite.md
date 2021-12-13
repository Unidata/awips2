# GOES 16/17

The ***goesr*** EDEX decoder supports the ingest of GOES products coming over NOAAPort and Unidata's IDD. These include [**single channel imagery**](#individual-channels), [**derived products**](#derived-products) (Level 2b netCDF files), gridded [**Geostationary Lightning Mapper**](#geostationary-lightning-mapper-glm) (GLM) products (produced by Eric Bruning at Texas Tech), [**CIRA created RGB**](#cira-geocolor) specific products, and [**vertical temperature/moisture profiles**](#vertical-temperature-and-moisture-profile). Using derived parameters, additional [**RGB**](#rgb-composites) and [**channel difference**](#channel-differences) products can be loaded. The ***dmw*** EDEX decoder supports the ingest of GOES [**derived motion winds**](#derived-motion-winds).

![](../images/GOESEW.png)

GOES East and West products are accessible in the **Satellite** menu.  The menu is broken into sections starting with common CONUS GOES East/West Combo products. There are submenus for each of the separate geospatial sectors:

* East Full Disk
* East CONUS
* East Mesoscale Sectors (x2)
* West Full Disk
* West CONUS
* West Mesoscale Sectors (x2)
* Hawaii
* Alaska
* Puerto Rico
  
Each sector submenu has products for individual channels and vertical profiles, as well as submenus for derived products, channel differences, RGB Composites, GLM data, and derived motion winds. GLM data can also be found with its own submenu option a little lower down the menu and under the **Surface** menu.

!!! warning "The RGB products are not available on MacOS or in a Virtual Machine running CAVE."

![](../images/satelliteMenuNew.png)

---

## LDM Pattern Actions

The Unidata IDD redistributes both the NOAAPort/SBN GOES tiled products as well as stitched together GOES products. While AWIPS can decode and ingest both, it's important to only be requesting from one or the other so you aren't creating duplicate processing.  The entries that should be used for GOES data are shown below which is found in the LDM's pqact.conf file, located in `/awips2/ldm/etc`.   (For the full list of pqact entries, you can view [this](https://github.com/Unidata/awips2/blob/unidata_18.2.1/rpms/awips2.upc/Installer.ldm/patch/etc/pqact.goesr) file).

    # GOES 16/17 Single Channel (ABI) via Unidata IDD
    NIMAGE	^/data/ldm/pub/native/satellite/GOES/([^/]*)/Products/CloudAndMoistureImagery/([^/]*)/([^/]*)/([0-9]{8})/([^/]*)(c[0-9]{7})(..)(.....).nc
        FILE	-close -edex	/awips2/data_store/GOES/\4/\7/CMI-IDD/\5\6\7\8.nc4

    # GOES 16/17 derived products + derived motion wind via SBN
    HDS	^(IXT.[8-9]9) (KNES) (..)(..)(..)
        FILE	-close -edex	/awips2/data_store/GOES/(\3:yyyy)(\3:mm)\3/\4/derivedProducts-SBN/\1_KNES_\2\3\4\5-(seq)
    NOTHER	^(IXT[WXY]01) (KNES) (..)(..)(..)
        FILE	-close -edex	/awips2/data_store/GOES/(\3:yyyy)(\3:mm)\3/\4/derivedProducts-SBN/\1_KNES_\2\3\4\5-(seq)
    
    # GOES 16 GLM Gridded Products via Texas Tech-->Unidata IDD 
    NIMAGE	^/data/ldm/pub/native/satellite/GOES/([^/]*)/Products/GeostationaryLightningMapper/([^/]*)/([0-9]{8})/([^/]*)(c[0-9]{7})(..)(.....).nc
        FILE	-close -edex	/awips2/data_store/GOES/\3/\6/GLM-IDD/\4\5\6\7.nc4

    # GOES CIRA derived products 
    NIMAGE	^/data/ldm/pub/native/satellite/GOES/([^/]*)/Products/GeoColor/([^/]*)/([^/]*)/([0-9]{8})/([^/]*)(c[0-9]{7})(..)(.....).nc
        FILE	-close -edex	/awips2/data_store/GOES/\4/\7/CIRA/GeoColor/\5\6\7\8.nc4
    NIMAGE	^/data/ldm/pub/native/satellite/GOES/([^/]*)/Products/DebraDust/([^/]*)/([^/]*)/([0-9]{8})/([^/]*)(c[0-9]{7})(..)(.....).nc
        FILE	-close -edex	/awips2/data_store/GOES/\4/\7/CIRA/DebraDust/\5\6\7\8.nc4
    NIMAGE	^/data/ldm/pub/native/satellite/GOES/([^/]*)/Products/CloudSnow/([^/]*)/([^/]*)/([0-9]{8})/([^/]*)(c[0-9]{7})(..)(.....).nc
        FILE	-close -edex	/awips2/data_store/GOES/\4/\7/CIRA/CloudSnow/\5\6\7\8.nc4

---

## Individual Channels

All geospatial sectors have 16 individual channel products that can be viewed.  Below are samples of Channel 14 (11.20&mu;m) for each of the sectors.

### East CONUS 1km

![](../images/goes-econus.png)

### East Full Disk 6km

![](../images/goes-efd.png)

### East Mesoscale Sectors (EMESO-1, EMESO-2)

Two floating mesoscale sectors (location will vary day to day from image shown)

![](../images/goes-emeso12.png)

### West CONUS 1km

![](../images/goes-wconus.png)

### West Full Disk

![](../images/goes-wfd.png)

### West Mesoscale Sectors (WMESO-1, WMESO-2)

Two floating mesoscale sectors (location will vary day to day from image shown)

![](../images/goes-wmeso12.png)

### Alaska

![](../images/goes-ak.png)

### Hawaii

![](../images/goes-hi.png)

### Puerto Rico (PRREGI)

![](../images/goes-pr.png)

---

## RGB Composites

RGB Composites are made by combining 3 channels and are available for each sector. Quite a few new RGB products have been added in Unidata's 18.2.1 release. These products are generated **on the fly in AWIPS** using the existing channel products from EDEX.

!!! warning "GOES RGB Imagery is NOT SUPPORTED on macOS or within a Virtual Machine"

	OpenGL Shading Language limitations prevent multi-channel imagery from displaying correctly on Mac or in a Virtual Machine.  Please use the Linux or Windows installs to view RGB products.

### Day Cloud Phase

![](../images/RGB-01-DayCloudPhase.png)

### Fire Temperature

![](../images/RGB-02-FireTemperature.png)

### Day Land Cloud

![](../images/RGB-03-DayLandCloud.png)

### Day Cloud Convection

![](../images/RGB-04-DayCloudConvection.png)

### Day Land Cloud Fires

![](../images/RGB-05-DayLandCloudFires.png)

### VIS/IR Sandwich

![](../images/RGB-06-VISIRSandwich.png)

### Simple Water Vapor

![](../images/RGB-07-SimpleWaterVapor.png)

### Air Mass

![](../images/RGB-08-AirMass.png)

### Ash 

![](../images/RGB-09-Ash.png)

### Day Convection

![](../images/RGB-10-DayConvection.png)

### Day Snow Fog

![](../images/RGB-11-DaySnowFog.png)

### Differential Water Vapor

![](../images/RGB-12-DifferentialWaterVapor.png)

### Dust

![](../images/RGB-13-Dust.png)

### CIMSS Natural Color

![](../images/RGB-14-CIMSSNaturalColor.png)

### Nighttime Microphysics

![](../images/RGB-15-NighttimeMicrophysics.png)

### SO2

![](../images/RGB-16-SO2.png)

### CIRA Geocolor

![](../images/RGB-17-CIRAGeocolor.png)

### CIRA Debra Dust

![](../images/RGB-18-CIRADebraDust.png)

### CIRA Cloud Snow

![](../images/RGB-19-CIRACloudSnow.png)

### Daytime Composite 1

![](../images/RGB-20-DaytimeComp1.png)

### Daytime Composite 5

![](../images/RGB-21-DaytimeComp5.png)

---

## Channel Differences

Channel differences are the result of subtracting one channel from another to produce a new product.  These products are generated **on the fly in AWIPS** using the existing channel products from EDEX.

There currently 10 channel differences that are offered in CAVE:

* Split Window (10.3 - 12.3 &mu;m)
* Split Cloud Top Phase (11.2 - 8.4 &mu;m)
* Night Fog (10.3 - 2.9 &mu;m)
* Day Fog (3.9 - 10.3 &mu;m)
* Split Fire (2.2 - 1.6 &mu;m)
* Split Ozone (9.6 - 10.3 &mu;m)
* Split Water Vapor (6.19 - 7.3 &mu;m)
* Split Snow (1.6 - 0.64 &mu;m)
* Vegetation (0.64 - 0.87 &mu;m)
* Upper Level Info (11.2 - 6.19 &mu;m)

!!! note "The rendering of these products uses the **Jep** package in Python, which has specific [install instructions](http://127.0.0.1:8000/install/install-cave/#method-2-direct-windows-install) for Windows."

---

## Derived Products

Derived products are also known as **Level 2+** products.  Currently there are only derived products from GOES East available in AWIPS. Each sector has a different set of products available. To find out some more information on some of the products please the [**Quick Guides**](http://rammb.cira.colostate.edu/training/visit/quick_guides/) compiled by CIRA.

!!! note "These may not all be available for each sector."

The current products offered in CAVE are listed below and to the right is which GOES East sector they are available for (F=Full Disk, C=CONUS, M=Mesoscale):

* Aerosol Detection - F,C,M
* Aerosol Optical Depth - F,C
* Clear Sky Mask - F,C,M
* Cloud Optical Depth - F,C
* Cloud Particle Size -F,C,M
* Cloud Top Height -F,C,M
* Cloud Top Phase -F,C,M
* Cloud Top Pressure -F,C
* Cloud Top Temperature - F,M
* Derived CAPE - F,C,M
* Derived K-Index - F,C,M
* Derived Lifted Index - F,C,M
* Derived Showalter Index - F,C,M
* Derived Total Totals - F,C,M
* Fire Area - F,C
* Fire Power - F,C
* Fire Temperature - F,C
* Instrument Flight Rule (IFR) Probability - C
* Low IFR Probability - C
* Marginal Visual Flight Rules (MVFR) Probability - C
* Cloud Thickness - C
* Land Skin Temperature - F,C,M
* RR/QPE - F
* Sea Surface Temperature - F
* Total Precip Water - F,C,M

---

## Geostationary Lightning Mapper (GLM)

Dr. Eric Bruning at Texas Tech has taken the raw GLM data and coded up some new gridded products that can be ingested and displayed in AWIPS.

* Minimum Flash Area
* Average Flash Area
* Flash Extent Density
* Group Extent Density
* Total Optical Energy


GLM data are located in the menu structure: **Satellite** > **[SECTOR]** > **GLM Products**. You can also access the data from  **Surface** > **GLM - Geostationary Lightning Mapper** submenus.

![Satellite GLM Menu](../images/goes-GLMMenu.png)

![Surface GLM Menu](../images/goes-GLMMenu2.png)


---

## Derived Motion Winds

Derived Motion Wind Vectors are produced using sequential ABI images and can provide information about winds at different levels. The wind vectors are computed using both visible and infrared imagery. Winds can be plotted by different pressure layers or individual channels. More information can be found [here](http://cimss.ssec.wisc.edu/goes/OCLOFactSheetPDFs/ABIQuickGuide_BaselineDerivedMotionWinds.pdf). Below is an image of the winds at different pressure layers.

![](../images/dmw.png)

---

## Vertical Temperature and Moisture Profile

Vertical Temperature and Moisture profiles are available in AWIPS. Similar to NUCAPS, when loaded in CAVE, a circle is displayed for each location that has a vertical profile available. When clicking on the circle, NSHARP will open with the vertical temperature and moisture profile. These profiles are GFS data that have been adjusted based on the satellite observations. More information can be found [here](https://rammb.cira.colostate.edu/training/visit/quick_guides/QuickGuide_GOES-R_Legacy_Temperature_Moisture%20Profiles.pdf).

![](../images/goes-VertProfileMap.png)

![](../images/goes-VertProfileSounding.png)

---

## HDF5 Data Store

Decoded GOES satellite data are stored in `/awips2/edex/data/hdf5/satellite/` under sector subdirectories:

    drwxr-xr-x	awips	fxalpha	4096	AKREGI
    drwxr-xr-x	awips	fxalpha	4096	Antarctic
    drwxr-xr-x	awips	fxalpha	4096	Arctic
    drwxr-xr-x	awips	fxalpha	4096	AREA0600
    drwxr-xr-x	awips	fxalpha	4096	AREA0700
    drwxr-xr-x	awips	fxalpha	4096	AREA3100
    drwxr-xr-x	awips	fxalpha	4096	AREA3101
    drwxr-xr-x	awips	fxalpha	12288	ECONUS
    drwxr-xr-x	awips	fxalpha	4096	EFD
    drwxr-xr-x	awips	fxalpha	4096	EMESO-1
    drwxr-xr-x	awips	fxalpha	4096	EMESO-2
    drwxr-xr-x	awips	fxalpha	4096	HIREGI
    drwxr-xr-x	awips	fxalpha	4096	NEXRCOMP
    drwxr-xr-x	awips	fxalpha	4096	PRREGI
    drwxr-xr-x	awips	fxalpha	4096	WCONUS
    drwxr-xr-x	awips	fxalpha	4096	WFD
    drwxr-xr-x	awips	fxalpha	4096	WMESO-1
    drwxr-xr-x	awips	fxalpha	4096	WMESO-2