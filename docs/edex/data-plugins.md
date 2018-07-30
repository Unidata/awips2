
<style>
td:first-child { font-weight: bold }
</style>
# EDEX Data Decoder Plugins

|  NAME  |  DESCRIPTION  |
|:-------|:--------------|
|  acars  |  Aircraft Communications Addressing and Reporting System observations  |
|  acarssounding  |  Vertical profiles derived from ACARS data  |
|  airep  |  Automated Aircraft Reports  |
|  airmet  |  “Airmen’s Meteorological Information”: aviation weather advisories for potentially hazardous, but non-severe weather   |
|  aqi | Air Quality Index point obs |
|  asdi | FAA Aircraft Situation Data for Industry |
|  atcf  |  Automated Tropical Cyclone Forecast  |
|  aww  |  Airport Weather Warning  |
|  binlightning  |  Lightning data from the National Lightning Detection Network  |
|  bufrascat  |  Advanced Scatterometer wind data  |
|  bufrhdw  |  GOES High Density Winds  |
|  bufrmos  |  Model Output Statistics  |
|  bufrmthdw  |  MTSAT (Japanese Multi-Functional Transport Satellite) High Density Winds  |
|  bufrncwf  |  National Convective Weather Forecast for Aviation  |
|  bufrsigwx  |  Aviation Significant Weather  |
|  bufrssmi  |  Special Sensor Microwave/Imager data from DMSP (Defesne Meteorological Satellite Program) satellites  |
|  bufrua  |  Upper air radiosonde data  |
|  ccfp  |  Aviation Collaborative Convective Forecast Product  |
|  climate-hmdb  |  Climate text products  |
|  convectprob  |  NOAA/CIMSS Prob Severe Model  |
|  convsigmet  |  Aviation Significant Meteorological Information for convective weather  |
|  crimss  |  NPP/NPOESS CrIMSS (Cross Track Infrared and Microwave Sounding Suite) soundings  |
|  cwa  |  Aviation Center Weather Advisory, issued by CWSUs (Center Weather Service Units)  |
|  cwat |  County Warning Area Threat produced by SCAN (System for Convection Analysis and Nowcasting). CWAT was formerly called SCAN Convective Threat Index (SCTI). |
|  dmw  |  GOES-R Derived Motion Winds  |
|  editedregions | Hazard Services Edited Regions |
|  editedevents  | Hazard Services Edited Events | 
|  ffg  |  Flash flood guidance metadata (countybased ffg from RFCs) |
|  ffmp | Flash Flood Monitoring and Prediction data (raw data inputs: radar, gridded flash flood guidance from River Forecast Centers, highresolution precipitation estimates [HPE] and nowcasts [HPN], QPF from SCAN and gage data from the IHFS [Integrated Hydrologic Forecast System]          database. Radar data [with WSR-88D product mnemonics and numbers] needed for FFMP are Digital Hybrid Reflectivity [DHR, 32] and  Digital Precipitation Rate [DPR, 176]. The raw GRIB files containing RFC Flash Flood Guidance are identified in the tables in Part 2 of this document as NWS_151 or FFG-XXX, where XXX is an RFC identifier such as TUA, KRF, or ALR. The WMO header for the RFC FFG begins with “ZEGZ98”. ) |
|  fog  |  Fog Monitor. Raw data inputs: METAR, Mesonet, maritime, buoys, MAROBs, and satellite [visible, 3.9 µm, and 10.7 µm])  |
|  freezingLevel  |  MPE Rapid Refresh Freezing Level scheduled process (MpeRUCFreezingLevel)  |
|  fssobs | Observations for the Fog monitor, SNOW, and SAFESEAS (raw data inputs: METAR, Mesonet, maritime, buoys, MAROBs). | 
|  geodata | NetCDF JTS Geometry records | 
|  geomag | SWPC Geomagnetic Forecast (RTKP) |
|  gfe  |  Graphical Forecast Editor grids  |
|  ghcd   | SWPC Generic High Cadence Data |
|  glm  | GOES Geostationary Lightning Mapper |
|  goesr  |  Plugins to decode and display GOES-R products  |
|  goessounding  |  GOES Satellite Soundings  |
|  gpd  |  NCEP Generic Point Data  |
|  grid  |  Binary gridded data grib1/grib2  |
|  idft  |  Ice Drift Forecasts  |
|  intlsigmet  |  International Significant Meteorological Information for Aviation  |
|  lma  |  Lightning Mapping Array  |
|  lsr  |  Local Storm Reports  |
|  madis | NCEP Meteorological Assimilation Data Ingest System |
|  manualIngest  |  Manual data ingest plugin  |
|  mcidas  |  NCEP decoder for McIDAS AREA files  |
|  metartohmdb  |  Adds metar records to the Verification and Climate database  |
|  modelsounding  |  Individual grid point soundings from the GFS and NAM models  |
|  modis  |  NASA Moderate-resolution Imaging Spectroradiometer  |
|  mpe  | Multi-sensor Precipitation Estimation |
|  mping | Meteorological Phenomena Identification Near the Ground (mPING) |
|  ncpafm  |  NCEP Point/Area Forecast Matrices data  |
|  ncscat  |  NCEP ASCAT/Quikscat records  |
|  nctaf  |  NCEP TAF decoders  |
|  nctext  |  NCEP Text decoders  |
|  ncuair  |  NCEP Upper Air decoder  |
|  ndm  |  National Dataset Maintenance ingester  |
|  nonconvsigmet  |  Aviation Significant Meteorological Information for non-convective weather  |
|  npp sounding | National Polar-Orbiting Partnership Satellites Soundings | 
|  ntrans  |  NCCEP Ntrans Metafiles  |
|  nucaps  |  Soundings from NOAA Unique CrIS/ATMS Processing System from NPP (National Polar-Orbiting Partnership) Satellites  |
|  obs  |  Surface observations from METARs  |
|  pgen  |  NCEP NAWIPS PGEN decoder  |
|  pirep  |  Pilot Reports  |
|  poessounding  |  Polar Operational Environmental Satellite soundings  |
|  preciprate | Precipitation Rate from SCAN. Raw data input: radar data [with WSR-88D product mnemonic and number] needed for preciprate are Digital Hybrid Reflectivity [DHR, 32]. |
|  qpf | Quantitative Precipitation Forecast from SCAN. (raw data inputs: radar and some RUC130 fields. Radar data [with WSR-88D product mnemonics and numbers] needed for SCAN’s QPF are 0.5 degree Base Reflectivity [Z, 19], 4 km Vertically Integrated Liquid [VIL, 57], and Storm Track [STI, 58]. The RUC130 field needed is 700 mb Wind, as defined in the SCANRunSiteConfig.xml file.)   |
|  radar  |  WSR-88D and TDWR data  |
|  redbook  |  Redbook graphics  |
|  regionalsat  |  Decoder implementation for netcdf3 files generated by the Alaska Region and GOES-R Proving Ground  |
|  satellite-gini  |  GINI-formatted satellite imagery (GOES, POES, VIIRS, FNEXRAD)  |
|  satellite-mcidas  |  McIDAS area files (Raytheon/D2D-developed)  |
|  satpre  |  Satellite-estimated Pecipiration (hydroApps)  |
|  scan   |  SCAN (System for Convection Analysis and Nowcasting). (Inputs for the SCAN Table include radar, cloud-to-ground lightning from the NLDN, fields from RAP13, and CWAT. Specific radar products [with WSR-88D product mnemonics and numbers] are: 1 km Composite Reflectivity [CZ, 37]; 0.5 degree Base Reflectivity [Z, 19]; 4 km Vertically Integrated Liquid [VIL, 57]; Storm Track [STI, 58]; Mesocyclone Detections [MD, 141]; and Tornadic Vortex Signature [TVS, 61]. |
|  sfcobs  |  Surface observations other than METAR format including buoys  |
|  sgwh  |  NCEP BUFR Significant Wave Height data - SGWH (Jason-1), SGWHA (Altika), SGWHC (CryoSat), SGWHE (Envisat), SGWHG (GFO), SGWH2 (Jason-2), or Jason-3 |
|  shef  |  Standard Hydrometeorological Exchange Format data.   |
|  solarimage  |  SWPC Solar imagery  |
|  spc | Storm Prediction Center Convective Outlook KML files |
|  ssha  |  NCEP Sea Surface Height Anomaly BUFR data  |
|  stormtrack  |  NCEP StormTrack Plug-In (Automatic Tropical Cyclone Forecast & Ensemble cyclones)  |
|  svrwx  |  SPC Local Storm Report Summaries  |
|  taf  |  Terminal Aerodrome Forecasts  |
|  tcg  |  Tropical Cyclone Guidance  |
|  tcm  |  Tropical Cyclone Forecast/Advisory  |
|  tcs  |  Tropical Cyclone Forecast/Advisory  |
|  text  |  Various Text Products  |
|  textlightning  |  Text lightning data  |
|  vaa  |  Volcanic ash advisories  |
|  viirs  |  NPP Visible Infrared Imaging Radiometer Suite data  |
|  vil | Cell-based Vertically Integrated Liquid from SCAN  (Input is radar) |
|  warning  |  Watches, Warnings, and Advisories   |
|  wcp  |  SPC Convective Watches  |
