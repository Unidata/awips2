
The NCEP/Hydro menu contains nine sections: SPC, TPC, NCO, HPC, MPC, CPC, AWC, Hydro, and Local Analyses/Statistical Guidance. Each section is further subdivided into related products, as described below. For more information on hydro products, refer to documentation prepared by the NWS' Office of Hydrology.

# SPC

Storm Prediction Center (SPC) Watches, Severe Weather Plots, SPC Convective Outlooks, and Fire Weather information. Severe Weather Plots are extracted from the STADTS and STAHRY text products and plotted to time-match the current display. The Severe Weather Plots data set in the NCEP/Hydro Menu can be interrogated (sampled) for more detailed information by clicking mouse Button 1 (B1) over a site.

# TPC

Contains the hurricane submenu, which comprises graphic products that display the Marine/Tropical Cyclone Advisory (TCM), the Public Tropical Cyclone Advisory (TCP), hourly forecasts, and model guidance.


# HPC

Contains 6-hour QPF (Quantitative Precipitation Forecast) data plus the submenus, described
below, for Precipitation and Temps & Weather products.

 * **Precipitation** Contains probabilities of daily precipitation, precipitation accumulation, and probabilities of daily snowfall. In addition, this submenu enables you to display QPF projections for 1 to 3 days in 6 hour increments, 4 to 5 days in 48 hour increments, and 1 to 5 days in 120 hour increments. The HPC Excessive Rainfall product consists of a contour graphic and image of the excessive rainfall for day 1 (with forecast times of 21, 24, 27, or 30 hours), and days 2 and 3 (both with forecast times of 48 and 72 hours). The HPC product will update the selected forecast cycle twice per day.
* **Temps & Weather** Contains daily Max/Min temperature anomalies, daily heat index
probabilities, and pressure and frontal analysis.

# MPC

Contains the Marine Guidance submenu, which includes marine analyses and model guidance. Note that  the Marine Prediction Center (MPC) is now called the Ocean Prediction Center (OPC).

# CPC

Contains threat charts and outlook grids derived from these two submenus:

* **Threat Charts** Contains drought monitoring data, daily threats assessment, and daily heat index forecasts.
* **Outlook Grids** Contains temperature and precipitation probabilities.

# AWC

Contains CCFP (Collaborative Convective Forecast Product), an aviation product. Formerly located under the Aviation option on the Upper Air menu, CCFP is a strategic forecast of convection to guide traffic managers in their system-wide approach to managing traffic. The forecast suite consists of 3 forecast maps with selectable lead times (4, 6, and 8 hours). The forecasts are issued by the Aviation Weather Center (AWC) between March 1 and October 30, eleven times per day.

CCFP is alpha-numeric information suitable for the graphical depiction of forecast areas of significant thunderstorms. The CCFP message covers the CONUS area, and includes information on the location of thunderstorm areas, and associated information such as storm tops, coverage, confidence, and direction/speed of movement.


# NCO

Contains Precip & Stability, Temps & Weather, National Centers model, NGM MOS (NGM-based MOS system), and the following Sounding-derived plots submenus.

* **Precip & Stability**: Contains precipitation, radar, and stability products.
* **Temps & Weather**: Contains Max/Min temperature, freezing level, weather depiction, and surface geostrophic wind and relative vorticity plots.
* **National Centers Models**: Contains model guidance from the National Centers
* **Sounding-derived plots**: Contains options to display model soundings (sometimes called "BUFR soundings" because they are packaged in BUFR format for transmission). These are soundings extracted directly from the model, including all levels not generated from the pressure-level grids used elsewhere in the system.
	* **Sounding Availability** This option displays the sounding locations (shown with asterisks) available from the latest model run; typically these locations coincide with TAF (Terminal Aerodrome Forecast) locations. The plot will update with each model run. Because the sounding data is quite voluminous, only soundings over your State(s) scale are saved.
	* **Surface** The Surface Plots, which mimic the METAR Surface Plots, are taken from the model-derived soundings and provide hourly forecast surface plots. Because you cannot see all forecast projections in a 32 frame loop (e.g., displaying the entire North American Model (NAM) or Global Forecasting System (GFS) run would require 61 frames), you will probably want to use the Time Options Tool (refer to Subsection 2.2.6.4) to view a subset of the forecast -- perhaps a continuous run of hours or every other hour for the whole run.
	* **Ceiling/Visibility** The "Ceil/Vis Plot" shows weather (rain, frz rain, snow) on the right, a stack of three cloud layers above, and visibility below the METAR station. The cloud layers are defined as low (990mb-640mb), mid (640mb-350mb), and high (<350mb). Each cloud layer shows a coverage circle with clear, sct, bkn, and ovc options. Next to one of the circles, there may be a cloud base. The cloud base is sent as a pressure, but is plotted in hft MSL based on a Standard Atmosphere conversion. Because the cloud layers and the cloud base are generated from separate algorithms at NCEP (National Centers for Environmental Prediction), it is possible to have broken or overcast clouds indicated but no base; alternatively, the base may be shown with a high overcast, while ignoring a mid broken layer. Also, a cloud base is reported if convective precipitation is indicated, even for only 10-20% cloud cover. As a result, one can see a cloud base associated with scattered clouds.
	* **1 Hr and 3 Hr Precip Amt** This option shows hourly amounts for NAM and 3 hour intervals for GFS at each location.
	* **Cloud Layers** This option displays the amount of low, middle, and high cloud cover, each as a standard sky coverage symbol, and weather type as a weather symbol.


# Hydro

Contains QPE, QPF, and RFC Flash Flood Guidance submenus. Hydro Applications, such as HydroView and MPE Editor, are loaded from the Perspectives dialog (Hydro and MPE, respectively) or from the HydroApps menu in the Hydro(View) Perspective (Hydrobase, RiverPro, XDAT, Forecast Service, River Monitor, Precip Monitor, SSHP, and Dam Catalog).

* **QPE**: Makes available mosaic images of RFC-generated Quantitative Precipitation Estimator (QPE) and the Multisensor Precipitation Estimator (MPE) grids, which are displayed using a 'truncated' grid color table that shows zero values in gray to let you see the limits of the site-specified domain. These mosaic images are generated by the RFCs in 1, 6, and 24 hour cycles. The MPE grids can be displayed as local contours or images.

	* **NESDIS** produces two types of Satellite Precipitation Estimates (SPE) based on GOES (Geostationary Operational Environmental Satellite) imagery series: Auto SPEs and Manual SPEs. Auto SPEs, which can be displayed directly from the QPE submenu, are produced hourly based on the most recent one-hour series of IR GOES imagery. This product is displayable on any AWIPS scale. The Auto SPE estimates are displayed in units of inches of precipitation that fell during the specified one hour period.
	* **Manual SPEs** are accessible through the Manual SPE submenu. You can access the Manual SPE submenu from the QPE submenu. Generation of these products requires substantial manual intervention by NESDIS personnel; consequently, these products are generated and distributed to AWIPS at variable frequencies, as significant precipitation events warrant (i.e., their frequency is variable). The duration (or valid period) of the Manual SPEs is also variable. Whereas the duration of Auto SPEs is always one hour, the duration of the Manual SPEs ranges from 1 to 12 hours. Furthermore, although each Manual SPE product is mapped to a CONUS grid, the area of analysis is usually regional (focusing on the significant precipitation event). Apart from these important differences, the Manual SPEs are very similar to the Auto SPEs.

* **QPF**: Displays QPF, which indicate how much precipitation will occur in a particular grid. QPFs, which are issued by the RFCs, display as contours by default. However, from the pop-up menu you can convert them to image form.
* **RFC Flash Flood Guidance**: Displays County and Zone Flash Flood Guidance (FFG) grids on any scale. The area for which the data is displayed is limited, but the site system manager may configure a larger area. In addition, 1h, 3h, and 6h mosaic RFC-generated FFG grids can be displayed for both local and other RFC locations.

# Local Analyses/Statistical Guidance

Model Output Statistical (MOS) plots derived from the MOS BUFR and Text Bulletins display forecast data for GFS MOS, GFS-Extended MOS, Eta MOS, and NGM MOS. The plots are accessed by selecting NGM or GFS-LAMP/MOS forecasts under the Local Analyses/Statistical Guidance option.



