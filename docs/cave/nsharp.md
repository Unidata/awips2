# NSHARP

NSHARP, which stands for the **N**ational Center **S**ounding and **H**odograph **A**nalysis and **R**esearch **P**rogram, is an AWIPS plugin originally based on NAWIPS NSHAREP, SPCs *BigSHARP* sounding display tool, and the Python package [SHARpy](https://github.com/aeroelastics/SHARPy). 

NSHARP is available a number of ways in CAVE:

* From the **D2D toolbar** select the NSHARP icon
* From the **Upper Air** menu select **NSHARP Soundings**
* From the **Upper Air** menu select a station from the RAOB menus
* From the **Upper Air** menu select **NUCAPS Soundings**
* From within **Models &gt; Volume Browser**
* From the **Maps** menu select **Sounding Locs** to 


## NSHARP Configurations

NSHARP has four configurations for use in different operational settings:

* **SPC Wide** - more insets and graphs at the expense of timeline/station inventory.

* **D2D Skewt Standard** - default for WFOs, larger SkewT with inventory, no Wind/Height, temperature advection, insets, or graphs.

* **D2D Lite** - Skew-T, table, and inventory only.

* **OPC** - Ocean Prediction Center display.

> If you would like to interactively explore the different graphical areas in&nbsp;NSHARP&nbsp;<strong>on the Web</strong>, see the&nbsp;<a href="http://www.wdtd.noaa.gov/buildTraining/nsharp-interactive/interactive.html" style="font-weight: 200;">NSHARP Interactive Overview</a>

!!! tip "To change the NSHARP confiuguration:"

	* Click the **Configure** button
	* Click **Display Pane Configuration** (second from bottom)
	* Choose configuration, apply, save, close


## Skew-T Display

The Skew-T display renders a vertical profile of temperature, dew point, and wind for RAOBs and model point soundings using a Skew-T Log-P diagram. Skew-T is the default upper air chart in AWIPS, and can be changed to *turbulence display* ("T") or an *icing display* ("I"). 

The upper-left red box is linked to the cursor readout when over the SkewT chart, reported as temperature, dewpoint, wind direction and speed, pressure, height AGL, and RH of the trace.

## Windspeed vs Height and Inferred Temperature Advection

The windspeed vs height and inferred temperature advection with height plot (referred to by label "iii" in the NSHARP schematic images) is situated next to the SkewT to show the values at the same heights. Inferred temperature advection is from the thermal wind. Use the AWIPS-2 NSHARP Interactive Overview page for more information about the Skew-T display.

## Hodograph Display

This panel contains the hodograph display from the sounding data (referred to by label "iv" in the NSHARP schematic images). The rings in the hodograph represent the wind speed in 20 knot increments. The hodograph trace uses different colors to highlight wind observations in 3 km height increments. This display also contains information such as the mean wind, Bunkers Left/Right Moving storm motion, upshear and downshear Corfidi vectors, and a user-defined motion. Use the AWIPS NSHARP Interactive Overview page for more information about the hodograph display.

## Insets

In the SPC Wide Screen Configuration there are four small insets beneath the hodograph containing storm-relative windspeed versus height, a Storm Slinky, Theta-E vs Pressure, Possible Watch Type, Thea-E vs Height, and storm-relative wind vectors (referred to by label "v" in the NSHARP schematic images). There are buttons in the NSHARP(D2D) control button tab that toggle the six possible contents in the four boxes. Use the AWIPS NSHARP Interactive Overview page for more information on the tables and a list/definition of the parameters available.

## Table Output Displays

The Table Output Displays (referred to by label "vi" in the NSHARP schematic images) contains five different pages of parameters ranging from parcel instability to storm relative shear to severe hazards potential. Use the AWIPS NSHARP Interactive Overview page for more information on the tables and a list/definition of the parameters available.

## Graphs/Statistics

In the SPC Wide Screen Configuration there are two graphs boxes under the insets (referred to by label "vii" in the NSHARP schematic images), and they can display information on Enhanced Bulk Shear, Significant Tornado Parameter, Significant Hail Parameter (SHIP), Winter Weather, Fire Weather, Hail model (not implemented), and the Sounding Analog Retrieval System (SARS). There are buttons in the NSHARP(D2D) control button tab that toggle the six possible contents in the two boxes. Use the AWIPS NSHARP Interactive Overview page for more information on the tables and a list/definition of the parameters available.

## Sounding Inventory

This section (referred to by label "viii" in the NSHARP schematic images) controls the inventory of the soundings that have been loaded for potential display in NSHARP. The different colors of the text represent variously that a sounding/station is being displayed, available for display, or not available for display. Use the AWIPS NSHARP Interactive Overview page for more information on how to use the sounding inventory and time line.



