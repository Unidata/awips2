
There are two main subsets of types of tools in CAVE: [Display Tools](#display-tools) and [Radar Tools](#radar-tools).

## Display Tools

The display tools are a subset of the tools available in CAVE.  These programs are accessible though the **Tools** dropdown menu.

![image](../images/toolsMenuDisplayTools.png)

Many of the tools listed under the Tools menu can be placed into an **editable state**. Do not enable the "Hide Legends" feature if you want to place a tool in an editable state, because access to editability is done by clicking the center mouse button, or right-clicking over the **Product Legend**.

---

### Az/Ran Overlay

This tool displays a movable azimuth/range radar map overlay. The overlay is in the "editable" state when displayed, and can be relocated by clicking the right mouse button.

![image](../images/az-range-overlay.gif)

---

### Baselines

Selecting Baselines displays 10 lines, labeled A-A' to J-J', along which cross-sections can be constructed from within the Volume Browser. Baselines come up editable.

"Snapping" an Interactive Baseline: If you are zoomed in over an area when you load Interactive Baselines and no Baselines appear, press the right mouse button to "snap" a Baseline to where the mouse cursor is. The system chooses a Baseline that has not been recently used. If you are working with a Baseline, a second click with the right mouse button will return you to the original Baseline, even if you modified another Baseline in the meantime.

![image](../images/baselines.png)

---

### Choose By ID

Choose By ID, which is a function of DMD (Digital Mesocyclone Display), is a method of selecting feature locations. The tool is used to monitor the same feature at a certain location. Without the Choose By ID tool, a monitored feature (over a period of time) could move away from its monitored location and another feature could move in its place. You can use Choose By ID to set points, baselines, and "Home" for conventional locations like METARs and RAOBs (Radiosonde Observations), but its primary use is for the WSR-88D-identified mesocyclone locations. You can also access the Choose By ID tool from the Tools menu on the Volume Browser.

![image](../images/chooseByID.png)

---

### Distance Bearing

Selecting this tool displays six editable lines, each of which shows the azimuth and range of the labeled end of the line relative to the unlabeled end of the line. You can make the lines editable by clicking the center mouse button over the legend at the lower right of the display. Once in edit mode, a line can be moved as a unit and/or either of its end points can be adjusted.

![image](../images/distanceBearing.png)

---

### Distance Speed

This tool can be used to determine the speed and direction of a storm or any other meteorological feature of interest. Selecting Distance Speed displays a Centroid Marker to move to the location of the storm or feature of interest in any two or more frames of displayed imagery (e.g., a satellite or radar loop). The system then displays a storm track with the direction (degrees) and speed (knots) of movement. When you select the Distance Speed option, the Distance Speed dialog box opens.

![image](../images/distanceSpeed.gif)

* **Mode**: You have the following selections from this option.

	* **Point**: A radio button that allows you to set the Centroid Marker as a single point.

	* **Polyline**: A radio button that allows you to set the Centroid Marker as a polyline.

* **Legend**: You have the following selections from this option.

	* **Time**: A radio button that allows you to display time with the Centroid Marker.

	* **Speed**: A radio button that allows you to display speed with the Centroid Marker.

---

### Distance Scale

Enabling this feature adds a scalebar to the bottom right hand of the main D2D display.  This tool can be used to determine the size of a storm or any other meteorological feature of interest.

![image](../images/distanceScale.gif)

---

### Feature Following Zoom

When you zoom in over a small area to be able to view a feature in detail, animation will often cause the feature to move into and then out of the field of view. This tool allows you to follow a feature of interest even when zoomed in to a small area.

To use this feature, first, you need to identify the location and motion of the feature, using Distance Speed or the WarnGen tracker. Once satisfied that the tracking icon is following the feature of interest, load this tool, and the center of the zoom area will track with the Distance Speed icon. Toggling the overlay off will resume the standard zooming behavior, and toggling it back on will reinvoke the feature following zoom.

---

### Home

Selecting the Home option displays a marker, which is an "X" with the word "Home" next to it.
Clicking on the Home Location Legend with the center mouse button makes the marker editable; drag the "X" or click with the right mouse button to change its location. When the Home Marker is displayed, use the **Sample** feature (click and hold to access the menu to turn on sampling) to display the range in miles and azimuth (in degrees) of the pointer location relative to the Home location.

---

### Points

The Points option initially displays a circular 10-point pattern, labeled A through J on the Map display. Points are used to generate model soundings, time-height cross-sections, time series, and variable vs. height plots using the Volume Browser. As with the Baselines, the locations of these Points can be edited in the following manner:

* **"Snapping" an Interactive Point**: If you are zoomed in over an area when you load Interactive
Points and no Points appear, click the right mouse button to "snap" a Point to where the mouse cursor is positioned. The system chooses a Point that has not been recently used. If you are currently working with a Point, then a second right mouse button click will place another Point at the location of your cursor.

* **Dynamic Reference Map**: When you generate a model sounding, a time-height cross-section, a
time series, or a variable vs. height plot, a small reference map indicating the location(s) of the
plotted sounding(s) is provided in the upper left corner of the Main Display Pane.

Points may be created, deleted, hidden, and manipulated (location, name, font, and color). Points are not limited in terms of number, location, or designation. Points may also be assigned to different groups to facilitate their use. Once the Points tools have been loaded, the addition, deletion, or manipulation of Points can be accomplished in three ways:

1. **Create Point Dialog Box**: The Create Point dialog box is opened by clicking and holding the right mouse button on the map (but not on any exisiting Point) and selecting the "New Point..." option.

	The Create Point dialog box opens with the Lat and Lon text boxes populated with the latitude and longiture values at the point where you had clicked the right mouse button. The latitude and longitude values can be viewed in "Degrees : Minutes : Seconds," "Degrees : Minutes," or "Degrees Only" (N and S refer to North and South; W and E refer to West and East).

	In the Create Point dialog box, you **must**:

	* Enter the Point's name

	And **may** do any of the following:

	* Modify the latitude and longitude values
	* Assign the Point's color and font use
	* Assign the Point to a group
	* Select whether the Point is movable or hidden

	By default, individual Points do not have an assigned color. They inherit the color of the Interactive Points layer reflected in the Interactive Points product legend. You can change the color of the Interactive Points layer by right clicking on the Interactive Points product legend and selecting a color from the dropdown list. The selected color then changes all points not having an assigned color to the new color.

	Points can be assigned to "**<No Group\>**" which will organize them in the root location containing the group names when accessed by the Edit Points dialog box (see below).

2. **Edit Point Dialog Box**: The Edit Point dialog box is opened by clicking and holding the right mouse button on a Point on the map and selecting the "Edit Point..." option. The latitude and longitude values can be viewed in "Degrees : Minutes : Seconds," "Degrees : Minutes," or "Degrees Only" (N and S refer to North and South; W and E refer to West and East).

	Besides the option of selecting the Edit Points dialog box, you also have the option
of selecting "Hide Point," "Delete Point," or "Move Point." Once hidden, the Point can be
unhidden using the Points List dialog box, where you would uncheck the checkbox under
the "Hidden" column adjacent to the Point that was hidden (see below). If "Delete Point" is
selected, a pop-up opens to confirm whether you want to delete the Point. Selecting the
"Move Point" option moves the Point to wherever you place the cursor on the map.

3. **Points List Dialog Box**: The Points List dialog box is opened by clicking and holding the right mouse button on the Interactive Points product legend and selecting the "Edit Points..." option.

	The Points List dialog box lists all the available groups and Points. Groups can be expanded to
review the list of Points assigned to that group by clicking the arrow next to the group name.
Initially, the default set of Points (A-J) are listed in the D2D Group, as shown above. In the
Points List dialog box, Points and groups may be dragged into and out of other groups to create
or disassemble subgroups. The Points List dialog box also includes three columns.

	* **Point Name**: Lists the group name and designated Points.
	* **Movable**: Checking the checkbox adjacent to the Point disables the Point from being
moved.
	* **Hidden**: Checking the checkbox adjacent to the Point hides the Point on the map.

---

### Put home cursor

The Put home cursor tool provides an easy way to locate a METAR observation station, a city and
state, or a latitude/longitude coordinate. For Canada and Mexico, only the METAR observation stations and latitude/longitude coordinates are accessible. When you select Put home cursor from the Tools dropdown menu, the Home marker X is displayed and the Put Home Cursor dialog box opens.

You can use the Home marker, as previously described in the Home Tool, and the new Home location
(station, city/state, or latitude/longitude) is identified in the Put Home Cursor dialog box.
Another way to use this tool is to type in the station, city and state, or latitude and longitude, and select Go, or hit Enter on the keypad, to move the Home marker to the specified location. The new location's nearest METAR site, city and state, and latitude and longitude appear in the Put Home Cursor dialog box. The Put Home Cursor dialog box contains the following options.

* **Location Selection**: There are three ways to find a desired location. Once you choose the
Station, City/State, or Lat/Lon radio button, an Entry Box is activated next to the respective label within the Put Home Cursor dialog box. Enter the desired location information.

* **Go**: This menu button initiates the search for the desired station, city/state, or latitude/longitude. The Home marker jumps to the newly specified location.

---

### Range Rings

The Range Rings Tool displays adjustable range rings around locations of interest to your local office. When you select Range Rings from the Tools dropdown menu, the Range Rings legend appears in the Main Display Pane. The tool comes up editable, and the rangeRing dialog box opens. (Clicking the middle mouse button over the legend toggles tool editability and closes/opens the rangeRing dialog box.) Within this dialog box, you can toggle on/off any of the target locations using the square selectors. Adjust the size of the radii (in nautical miles) by typing a new value in the entry boxes associated with each location and pressing the Apply button. You can also add labels at the center of the range ring and/or at any of the radial distances using the Labels Options menu associated with each location. Using the Movable Rings, you can add a new location at a specific point by using the Interactive Points Tool, or by typing in latitude/longitude coordinates. There is no practical limit on the number of new locations you can add to the display. The list of locations is pre-set but can be customized at a field site.

![image](../images/rangeRings.png)

---

### Sunset/Sunrise

By typing a date, as well as the latitude and longitude of a location into the Sunrise/Sunset Tool dialog box, you can obtain the time (for any time zone) of sunrise and sunset, as well as the total length of daylight for that date. Additional features include the ability to calculate the sunrise/sunset in a different hemisphere, and the azimuthal angles, relative to true north, of the sunrise and sunset.

![image](../images/sunset-sunrise.png)

---

## Text Window

Selecting this option brings up a Text Display window that behaves in the same way as a window on the [Text Workstation](#text-workstation), except that the scripts menu is disabled.

---

### Time of Arrival / Lead Time

Selecting the Time Of Arrival / Lead Time option displays a tracking line from a feature's initial starting point in a past frame to its final position in the current frame. Once the final position is set, an Arrival Point is displayed. You can drag this point anywhere along the line to get the Time Of Arrival / Lead Time and Distance. You can also change the Mode from Point to Circular Front or Polyline anywhere along the line to better represent the feature(s).

---

### Units Calculator

This tool converts the units of the first column into differing units of the second column. The units are grouped into temperature, speed, distance, time, and atmospheric pressure. First, simply type the number and select the units of the value you wish to convert in the firstcolumn entry box. Then in the second column, select the desired units to which you want the original value converted. The new value will appear in the second column entry box.

![image](../images/unitsCalculator.png)

---

### Text Workstation

By selecting one of the "Text" buttons, a text window opens up. In National Weather Service operations, the text workstation is used to edit new warning text as well as look up past warnings, METARs, and TAFs. This functionality is **disabled** in the NSF Unidata AWIPS version.

![](../images/textWorkstation.png)

---

## Radar Tools

The radar tools are a subset of the tools available in CAVE. These programs are accessible though the **Tools** dropdown menu, and in individual site radar menus.

![image](../images/toolsMenuRadarTools.png)

---

### Estimated Actual Velocity (EAV)

A velocity (V) display from the radar shows only the radial component of the wind, so the indicated speed depends on the direction of the wind and the azimuth (direction) from the radar. Consider, for example, a north wind. Straight north of the radar, the full speed of the wind will be seen on the V product. As one moves around to the east of the radar, the radial component gets smaller, eventually reaching zero straight east of the radar. If the wind direction is known, then the actual wind speed can be computed by dividing the observed radial speed by the cosine of the angle between the radar radial and the actual direction. The EAV tool allows you to provide that angle and use the sampling function of the display to show the actual wind speed.

![image](../images/estimatedActualVelocity.gif)

---

### Radar Display Controls

<span style="float:right;">
![image](../images/radarDisplayControls.png)
</span>

The Radar Display Controls dialog box is derived from the Radar Tools submenu and provides options that control the appearance of the Storm Track Information (STI), the Hail Index (HI), the Tornado Vortex Signature (TVS), the Digital Mesocyclone Display (DMD) products, the Microburst Alert (MBA) products, the Storm Relative Motion (SRM), and the SAILS products. The Radar Display Controls dialog box options are described below.

> **Note**: Our version of CAVE may not have all the products that these options are applicable to.

The Radar Display Controls dialog box is divided into eight sections: [STI](#sti-storm-track-information), [HI](#hi-hail-index), [TVS](#tvs-tornado-vortex-signature), [DMD/MD/TVS](#dmd-md-tvs), [DMD](#dmd-digital-mesocyclone-display), [MBA](#mba-microburst-alert), [SRM](#srm-storm-relative-motion), and [SAILS](#sails-supplemental-adaptive-intra-volume-low-level-scan). Each section has the following options:

#### STI (Storm Track Information)

This section has options to adjust the appearance of the STI graphic product.

* **Number of storms to show**: This slider bar lets you choose the maximum number of storms (0 to 100) you wish to display on the STI product. The default value is 20 storms.
* **Type of track to show**: This options menu allows you to choose the type of storm track that you want displayed.

#### HI (Hail Index)

This portion of the Radar Display Controls dialog box contains options that alter the appearance of the HI radar graphic product. You can set the low and high algorithm thresholds of the Probability of Hail (POH) and the Probability of Severe Hail (POSH). Storms that meet the low POH threshold are indicated by small open triangles, while small solid triangles mark those that meet the high POH threshold. Similarly, large open triangles or solid triangles are plotted for the POSH low and high thresholds, respectively.

* **Low hail probability (POH)**: The storms that meet or exceed the threshold are indicated by small open triangles. The default setting is 30.
* **Low severe hail probability (POSH)**: The storms that meet or exceed the threshold are indicated by large open triangles. The default setting is 30.
* **High hail probability**: The storms that meet or exceed the threshold are indicated by small solid triangles. The default setting is 50.
* **High severe hail probability**: The storms that meet or exceed the threshold are indicated by small solid triangles. The default setting is 50.

#### TVS (Tornado Vortex Signature)

There is one option in this section of the Radar Display Controls dialog box.

* **Show elevated TVS**: This toggle button lets you control the appearance of the elevated TVS radar graphic product.

#### DMD, MD, TVS

There is one option in this section of the Radar Display Controls dialog box.

* **Show extrapolated features**: With this option, you can choose whether to show the time-extrapolated features using DMD, MD, or TVS.

#### DMD (Digital Mesocyclone Display)

* **Minimum feature strength**:  A mesocyclone clutter filter which specifies the minimum 3D strength rank use to display a mesocyclone (default is 5).
* **Show overlapping Mesos**:  Toggles whether to show overlapping mesocyclones.
* **Type of track to show**:  This dropdown has option available for whether to display past and/or forcast tracks.

#### MBA (Microburst Alert)

* **Show Wind Shear**:  This option allows you to choose whether to display wind shear associated with microburts alerts.

#### SRM (Storm Relative Motion)

The first three options in the SRM section allow you to choose where you want to derive the storm motion from.

* **Storm Motion from WarnGen Track**:  Selecting this option will display the storm motion from a WarnGen Track.
* **Average Storm Motion from STI**:  Selecting this option will display the average storm motion from from the storm track information (STI).
* **Custom Storm Motion**:  Selecting this option allows you to specify a custom storm motion with the selections below.
    - **Direction**: This slider allows you to choose the direction (in degrees??) of the storm motion.
    - **Speed**:  This slider allows you to specify the speed (in mph??) of the storm motion.

#### SAILS (Supplemental Adaptive Intra-Volume Low Level Scan)

* **Enable SAILS Frame Coordinator**:  
**Enabled (default)**: keyboard shortcuts change where tilting up from 0.5 degree SAILS tilt will step to the next higher tilt (similar to GR2 Analyst) and Ctrl right arrow will step to the most recent tilt available for any elevation angle.  
**Disabled**: keyboard shortcuts change where tilting up from 0.5 degree SAILS tilt will not go anywhere (old confusing behavior) and Ctrl right arrow will step to the most recent time of the current tilt.

---

### VR - Shear

This tool is used in conjunction with Doppler velocity data to calculate the velocity difference (or "shear") of the data directly under the end points. As with the Baselines, this feature comes up editable and the end points can be dragged to specific gates of velocity data. When in place, the speed difference (kts), distance between end points (nautical miles), shear (s-1), and distance from radar (Nmi) are automatically plotted next to the end points and in the upper left corner of the Main Display Pane. A positive shear value indicates cyclonic shear, while a negative value indicates anticyclonic shear. If either end point is not directly over velocity data, the phrase "no data" is reported for the shear value. This tool is also useful in determining gate-to-gate shear. Simply place the two end points directly over adjacent gates of velocity data.

* **"Snapping" VR Shear**: If you are zoomed in over an area when you load VR - Shear, and the VR - Shear Baseline does not appear, click the right mouse button to "snap" the Baseline to where the mouse cursor is located.
* **VR - Shear in 4 Panel**: You can use the VR - Shear Tool when the large display is in 4 panel
mode. The VR - Shear overlay is loaded in different colors for each panel. There are actually
four copies of the program running, and each behaves independently. This means that you can
get accurate readings in any one of the four panels — one VR - Shear panel is editable at a time. To activate, click the center mouse button on the VR - Shear legend in the desired panel and position the query line to the echoes of interest.
