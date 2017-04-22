## Default Maps

The first toolbar menu item is a dropdown menu for different geographic areas and map projections.  The efault view is always **CONUS**, which is a North Polar Steregraphic projection centered on the Continental United States.

Default projections and areas available in the menu

   * **N. Hemisphere** (North Polar Stereographic)
   * **Regional** (for the selected localization site)
   * **WFO** (for the selected localization site)
   * **World Mercator**
   * **World CED**
   * **World Mollweide**
   * and **Regional** Mercator projections for **Africa**, **Australia/NZ**, **South America**, **Europe**, **Alaska**, **Hawaii**, **Puerto Rico**, **Japan**, **Pacific Ocean**, and the north and south poles.

![image](../images/mapscales.jpg)

---

## New Map / New View

**File > New Map**

Opens a new map editor tab with the default projection (CONUS Polar Stereographic).  

![image](../images/67Wwz3L.png)

This can also be done by **right-click** on any tab and selecting **New Editor**

![image](../images/Sd3qL6LJ55.gif)

---

## New Projection

**File > New Projection**

Create a new map projection.

![image](../images/VANzMW2.png)

---

## Saving / Loading Bundles

**File > Save Bundle**

Save a product display within the AWIPS system, synching the bundle between CAVE and the EDEX server.

![image](../images/xl53gG4.png)

**File > Load Bundle**

Load a previously-saved bundle from within the AWIPS system.

![image](../images/VWiGMHp.png)

**File > Delete Bundle**

Select and remove a saved bundle.

![image](../images/eRqHZpD.png)

**File > Load Bundle from Disk**

Load a previously-saved display from a path within the file directory of the workstation.

**File > Save Bundle to Disk**

Save a product display to a path within the file directory of the workstation.

**File > Load Map from Disk**

Load a map or product display from a path within the file directory of the workstation.

**File > Save Map to Disk**

Save a map or product display to a path within the file directory of the workstation.

---

## Screenshots

**File > Export > Image**

which captures a screenshot of the current view

![image](../images/UxmboZS.png)

---

## KML

**File > Export > KML**

The "Export" submenu also includes a "KML" option, which allows users to save D2D displays or
GFE grids in the KML (Keyhole Markup Language) file format. When zipped (compressed), the KML
file format forms a KMZ file, which can be used in applications such as Google Earth.

![image](../images/YRP9kOM.png)

The KML dialog box includes options to select frames to export. This includes exporting all frames,
the current/displayed frame, a range of frames, and, in GFE, the selected time range as highlighted in
the Grid Manager. Additional options are available for selection under the "Other Options" section:

* **Export Hidden**: When selected, all displayed and hidden products listed in the Product Legend section of the Main Display Pane will be exported.

* **Export Maps**: When selected, all enabled maps displayed within the Main Display Pane will be
exported.

* **Shade Earth**: When selected, a shaded background is applied to the exported product. If loaded in Google Earth, the earth will be overlaid with a black backdrop, and data will be displayed as it would in D2D with a black background.

* **Show Background Tiles**: When selected, data (such as plot data) will display on top of black
tiles when loaded in Google Earth.

---

## CAVE Import Formats

CAVE supported the following geo-referenced data files. CAVE can import the following through formats through the **CAVE** -> **Import** menu.

1. **GIS Data**

2. **BCD File**

3. **GeoTIFF**

4. **LPI File**

5. **SPI File**

6. **Displays**

![image](../images/image_14.png)

---

## CAVE Export Formats

CAVE can export to the following through the **CAVE** -> **Export** menu.

1. **KML**

2. **Editor Display**

3. **Perspective Display**

4. **Capture Current Frame**

5. **Capture All Frames**

6. **Print Screen**

![image](../images/image_15.png)
