# Ingest a New Grid

Unrecognized grids can be decoded by EDEX simply by dropping `*.grib` or `*.grib2` files into `/awips2/data_store/ingest/`

!!! note "This page explains how to ingest `.grib2` products.  To view information about `.grib` products, [please see this page](../new-grid-grib1-old)."

To add support for a new grid, two edits must be made:

* **Geospatial projection** must be defined in a [***grid navigation file***](#create-grid-projection-file)
* **Grid name**, **center**, **subcenter**, and **process ID** must be defined in a [***model definition file***](#create-model-definition)

If the parameters in the grib file haven't been previously specified, another change *may* be needed as well:

* **Center**, **subcenter**, **discipline**, **category**, and possibly **parameter ID** information may need to be defined in a [***table***](#adding-a-table)
 
---

## Ingest an Unsupported Grid

### Download Test Data

Download an example grib2 file (make sure the extension is `.grib2` or the [EDEX distribution file](../data-distribution-files/#editing-an-edex-data-distribution-file) may not recognize it), and then copy to the manual ingest point `/awips2/data_store/ingest/`:

    wget https://downloads.unidata.ucar.edu/awips2/current/files/CPTI_00.50_20180502-000144.grib2 -O cpti.grib2

    cp cpti.grib2 /awips2/data_store/ingest/

### Check Grib Logs

Confirm that the grib file decodes in the grib log file.
    
Look in the current log file (/awips2/edex/logs/edex-ingestGrib-[YYYYMMDD].log) for the following:

    INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.1200 (sec) Latency: 21.8080 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.1180 (sec) Latency: 21.8140 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.4230 (sec) Latency: 21.8360 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.2240 (sec) Latency: 21.9140 (sec)
    
    ...
    
!!! warning "This step will fail for our example because the parameter is not yet defined.  The error will look like:"

<pre>
INFO  2020-07-20 20:34:17,710 2565 [GribPersist-1] GridDao: EDEX - Discarding record due to missing or unknown parameter mapping: /grid/2018-05-02_00:01:44.0_(0)/GribModel:161:0:97/null/null/403/Missing/FH/500.0/-999999.0
INFO  2020-07-20 20:34:17,710 2566 [GribPersist-1] Ingest: EDEX: Ingest - grib2:: /awips2/data_store/ingest/CPTI_00.50_20180502-000144.grib2 processed in: 2.3550 (sec)
INFO  2020-07-20 20:34:17,827 2567 [Ingest.GribDecode-6] grib: EDEX - No parameter information for center[161], subcenter[0], tableName[4.2.209.3], parameter value[61]
</pre>
    
In order to successfully ingest the example file, you must [**define the appropriate table**](#adding-a-table).

### Check HDF5 Data

Check that the hdf5 data directory exists for our unnamed grid

        ls -latr /awips2/edex/data/hdf5/grid/GribModel:161:0:97

Though the grib file has been decoded, it has been given a generic name with its **center, subcenter, and process IDs** (161, 0, 97, respectively). 
    
---

## Determine Grid Projection

When a grid is ingested a record is added to the `grid_coverage` table with its navigation information:

    psql metadata
  
    metadata=> select nx,ny,dx,dy,majoraxis,minoraxis,la1,lo1,lov,latin1,latin2,spacingunit,lad,la2,latin,lo2,firstgridpointcorner from gridcoverage where id=(select distinct(location_id) from grid_info where datasetid='GribModel:161:0:97');
     nx  | ny  |  dx   |  dy   | majoraxis | minoraxis |    la1    | lo1 | lov | latin1 | latin2 | spacingunit | lad | la2 | latin | lo2 | firstgridpointcorner 
    -----+-----+-------+-------+-----------+-----------+-----------+-----+-----+--------+--------+-------------+-----+-----+-------+-----+----------------------
     600 | 640 | 0.005 | 0.005 |           |           | 40.799999 | 261 |     |        |        | degree      |     |     |       |     | UpperLeft
    (1 row)
  Compare with the projection info returned by wgrib2 on the original file (look at the bolded sections below and make sure they match up with the corresponding entries returned from the database above):
<!--
    Using html for this code block so that certain sections within the code can be emphasized (bolded)
-->
<pre>
wgrib2 -grid -nxny cpti.grib2
1:0:grid_template=0:winds(N/S):
	<b>lat-lon grid:(600 x 640)</b> units 1e-06 <b>input WE:NS</b> output WE:SN res 48
	lat <b>40.799999</b> to 37.599999 by <b>0.005000</b>
	lon <b>260.999999</b> to 263.999999 by <b>0.005000</b> #points=384000:(600 x 640)
  ...
</pre>
Notice that our grib2 file has a **Lat/lon Grid** projection, that starts in the **UpperLeft** corner (as defined by input West to East, North to South).  Where:

* **nx** is **600**
* **ny** is **640**
* **dx** is **0.005**
* **dy** is **0.005**
* **la1** is **40.799999**
* **lo1** is **261**

We will need these values for the next step. 

!!! note "**There is a tolerance of +/- 0.1 degrees** to keep in mind when defining your coverage (la1 and lo1) area."

---

## Create Grid Projection File
    
### Projection Types
    
!!! note "You may not have information for every tag listed, for example it's not required for the latLonGridCoverage to have spacingUnit, la2, lo2."    

Grid projection files are stored in `/awips2/edex/data/utility/common_static/base/grib/grids/` and there are four grid coverage types available:

1. **lambertConformalGridCoverage** (example: `RUCIcing.xml`)

        <lambertConformalGridCoverage>
            <name>305</name>
            <description>Regional - CONUS (Lambert Conformal)</description>
            <la1>16.322</la1>
            <lo1>-125.955</lo1>
            <firstGridPointCorner>LowerLeft</firstGridPointCorner>
            <nx>151</nx>
            <ny>113</ny>
            <dx>40.63525</dx>
            <dy>40.63525</dy>
            <spacingUnit>km</spacingUnit>
            <minorAxis>6356775.0</minorAxis>
            <majorAxis>6378160.0</majorAxis>
            <lov>-95.0</lov>
            <latin1>25.0</latin1>
            <latin2>25.0</latin2>
        </lambertConformalGridCoverage>

2. **polarStereoGridCoverage** (example `seaice_south1_grid.xml`)
 
        <polarStereoGridCoverage>
            <name>405</name>
            <description>Sea Ice south 690X710 13km grid</description>
            <la1>-36.866</la1>
            <lo1>139.806</lo1>
            <firstGridPointCorner>LowerLeft</firstGridPointCorner>
            <nx>690</nx>
            <ny>710</ny>
            <dx>12.7</dx>
            <dy>12.7</dy>
            <spacingUnit>km</spacingUnit>
            <minorAxis>6371229.0</minorAxis>
            <majorAxis>6371229.0</majorAxis>
            <lov>100.0</lov>
        </polarStereoGridCoverage>

3. **latLonGridCoverage** (example `UkmetHR-SHemisphere.xml`)
    
        <latLonGridCoverage>
            <name>864162002</name>
            <description>UKMet HiRes combined - Southern Hemisphere
                Longitude range 71.25E - 70.416E </description>
            <la1>-89.721</la1>
            <lo1>71.25</lo1>
            <firstGridPointCorner>LowerLeft</firstGridPointCorner>
            <nx>864</nx>
            <ny>162</ny>
            <dx>0.833</dx>
            <dy>0.556</dy>
            <spacingUnit>degree</spacingUnit>
            <la2>-0.278</la2>
            <lo2>70.416</lo2>
        </latLonGridCoverage>
    
4. **mercatorGridCoverage** (example `gridNBM_PR.xml`)

        <mercatorGridCoverage>
            <name>NBM_PR</name>
            <description> National Blend Grid over Puerto Rico - (1.25 km)</description>
            <la1>16.9775</la1>
            <lo1>-68.0278</lo1>
            <firstGridPointCorner>LowerLeft</firstGridPointCorner>
            <nx>339</nx>
            <ny>225</ny>
            <dx>1.25</dx>
            <dy>1.25</dy>
            <la2>19.3750032477232</la2>
            <lo2>-63.984399999999994</lo2>
            <latin>20</latin>
            <spacingUnit>km</spacingUnit>
            <minorAxis>6371200</minorAxis>
            <majorAxis>6371200</majorAxis>
        </mercatorGridCoverage>

### Creating a New Projection File

Copy an existing xml file with the same grid projection type (in this case **latLonGridCoverage**) to a new file `cpti.xml`:

    cd /awips2/edex/data/utility/common_static/base/grib/grids/
    cp MRMS-1km-CONUS.xml cpti.xml
  
And edit the new `cpti.xml` to define the projection values using the [output from wgrib2 or the database](#determine-grid-projection) (example provided):

    vi cpti.xml
  
    <latLonGridCoverage>
        <name>600640</name>
        <description>Small domain for CPTI products</description>
        <la1>40.799999</la1>
        <lo1>261</lo1>
        <firstGridPointCorner>UpperLeft</firstGridPointCorner>
        <nx>600</nx>
        <ny>640</ny>
        <dx>0.005</dx>
        <dy>0.005</dy>
        <spacingUnit>degree</spacingUnit>
    </latLonGridCoverage>
  
!!! note "Notice the `<name>600640</name>` tag was created by using the number of grid points (600 and 640). This name can be anything as long as it is unique and will be used to match against in the model definition."

---

## Create Model Definition 

Model definition XML files are found in **/awips2/edex/data/utility/common_static/base/grib/models/**. 

Since our grib2 file has a center of 161 (NOAA) we will edit the **gribModels_NOAA-161.xml** file.

    cd /awips2/edex/data/utility/common_static/base/grib/models/
    
    vi gribModels_NOAA-161.xml

In `<gribModelSet>`, under the `<-- Subcenter 0 -->` comment, add an entry:

    <model>
        <name>CPTI</name>
        <center>161</center>
        <subcenter>0</subcenter>
        <grid>600640</grid>
        <process>
            <id>97</id>
        </process>
    </model>

Save the model file and restart edex:

    sudo service edex_camel restart ingestGrib
    
Now if you drop `cpti.grib2` into the manual endpoint again, it should ingest without any persistence errors.

---

## Adding a Table

If you ingest a piece of data and the parameter appears as unknown in the metadata database, ensure that the correct parameter tables are in place for the center/subcenter.

The tables are located in **/awips2/edex/data/utility/common_static/base/grib/tables/**.  They are then broken into subdirectories using the following structure: **/[Center]/[Subcenter]/4.2.[Discipine].[Category].table**. 

!!! note "There are also default parameters that all grib products may access located in this directory: **/awips2/edex/data/utility/common_static/base/grib/tables/-1/-1/**"

If you are using a grib2 file, then you can use either the log output or the `-center`, `-subcenter`, and `-full_name` options on `wgrib2` to get the center, subcenter, discipline, category, and parameter information:

The table would be found in the directory structure using this file's center and subcenter.  

### Finding Center
The center can be found by either:

 * Running the following command:
    
        wgrib2 -center cpti.grib2
          1:0:center=US NOAA Office of Oceanic and Atmospheric Research
        ...
        
    And then looking up the corresponding value for "US NOAA Office of Oceanic and Atmospheric Research" at [**this website**](https://www.nco.ncep.noaa.gov/pmb/docs/on388/table0.html), where it happens to be **161**.

**OR:**

* Running the following command:
  <pre>
wgrib2 -varX cpti.grib2
1:0:var209_255_1_<b>161</b>_3_61
...</pre>
  
    Where the 4th argument after "var" is the center id, in this case **161**.

### Finding Subcenter

To get the subcenter, simply run:
<pre>
wgrib2 -subcenter cpti.grib2
  1:0:subcenter=<b>0</b>
...
</pre>
The subcenter of this file is **0**.

Based on the center and subcenter, the corresponding directory is:

    /awips2/edex/data/utility/common_static/base/grib/tables/161/0/

### Finding Discipline and Category

To find the exact table, we need the discipline and category:

<pre>
wgrib2 -full_name cpti.grib2
	1:0:var<b>209</b>_<b>3</b>_<b>61</b>.500_m_above_mean_sea_level
  ...
</pre>

In this case the **discipline is 209** and **category is 3**, so the corresponding table is:

    4.2.209.3.table

### Corresponding Table      

The full path to the corresponding table would be:

    /awips2/edex/data/utility/common_static/base/grib/tables/161/0/4.2.209.3.table

The parameter ID was also listed in that output as **61**.  Make sure that specific parameter information is defined in the table:
<pre>
...
56:56:Reflectivity at -20C:dBZ:ReflectivityM20C
57:57:Reflectivity At Lowest Altitude (RALA):dBZ:ReflectivityAtLowestAltitude
58:58:Merged Reflectivity At Lowest Altitude (RALA):dBZ:MergedReflectivityAtLowestAltitude
59:59:CPTI 80mph+:%:CPTI80mph
<b>61:61:CPTI 110mph+:%:CPTI110mph</b>
</pre>

You will have to restart ingestGrib for the changes to take place:

      sudo service edex_camel restart ingestGrib
      
Now you can try [re-ingesting the grib2 file](#download-test-data).
      
---

## Creating Menu Items

After you have confirmed that the grid was ingested with the given name, you can [edit the D2D product menus to display the new grid](../cave/d2d-edit-menus.md).

---

## Using wgrib2

Mentioned in this page are a few command parameters for `wgrib2` such as `-grid`, `varX`, `-center`, `-subcenter`, and `-full_name`.

A complete [list of all available parameters can be found here](https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/long_cmd_list.html).

---

## Troubleshooting Grib Ingest

* Make sure the latitude and longitude entries in your coverage specification file match those of your ingested raw grib file. There is a tolerance of +/- 0.1 degree to keep in mind when defining your coverage area.

* If some of the information is unknown, using a grib utility application such as *wgrib* and *wgrib2* can be useful in determining the information that must be added to correctly process a new grib file.

* If you are experiencing `Segmentation fault` errors when running wgrib2, it may be best to install the latest version using the following command:

        yum install wgrib2
  And then you may either need to change where `wgrib2` points to, or use `/bin/wgrib2` to run the recently downloaded version.
