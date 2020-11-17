# Ingest a New Grid

Unrecognized grids can be decoded by EDEX simply by dropping `*.grib` or `*.grib2` files into `/awips2/data_store/ingest/`

To add support for a new grid, two edits must be made:

* **Geospatial projection** must be defined in a [***grid navigation file***](#create-grid-projection-file)
* **Grid name**, **center**, **subcenter**, and **process ID** must be defined in a [***model definition file***](#create-model-definition)

If the parameters in the grib file haven't been previously specified, another change *may* be needed as well:

* **Center**, **subcenter**, **discipline**, **category**, and possibly **parameter ID** information may need to be defined in a [***table***](#adding-a-table)
 
---

## Ingest an Unsupported Grid

### Grib Products

1. Download an example grib1 file and rename to a `*.grib` extension, then copy to the manual ingest point `/awips2/data_store/ingest/` 

        wget https://www.unidata.ucar.edu/software/awips2/14102318_nmm_d01.GrbF00600 -O wrf.grib
    
        cp wrf.grib /awips2/data_store/ingest/
    
    Remember that the data distribution file (`/awips2/edex/data/utility/common_static/base/distribution/grib.xml`) will match filenames which have the `*.grib*` extension.

2. Confirm that the grib file decodes in the grib log file:
    
        edex log grib
    
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.1200 (sec) Latency: 21.8080 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.1180 (sec) Latency: 21.8140 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.4230 (sec) Latency: 21.8360 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.2240 (sec) Latency: 21.9140 (sec)
        
        ...

3. Check that the hdf5 data directory exists for our unnamed grid

        ls -latr /awips2/edex/data/hdf5/grid/GribModel:7:0:89

    Though the grib file has been decoded, it has been given a generic name with its center, subcenter, and process IDs (7, 0, 89, respectively). 
    
### Grib2 Products

1. Download an example grib2 file and rename to a `*.grib2` extension, then copy to the manual ingest point `/awips2/data_store/ingest/` 

        wget https://www.unidata.ucar.edu/software/awips2/CPTI_00.50_20180502-000144.grib2 -O cpti.grib2
    
        cp cpti.grib2 /awips2/data_store/ingest/
    
    Remember that the data distribution file (`/awips2/edex/data/utility/common_static/base/distribution/grib.xml`) will match filenames which have the `*.grib*` extension.

2. Confirm that the grib file decodes in the grib log file:
    
        edex log grib
    
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.1200 (sec) Latency: 21.8080 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.1180 (sec) Latency: 21.8140 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.4230 (sec) Latency: 21.8360 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/ingest/cpti.grib2 processed in: 0.2240 (sec) Latency: 21.9140 (sec)
        
        ...
        
    **Note:** This step will likely fail, because the parameter is not yet defined.  The error will look like:

        INFO  2020-07-20 20:34:17,710 2565 [GribPersist-1] GridDao: EDEX - Discarding record due to missing or unknown parameter mapping: /grid/2018-05-02_00:01:44.0_(0)/GribModel:161:0:97/null/null/403/Missing/FH/500.0/-999999.0
        INFO  2020-07-20 20:34:17,710 2566 [GribPersist-1] Ingest: EDEX: Ingest - grib2:: /awips2/data_store/ingest/CPTI_00.50_20180502-000144.grib2 processed in: 2.3550 (sec)
        INFO  2020-07-20 20:34:17,827 2567 [Ingest.GribDecode-6] grib: EDEX - No parameter information for center[161], subcenter[0], tableName[4.2.209.3], parameter value[61]
        
    In order to successfully ingest the example file, [**define the appropriate table**](#grib2-products_4).

3. Check that the hdf5 data directory exists for our unnamed grid

        ls -latr /awips2/edex/data/hdf5/grid/GribModel:161:0:97

    Though the grib file has been decoded, it has been given a generic name with its center, subcenter, and process IDs (161, 0, 97, respectively). 
    
---

## Determine Grid Projection

### Grib Products

When the grid was ingested a record was added to the `grid_coverage` table with its navigation information:

    psql metadata
    
    metadata=# select nx,ny,dx,dy,majoraxis,minoraxis,la1,lo1,lov,latin1,latin2 from gridcoverage where id=(select distinct(location_id) from grid_info where datasetid='GribModel:7:0:89');
     nx  | ny  |        dx        |        dy        | majoraxis | minoraxis |       la1        |        lo1        |        lov        |      latin1      |      latin2      
    -----+-----+------------------+------------------+-----------+-----------+------------------+-------------------+-------------------+------------------+------------------
     201 | 155 | 4.29699993133545 | 4.29699993133545 |   6378160 |   6356775 | 42.2830009460449 | -72.3610000610352 | -67.0770034790039 | 45.3680000305176 | 45.3680000305176
    (1 row)

Compare with the projection info returned by wgrib on the original file (look at the bolded sections below and make sure they match up with the corresponding entries returned from the database above):
<!--
Using html for this code block so that certain sections within the code can be emphasized (bolded)
-->
<pre>
wgrib -V wrf.grib  
rec 799:27785754:date 2014102318 ALBDO kpds5=84 kpds6=1 kpds7=0 levels=(0,0) grid=255 sfc 6hr fcst: bitmap: 736 undef
  ALBDO=Albedo [%]
  timerange 0 P1 6 P2 0 TimeU 1  <b>nx 201 ny 155</b> GDS grid 3 num_in_ave 0 missing 0
  center 7 subcenter 0 process 89 Table 2 scan: WE:SN winds(grid) 
  <b>Lambert Conf: Lat1 42.283000 Lon1 -72.361000 Lov -67.077000</b>
      <b>Latin1 45.368000 Latin2 45.368000</b> LatSP 0.000000 LonSP 0.000000
      North Pole (201 x 155) <b>Dx 4.297000 Dy 4.297000</b> scan 64 mode 8
  min/max data 5 21.9  num bits 8  BDS_Ref 50  DecScale 1 BinScale 0
</pre>

Notice that our grib file has a **Lambert Conformal** projection.  We will need these values for the next step. Note that **there is a tolerance of +/- 0.1 degrees** to keep in mind when defining your coverage area.

### Grib2 Products

When the grid was ingested a record was added to the `grid_coverage` table with its navigation information:

    psql metadata
    
    metadata=# select nx,ny,dx,dy,majoraxis,minoraxis,la1,lo1,lov,latin1,latin2 from gridcoverage where id=(select distinct(location_id) from grid_info where datasetid='GribModel:161:0:97');
    
    nx  | ny  |  dx   |  dy   | majoraxis | minoraxis |    la1    | lo1 | lov | latin1 | latin2 
    -----+-----+-------+-------+-----------+-----------+-----------+-----+-----+--------+--------
    600 | 640 | 0.005 | 0.005 |           |           | 40.799999 | 261 |     |        |       
    (1 row)
  Compare with the projection info returned by wgrib2 on the original file (look at the bolded sections below and make sure they match up with the corresponding entries returned from the database above):
<!--
    Using html for this code block so that certain sections within the code can be emphasized (bolded)
-->
<pre>
wgrib2 -grid -nxny cpti.grib2
1:0:grid_template=0:winds(N/S):
	<b>lat-lon grid:(600 x 640)</b> units 1e-06 input WE:NS output WE:SN res 48
	lat <b>40.799999</b> to 37.599999 by <b>0.005000</b>
	lon <b>260.999999</b> to 263.999999 by <b>0.005000</b> #points=384000:(600 x 640)
  ...
</pre>
Notice that our grib2 file has a **Lat/lon Grid** projection.  Where:

* **nx** is **600**
* **ny** is **640**
* **dx** is **0.005**
* **dy** is **0.005**
* **la1** is **40.799999**
* **lo1** is **261**

We will need these values for the next step. Note that **there is a tolerance of +/- 0.1 degrees** to keep in mind when defining your coverage (la1 and lo1) area.

---

## Create Grid Projection File
    
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

### Grib Products

Copy an existing xml file with the same grid projection type (in this case **lambertConformalGridCoverage**) to a new file `wrf.xml`:

    cd /awips2/edex/data/utility/common_static/base/grib/grids/
    cp RUCIcing.xml wrf.xml

And edit the new `wrf.xml` to define the projection values using the [output from wgrib or the database](#determine-grid-projection) (example provided):

    vi wrf.xml
    
    <lambertConformalGridCoverage>
        <name>201155</name>
        <description>Regional - CONUS (Lambert Conformal)</description>
        <la1>42.2830009460449</la1>
        <lo1>-72.3610000610352</lo1>
        <firstGridPointCorner>LowerLeft</firstGridPointCorner>
        <nx>201</nx>
        <ny>155</ny>
        <dx>4.29699993133545</dx>
        <dy>4.29699993133545</dy>
        <spacingUnit>km</spacingUnit>
        <minorAxis>6356775.0</minorAxis>
        <majorAxis>6378160.0</majorAxis>
        <lov>-67.0770034790039</lov>
        <latin1>45.3680000305176</latin1>
        <latin2>45.3680000305176</latin2>
    </lambertConformalGridCoverage>

> **Note**: Notice the `<name>201155</name>` tag was defined from the number of grid points (201 and 155). This value will be matched against an entry in our models file (below) to set the name of the model (e.g. WRF).

### Grib2 Products

Copy an existing xml file with the same grid projection type (in this case **latLonGridCoverage**) to a new file `cpti.xml`:

    cd /awips2/edex/data/utility/common_static/base/grib/grids/
    cp MRMS-1km.xml cpti.xml
  
And edit the new `cpti.xml` to define the projection values using the [output from wgrib2 or the database](#grib2-products_1) (example provided):

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
  
> **Note**: Notice the `<name>384000</name>` tag was defined from the number of grid points (600 and 640). This value will be matched against an entry in our models file (below) to set the name of the model (e.g. CPTI).

---

## Create Model Definition 

Model definition XML files are found in **/awips2/edex/data/utility/common_static/base/grib/models/**. 

### Grid Prodcuts

Since our grib file has a center ID of 7 (NCEP) we will edit the **gribModels_NCEP-7.xml** file.

    cd /awips2/edex/data/utility/common_static/base/grib/models/

    vi gribModels_NCEP-7.xml

In `<gribModelSet>` add an entry:

        <model>
            <name>WRF</name>
            <center>7</center>
            <subcenter>0</subcenter>
            <grid>201155</grid>
            <process>
                <id>89</id>
            </process>
        </model>

Save the file and restart EDEX for the changes to take effect:

    sudo service edex_camel restart ingestGrib

Now copy the `wrf.grib` file *again* to **/awips2/data_store/ingest/**.  If everything is correct we will not see any persistence errors since the grid is now named **WRF** and not **GribModel:7:0:89**.

    cp wrf.grib /awips2/data_store/ingest/

    edex log grib

After you have confirmed that the grid was ingested with the given name, you can [edit the D2D product menus to display the new grid](../cave/d2d-edit-menus.md).

### Grib2 Products

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

### Grib products

The center and subcenter have been identified previously [here](#grib-products) and [here](#grib-products_1), as 7 and 0, respectively.  So, the corresponding directory is:

    /awips2/edex/data/utility/common_static/base/grib/tables/7/0/

To find the **discipline** of a grib product, you need the **process** and **table** values from the grib file.  These are output with the `wgrib -V` command:

<pre>
wgrib -V wrf.grib  
rec 799:27785754:date 2014102318 ALBDO kpds5=84 kpds6=1 kpds7=0 levels=(0,0) grid=255 sfc 6hr fcst: bitmap: 736 undef
  ALBDO=Albedo [%]
  timerange 0 P1 6 P2 0 TimeU 1  nx 201 ny 155 GDS grid 3 num_in_ave 0 missing 0
  center 7 subcenter 0 <b>process 89 Table 2</b> scan: WE:SN winds(grid) 
  Lambert Conf: Lat1 42.283000 Lon1 -72.361000 Lov -67.077000
      Latin1 45.368000 Latin2 45.368000< LatSP 0.000000 LonSP 0.000000
      North Pole (201 x 155) Dx 4.297000 Dy 4.297000 scan 64 mode 8
  min/max data 5 21.9  num bits 8  BDS_Ref 50  DecScale 1 BinScale 0
</pre>

For our example, the process is **89** and table is **2**.  Next, take a look in:

     /awips2/edex/data/utility/common_static/base/grid/grib1ParameterConvTable.xml
     
And find the entry that has grib1 data with TableVersion 2 and Value 89:

    <grib1Parameter>
        <center>7</center>
        <grib1TableVersion>2</grib1TableVersion>
        <grib1Value>89</grib1Value>
        <grib2discipline>0</grib2discipline>
        <grib2category>3</grib2category>
        <grib2Value>10</grib2Value>
      </grib1Parameter>

Here, we can see the discipline and category values (referred to as x above) are 0 and 3, respectively.

So, the table needed for our example file is:

    /awips2/edex/data/utility/common_static/base/grib/tables/7/0/4.2.0.3.table

### Grib2 Products 

If you are using a grib2 file, then you can use either the log output or the `-center`, `-subcenter`, and `-full_name` options on `wgrib2` to get the center, subcenter, discipline, category, and parameter information:

The table would be found in the directory structure using this file's center and subcenter.  The center can be found by either:

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
...
</pre>
    Where the 4th argument after "var" is the center id, in this case **161**.

To get the subcenter, simply run:
<pre>
wgrib2 -subcenter cpti.grib2
  1:0:subcenter=<b>0</b>
...
</pre>
The subcenter of this file is **0**.

So based on the center and subcenter, the corresponding directory is:

    /awips2/edex/data/utility/common_static/base/grib/tables/161/0/

To find the exact table, we need the discipline and category:

<pre>
wgrib2 -full_name cpti.grib2
	1:0:var<b>209</b>_<b>3</b>_<b>61</b>.500_m_above_mean_sea_level
  ...
</pre>

In this case the **discipline is 209** and **category is 3**, so the corresponding table is:

    4.2.209.3.table
      
So, the full path to the corresponding table would be:

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
      
Now you can try [re-ingesting the grib2 file](#grib2-products).
      
---

## Troubleshooting Grib Ingest

* Make sure the latitude and longitude entries in your coverage specification file match those of your ingested raw grib file. There is a tolerance of +/- 0.1 degree to keep in mind when defining your coverage area.

* If some of the information is unknown, using a grib utility application such as *wgrib* and *wgrib2* can be useful in determining the information that must be added to correctly process a new grib file.

* If you are experiencing `Segmentation fault` errors when running wgrib2, it may be best to install the latest version using the following command:

        yum install wgrib2
  And then you may either need to change where `wgrib2` points to, or use `/bin/wgrib2` to run the recently downloaded version.
