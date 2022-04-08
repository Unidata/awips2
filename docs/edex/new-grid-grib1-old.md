# Ingest a New Grid Using .grib Files

Unrecognized grids can be decoded by EDEX simply by dropping `*.grib` or `*.grib2` files into `/awips2/data_store/ingest/`

To add support for a new grid, two edits must be made:

* **Geospatial projection** must be defined in a [***grid navigation file***](#create-grid-projection-file)
* **Grid name**, **center**, **subcenter**, and **process ID** must be defined in a [***model definition file***](#create-model-definition)

If the parameters in the grib file haven't been previously specified, another change *may* be needed as well:

* **Center**, **subcenter**, **discipline**, **category**, and possibly **parameter ID** information may need to be defined in a [***table***](#adding-a-table)
 
---

## Ingest an Unsupported Grid

### Download Test Data

Download an example grib1 file and rename to a `*.grib` extension, then copy to the manual ingest point `/awips2/data_store/ingest/` 

    wget https://downloads.unidata.ucar.edu/awips2/current/files/14102318_nmm_d01.GrbF00600 -O wrf.grib

    cp wrf.grib /awips2/data_store/ingest/
    
Remember that the data distribution file (`/awips2/edex/data/utility/common_static/base/distribution/grib.xml`) will match filenames which have the `*.grib*` extension.

### Check Grib Logs

Confirm that the grib file decodes in the grib log file:
    
    edex log grib

    INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.1200 (sec) Latency: 21.8080 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.1180 (sec) Latency: 21.8140 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.4230 (sec) Latency: 21.8360 (sec)
    INFO [Ingest.GribDecode] /awips2/data_store/ingest/grib/20141026/14/wrf.grib processed in: 0.2240 (sec) Latency: 21.9140 (sec)
    
    ...

### Check HDF5 Data

Check that the hdf5 data directory exists for our unnamed grid

    ls -latr /awips2/edex/data/hdf5/grid/GribModel:7:0:89

Though the grib file has been decoded, it has been given a generic name with its center, subcenter, and process IDs (7, 0, 89, respectively). 
    
---

## Determine Grid Projection

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

Notice that our grib file has a **Lambert Conformal** projection.  We will need these values for the next step.

!!! note  "**There is a tolerance of +/- 0.1 degrees** to keep in mind when defining your coverage area."

---

## Create Grid Projection File

### Projection Types
  
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

!!! note "Notice the `<name>201155</name>` tag was created by using the number of grid points (201 and 155). This name can be anything as long as it is unique and will be used to match against in the model definition."

---

## Create Model Definition 

Model definition XML files are found in **/awips2/edex/data/utility/common_static/base/grib/models/**. 

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

---

## Adding a Table

If you ingest a piece of data and the parameter appears as unknown in the metadata database, ensure that the correct parameter tables are in place for the center/subcenter.

The tables are located in **/awips2/edex/data/utility/common_static/base/grib/tables/**.  They are then broken into subdirectories using the following structure: **/[Center]/[Subcenter]/4.2.[Discipine].[Category].table**. 

The center and subcenter have been identified previously [here](#determine-grid-projection), as 7 and 0, respectively.  So, the corresponding directory is:

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
