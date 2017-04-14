---
layout: default
type: guide
shortname: Docs
title: Add a New Grid to EDEX
---

Unrecognized grids can be decoded by EDEX simply by dropping `*.grib` or `*.grib2` files into `/awips2/data_store/ingest/`

To add support for a new grid, two edits must be made:

* **Geospatial projection** must be defined in a *grid navigation file*
* **Grid name**, **center**, **subcenter**, and **process ID** must be defined in a *model definition file*.
 

# Ingest an Unsupported Grid

1. Download an example grib1 file and rename to a `*.grib` extension, then copy to the manual ingest point `/awips2/data_store/ingest/` 

        wget http://www.unidata.ucar.edu/staff/mjames/14102318_nmm_d01.GrbF00600 -O wrf.grib
    
        cp wrf.grib /awips2/data_store/ingest/
    
    Remember that the data distribution file (`/awips2/edex/data/utility/edex_static/base/distribution/grib.xml`) will match filename which have the `*.grib` extension.

2. Confirm that the grib file decodes in the grib log file:
    
        edex log grib
    
        INFO [Ingest.GribDecode] /awips2/data_store/manual/grib/20141026/14/wrf.grib processed in: 0.1200 (sec) Latency: 21.8080 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/manual/grib/20141026/14/wrf.grib processed in: 0.1180 (sec) Latency: 21.8140 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/manual/grib/20141026/14/wrf.grib processed in: 0.4230 (sec) Latency: 21.8360 (sec)
        INFO [Ingest.GribDecode] /awips2/data_store/manual/grib/20141026/14/wrf.grib processed in: 0.2240 (sec) Latency: 21.9140 (sec)
        
        ...

3. Check that the hdf5 data directory exists for our unnamed grid

        ls -latr /awips2/edex/data/hdf5/grid/GribModel:7:0:89

    Though the grib file has been decoded, it has been given a generic name with its center, subcenter, and process IDs (7, 0, 89, respectively). 

# Determine Grid Projection

When the grid was ingested a record was added to the `grid_coverage` table with its navigation information:

    psql metadata
    
    metadata=# select nx,ny,dx,dy,majoraxis,minoraxis,la1,lo1,lov,latin1,latin2 from gridcoverage where id=(select distinct(location_id) from grid_info where datasetid='GribModel:7:0:89');
     nx  | ny  |        dx        |        dy        | majoraxis | minoraxis |       la1        |        lo1        |        lov        |      latin1      |      latin2      
    -----+-----+------------------+------------------+-----------+-----------+------------------+-------------------+-------------------+------------------+------------------
     201 | 155 | 4.29699993133545 | 4.29699993133545 |   6378160 |   6356775 | 42.2830009460449 | -72.3610000610352 | -67.0770034790039 | 45.3680000305176 | 45.3680000305176
    (1 row)

Compare with the projection info returned by wgrib on the original file:

    wgrib -V wrf.grib
    
    rec 799:27785754:date 2014102318 ALBDO kpds5=84 kpds6=1 kpds7=0 levels=(0,0) grid=255 sfc 6hr fcst: bitmap: 736 undef
      ALBDO=Albedo [%]
      timerange 0 P1 6 P2 0 TimeU 1  nx 201 ny 155 GDS grid 3 num_in_ave 0 missing 0
      center 7 subcenter 0 process 89 Table 2 scan: WE:SN winds(grid) 
      Lambert Conf: Lat1 42.283000 Lon1 -72.361000 Lov -67.077000
          Latin1 45.368000 Latin2 45.368000 LatSP 0.000000 LonSP 0.000000
          North Pole (201 x 155) Dx 4.297000 Dy 4.297000 scan 64 mode 8
      min/max data 5 21.9  num bits 8  BDS_Ref 50  DecScale 1 BinScale 0

Notice that our grib1 file is a **Lambert Conformal** projection.  We will need these values for the next step. Note that **there is a tolerance of +/- 0.1 degrees** to keep in mind when defining your coverage area.

# Create Grid Projection File
    
Grid projection files are stored in `/awips2/edex/data/utility/edex_static/base/grib/grids/` and there are four grid coverage types available:

1. **lambertConformalGridCoverage** example

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

2. **polarStereoGridCoverage** example
 
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

3. **latLonGridCoverage** example
    
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
    
4. **mercatorGridCoverage** example

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
    
Copy an existing file file with the same grid projection type (in this case **lambertConformalGridCoverage**) to a new file `wrf.xml`

    cd /awips2/edex/data/utility/edex_static/base/grib/grids/
    cp RUCIcing.xml wrf.xml

And edit the new `wrf.xml` to define the projection values (example provided):

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

> Notice `<name>201155</name>` defined from the number of grid points (201 x 155). This value will be matched against an entry in our models file (below) to set the name of the model (e.g. WRF).

# Create Model Definition 

Model definition XML files are found in **/awips2/edex/data/utility/edex_static/base/grid/models/**. Since our grib1 file has a center ID of 7 (NCEP) we will edit the **gribModels_NCEP-7.xml** file.

    cd /awips2/edex/data/utility/edex_static/base/grib/models/

    vi gribModels_NCEP-7.xml

in `<gribModelSet>` add an entry

        <model>
            <name>WRF</name>
            <center>7</center>
            <subcenter>0</subcenter>
            <grid>201155</grid>
            <process>
                <id>89</id>
            </process>
        </model>

save the file and restart EDEX for the changes to take effect.

    sudo service edex_camel restart

Now copy the `wrf.grib` file *again* to **/awips2/data_store/ingest/**.  If everything is correct we will not see any persistence errors since the grid is now named **WRF** and not **GribModel:7:0:89**.

    cp wrd.grib /awips2/data_store/ingest/

    edex log grib

After you have confirmed that the grid was ingested with the given name, you can [edit the D2D product menus to display the new grid](../cave/d2d-edit-menus.html).

# Troubleshooting Grib Ingest

If you ingest a piece of data and the parameter appears as unknown in the metadata database, ensure that the correct parameter tables are in place for the center/subcenter.

Make sure the latitude and longitude entries in your coverage specification file match those of your ingested raw grib file. There is a tolerance of +/- 0.1 degree to keep in mind when defining your coverage area.

If some of the information is unknown, using a grib utility application such as *wgrib* and *wgrib2* (not delivered) can be useful in determining the information that must be added to correctly process a new grib file.
