Attributes {
    NC_GLOBAL {
        String title "North American grid 221 RAP forecast starting from 00Z29jul2012, downloaded Jul 29 01:38 UTC";
        String Conventions "COARDS", "GrADS";
        String dataType "Grid";
        String history "Sun Jul 29 01:40:06 UTC 2012 : imported by GrADS Data Server 2.0";
    }
    lon {
        String grads_dim "x";
        String grads_mapping "linear";
        String grads_size "708";
        String units "degrees_east";
        String long_name "longitude";
        Float64 minimum 148.64059400000;
        Float64 maximum 357.32093157164;
        Float32 resolution 0.29516312;
    }
    lat {
        String grads_dim "y";
        String grads_mapping "linear";
        String grads_size "287";
        String units "degrees_north";
        String long_name "latitude";
        Float64 minimum 0.89755900000;
        Float64 maximum 85.30135900000;
        Float32 resolution 0.29511818;
    }
    time {
        String grads_dim "t";
        String grads_mapping "linear";
        String grads_size "19";
        String grads_min "00z29jul2012";
        String grads_step "1hr";
        String units "days since 1-1-1 00:00:0.0";
        String long_name "time";
        String minimum "00z29jul2012";
        String maximum "18z29jul2012";
        Float32 resolution 0.041666668;
    }
    lev {
        String grads_dim "z";
        String grads_mapping "levels";
        String units "millibar";
        String long_name "altitude";
        Float64 minimum 1000.00000000000;
        Float64 maximum 100.00000000000;
        Float32 resolution 25.0;
    }
    ciceprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) cloud ice [kg/m^2] ";
    }
    no4lftx180_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-0 mb above ground best (4 layer) lifted index [k] ";
    }
    acpcpsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface convective precipitation [kg/m^2] ";
    }
    bgrunsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface baseflow-groundwater runoff [kg/m^2] ";
    }
    capesfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface convective available potential energy [j/kg] ";
    }
    cape255_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 255-0 mb above ground convective available potential energy [j/kg] ";
    }
    cape180_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-0 mb above ground convective available potential energy [j/kg] ";
    }
    cfrzrsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface categorical freezing rain (yes=1; no=0) [non-dim] ";
    }
    cicehlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level cloud ice [kg/m^2] ";
    }
    cicepsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface categorical ice pellets (yes=1; no=0) [non-dim] ";
    }
    cinsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface convective inhibition [j/kg] ";
    }
    cin255_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 255-0 mb above ground convective inhibition [j/kg] ";
    }
    cin180_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-0 mb above ground convective inhibition [j/kg] ";
    }
    clwmrprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) cloud mixing ratio [kg/kg] ";
    }
    clwmrhlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level cloud mixing ratio [kg/kg] ";
    }
    crainsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface categorical rain (yes=1; no=0) [non-dim] ";
    }
    csnowsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface categorical snow (yes=1; no=0) [non-dim] ";
    }
    dpt2m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 2 m above ground dew point temperature [k] ";
    }
    dswrfsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface downward short-wave rad. flux [w/m^2] ";
    }
    epotsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface pseudo-adiabatic potential temperature (or equivalent potential temperature) [k] ";
    }
    grleprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) grauple [kg/kg] ";
    }
    gustsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface wind speed (gust) [m/s] ";
    }
    hgtsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface geopotential height [gpm] ";
    }
    hgtprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) geopotential height [gpm] ";
    }
    hgthlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level geopotential height [gpm] ";
    }
    hgtclb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** cloud base geopotential height [gpm] ";
    }
    hgttop0c {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** highest tropospheric freezing level geopotential height [gpm] ";
    }
    hgtclt {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** cloud top geopotential height [gpm] ";
    }
    hgt0c {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 0c isotherm geopotential height [gpm] ";
    }
    hlcy3000_0m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 3000 m above ground - 0 m above ground storm relative helicity [m^2/s^2] ";
    }
    hlcy1000_0m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1000 m above ground - 0 m above ground storm relative helicity [m^2/s^2] ";
    }
    hpblsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface planetary boundary layer height [m] ";
    }
    icecsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface ice cover [proportion] ";
    }
    landsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface land cover (1=land, 0=sea) [proportion] ";
    }
    lftxl100_100 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 500-1000 mb surface lifted index [k] ";
    }
    ltngsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface lightning [non-dim] ";
    }
    mslmamsl {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** mean sea level mslp (maps system reduction) [pa] ";
    }
    mstavsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface moisture availability [%] ";
    }
    ncpcpsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface large-scale precipitation (non-convective) [kg/m^2] ";
    }
    plpl255_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 255-0 mb above ground pressure of level from which parcel was lifted [pa] ";
    }
    pratesfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface precipitation rate [kg/m^2/s] ";
    }
    pressfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface pressure [pa] ";
    }
    pres80m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 80 m above ground pressure [pa] ";
    }
    preshlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level pressure [pa] ";
    }
    pres0c {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 0c isotherm pressure [pa] ";
    }
    presmwl {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** max wind pressure [pa] ";
    }
    prestrop {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** tropopause pressure [pa] ";
    }
    pwatclm {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** entire atmosphere (considered as a single layer) precipitable water [kg/m^2] ";
    }
    refcclm {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** entire atmosphere (considered as a single layer) maximum/composite radar reflectivity [db] ";
    }
    rhprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) relative humidity [%] ";
    }
    rh2m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 2 m above ground relative humidity [%] ";
    }
    rh30_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 30-0 mb above ground relative humidity [%] ";
    }
    rh60_30mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 60-30 mb above ground relative humidity [%] ";
    }
    rh90_60mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 90-60 mb above ground relative humidity [%] ";
    }
    rh120_90mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 120-90 mb above ground relative humidity [%] ";
    }
    rh150_120mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 150-120 mb above ground relative humidity [%] ";
    }
    rh180_150mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-150 mb above ground relative humidity [%] ";
    }
    rhtop0c {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** highest tropospheric freezing level relative humidity [%] ";
    }
    rh0c {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 0c isotherm relative humidity [%] ";
    }
    rwmrprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) rain mixing ratio [kg/kg] ";
    }
    snmrprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) snow mixing ratio [kg/kg] ";
    }
    snodsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface snow depth [m] ";
    }
    soilm0_200cm {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 0-2 m below ground soil moisture content [kg/m^2] ";
    }
    soilwsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface volumetric soil moisture content [fraction] ";
    }
    soilwl106 {
        Float32 _FillValue 9.999E20, 9.999E20, 9.999E20, 9.999E20, 9.999E20;
        Float32 missing_value 9.999E20, 9.999E20, 9.999E20, 9.999E20, 9.999E20;
        String long_name "** 0.05 m underground volumetric soil moisture content [fraction] ", "** 0.2 m underground volumetric soil moisture content [fraction] ", "** 0.4 m underground volumetric soil moisture content [fraction] ", "** 1.6 m underground volumetric soil moisture content [fraction] ", "** 3 m underground volumetric soil moisture content [fraction] ";
    }
    sotypsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface soil type [-] ";
    }
    spfh2m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 2 m above ground specific humidity [kg/kg] ";
    }
    spfh80m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 80 m above ground specific humidity [kg/kg] ";
    }
    spfhhlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level specific humidity [kg/kg] ";
    }
    ssrunsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface storm surface runoff [kg/m^2] ";
    }
    tkeprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) turbulent kinetic energy [j/kg] ";
    }
    tmpsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface temperature [k] ";
    }
    tmpprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) temperature [k] ";
    }
    tmp2m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 2 m above ground temperature [k] ";
    }
    tmp80m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 80 m above ground temperature [k] ";
    }
    tmphlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level temperature [k] ";
    }
    tmp30_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 30-0 mb above ground temperature [k] ";
    }
    tmp60_30mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 60-30 mb above ground temperature [k] ";
    }
    tmp90_60mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 90-60 mb above ground temperature [k] ";
    }
    tmp120_90mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 120-90 mb above ground temperature [k] ";
    }
    tmp150_120mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 150-120 mb above ground temperature [k] ";
    }
    tmp180_150mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-150 mb above ground temperature [k] ";
    }
    tmptrop {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** tropopause temperature [k] ";
    }
    tsoilsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface soil temperature [k] ";
    }
    tsoill106 {
        Float32 _FillValue 9.999E20, 9.999E20, 9.999E20, 9.999E20, 9.999E20;
        Float32 missing_value 9.999E20, 9.999E20, 9.999E20, 9.999E20, 9.999E20;
        String long_name "** 0.05 m underground soil temperature [k] ", "** 0.2 m underground soil temperature [k] ", "** 0.4 m underground soil temperature [k] ", "** 1.6 m underground soil temperature [k] ", "** 3 m underground soil temperature [k] ";
    }
    ugrdprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) u-component of wind [m/s] ";
    }
    ugrd10m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 10 m above ground u-component of wind [m/s] ";
    }
    ugrd80m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 80 m above ground u-component of wind [m/s] ";
    }
    ugrdhlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level u-component of wind [m/s] ";
    }
    ugrd30_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 30-0 mb above ground u-component of wind [m/s] ";
    }
    ugrd60_30mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 60-30 mb above ground u-component of wind [m/s] ";
    }
    ugrd90_60mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 90-60 mb above ground u-component of wind [m/s] ";
    }
    ugrd120_90mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 120-90 mb above ground u-component of wind [m/s] ";
    }
    ugrd150_120mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 150-120 mb above ground u-component of wind [m/s] ";
    }
    ugrd180_150mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-150 mb above ground u-component of wind [m/s] ";
    }
    ugrdmwl {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** max wind u-component of wind [m/s] ";
    }
    ugrdtrop {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** tropopause u-component of wind [m/s] ";
    }
    ustm6000_0m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 6000 m above ground - 0 m above ground u-component storm motion [m/s] ";
    }
    vgrdprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) v-component of wind [m/s] ";
    }
    vgrd10m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 10 m above ground v-component of wind [m/s] ";
    }
    vgrd80m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 80 m above ground v-component of wind [m/s] ";
    }
    vgrdhlev1 {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 1 hybrid level v-component of wind [m/s] ";
    }
    vgrd30_0mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 30-0 mb above ground v-component of wind [m/s] ";
    }
    vgrd60_30mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 60-30 mb above ground v-component of wind [m/s] ";
    }
    vgrd90_60mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 90-60 mb above ground v-component of wind [m/s] ";
    }
    vgrd120_90mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 120-90 mb above ground v-component of wind [m/s] ";
    }
    vgrd150_120mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 150-120 mb above ground v-component of wind [m/s] ";
    }
    vgrd180_150mb {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 180-150 mb above ground v-component of wind [m/s] ";
    }
    vgrdmwl {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** max wind v-component of wind [m/s] ";
    }
    vgrdtrop {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** tropopause v-component of wind [m/s] ";
    }
    vgtypsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface vegetation type [integer(0-13)] ";
    }
    vissfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface visibility [m] ";
    }
    vstm6000_0m {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** 6000 m above ground - 0 m above ground v-component storm motion [m/s] ";
    }
    vvelprs {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** (1000 975 950 925 900.. 200 175 150 125 100) vertical velocity (pressure) [pa/s] ";
    }
    weasdaccsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface water equivalent of accumulated snow depth [kg/m^2] ";
    }
    weasdsfc {
        Float32 _FillValue 9.999E20;
        Float32 missing_value 9.999E20;
        String long_name "** surface water equivalent of accumulated snow depth [kg/m^2] ";
    }
}
