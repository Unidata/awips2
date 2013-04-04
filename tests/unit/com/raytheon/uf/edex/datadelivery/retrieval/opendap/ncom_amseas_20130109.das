Attributes {
    NC_GLOBAL {
        String title "NCOM Regional American Seas for 00Z09jan2013 Forecast, downloaded Jan 09 15:14 UTC";
        String Conventions "COARDS", "GrADS";
        String dataType "Grid";
        String history "Wed Jan 09 15:50:57 UTC 2013 : imported by GrADS Data Server 2.0";
    }
    lon {
        String grads_dim "x";
        String grads_mapping "linear";
        String grads_size "1510";
        String units "degrees_east";
        String long_name "longitude";
        Float64 minimum 262.00000000000;
        Float64 maximum 305.00650000000;
        Float32 resolution 0.0285;
    }
    lat {
        String grads_dim "y";
        String grads_mapping "linear";
        String grads_size "1000";
        String units "degrees_north";
        String long_name "latitude";
        Float64 minimum 5.00000000000;
        Float64 maximum 31.97300000000;
        Float32 resolution 0.027;
    }
    time {
        String grads_dim "t";
        String grads_mapping "linear";
        String grads_size "33";
        String grads_min "00z09jan2013";
        String grads_step "3hr";
        String units "days since 1-1-1 00:00:0.0";
        String long_name "time";
        String minimum "00z09jan2013";
        String maximum "00z13jan2013";
        Float32 resolution 0.125;
    }
    lev {
        String grads_dim "z";
        String grads_mapping "levels";
        String units "millibar";
        String long_name "altitude";
        Float64 minimum 0.00000000000;
        Float64 maximum 5000.00000000000;
        Float32 resolution 128.20512;
    }
    water_u {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "eastward water velocity [meter/sec] ";
    }
    tau {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "hours since analysis ";
    }
    surf_atm_press {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "surface atmospherics pressure [pascals] ";
    }
    surf_temp_flux {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "model surface temperature flux [degc-m/s] ";
    }
    surf_salt_flux {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "model surface salinity flux [psu-m/s] ";
    }
    surf_solar_flux {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "model surface shortwave flux [degc-m/s] ";
    }
    surf_roughness {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "model surface roughness [m] ";
    }
    swsgx {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "grid x surface wind stress [newton/meter2] ";
    }
    swsgy {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "grid y surface wind stress [newton/meter2] ";
    }
    surf_el {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "water surface elevation [m] ";
    }
    water_v {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "northward water velocity [meter/sec] ";
    }
    water_temp {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "water temperature [degc] ";
    }
    salinity {
        Float32 _FillValue -9.99E8;
        Float32 missing_value -9.99E8;
        String long_name "salinity [psu] ";
    }
}
