---
layout: default
type: guide
shortname: Docs
title: Regular expressions
subtitle: EDEX Admin
---

AWIPS uses regular expressions for data filtering at two steps in the ingest process:

1. the [LDM](system-architecture.html#ldm) uses regular expressions to determine which data to write to **/awips2/data_store**/.

      An example for radars products defined in **/awips2/ldm/etc/pqact.conf**
        NEXRAD3        ^(SDUS[23578].) .... (......) /p(...)(...)
               FILE    -overwrite -close -edex /awips2/data_store/radar/\4/\3/\1_\4_\3_\2_(seq).rad

      The `FILE` option determines the actions on the product, in this case the name of the file (using `\n` numeration) as determined by the values captured inside of parentheses ([read more about LDM pattern actions...](http://www.unidata.ucar.edu/software/ldm/ldm-current/basics/pqact.conf.html))
      
      
2. EDEX Ingest uses regular expressions to determine routing of raw data to decoder plug-ins based on WMO header and file name ([Read more about WMO headers...](http://www.nws.noaa.gov/tg/table.html)). 

    An example for products defined in **/awips2/edex/data/utility/edex_static/base/distribution/radar.xml**
        <requestPatterns >
            <regex>^SDUS[234578]. .*</regex>
            <regex>^Level2_.*</regex>
            <regex>^Level3_.*</regex>
        </requestPatterns>

Standard LDM regular expressions from **/awips2/ldm/etc/pqact.conf** 

# Level 3 Radar (All)

    NEXRAD3 ^(SDUS[23578].) .... (......) /p(...)(...)
            FILE    -overwrite -close -edex /awips2/data_store/radar/\4/\3/\1_\4_\3_\2_(seq).rad
            
# Level 3 Radar (Subset) 

    NEXRAD3 ^(SDUS[23578].) .... (......) /p(DHR|DPR|DSP|DTA|DAA|DU3|DU6|DVL|EET|HHC|N3P|N0C|N0K|N0Q|N0S|N0U|N0X|N0Z|NCR|NMD|OHA)(...)
            FILE    -overwrite -close -edex /awips2/data_store/radar/\4/\3/\1_\4_\3_\2_(seq).rad

# FNEXRAD Composites

    FNEXRAD ^rad/NEXRCOMP/(...)/(...)_(........)_(....)
            PIPE    -close -edex -log
            pngg2gini -vl logs/pngg2gini.log
            /awips2/data_store/ingest/nexrcomp_\1_\4_\3_\2_(seq).rad
    
# Satellite Imagery
    
    # NOAAPORT GINI images
    NIMAGE  ^satz/ch[0-9]/.*/(.*)/([12][0-9])([0-9][0-9])([01][0-9])([0-3][0-9]) ([0-2][0-9])([0-5][0-9])/(.*)/(.*km)/
            FILE    -close -overwrite -edex /awips2/data_store/sat/\8/\9/\1_\2\3\4\5_\6\7
    
    # -------- GOES-East/West Northern Hemisphere Composites --------
    # GOES-East/West VIS composites
    UNIWISC ^pnga2area Q. (CV) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GEWCOMP_\5_VIS_VIS_\6_\7
    # GOES-East/West 3.9 um composites
    UNIWISC ^pnga2area Q. (CS) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GEWCOMP_\5_3.9_3.9_\6_\7
    # GOES-East/West WV composites
    UNIWISC ^pnga2area Q. (CW) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GEWCOMP_\5_WV_WV_\6_\7
    # GOES-East/West IR composites
    UNIWISC ^pnga2area Q. (CI) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GEWCOMP_\5_IR_IR_\6_\7
    # GOES-East/West 13.3 um composites
    UNIWISC ^pnga2area Q. (CL) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GEWCOMP_\5_13.3_13.3_\6_\7
    # ------------------- SSEC Global Composites -------------------
    # Global WV composite
    UNIWISC ^pnga2area Q. (GW) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GLOBAL_\5_WV_WVCOMP_\6_\7
    # Global IR composites
    UNIWISC ^pnga2area Q. (GI) (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_GLOBAL_\5_IR_IRCOMP_\6_\7
    # ----------------- Mollweide Global Composites -----------------
    # Mollweide Global Water Vapor
    UNIWISC ^pnga2area Q. (UY) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_MOLLWEIDE_30km_WV_MOLLWV_\6_\7
    # Mollweide Global IR
    UNIWISC ^pnga2area Q. (UX) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_MOLLWEIDE_30km_IR_MOLLIR_\6_\7
    # These work
    # GOES Visible (UV 4km VIS disabled)
    UNIWISC ^pnga2area Q. (EV|U9) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_\1_\3_\5_VIS_\4_\6_\7
    # GOES Water Vapor
    UNIWISC ^pnga2area Q. (UW|UB) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_\1_\3_\5_WV_\4_\6_\7
    # GOES Thermal Infrared
    UNIWISC ^pnga2area Q. (UI|U5) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_\1_\3_\5_IR_\4_\6_\7
    # GOES other
    UNIWISC ^pnga2area Q. (UD|UE|U7|U8|) (.*) (.*)_IMG (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_\1_\3_\5_\4_\6_\7
    # Arctic
    UNIWISC ^pnga2area Q. (U[LNGHO]) (.*) (.*) (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ARCTIC_4km_\4_\6_\7
    # Antarctic VIS Composite
    UNIWISC ^pnga2area Q. (UJ) (.*) (.*)_IMG (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ANTARCTIC_4km_VIS_\3_\4_\6_\7
    # Antarctic PCOL Composite
    UNIWISC ^pnga2area Q. (UK) (.*) (.*)_IMG (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ANTARCTIC_4km_PCOL_\3_\4_\6_\7
    # Antarctic WV Composite
    UNIWISC ^pnga2area Q. (UF) (.*) (.*)_IMG (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ANTARCTIC_4km_WV_\3_\4_\6_\7
    # Antarctic Composite IR
    UNIWISC ^pnga2area Q. (U1) (.*) (.*)_IMG (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ANTARCTIC_4km_IR_\3_\4_\6_\7
    # GOES Sounder Derived Image Products from University of Wisconsin CIMSS
    # CIMSS CAPE - McIDAS product code CE
    UNIWISC ^pnga2area Q0 CE .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_CAPE_\4_\5
    # CIMSS Cloud Top Pressure - McIDAS product code CA
    UNIWISC ^pnga2area Q0 CA .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_CTP_\4_\5
    # CIMSS Lifted Index - McIDAS product code CD
    UNIWISC ^pnga2area Q0 CD .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_LI_\4_\5
    # CIMSS Ozone - McIDAS product code CF
    UNIWISC ^pnga2area Q0 CF .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_OZONE_\4_\5
    # CIMSS Total Column Precipitable Water - McIDAS product code CB
    UNIWISC ^pnga2area Q0 CB .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_PW_\4_\5
    # CIMSS Sea Surface Temperature - McIDAS product code CC
    UNIWISC ^pnga2area Q0 CC .... (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_SST_\4_\5
    # CIMSS Northern Hemisphere Wildfire ABBA - McIDAS product code CG (inactive)
    UNIWISC ^pnga2area Q0 CG (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_FIRESNH_\4_\5
    # CIMSS Southern Hemisphere Wildfire ABBA - McIDAS product code CH (inactive)
    UNIWISC ^pnga2area Q0 CH (.*) (.*) (.*) (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_SOUNDER_\3_FIRESSH_\4_\5


# Gridded Model Data
  
      # GFS 0.5 deg (gfs.tCCz.pgrb2.0p50.fFFF) all hours out to F384
      CONDUIT ^data/nccf/com/.*gfs.t[0-9][0-9]z.(pgrb2.0p50).*!(grib2)/[^/]*/(SSIGFS|GFS)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*! (......)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/conduit/GFS/\5_\6Z_\7_\8-(seq).\1.grib2
      # NAM-40km (awip3d)  - exclude awip12 = NAM12 since it is on NGRID (exclude NAM 90km)
      CONDUIT ^data/nccf/com/nam/.*nam.*(awip3d).*!(grib2)/ncep/(NAM_84)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-1]..)/([^/]*)/.*! (......)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/conduit/\3/\5_\6Z_\7_\8-(seq).\1.grib2
      # DGEX
      NGRID   ^[LM].E... KWBD ...... !grib2/[^/]*/([^/]*)/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/DGEX/\1_\2_\3Z_\4_\5-(seq).grib2
      # NOAAport HRRR
      NGRID   Y.C.[0-9][0-9] KWBY ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{12})F(...)/(.*)/.*
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/HRRR/\1_F\2_\3_(seq).grib2
      # GFS40 40km
      NGRID   ^[LM].R... KWBC ...... !grib2/[^/]*/([^/]*)/#(212)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # RAP-13km
      NGRID   ^[LM].D... KWBG ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # RTMA 197 (5km)
      NGRID   ^[LM].M... KWBR ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # RTMA-Mosaic 2.5km (I) and URMA2.5 (Q)
      NGRID   ^[LM].[IQ]... KWBR ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # NamDNG 2.5 and 5km
      NGRID   ^[LM].[IM]... KWBE ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # NAM12 (#218)
      NGRID   ^[LM].B... KWBE ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/noaaport/GRID\2/\1_\3_\4Z_\5_\6-(seq).grib2
      # GEM 000  CMC_reg_USWRF_NTAT_0_ps15km_2015042818_P003.grib2
      CMC     CMC_reg_(.*)km_(..........)_P(...).grib2
              FILE    -overwrite -log -close -edex    /awips2/data_store/grib2/cmc/cmc_reg_\1km_\2_P\3.grib2
      # FNMOC
      FNMOC   ^US058.*(0018_0056|0022_0179|0027_0186|0060_0188|0063_0187|0110_0240|0111_0179|0135_0240|0078_0200)_(.*)_(.*)_(.*)-.*
              FILE    -log -overwrite -close -edex /awips2/data_store/grib2/fnmoc/US_058_\1_\2_\3_\4-(seq).grib
      
                     
