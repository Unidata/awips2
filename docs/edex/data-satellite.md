
# Satellite Imagery
    
## NOAAport GINI Images

    NIMAGE  ^satz/ch[0-9]/.*/(.*)/([12][0-9])([0-9][0-9])([01][0-9])([0-3][0-9]) ([0-2][0-9])([0-5][0-9])/(.*)/(.*km)/
            FILE    -close -overwrite -edex /awips2/data_store/sat/\8/\9/\1_\2\3\4\5_\6\7
    
## UNIWISC GOES-East/West Northern Hemisphere Composites 

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
            
## 20km Rectilinear Global Composites 

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
            
## 30km Mollweide Global Composites 

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
            
## Arctic Composite Imagery

    UNIWISC ^pnga2area Q. (U[LNGHO]) (.*) (.*) (.*)um (.*) (........) (....)
            PIPE    -close -log
            pnga2area -vl logs/pnga2area.log
            /awips2/data_store/ingest/uniwisc_ARCTIC_4km_\4_\6_\7

## Antarctic Composite Imagery

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
            
## GOES Sounder Derived Imagery

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
