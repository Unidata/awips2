---
layout: default
type: guide
shortname: Docs
title: Gridded Data
subtitle: Data Types
---


# Available IDD Grids

The file `/awips2/ldm/etc/pqact.conf` defines which grids the LDM will request for EDEX ingest.  After editing this file (as user `awips`) you should run `ldmadmin pqactHUP` to re-read the new edits (`ldmadmin restart` will also work).



## DGEX

### DGEX CONUS 12km

    NGRID   ^[LM].E... KWBD ...... !grib2/[^/]*/([^/]*)/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/DGEX/DGEX_CONUS_12km_\1_\2_\3Z_\4_\5-(seq).grib2

### AK-DGEX Alaska 12km

    NGRID   ^[LM].F... KWBD ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-DGEX/DGEX_Alaska_12km_\1_\2Z_\3_\4-(seq).grib2
        
## GFS



### GFS Global 0.25 degree

    CONDUIT ^data/nccf/com/.*gfs.t[0-9][0-9]z.pgrb2.0p25.*!grib2/[^/]*.*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/GFS0p25/GFS_Global_0p25deg_\1\2\3_\4_\5-(seq).grib2
            
### GFS Global 1.0 degree
    
    CONDUIT        ^data/nccf/com/.*gfs.t[0-9][0-9]z.pgrb2.1p00.*!grib2/[^/]*/.*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*! (......)
           FILE    -edex -log
           /awips2/data_store/grid/GFS1p0/GFS_Global_onedeg_\1\2\3_\4_\5-(seq).grib2

### GFS Global 2.5 degree
    
    CONDUIT        ^data/nccf/com/.*gfs.t[0-9][0-9]z.pgrb2.2p50.*!grib2/[^/]*/.*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*! (......)
           FILE    -edex -log
           /awips2/data_store/grid/GFS2p5/GFS_Global_2p50deg_\1\2\3_\4_\5-(seq).grib2

### GFS Global 1.0 degree (NOAAPORT)

    NGRID  ^[YZ].P... KWBC ...... !grib2/ncep/GFS.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/GFS1p0_noaaport/GFS_Global_onedeg_noaaport_\1_\2_\3_\4.grib2

### GFS Pacific 40 km
    
    NGRID   ^[LM].O... KWBC ...... !grib2.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/GFSPacific-40km/GFSPacific-40km_\1_\2_\3_\4-(seq).grib2

### GFS Pacific 20 km Mercator

    NGRID   ^[YZ].F... KWBC ...... !grib2.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/GFSPacific-20km/GFSPacific-20km_\1_\2_\3_\4-(seq).grib2
    
    
### GFS Puerto Rico 0.5 degree
    
    NGRID   ^[LM].T... KWBC ...... !grib2.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/PR-GFS0p5/PR-GFS0p5_\1_\2_\3_\4-(seq).grib2
    
### GFS Puerto Rico 20 km Lat/Lon 0.25 degree
    
    NGRID   ^[YZ].E... KWBC ...... !grib2.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/PR-GFS0p25/PR-GFS0p25_\1_\2_\3_\4-(seq).grib2
    
### GFS CONUS 80 km
    
    HDS     ^[YZ].Q... KWBC ...... !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/GFS80/GFS80_\1_\2_\3_\4-(seq).grib2
    
### GFS Alaska 95 km (GFS95)
    
    NGRID   ^[LM].H... KWBC ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-GFS95/AK-GFS95\1_\2Z_\3_\4-(seq).grib2
    
### GFS CONUS 20 km (GFS20)
    
    NGRID   ^[YZ].N... KWBC ...... !grib.*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/GFS20/GFS20_\1_\2_\3_\4-(seq).grib2
    
### GFS Alaska 20 km (AK-GFS22)
    
    NGRID   ^[YZ].B... KWBC ...... !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-GFS20/AK-GFS20_\1_\2_\3_\4-(seq).grib2

### GFSGuide

    NGRID   ^[LM].I... KWBJ ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/GFSGuide/GFSGuide_\1_\3_\4Z_\5_\6-(seq).grib2
            
            
## RTMA and URMA
         
### RTMA 197 (5km)
    
    NGRID   ^[LM].M... KWBR ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/RTMA5/RTMA_5km_\1_\3_\4Z_\5_\6-(seq).grib2
    
### RTMA-Mosaic 2.5km (I)
    
    NGRID   ^[LM].I... KWBR ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/RTMA/RTMA_2p5km_\1_\3_\4Z_\5_\6-(seq).grib2
    
### URMA2.5 (Q)
    
    NGRID   ^[LM].Q... KWBR ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/URMA25/URMA_2p5km_\1_\3_\4Z_\5_\6-(seq).grib2

## NAM

### NAM CONUS 12 km (NAM12) - NOAAport
    
    NGRID  ^[LM].B... KWBE ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/NAM12/noaaport/NAM_CONUS_12km_noaaport_\1_\3_\4Z_\5_\6-(seq).grib2

### NAM Alaska 11 km (AK-NAM11)
    
    NGRID   ^[LM].S... KWBE ...... !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NAM11/NAM_Alaska_11km_\1_\2_\3_\4-(seq).grib2
    
### NAM Alaska 45 km GRID216 - CONDUIT (AK-NAM45)
    
    CONDUIT ^data/nccf/com/nam/prod/nam.*t(..)z.awipak.* !grib2/ncep/NAM_84/#000/(............)(F...)/(.*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/AK-NAM45/conduit/NAM_Alaska_45km_conduit_\2_\3_\4_\5-(seq).grib2
    
### NAM CONUS 12 km (NAM12) - CONDUIT
    
    CONDUIT ^data/nccf/com/nam/.*nam.*awip12.*!grib2/ncep/NAM_84/#[^/]*/([0-9]{8})([0-9]{4})(F[0-1]..)/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/NAM12/conduit/NAM_CONUS_12km_conduit_\1_\2Z_\3_\4-(seq).\5.grib2
    
### NAM CONUS 40 km (NAM40) - CONDUIT
    
    CONDUIT ^data/nccf/com/nam/.*awip3d.*!grib2/ncep/NAM_84/#[^/]*/([0-9]{8})([0-9]{4})(F[0-1]..)/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/NAM40/conduit/NAM_CONUS_40km_conduit_\1_\2Z_\3_\4-(seq).\5.grib2
    
### NAM CONUS 40 km (NAM40) - NOAAport
    
    HDS    ^[YZ].[A-WYZ].{1,3} KWBD ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/NAM40/noaaport/NAM_CONUS_40km_noaaport_\1\2_\3_\4-(seq).grib
    
### NAM Alaska 95 km (AK-NAM95)
    
    HDS     ^[YZ].N... KWBE .......*/m(ETA|NAM) !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NAM95/noaaport/NAM_Alaska_95km_\2_\3_\4_\5-(seq).grib2
    
### NAM CONUS 80 km (NAM80)
    
    HDS     ^[YZ].Q... KWB. [0-3][0-9][0-2][0-9].*/m(ETA|NAM) !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/NAM80/NAM_CONUS_80km_\1_\2Z_\3_\4-(seq).grib2
    
### NAM CONUS 20 km (NAM20) (removed??)
    
    HDS     ^[YZ].U... KWB. [0-3][0-9][0-2][0-9].*/m(ETA|NAM) !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/NAM20/NAM_CONUS_20km_\2_\3Z_\4_\5-(seq).\1.grib2
            
### NAM Alaska 45 km GRID216 - NOAAport (AK-NAM45)
    
    HDS     ^[YZ].V... KWB. .......*/m(ETA|NAM) !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NAM45/noaaport/NAM_Alaska_45km_noaaport_\2_\3_\4_\5-(seq).grib2
    
### NAM Alaska 22 km (AK-NAM22)
    
    HDS     ^[YZ].Y... KWBE .......*/m(ETA|NAM) !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NAM22/NAM_Alaska_22km_\2_\3_\4_\5-(seq).grib2
    
### NAM Puerto Rico Grid 237 (PR-NAM)
    
    HDS     ^[YZ].Z.{1,3} KWBE .......*/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/PR-NAM12/GRID\8/Regional_NAM_GRID\8_\2_\3_\4_\5-(seq).grib
    
### NAM Polar 90 km
    
    CONDUIT ^data/nccf/com/nam/prod/nam........./nam.t..z.grbgrd.*NAM_84/#[^/]*/([0-9]{8})([0-9]{4})(F[0-1]..)/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/NAM90/NAM_Polar_90km_\1_\2Z_\3_\4.\5.grib2
    
### NAM Fire Weather Nest
    
    CONDUIT        ^data/nccf/com/nam/prod/nam........./nam.t..z.firewxnest.*NMM_89/#[^/]*/([0-9]{8})([0-9]{4})(F[0-1]..)/([^/]*)/.*! (......)
           FILE    -edex -log
           /awips2/data_store/grid/NAMFirewxnest/NAM_Firewxnest_\1_\2Z_\3_\4.\5.grib2
    
### NamDNG 2.5 km NGRID (NamDNG)
    
    NGRID   ^[LM].I... KWBE ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/NamDNG/ngrid/NamDNG_2p5km_\1_\3_\4Z_\5_\6-(seq).grib2
    
### NamDNG 5 km (NamDNG5)
    
    NGRID  ^[LM].M... KWBE ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/NamDNG5/NamDNG_5km_\1_\3_\4Z_\5_\6-(seq).grib2
    
### AK-NamDNG5
    
    HDS     ^[LM].A.{1,3} KWB. .......*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NamDNG5/AK-NamDNG5_\1_\2\3\4-(seq).grib2
    
### PR-NamDNG5
    
    HDS     ^[LM].C.{1,3} KWB. .......*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/PR-NamDNG5/PR-NamDNG5_\1_\2\3\4-(seq).grib2
    
### Hawaii-NamDNG5
    
    HDS     ^[LM].H.{1,3} KWB. .......*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/Hawaii-NamDNG5/Hawaii-NamDNG5_\1_\2\3\4-(seq).grib2
    
### AK NamDNG 3km
    
    HDS     ^[LM].K.{1,3} KWB. .......*/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AK-NamDNG-3km/AK-NamDNG-3km_\1_\2\3\4.grib2



## RAP
    
### RAP CONUS 13 km (RAP13)
    
    NGRID   ^[LM].D... KWBG ...... !grib2/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/RAP13/RR_CONUS_13km_\3_\4Z_\5_\6-(seq).grib2
    
### RAP CONUS 20 km (RAP20)
    
    CONDUIT ^data/nccf/com/rap.*awp252.*!grib2/ncep/RUC2/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/RAP20/RR_CONUS_20km_\1_\2Z_\3_\4-(seq).\5.grib2
    
### RAP CONUS 40 km (RAP40) - NOAAport
    
    HDS    ^[YZ].W.{1,3} KWBG ......[^!]*!grib/ncep/RUC2/#236/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/RAP40/noaaport/RR_CONUS_40km_noaaport_\1_\2Z_\3_\4-(seq).grib
    
### RAP CONUS 40 km (RAP40) - CONDIUT

    CONDUIT ^data/nccf/com/rap.*awp236.*!grib2/ncep/RUC2/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/RAP40/conduit/RR_CONUS_40km_conduit_\1_\2Z_\3_\4-(seq).\5.grib2

## HRRR

### HRRR - NOAAport 1hr

    NGRID   Y.C.[0-9][0-9] KWBY ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{12})F(...)/(.*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_\1_F\2_\3-(seq).grib2

### HRRRX - Experimental GSD Hourly and Sub-Hourly
    
    FSL2    ^GRIB2\.FSL\.HRRR\.(.......)_Lambert\.(.*)(Minute|Hour)\.(.*)\.(.*)\.([0-9]{12}).*
            FILE    -edex -log
            /awips2/data_store/grid/HRRRX/HRRRX_CONUS_3km_\6-\1_\3\2_\4_\5-(seq).grib2
    
## SHEF

### SREF CONUS 40 km Ensemble Derived Products
    
    NGRID  ^[LM].R... KWBL ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{12})F(...)/(.*)/.*
           FILE    -edex -log
           /awips2/data_store/grid/SREF40/noaaport/SREF_CONUS_40km_ensprod_(\1:yyyy)(\1:mm)\1_\2.grib2
    
### SREF CONUS 40 km Bias Corrected Ensemble Derived Products
    
    CONDUIT        ^data/nccf/com/sref/prod/sref\........./../(ensprod_biasc)/.*pgrb212.*!grib2/ncep/.*/#000/(............)(F...)/(.*)/.*! (......)
           FILE    -edex -log
           /awips2/data_store/grid/SREF40/conduit/SREF_CONUS_40km_\1_\2_\3_\4_\5-(seq).grib2
    
### SREF CONUS 40 km Bias Corrected Ensemble Members
    
    CONDUIT        ^data/nccf/com/sref/prod/sref\.(........)/(..)/(pgrb_biasc)/.*pgrb212.*!grib2/ncep/.*/#000/............(F...)/(.*)/.*! (......)
           FILE    -edex -log
           /awips2/data_store/grid/SREF40/conduit/SREF_CONUS_40km_\3_\1_\200_\4_\5_\6.grib2
    
### SREF Alaska 45 km Ensemble Derived Products
    
    NGRID  ^[LM].V... KWBL .......*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/AK-SREF45/SREF_Alaska_45km_ensprod_\1\2_\3_\4-(seq).grib2
    
### SREF Pacific Northeast 0.4 degree Ensemble Derived Products
    
    NGRID  ^[LM].X... KWBL .......*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
           FILE    -edex -log
           /awips2/data_store/grid/EPac-SREF/SREF_PacificNE_0p4_ensprod_\1\2_\3_\4-(seq).grib2
    
## FNMOC

### Navy Coupled Ocean Data Assimilation (NCODA) - Global 0.25 degree

    FNMOC   ^US058.*0078_0200_(.*)_(.*)_(.*)-(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-NCODA/fnmoc_NCODA_Global_Ocean_\1_\2_\3_\4.grib

### NAVy Global Environmental Model (NAVGEM) - 0.5 degree

    FNMOC   ^US058.*0018_0056_(.*)_(.*)_(.*)-(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-NAVGEM/fnmoc_NAVGEM_Global_0p5deg_\1_\2_\3_\4.grib

### Forecast of Aerosol Radiative Optical Properties (FAROP) - Global 1.0 degree

    FNMOC   ^US058.*0135_0240_(.*)_(.*)_(.*)-(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-FAROP/fnmoc_FAROP_Global_1p0deg_\1_\2_\3_\4.grib

### WAVEWATCH III (WW3) - Global 1.0 degree

    FNMOC   ^US058.*0110_0240_(.*)_(.*)_(.*)-(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-WW3-Global_1p0deg/fnmoc_WW3_Global_1p0deg_\1_\2_\3_\4.grib

### WW3 Europe
    
    FNMOC   ^US058.{4}-GR1dyn\.WW3-EURO_EURO-..-.._.{5}.{4}(.{4})(..)(..)(..)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-WW3-Europe/fnmoc_WW3_Europe_\1\2\3_\400_\5.grib

### Coupled Ocean/Atmospheric Mesoscale Prediction System (COAMPS) - Western Atlantic
    
    FNMOC   ^US058.{4}-GR1dyn\.COAMPS-NWATL_.{5}-.{2}-.{2}_(.{9})(..........)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-COAMPS-Western_Atlantic/fnmoc_COAMPS_Western_Atlantic_\1\2\3-(seq).grib
            
### COAMPS Europe

    FNMOC   ^US058.{4}-GR1dyn\.COAMPS-EURO_.{4}-.{2}-.{2}_(.{9})(..........)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-COAMPS-Europe/fnmoc_COAMPS_Europe_\1\2\3-(seq).grib

### COAMPS Equatorial America

    FNMOC   ^US058.{4}-GR1dyn\.COAMPS-EQAM_.{4}-.{2}-.{2}_(.{9})(..........)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-COAMPS-Equatorial_America/fnmoc_COAMPS_Equatorial_America_\1\2\3-(seq).grib

### COAMPS Northeast Pacific

    FNMOC   ^US058.{4}-GR1dyn\.COAMPS-NEPAC_.{5}-.{2}-.{2}_(.{9})(..........)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-COAMPS-Northeast_Pacific/fnmoc_COAMPS_Northeast_Pacific_\1\2\3-(seq).grib

### COAMPS Southern California

    FNMOC   ^US058.{4}-GR1dyn\.COAMPS-SOCAL_.{5}-.{2}-.{2}_(.{9})(..........)(.*)
            FILE    -edex -log
            /awips2/data_store/grid/FNMOC-COAMPS-Southern_California/fnmoc_COAMPS_Southern_California_\1\2\3-(seq).grib


## Multi-Radar Multi-Sensor (MRMS) - NOAAport

### Full Feed
    
    NGRID  ^YAU[CDLMPQS].. KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
           FILE    -edex -log
           /awips2/data_store/grid/MRMS/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Precipitation Products
    
    NGRID   ^YAU[DP].. KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-precip/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Model Parameters (on different grid)
    
    NGRID   ^YAUM.. KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-model/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Lightning Products from NLDN
    
    NGRID   ^YAUL.. KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-lightning/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Rotation Track Products (on different grid)
    
    NGRID   ^YAUS0[0-4] KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-rotation/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Mid-level Rotation Track Products (on different grid)
    
    NGRID   ^YAUS0[5-9] KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-rotation-ml/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Merged Base Reflectivity
    
    NGRID   ^YAUQ.. KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-merged/MRMS_\1_\2_\3_\4.grib2
    
### MRMS Radar Products
    
    NGRID   ^YAU(C[0-9]|S[1-9])[0-9] KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-radar/MRMS_\2_\3_\4_\5.grib2
    
### MRMS Anything else (mainly future proofing)
    
    NGRID   ^YAU[ABE-KNORT-Z][0-9][0-9] KWNR ...... !grib2/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]..)/([^/]*)/.*
            FILE    -edex -log
            /awips2/data_store/grid/MRMS-other/MRMS_\1_\2_\3_\4.grib2

## ECMWF

### ECMF-Global, ECMF1..ECMF12

    HDS     ^H..... ECM. ......[^!]*!grib.*/[^/]*/[^/]*/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/ECMWF/ECMWF-\1_\2_\3_\4_\5-(seq).grib
    

### ECMWF HiRes

    HDS     ^([LM].Z.{1,3}) KWBX (..)(..)(..)
            PIPE    -close /awips2/ldm/decoders/decrypt_file
            /awips2/data_store/grid/ECMWF_HiRes/ecmwf_decrypted_\1_KWBX_\2\3\4-(seq).grib2
    EXP     (.*ecmwf_decrypted.*)
            FILE    -edex -log \1

## Other

### Canadian GEM Regional Model (CMC)

    CMC     ^CMC_reg_(.*)km_(..........)_P(...).grib2
            FILE    -edex -log
            /awips2/data_store/grid/CMC/CMC_reg_\1km_\2_P\3.grib2
        
### UKMET Global

    HDS     ^H..... EGRR ......[^!]*!grib/ukmet/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
        FILE    -edex -log
        /awips2/data_store/grid/UKMET-\1-GRID\2/UKMET-\1-GRID\2_\3\4_\5_\6-(seq).grib1

### National Precipitation Verification Unit (NPVU) - RFC Multisensor Precipitation Estimates (MPE) (MPE-Local-..., MPE-Mosaic-...)

    HDS     ^ZETA98 (....) ([0-3][0-9])([0-2][0-9]).*/m(.......)
            FILE    -edex -log
            /awips2/data_store/grid/MPE-\1/MPE-\1_\4_(\2:yyyy)(\2:mm)\2.grib

### Automated Satellite Precipitation Estimates from NESDIS (hourly) (AUTOSPE)
    
    HDS     ^ZETA98 K[NW][EN][ES] ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})/([^/]*)/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/AUTOSPE/AUTOSPE-\1\2_\3_\4-(seq).grib2

### River Forecast Center (RFC) Quantitative Precipitation Estimation (QPE)
    
    HDS     ^ZETA98 (KTUA|PACR|KSTR|KRSA|KORN|KRHA|KKRF|KMSR|KTAR|KPTR|KTIR|KALR|KFWR) ......[^!]*!(grib|grib2)/[^/]*/([^/]*)/#255/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/QPE-RFC/QPE-RFC-\1_\3_\4_\5_\6_\7-(seq).grib

### Ocean Sea Surface Temperature (SST) Grids #61-64
    
    HDS     ^H.[T-W]
            FILE    -edex -log
            /awips2/data_store/grid/SST/%Y%m%d%H%M.sst.grib

### HPCGuide

    NGRID   ^([LM][ABCDFGH]U...) (KWBN) (..)(..)(..)[^!]*!(grib|grib2)/[^/]*/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/HPCGuide/GRID\8/HPCGuide_GRID\8_\(10)Z_\(11)_\(12)-\1_\2_\3\4\5-(seq).grib2

### National Convective Weather Forecast (NCWF)

    ANY     ^ZDIA98 (....) ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})/(F[0-9]{3})
            FILE    -edex -log
            /awips2/data_store/grid/NCWF/NCWF_\2_\3_\4_\1-(seq).grib

### National Operational Hydrologic Remote Sensing Center Snow Analysis (NOHRSC-SNOW)
    
    HDS     ^[YZ][ES]QA88 KMSR ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/NOHRSC-SNOW/NOHRSC-SNOW_\1_\2_\3_\4-(seq).grib

### GFS MOS-Based Localized Aviation MOS Program (LAMP) guidance - LAMP2p5, GFSLAMP5
    
    NGRID   ^([LM].[ABDHMNRSTU].{1,3}) (KWNO|KMDL) (..)(..)(..)[^!]*!(grib2)/[^/]*/(LAMP)/#([^/]*)/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/LAMP/\7_\9_\(10)Z_\(11)_\(12)-\1_\2-(seq).grib2

### Radar Coded Messages (RCM)

    HDS     ^HAXA00 KWBC ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/RCM/RCM_\1_\2_\3_\4-(seq).grib

### National Digital Forecast Database (NDFD)

    CONDUIT grib2/nwstg/NWS_0/..../(........)(....)(F...)/(.*)/.*! (......)
            FILE    -edex -log
            /awips2/data_store/grid/NDFD/NDFD_\1\2\3_\4-\5.grib2

### NDFD WPC Quantitative Precipitation Forecast (HPCqpfNDFD)
    
    NGRID   ^[LM].[MN].98 KWNH ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/HPCqpf-ngrid/HPCqpf_\1_\2_\3_\4-(seq).grib2

### NDFD WPC Day 1-3 Excessive Rainfall Outlook (HPCqpfNDFD)

    HDS     ^[LM].[MN].98 KWNH ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/HPCqpf-hds/HPCqpf_\1_\2_\3_\4-(seq).grib

### MOSGuide/MOSGuideExtended/GMOS

    NGRID   ^(Y.UZ9[0-9]) (KWB.) (..)(..)(..)
            FILE    -edex -log
            /awips2/data_store/grid/MOSGuide/MOSGuide_\1_\2_\3\4\5-(seq).grib2


### Flash Flood Guidance (FFG) grids - 1HR=HPBL, 3HR=5WAVH, 6HR=CNWAT (FFG-PTR...)

    HDS     ^ZEGZ98 K.{3} .......*/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/FFG/FFG-\1_\2_\3_\4_\5-(seq).grib

### PROB3HR/#236

    HDS     ^Z[DE]W[A-D][89]8 KWNO ...... /m0 !grib.*/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/PROB3HR/PROB3HR_\1_\2_\3_\4-(seq).grib

### National Data Buoy Center (NDBC) High Frequency Radar (HFR) Total Vector Velocity (TVV)

    NGRID   ^OUTA98 KWNB (......)[^!]*!grib2
            FILE    -edex -log
            /awips2/data_store/grid/HFR/HFR_\1-(seq).grib2

### Regional River Forecast Cebter (RFC) Quantitative Precipitation Forecast (QPF) (RFCqpf)

    HDS     ^YEI.[89]8 (KALR|KFWR|KKRF|KMSR|KORN|KPTR|KRHA|KRSA|KSTR|KTAR|KTIR|KTUA) .......*/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/[^/]*/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/Regional_RFC_QPF/GRID\2/\3_\4_\5_\6-(seq).grib

### GRID218 = HPCqpf

    HDS     ^(ZEX.98) KWNH (..)(..)(..)[^!]*!(grib|grib2)/[^/]*/([^/]*)/#218/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/HPCqpf/HPCqpf_\9Z_\(10)_\(11)-\1_KWNH_\2\3\4-(seq).grib

### Regional River Forecast Cebter (RFC) Quantitative Precipitation Forecast (QPF)
    
    HDS     ^(YEI.[89]8) KWNH (..)(..)(..)[^!]*!(grib|grib2)/ncep/([^/]*)/#([^/]*)/([0-9]{8})([0-9]{4})/(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/RFC_QPF_GRID\7/\9Z_\(10)_\(11)-\1_KWNH_\2\3\4-(seq).grib
    

## Ocean Models

### WAVE 233 Grid - Global NOAA WAVEWATCH III (WaveWatch)
    
    HDS     ^O.J.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-GWW233/GWW233_\1_F\2_\3_\4_%H%M%S.grib

### WAVE 238 Grid - Regional Western North Atlantic (WAVE-WNA)
    
    HDS     ^O.M.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-WNAWAVE238/WNAWAVE238_\1_F\2_\3_\4_%H%M%S.grib

### WAVE 238 Grid - Regional Western North Atlantic Hurricane (WAVE-WNA-Hurr)
    
    HDS     ^O.O.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-HurWave238/HurWave238_\1_F\2_\3_\4_%H%M%S.grib

### WAVE 239 Grid - Regional Alaska Waters (WAVE-AK)

    HDS     ^O.N.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-AKWAVE239/AKWAVE239_\1_F\2_\3_\4_%H%M%S.grib

### WAVE 253 Grid - Regional Eastern North Pacific (WAVE-ENP)

    HDS     ^O.S.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-ENPWAVE253/ENPWAVE253_\1_F\2_\3_\4_%H%M%S.grib

### WAVE 253 Grid - Regional Eastern North Pacific Hurricane (WAVE-ENP-Hurr)

    HDS     ^O.Q.88 KWBJ.*!grib/ncep/[^/]*/#[^/]*/(............)/F(...)/(.*)/(.*)/
            FILE    -edex -log
            /awips2/data_store/grid/WAVE-HDS-HurWave253/HurWave253\1_F\2_\3_\4_%H%M%S.grib
            
### WW3 Global

    NGRID   E.A.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Global/WW3_Global_\1_\200.grib2

### WW3 Regional Alaska

    NGRID   E.E.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Regional_Alaska/WW3_Regional_Alaska_\1_\200.grib2

### WW3 Coastal Alaska
    
    NGRID   E.F.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Coastal_Alaska/WW3_Coastal_Alaska_\1_\200.grib2

### WW3 Eastern Pacific (Regional)

    NGRID   E.D.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Regional_Eastern_Pacific/WW3_Regional_Eastern_Pacific_\1_\200.grib2

### WW3 US East Coast (Regional)
    
    NGRID   E.B.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Regional_US_East_Coast/WW3_Regional_US_East_Coast_\1_\200.grib2

### WW3 US East Coast (Coastal)

    NGRID   E.H.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Coastal_US_East_Coast/WW3_Coastal_US_East_Coast_\1_\200.grib2

### WW3 US West Coast (Regional)
    
    NGRID   E.C.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Regional_US_West_Coast/WW3_Regional_US_West_Coast_\1_\200.grib2
        
### WW3 US West Coast (Coastal)
    
    NGRID   E.G.88 KWBJ.*ncep/[^/]*/#[^/]*/(............)F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/WW3_Coastal_US_West_Coast/WW3_Coastal_US_West_Coast_\1_\200.grib2

### ESTOFS - US
    
    NGRID   ^E[EHC]I[A-Z]88 KWBM ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
        /awips2/data_store/grid/ESTOFS/ESTOFS_\1_\2_\3_\4-(seq).grib

### ESTOFS - Puerto Rico
    
    NGRID   ^E[EHC]P[A-Z]88 KWBM ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/ESTOFS-PR/ESTOFS-PR_\1_\2_\3_\4-(seq).grib

### ESTOFS Pacific - Alaska
    
    NGRID   ^E[EHC]A[A-Z]88 KWBM ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/ESTOFS-AK/ESTOFS-AK_\1_\2_\3_\4-(seq).grib


### ESTOFS Pacific - CONUS (West Coast)
    
    NGRID   ^E[EHC]D[A-Z]88 KWBM ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/ESTOFS-WC/ESTOFS-WC_\1_\2_\3_\4-(seq).grib

### ESTOFS Pacific - Hawaii
    
    NGRID   ^E[EHC]H[A-Z]88 KWBM ......[^!]*!grib.*/[^/]*/[^/]*/#[^/]*/([0-9]{8})([0-9]{4})(F[0-9]{3})/([^/]*)
            FILE    -edex -log
            /awips2/data_store/grid/ESTOFS-HI/ESTOFS-HI_\1_\2_\3_\4-(seq).grib

### Extra-Tropical Storm Surge (ETSS)
    
    NGRID   ^MHU... KNHC (..)(..)(..)
            FILE    -edex -log
            /awips2/data_store/grid/ETSS/ETSS_\1\2\3-(seq).grib2

### GLERL
    
    HDS     ^O.N.88 KWNB.*!grib/161/([^/]*)/#([^/]*)/(............)/F(...)/.*
            FILE    -edex -log
            /awips2/data_store/grid/GLERL/GLERL_\1_F\2_%H%M%S.grib



    
# Important Files and Directories


|---|---|
| location on disk  | **/awips2/edex/data/hdf5/grid**  |
| definition files  | **/awips2/edex/data/utility/edex_static/base/grib/models**  |
| navigation files  | **/awips2/edex/data/utility/edex_static/base/grib/grids**  |
| grib1 definitions | **/awips2/edex/data/utility/common_static/base/grid** |
| D2D files  | **/awips2/edex/data/utility/edex_static/base/grib/grids**  |
| metadata tables | **grid** |
|                 | **grid_info** |
|                 | **gridcoverage** |
