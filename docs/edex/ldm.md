---
layout: default
type: guide
shortname: Docs
title: Default LDM feeds
subtitle: EDEX Admin
---



# Default LDM Feeds for EDEX

from `/awips2/ldm/etc/ldmd.conf`

    REQUEST NEXRAD3 "./p(DHR|DPR|DSP|DTA|DAA|DVL|EET|HHC|N0Q|N0S|N0U|OHA|NVW|NTV|NST)." idd.unidata.ucar.edu
    REQUEST FNEXRAD|IDS|DDPLUS|UNIWISC ".*" idd.unidata.ucar.edu
    REQUEST NGRID ".*" idd.unidata.ucar.edu
    REQUEST NOTHER "^TIP... KNES.*" idd.unidata.ucar.edu
    REQUEST HDS|NIMAGE ".*" idd.unidata.ucar.edu

    REQUEST CONDUIT "nam" idd.unidata.ucar.edu
    REQUEST CONDUIT "rap" idd.unidata.ucar.edu
    REQUEST CONDUIT "pgrb2" idd.unidata.ucar.edu
    REQUEST CONDUIT "nwstg" idd.unidata.ucar.edu

> Remember than LDM commands such as these require **TAB SEPARATION** between items.

# Optional LDM Feeds

FNMOC and CMC models

    REQUEST FNMOC ".*" idd.unidata.ucar.edu
    REQUEST CMC ".*" idd.unidata.ucar.edu
    
Lightning (restricted to educational use with rebroadcasting restricted)

    REQUEST        LIGHTNING       ".*"    striker2.atmos.albany.edu
    REQUEST        LIGHTNING       ".*"    idd.unidata.ucar.edu
    
FSL/GSD Experimental HRRR (Sub-hourly)

    REQUEST FSL2 "^GRIB2.FSL.HRRR" hrrr.unidata.ucar.edu

# Restart the LDM

    sudo service edex_ldm restart

    ldmadmin restart
        
# Monitor Incoming Data Feeds

To watch incoming data in real-time:

    notifyme -vl - 
    
To watch for a specific product and feed and time (360 sec = 6 min):

    notifyme -vl - -h localhost -f NEXRAD3 -p DHR -o 360
    
To watch the same on a remote queue:

    notifyme -vl - -h idd.unidata.ucar.edu -f NEXRAD3 -p DHR -o 360
    
# Logging

    edex log ldm
    
    [edex] EDEX Log Viewer
    
     :: Viewing /awips2/ldm/logs/ldmd.log. Press CTRL+C to exit
    
    Aug 26 15:05:10 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_MXUPHL01-21387192.grib2":     406227 20160826210510.477   NGRID 21387192  YZCG86 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/MXUPHL01/5000-2000 m HGHT
    Aug 26 15:05:11 edextest edexBridge[5812] NOTE: Sent 2 messages (0 at the end of the queue, 2 normally).
    Aug 26 15:05:11 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_CICEP-21387200.grib2":     369464 20160826210511.484   NGRID 21387200  YMCG98 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/CICEP/0 - NONE
    Aug 26 15:05:12 edextest edexBridge[5812] NOTE: Sent 9 messages (0 at the end of the queue, 9 normally).
    Aug 26 15:05:12 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_LTNG-21387205.grib2":     482800 20160826210512.254   NGRID 21387205  YZCG98 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/LTNG/0 - EATM
    Aug 26 15:05:13 edextest edexBridge[5812] NOTE: Sent 1 messages (0 at the end of the queue, 1 normally).
    Aug 26 15:05:13 edextest pngg2gini[22317] NOTE: Starting Up
    Aug 26 15:05:13 edextest pngg2gini[22317] INFO: output file pathname: /awips2/data_store/ingest/nexrcomp_4km_2105_20160826_ntp_0.rad
    Aug 26 15:05:13 edextest pngg2gini[22317] NOTE: unPNG::   117632    888533  7.5535
    Aug 26 15:05:13 edextest pngg2gini[22317] NOTE: Exiting
    Aug 26 15:05:14 edextest edexBridge[5812] NOTE: Sent 17 messages (0 at the end of the queue, 17 normally).
