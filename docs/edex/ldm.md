# LDM Feeds

## Default LDM Feeds for EDEX

Data feeds are defined by the **ldmd.conf** file in `/awips2/ldm/etc/ldmd.conf`.  The default feeds that come "turned on" with our EDEX are the following:

    REQUEST NEXRAD3 "./p(DHR|DPR|DSP|DTA|DAA|HHC|N.P|N.Q|N.S|N.U|OHA)." idd.unidata.ucar.edu
    REQUEST FNEXRAD|IDS|DDPLUS ".*" idd.unidata.ucar.edu
    REQUEST UNIWISC|NIMAGE ".*" idd.unidata.ucar.edu        # AREA/GINI
    REQUEST EXP "WwWind" idd.unidata.ucar.edu               # ESPL/PSD Profilers
    REQUEST DIFAX "GLM" idd.unidata.ucar.edu                # GOES GLM
    REQUEST EXP ".*" lead.unidata.ucar.edu                  # GOES ABI netCDF4 (full sector)
    REQUEST NGRID ".*" idd.unidata.ucar.edu
    REQUEST HDS ".*" idd.unidata.ucar.edu
    REQUEST CONDUIT "nam" idd.unidata.ucar.edu              # NAM12
    REQUEST CONDUIT "pgrb2" idd.unidata.ucar.edu            # GFS0p25

> **Note**: Remember that LDM commands such as these require **TAB SEPARATION** between items.

## Optional LDM Feeds

Some additional feeds are included but commented out using '#'.  To activate the feed, simply remove the #, save the file, and [restart the LDM](#restart-the-ldm).

### FNMOC and CMC models

    REQUEST FNMOC ".*" idd.unidata.ucar.edu
    REQUEST CMC ".*" idd.unidata.ucar.edu
    
### Lightning (restricted to educational use with rebroadcasting restricted)

    REQUEST        LIGHTNING       ".*"    striker2.atmos.albany.edu
    REQUEST        LIGHTNING       ".*"    idd.unidata.ucar.edu
    
### FSL/GSD Experimental HRRR (Sub-hourly)

    REQUEST FSL2 "^GRIB2.FSL.HRRR" hrrr.unidata.ucar.edu

## Restart the LDM

Use the following commands to restart the LDM:

    sudo service edex_ldm restart

    ldmadmin restart
        
## Monitor Incoming Data Feeds

To watch incoming data in real-time:

    notifyme -vl - 
    
To watch for a specific product and feed and time (360 sec = 6 min):

    notifyme -vl - -h localhost -f NEXRAD3 -p DHR -o 360
    
To watch the same on a remote queue:

    notifyme -vl - -h idd.unidata.ucar.edu -f NEXRAD3 -p DHR -o 360
    
## LDM Logging

To open a real-time readout of LDM logging you can run use the `edex` command.  To exit, press `CTRL+C`.

    edex log ldm
    
    [edex] EDEX Log Viewer
    
     :: Viewing /awips2/ldm/logs/ldmd.log. Press CTRL+C to exit
    
    Aug 26 15:05:10 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_MXUPHL01-21387192.grib2":     406227 20160826210510.477   NGRID 21387192  YZCG86 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/MXUPHL01/5000-2000 m HGHT
    Aug 26 15:05:11 edextest edexBridge[5812] NOTE: Sent 2 messages (0 at the end of the queue, 2 normally).
    Aug 26 15:05:11 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_CICEP-21387200.grib2":     369464 20160826210511.484   NGRID 21387200  YMCG98 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/CICEP/0 - NONE
    Aug 26 15:05:12 edextest edexBridge[5812] NOTE: Sent 9 messages (0 at the end of the queue, 9 normally).
    Aug 26 15:05:12 edextest pqact[5811] NOTE: Filed in "/awips2/data_store/grid/HRRR/HRRR_CONUS_2p5km_201608262000_F006_LTNG-21387205.grib2":     482800 20160826210512.254   NGRID 21387205  YZCG98 KWBY 262000 !grib2/ncep/HRRR/#255/201608262000F006/LTNG/0 - EATM
    Aug 26 15:05:13 edextest edexBridge[5812] NOTE: Sent 1 messages (0 at the end of the queue, 1 normally).
