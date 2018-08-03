
## EDEX Service Manager

There are five EDEX service installed into `/etc/init.d/`, four of which run on boot:

    service postgres start
    service httpd-pypies start
    service qpidd start
    service edex_camel start

The fifth, `edex_ldm`, does **not run at boot** to prevent filling up disk space if EDEX is not running. 

    service edex_ldm start

All of these services are started and stopped by a single program `edex`,

### edex start

    edex start
    
    Starting EDEX PostgreSQL:                                  [  OK  ]
    Starting httpd:                                            [  OK  ]
    Starting QPID                                              [  OK  ]
    Starting EDEX Camel (request): 
    Starting EDEX Camel (ingest): 
    Starting EDEX Camel (ingestGrib): 
    Starting AWIPS LDM:The product-queue is OK.
    ...

### edex stop

    edex stop

    Stopping EDEX Camel (request): 
    Stopping EDEX Camel (ingest): 
    Stopping EDEX Camel (ingestGrib): 
    Stopping QPID                                              [  OK  ]
    Stopping httpd:                                            [  OK  ]
    Stopping EDEX PostgreSQL:                                  [  OK  ]
    Stopping AWIPS LDM:Stopping the LDM server...
    ...
    
### edex setup

    edex setup
    
    [edex] EDEX IP and Hostname Setup
     Checking /awips2/database/data/pg_hba.conf [OK]
     Checking /awips2/edex/bin/setup.env [OK]
    
    [edit] Hostname edex.unidata.ucar.edu added to /awips2/ldm/etc/ldmd.conf
    [done]

This command configures and/or confirms that the EDEX hostname and IP address definitions exist (`edex setup` is run by `edex start`).

> If your EDEX server is running but you see the message "Connectivity Error: Unable to validate localization preferences" in CAVE, it may mean that the domain name defined in `/awips2/edex/bin/setup.env` can not be resolved from *outside* the server.  Some machines have different **internally-resolved** and **externally-resolved** domain names (cloud-based especially). The name defined in `setup.env` must be **externally-resolvable**.

### edex log

    edex log
    
    [edex] EDEX Log Viewer

     :: No log specified - Defaulting to ingest log
     :: Viewing /awips2/edex/logs/edex-ingest-20151209.log. Press CTRL+C to exit
    
    INFO [Ingest.binlightning-1] /awips2/data_store/SFPA42_KWBC_091833_38031177.2015120918 processed in: 0.0050 (sec) Latency: 0.0550 (sec)
    INFO [Ingest.obs-1] /awips2/data_store/metar/SAIN31_VABB_091830_131392869.2015120918 processed in: 0.0810 (sec) Latency: 0.1800 (sec)

More edex logs...

    edex log grib
    edex log request
    edex log ldm
    edex log radar
    edex log satellite
    edex log text

### edex users

To see a list of clients connecting to your EDEX server, use the `edex users [YYYYMMDD]` command, where `YYYYMMDD` is the optional date string.

    edex users
    
     -- EDEX Users 20160826 --
    user@101.253.20.225
    user@192.168.1.67
    awips@0.0.0.0
    awips@sdsmt.edu

### edex purge

To view any stuck purge jobs in PortgreSQL (a rare but serious problem if your disk fills up).  The solution to this is to run `edex purge reset`.