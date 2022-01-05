# EDEX Basic Commands 

!!! note "These steps should be run as user *awips* with sudo"

Unidata's EDEX install also comes with a [simple **`edex`** program](#edex-commands) that can help execute basic EDEX utilities.  The most basic of the commands are the following:

To start all EDEX services:

    edex start
    
To stop all EDEX services:

    edex stop

---

## Service and Boot Settings

These commands will start and stop five EDEX service files installed into **/etc/init.d/**, four of which are run on boot:

    service postgres start
    service httpd-pypies start
    service qpidd start
    service edex_camel start

The fifth, `edex_ldm`, does **not run at boot** to prevent filling up disk space if EDEX is not running:

    service edex_ldm start

All of these services are started and stopped by the single program: `edex` as mentioned above.

### LDM Troubleshooting

If the EDEX machine is shut down abruptly, when restarted, it should start up the processes mentioned [above](#service-and-boot-settings).  If `sudo service edex_ldm start` does not start up LDM smoothly, please try these steps:

!!! note "All of the following commands should be run as user *awips* and the `service` commands may need to be run with `sudo`."

-  Run `sudo service edex_ldm start` or `ldmadmin start` and recieve this message:
        
        ldmadmin start
        
        start_ldm(): PID-file "/awips2/ldm/ldmd.pid" exists.  Verify that all 
        is well and then execute "ldmadmin clean" to remove the PID-file.  

- Run `ldmadmin clean` and `sudo service edex_ldm start` and receive this error:

        ldmadmin clean
        sudo service edex_ldm start
        
        Checking the product-queue...
        The writer-counter of the product-queue isn't zero.  Either a process
        has the product-queue open for writing or the queue might be corrupt.
        Terminate the process and recheck or use
            pqcat -l- -s -q /awips2/ldm/var/queues/ldm.pq && pqcheck -F -q
            /awips2/ldm/var/queues/ldm.pq
        to validate the queue and set the writer-counter to zero.
        LDM not started

- To resolve the above, run:

        pqcat -l- -s -q /awips2/ldm/var/queues/ldm.pq && pqcheck -F -q  /awips2/ldm/var/queues/ldm.pq 
        ldmadmin delqueue
        ldmadmin mkqueue
        sudo service edex_ldm start

---

## EDEX Commands

Unidata's version of EDEX installs with a helpful `edex` script that can be used for basic EDEX tasks.

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

### edex start base

To start all EDEX services *except* the LDM:

    edex start base

---

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
    
---

### edex setup

    edex setup
    
    [edex] EDEX IP and Hostname Setup
     Checking /awips2/database/data/pg_hba.conf [OK]
     Checking /awips2/edex/bin/setup.env [OK]
    
    [edit] Hostname edex.unidata.ucar.edu added to /awips2/ldm/etc/ldmd.conf
    [done]

This command configures and/or confirms that the EDEX hostname and IP address definitions exist (`edex setup` is run by `edex start`).

> **Note**: If your EDEX server is running but you see the message **"Connectivity Error: Unable to validate localization preferences"** in CAVE, it may mean that the domain name defined in **/awips2/edex/bin/setup.env** can not be resolved from *outside* the server.  Some machines have different *internally-resolved* and *externally-resolved* domain names (cloud-based especially). The name defined in **setup.env** must be *externally-resolvable*.

---

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

---

### edex qpid

Shows a list of the the Qpid message queue to monitor data ingest (messages in vs messages out, i.e. decoded):

    [centos@js-156-89 ~]$ edex qpid
    Queues
      queue                       dur  excl  msg   msgIn  msgOut  bytes  bytesIn  bytesOut  cons  bind
      ================================================================================================
      external.dropbox            Y    Y       11  1.26m  1.26m    621   79.6m    79.6m        5     1
      Ingest.Radar                Y    Y        4   589k   589k    184   27.1m    27.1m        5     1
      Ingest.GribDecode           Y    Y        0   370k   370k      0    103m     103m       11     1
      Ingest.GribSplit            Y    Y        2   361k   361k    201   31.9m    31.9m        5     1
      Ingest.modelsounding        Y    Y        0   100k   100k      0   6.54m    6.54m        1     1
      Ingest.Text                 Y    Y        0  97.8k  97.8k      0   5.25m    5.25m        2     1
      Ingest.GOESR                Y    Y        0  83.4k  83.4k      0   6.92m    6.92m        2     1
      Ingest.obs                  Y    Y        0  46.2k  46.2k      0   2.40m    2.40m        1     1
      Grid.PostProcess            Y    Y        0  20.2k  20.2k      0   6.68m    6.68m        1     1
      Ingest.sfcobs               Y    Y        0  10.5k  10.5k      0    577k     577k        1     1
      Ingest.goessounding         Y    Y        0  6.68k  6.68k      0    427k     427k        1     1
      Ingest.Glm                  Y    Y        0  5.61k  5.61k      0    581k     581k        1     1
      Ingest.aww                  Y    Y        0  3.32k  3.32k      0    182k     182k        1     1
   

---

### edex users

To see a list of clients connecting to your EDEX server, use the `edex users [YYYYMMDD]` command, where `[YYYYMMDD]` is the optional date string.

    edex users
    
     -- EDEX Users 20160826 --
    user@101.253.20.225
    user@192.168.1.67
    awips@0.0.0.0
    awips@sdsmt.edu

---

### edex purge

To view any stuck purge jobs in PortgreSQL (a rare but serious problem if your disk fills up).  The solution to this is to run `edex purge reset`.
