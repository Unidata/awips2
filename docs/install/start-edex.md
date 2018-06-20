
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
     Checking /awips2/data/pg_hba.conf [OK]
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

---

## EDEX Memory Configuration

The directory `/awips2/edex/etc/` contains files which define the amount of memory used for each of the three EDEX JVMs (ingest, ingestGrib, request):

	ls -al /awips2/edex/etc/
	-rw-r--r-- 1 awips fxalpha 1501 Dec  7 00:37 default.sh
	-rw-r--r-- 1 awips fxalpha 1655 Dec 12 19:47 ingestGrib.sh
	-rw-r--r-- 1 awips fxalpha  937 Dec 12 19:46 ingest.sh
	-rw-r--r-- 1 awips fxalpha 1231 Dec 12 19:47 request.sh

Each file contains the **Xmx** definition for maximum memory:

	export INIT_MEM=512 # in Meg
	export MAX_MEM=4096 # in Meg

After editing these files, you must restart edex (`service edex_camel restart`).

---

## EDEX Plugin Configuration

The directory `/awips2/edex/conf/modes` contains XML files with rules defining which plugins are included or excluded with each JVM (ingest, ingestGrid, request):

	ls -la /awips2/edex/conf/modes
	-rw-r--r-- 1 awips fxalpha 1982 Dec  6 21:26 grid-modes.xml
	-rw-r--r-- 1 awips fxalpha  928 Dec  6 21:24 ingest-modes.xml
	-rw-r--r-- 1 awips fxalpha 1689 Dec  6 21:24 request-modes.xml

EDEX services are all registered through spring. By including or excluding specific spring files we can determine at startup which services the EDEX instance should start.

All mode files are merged at startup. Modes files with modes that have the same name are combined so the end result is an aggregate of patterns in all files.  Include and exclude tags should have regular expressions that are compatible with Java's Pattern class.  If you provide no `<include>` tag for a particular mode, the include defaults to `.*`.

An example of `/awips2/edex/conf/modes/ingest-modes.xml`, with a number of unused plugin decoders excluded because the data are not available outside of the SBN:
	
	<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
	<edexModes>
		<mode name="ingest">
			<exclude>.*request.*</exclude>
			<exclude>edex-security.xml</exclude>
			<exclude>taf.*</exclude>
			<exclude>modis.*</exclude>
			<exclude>shef.*</exclude>
			<exclude>idft.*</exclude>
			<exclude>ffmp.*</exclude>
			<exclude>stormtrack.*</exclude>
			<exclude>retrieval.*</exclude>
			<exclude>regionalsat.*</exclude>
			<exclude>pointset-netcdf.*</exclude>
			<exclude>ncscat.*</exclude>
			<exclude>bufrobs.*</exclude>
			<exclude>bufrmthdw.*</exclude>
			<exclude>sgwh.*</exclude>
		</mode>
	</edexModes>



