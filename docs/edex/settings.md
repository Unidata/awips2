# EDEX Settings

## Plugin Configuration

The directory **/awips2/edex/conf/resources** contains configuration text files for specific plugins, which allow for user-defined values which are read by AWIPS plugins on EDEX start:

    acarssounding.properties
    autobldsrv.properties
    com.raytheon.edex.plugin.gfe.properties
    com.raytheon.edex.text.properties
    com.raytheon.uf.common.registry.ebxml.properties
    com.raytheon.uf.edex.archive.cron.properties
    com.raytheon.uf.edex.database.properties
    com.raytheon.uf.edex.registry.ebxml.properties
    distribution.properties
    edex-localization-http.properties
    edex-requestsrv.properties
    edex-uengine.properties
    eventBus.properties
    ftp.properties
    goesr.properties
    grib.properties
    maintenance.properties
    proxy.properties
    purge.properties
    quartz.properties
    radar.properties
    stats.properties
    textdbsrv.properties
    warning.properties
    
Look at **purge.properties** for example:

    # Master switch to enable and disable purging
    purge.enabled=true
    
    # Interval at which the purge job kicks off
    purge.cron=0+0/15+*+*+*+?
    
    # Interval at which the outgoing files are purged
    purge.outgoing.cron=0+30+*+*+*+?
    
    # Interval at which the logs are purged
    purge.logs.cron=0+30+0+*+*+?
    
    # Interval at which hdf5 orphans are purged
    purge.orphan.period=24h
    
    # Number of days older than the earliest known data to delete.
    purge.orphan.buffer=7
    ...


In **grib.properties**, **goesr.properties**, and **radar.properties** you can adjust the number of decoder threads for each plugin.

    cat radar.properties
    
    # Number threads for radar products ingested from the SBN
    radar-decode.sbn.threads=5

---

## Ingest Modes

By default, EDEX starts three "modes": *ingest*, *ingestGrib*, and *request* (each as its own JVM).

The file **/awips2/edex/conf/modes/modes.xml** contains all available mode definitions, including some specific modes for Hydro Server Applications, ebXML Registries, Data Delivery, and more.

EDEX services are registered through spring, and by including or excluding specific spring files (usually by datatype plugin name) we can finely customize EDEX startup. 

In **/awips2/edex/conf/modes/modes.xml** there are a number of unused plugin decoders excluded because the data are not available outside of the SBN:
	
	...
	<mode name="ingest">
            <exclude>.*request.*</exclude>
            <exclude>edex-security.xml</exclude>
            <exclude>ebxml.*\.xml</exclude>
            <exclude>grib-decode.xml</exclude>
            <exclude>grid-staticdata-process.xml</exclude>
            <exclude>.*(dpa|taf|nctext).*</exclude>
            <exclude>webservices.xml</exclude>
            <exclude>.*datadelivery.*</exclude>
            <exclude>.*bandwidth.*</exclude>
            <exclude>.*sbn-simulator.*</exclude>
            <exclude>hydrodualpol-ingest.xml</exclude>
            <exclude>grid-metadata.xml</exclude>
            <exclude>.*ogc.*</exclude>
            <exclude>obs-ingest-metarshef.xml</exclude>
            <exclude>ffmp-ingest.xml</exclude>
            <exclude>scan-ingest.xml</exclude>
            <exclude>cwat-ingest.xml</exclude>
            <exclude>fog-ingest.xml</exclude>
            <exclude>vil-ingest.xml</exclude>
            <exclude>preciprate-ingest.xml</exclude>
            <exclude>qpf-ingest.xml</exclude>
            <exclude>fssobs-ingest.xml</exclude>
            <exclude>cpgsrv-spring.xml</exclude>
    </mode>
	...

In this example, request, ebXML, grib plugins, OGC and other plugins are excluded because they are included in their own mode/JVM.

> **Note**: TAF and NCTEXT plugins are disabled here due to performance issues.

---

## JVM Memory

The directory **/awips2/edex/etc/** contains files which define the amount of memory used for each of the three EDEX JVMs (ingest, ingestGrib, request):

	ls -al /awips2/edex/etc/
    -rw-r--r-- 1 awips fxalpha 1287 Jul 24 18:41 centralRegistry.sh
    -rw-r--r-- 1 awips fxalpha 1155 Jul 24 18:42 default.sh
    -rw-r--r-- 1 awips fxalpha 1956 Jul 24 18:41 ingestGrib.sh
    -rw-r--r-- 1 awips fxalpha  337 Jul 24 18:36 ingest.sh
    -rw-r--r-- 1 awips fxalpha  848 Jul 24 18:42 profiler.sh
    -rw-r--r-- 1 awips fxalpha 1188 Jul 24 18:41 registry.sh
    -rw-r--r-- 1 awips fxalpha  601 Jul 24 18:36 request.sh

Each file contains the **Xmx** definition for maximum memory:
  
    ...
  	export INIT_MEM=512 # in Meg
  	export MAX_MEM=4096 # in Meg
    ...

After editing these files, you must restart : `service edex_camel restart`.

---
