
PDA: ESPDS Product Distribution & Access
ESPDS: NOAA/NESDIS Environmental Satellite Processing and Distribution System
DPA: Data Provider Agent


* need to run keygenutil once and then copy keystore files to edex src directories for deployment
* "securityConfiguration" is so ingrained into data delivery that you cannot harvest from open servers (GRADS, THREDDS) without having a security config (which is only used to connect to ESPDS PDA server).
* need to specify opendap/grads server in existing file, and add TDS file for the same with a catalog DOD URL:

<constant name="SUBSET_REQUEST_URL" value="https://pda.espds.com" />

    <mode name="centralRegistry">
        <!-- Central Registry production mode -->
        <includeMode>dataDeliveryTemplate</includeMode>
        <includeMode>ebxmlRegistry</includeMode>
        <include>bandwidth-datadelivery-.*-ncf.xml</include>
        <!--<exclude>.*datadelivery.*-wfo.*</exclude>-->
        <!--<exclude>.*edex-security.*</exclude> -->
        <exclude>.*datadelivery.*-monolithic.*</exclude>
    </mode>

Spring-enabled Plugins:
-----------------------
bandwidth-datadelivery, 
bandwidth-datadelivery-daos, 
bandwidth-datadelivery-edex-impl, 
bandwidth-datadelivery-edex-impl-wfo, 
bandwidth-datadelivery-eventbus, 
bandwidth-datadelivery-schedule-route, 
bandwidth-datadelivery-wfo, 
bandwidth-datadelivery-wfo-router, 
bandwidth-graph-datadelivery, 
crawler-datadelivery, 
dataaccess-common, 
database-common, 
datadelivery-common, 
datadelivery-cron, 
datadelivery-handlers, 
datadelivery-handlers-impl, 
datadelivery-router, 
datadelivery-service, 
datadelivery-service-handlers, 
datadelivery-subscription-verification, 
datadelivery-wfo-cron, 
datadelivery-wfo-retrieval-process, 
ebxml, 
ebxml-constants, 
ebxml-eventbus, 
ebxml-federation-monitors, 
ebxml-garbagecollector-edex-impl, 
ebxml-impl, 
ebxml-jaxb, 
ebxml-querytypes, 
ebxml-registry-common, 
ebxml-registry-dao, 
ebxml-registry-init, 
ebxml-request-router, 
ebxml-subscription, 
ebxml-thrift-client, 
ebxml-validator-plugins, 
ebxml-webserver, 
ebxml-webservices, 
ebxml-xacml, 
edex-security, 
event-common, 
event-datadelivery, 
event-datadelivery-common, 
event-datadelivery-ingest, 
eventbus-common, 
fssobs-common, 
geo-common, 
goessounding-common, 
grid-common, 
grid-metadata, 
gridcoverage-common, 
harvester-datadelivery, 
harvester-datadelivery-registry, 
jaxb-datadelivery-registry, 
level-common, 
levelhandler-common, 
nwsauth-request, 
obs-common, 
parameter-common, 
persist-ingest, 
pointdata-common, 
purge-logs, 
registry-federation-datadelivery, 
request-service, 
request-service-common, 
retrieval-datadelivery,
retrieval-datadelivery-daos, 
retrieval-datadelivery-ncf, 
retrieval-datadelivery-wfo, 
satellite-common, 
satellite-dataplugin-common, 
sfcobs-common, stats-common, 
system-status-datadelivery, 
thrift-bandwidth, 
time-common, 
topo-dataaccess-common, 
utility-common, 
utility-request



acars-common, acars-common-dataaccess, acarssounding-common, activetable-common, activetable-request, airep-common, airep-common-dataaccess, airmet-common, alertviz-request, archiveadmim-request, atcf-common, auth-request, awipstools-request, aww-common, binlightning-common, binlightning-common-dataaccess, bufrascat-common, bufrhdw-common, bufrmos-common, bufrmos-common-dataaccess, bufrmthdw-common, bufrncwf-common, bufrsigwx-common, bufrssmi-common, bufrua-common, bufrua-common-dataaccess, bufrua-request, ccfp-common, climate-common-dataaccess, climate-hmdb-common, convectprob-common, convsigmet-common, cwa-common, cwat-common, dat-request, dataaccess-common, dataaccess-request, database-common, database-request, datadelivery-common, dd-request-router, dissemination-request, dmw-common, ebxml-registry-common, ebxml-request-router, edex-message-common, edex-request, event-common, event-datadelivery-common, eventbus-common, ffg-common, ffmp-common, ffmp-dataplugin-common, fog-common, fssobs-common, gempak-common, gempak-request, geo-common, geomag-common, geomag-request, gfe-common, gfe-dataplugin-common, gfe-request, ghcd-common, ghcd-request, goessounding-common, gpd-common, gpd-request, grid-common, grid-dataplugin-common, grid-request, gridcoverage-common, hpe-request, hydro-common, idft-common, intlsigmet-common, level-common, levelhandler-common, localization-http-request, lsr-common, manualIngest-common, manualIngest-request, maps-dataplugin-common, mcidas-common, menus-request, message-common, modelsounding-common, modelsounding-common-dataaccess, modis-common, mosaic-common, mping-common, ncep-common, ncgrib-request, ncpafm-common, ncscat-common, nctaf-common, nctext-common, ncuair-common, nonconvsigmet-common, ntrans-common, nucaps-common, obs-common, obs-common-dataaccess, obs-message-common, obstation-dataplugin-common, ohd-common, ohd-common-database, ohd-request, parameter-common, persist-request, pgen-common, pgen-request, pirep-common, pirep-common-dataaccess, poessounding-common, pointdata-common, pointdata-request, pointset-common, preciprate-common, profiler-common, profiler-common-dataaccess, qc-common, qpf-common, radar-common, radar-dataplugin-common, radar-request, redbook-common, remotescript-request, request-service, request-service-common, rpgenvdata-request, satellite-common, satellite-dataplugin-common, satellite-request, scan-common, sfcobs-common, sfcobs-common-dataaccess, sgwh-common, sgwhv-common, shef-common, site-common, site-request, solarimage-common, soundingrequest-request, ssha-common, stats-common, stats-request, stormtrack-common, stq-common, svrwx-common, taf-common, tcg-common, tcm-common, tcs-common, text-common, text-dbsrv-common, text-dbsrv-request, text-request, text-subscription-common, text-subscription-request, time-common, topo-dataaccess-common, uengine-request, units-common, useradmin-common, useradmin-request, utility-common, utility-request, vaa-common, viirs-common, vil-common, warning-common, warning-common-dataaccess, warning-request, wcp-common




 diff request.sh centralRegistry.sh
21,25d20
< export INIT_MEM=128 # in Meg
< export MAX_MEM=2048 # in Meg
< export SERIALIZE_POOL_MAX_SIZE=48
< export SERIALIZE_STREAM_INIT_SIZE_MB=2
< export SERIALIZE_STREAM_MAX_SIZE_MB=8
26a22,27
> export MAX_MEM=3072
> export MAX_PERM_SIZE=192m
> export EDEX_DEBUG_PORT=5011
> export EDEX_JMX_PORT=1622
> export LOG_CONF=logback-registry.xml
> export MGMT_PORT=9607
28,32c29,32
< export EDEX_DEBUG_PORT=5005
< export EDEX_JMX_PORT=1616
< export LOG_CONF=logback-request.xml
< export MGMT_PORT=9601
< export HTTP_PORT=9581
---
> export METADATA_POOL_TIMEOUT=60
> export CLUSTER_ID=NCF
> 
> export SOFT_REF_LRU_POLICY_MS_PER_MB=50





* can not run centralRegistry and request JVM, Address already in use java.net.BindException: Address already in use ( 9581 ?)


[edex status]
 EDEXregistry :: running :: pid 13445 17181

[awips@senne awips2-data-delivery]$ netstat -l -p|grep 13445
tcp        0      0 *:telelpathattack           *:*                         LISTEN      13445/java
tcp        0      0 *:us-cli                    *:*                         LISTEN      13445/java
tcp        0      0 *:9581                      *:*                         LISTEN      13445/java


awips    13410  0.0  0.0 106100  1212 pts/8    S+   10:18   0:00 /bin/bash ./dev centralRegistry
awips    13411  0.0  0.0 106104  1312 pts/8    S+   10:18   0:00 /bin/bash /awips2/edex/bin/start.sh -b centralRegistry
awips    13423  0.3  0.1 2028300 57628 pts/8   Sl+  10:18   0:05 java -Xmx32m -XX:MaxPermSize=12m -XX:ReservedCodeCacheSize=4m -Djava.io.tmpdir=/awips2/tmp -jar /awips2/yajs
awips    13445  7.3  2.5 8121576 821904 ?      SNsl 10:18   1:59 /awips2/java/bin/java -Dedex.run.mode=centralRegistry -Daw.site.identifier=OAX -Dedex.home=/awips2/edex -XX:

awips    19546  0.0  0.0 106100  1212 pts/8    S+   10:58   0:00 /bin/bash ./dev centralRegistry
awips    19547  0.0  0.0 106104  1312 pts/8    S+   10:58   0:00 /bin/bash /awips2/edex/bin/start.sh -b centralRegistry
awips    19580 24.5  2.4 8136208 795500 ?      SNsl 10:58   1:23 /awips2/java/bin/java -Dedex.run.mode=centralRegistry -Daw.site.identifier=OAX -Dedex.home=/awips2/edex -XX:MaxPermSize=192m -Dorg.apache.camel.jmx.disabled=true -Duser.timezone=GMT -Djava.io.tmpdir=/awips2/tmp -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/data/fxa/cave/senne/ -XX:SoftRefLRUPolicyMSPerMB=50 -Dqpid.dest_syntax=BURL -Ddb.addr=localhost -Ddb.port=5432 -Ddc.db.name=dc_ob7oax -Dfxa.db.name=fxatext -Dhm.db.name=hmdb -Dih.db.name=hd_ob92oax -Ddb.metadata.pool.max=50 -Ddb.metadata.pool.timeout=60 -Ddata.archive.root=/awips2/data_store -Djava.util.logging.config.file=/awips2/edex/conf/logging.properties -Dthrift.stream.maxsize=200  -Xms512m -Xmx3072m -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5011 -DMAX_PERM_SIZE=192m -DHM_DB_NAME=hmdb -DMAX_MEM=3072 -DJAVA_HOME=/awips2/java -DAW_SITE_IDENTIFIER=OAX -DSHORT_HOSTNAME=senne -DDATA_ARCHIVE_ROOT=/awips2/data_store -DIH_DB_NAME=hd_ob92oax -DDB_ADDR=localhost -DHOSTNAME=senne -DEDEX_HOME=/awips2/edex -DPROFILER_PARAM_1= -DINIT_MEM=512 -DFXA_DB_NAME=fxatext -DDC_DB_NAME=dc_ob7oax -DMETADATA_POOL_TIMEOUT=60 -DLOG_CONF=logback-registry.xml -DMETADATA_POOL_MAX=50 -DHOME=/home/awips -DSOFT_REF_LRU_POLICY_MS_PER_MB=50 -DAWIPS2_TEMP=/awips2/tmp -DDB_PORT=5432 -DWRAPPER_ON_EXIT_ACTION=RESTART -DCONSOLE_LOGLEVEL=DEBUG -classpath ...
awips    20819  0.0  0.0 103256   876 pts/15   S+   11:04   0:00 grep centralReg
[awips@senne awips2-data-delivery]$ netstat -l -p|grep 19580
(Not all processes could be identified, non-owned process info
 will not be shown, you would have to be root to see it all.)
tcp        0      0 *:telelpathattack           *:*                         LISTEN      19580/java          
tcp        0      0 *:us-cli                    *:*                         LISTEN      19580/java          
tcp        0      0 *:9581                      *:*                         LISTEN      19580/java   
    

# NCF registry (havester/crawler)

 the Harvester only runs at the Central Registry. As a
result, the WFO's no longer need to be concerned with the configuration of the Harvester.
The NCF (Central Registry) handles the maintenance of the Crawler. 


/etc/init.d/edex_camel start centralRegistry 





# Site (subscribe)

/etc/init.d/edex_camel start registry 






Data Delivery has been implemented into the AWIPS(II) baseline to provide access to data that is not resident locally at a Weather Forecast Office, River Forecast Center, or National Center. Data Delivery gives users the ability to create queries (One Time Requests) and
subscriptions to data sets (provided by the NOAA Operational Model Archive and Distribution System (NOMADS) data provider).
 
 ## Start and Stop
 
There are two different scripts to start the registry, and the script that gets executed  depends on whether or not you are at the NCF (Central Registry), which runs the  Crawler/Harvester, or an intermediary or node registry.
 
Start the registry for site:
 
    /etc/init.d/edex_camel start registry

Start the registry for the NCF along with the Crawler/Harvester:
 
    /etc/init.d/edex_camel start centralRegistry 
    
## Registry Configuration 

In operational use by the NWS, the Data Delivery federation Harvester is only run at the Central Registry. As a result, WFO's no longer need to  configure the Harvester, and the NCF (Central Registry) handles the maintenance of the Crawler. 

Data Delivery employs HTTPS secure transfers for registry-to-registry and some provider-to-registry communications. This means that Certificates for each registry node that directly connects to another registry node must be loaded into the trust store of each connecting node. This can be accomplished using a new utility located in the `/awips2/edex/conf/security` directory. The tool is called the `keystoreUtil.sh`. 

Another configuration file of note new (starting 14.4.1) is the “security.properties”. It is
also located in the “/awips2/edex/conf/resources” directory but under the further
directories of “/site/${SITE}”
Where, ${SITE} is your local WFO/ClusterID. It is written by the “keystoreUtil.sh” and
is read by the running system to gather each federation nodes SSL credentials for
communicating with other nodes. Manual editing of this file should be unnecessary.
Example... 

 #The following configuration items are used with the wss4j in/out interceptors
 org.apache.ws.security.crypto.merlin.keystore.file=security/keystore.jks
 org.apache.ws.security.crypto.merlin.keystore.password=
 org.apache.ws.security.crypto.merlin.keystore.type=JKS
 org.apache.ws.security.crypto.merlin.keystore.alias=OAX 
 
 ## Registry Federation Configuration

The following files must be present for a NCF registry to participate in the federation and receive replicated objects:

federationConfig.xml Located at: /awips2/edex/data/utility/edex_static/ebxml/federation/federationConfig.xml 

A sample of this file is provided:

    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    <federationProperties>
     <conformanceProfile>RegistryFull</conformanceProfile>
     <specificationVersion>4.0</specificationVersion>
     <catalogingLatency>PT0.000S</catalogingLatency>
     <replicationSyncLatency>PT0.000S</replicationSyncLatency>
     <siteIdentifier>NCF</siteIdentifier>
     <siteDescription>AWIPS Network Control Facility</siteDescription>
     <sitePrimaryContactFirstName>National</sitePrimaryContactFirstName>
     <sitePrimaryContactMiddleName>Weather</sitePrimaryContactMiddleName>
     <sitePrimaryContactLastName>Service</sitePrimaryContactLastName>
     <siteAddressStreetNumber>1325</siteAddressStreetNumber>
     <siteAddressStreet>East West Highway</siteAddressStreet>
     <siteAddressCity>Silver Spring</siteAddressCity>
     <siteAddressState>MD</siteAddressState>
     <siteAddressCountry>USA</siteAddressCountry>
     <siteAddressPostalCode>20910</siteAddressPostalCode>
    </federationProperties> 
    
