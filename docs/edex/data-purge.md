---
layout: default
type: guide
shortname: Docs
title: EDEX Data Purging
---




AWIPS uses a plugin-based purge strategy for HDF5 data, allowing the user to change the purge frequency for each plugin individually, and specific products for a particular plugin.

> Purge is triggered by a quartz timer event that fires at 30 minutes after each hour. 

Purging rules are defined in XML files in the Localization Store, accessible from the CAVE localization perspective. On EDEX, most are located in `/awips2/edex/data/utility/common_static/base/purge`, and follow the **base/site** localization pattern (e.g. site purge files are in `site/XXX/purge` rather than `base/purge`, where XXX is the site identifier.

# Time-base purge

If a plugin has no XML file, the default rule of 1 day (24 hours) is used, from `/awips2/edex/data/utility/common_static/base/purge/defaultPurgeRules.xml` 

    <purgeRuleSet>
        <defaultRule>
            <period>01-00:00:00</period>
        </defaultRule>
    </purgeRuleSet>

Time-based purging uses the *reference time* of the data, and dtermination of the reference time is decoder based. 


> ### 30-day NEXRAD3 archive
>Modify `/awips2/edex/data/utility/common_static/base/purge/radarPurgeRules.xml` to increase the data retention period from 7 to 31 days:
>
    <purgeRuleSet>
            <defaultRule>
                    <period>31-00:00:00</period>
            </defaultRule>
    </purgeRuleSet>
>
>**Note**: you do NOT have to restart EDEX when you change a purge rule!


# Frame-based purge

Some plugins use frame-base purging, retaining and certain number of prpduct "versions". 

`/awips2/edex/data/utility/common_static/base/purge/satellitePurgeRules.xml`
    
    <purgeRuleSet>
        <key>sectorID</key>
        <key>physicalElement</key>
        <defaultRule>
                <versionsToKeep>196</versionsToKeep>
        </defaultRule>
        <rule>
                <keyValue>NEXRCOMP</keyValue>
                <versionsToKeep>288</versionsToKeep>
        </rule>
        <rule>
                <keyValue>Alaska National</keyValue>
                <versionsToKeep>48</versionsToKeep>
        </rule>

> In the above example, notice a *default rule* (196) as well as specific sectors with their own rules. 

# Logging

Data purge events are logged to the file `edex-ingest-purge-<yyyymmdd>.log`, where `<yyyymmdd>` is the date stamp. 


    tail -f edex-ingest-purge-20120327.log 
    
    --------START LOG PURGE---------
    INFO  2012-03-27 00:30:00,027 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Skipped file with invalid fileName: afos-trigger.log
    INFO  2012-03-27 00:30:00,193 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Removed 1 old files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Archived 14 files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::Skipped processing 1 files
    INFO  2012-03-27 00:31:23,155 [DefaultQuartzScheduler_Worker-3] PurgeLogger: EDEX - PURGE LOGS::---------END LOG PURGE-----------


# All Purge Rules

To see all purge rule directories (base, site, region, configured):

    find /awips2/edex/data/utility -name purge
    
    /awips2/edex/data/utility/edex_static/region/CR/purge
    /awips2/edex/data/utility/edex_static/base/purge
    /awips2/edex/data/utility/edex_static/configured/OAX/purge
    /awips2/edex/data/utility/edex_static/site/OAX/purge
    /awips2/edex/data/utility/common_static/region/CR/purge
    /awips2/edex/data/utility/common_static/base/purge
    /awips2/edex/data/utility/common_static/configured/OAX/purge
    /awips2/edex/data/utility/common_static/site/AFC/purge
    /awips2/edex/data/utility/common_static/site/OAX/purge
    

To see a list of the data plug-ins that have purge rules:
    
    find /awips2/edex/data/utility -name "*PurgeRules.xml"
    
    /awips2/edex/data/utility/edex_static/base/purge/airmetPurgeRules.xml
    /awips2/edex/data/utility/edex_static/base/purge/convsigmetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufruaPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/obsPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/nctextPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ncscatPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/vaaPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosHPCPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrhdwPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/cwaPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/airmetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ffmpPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/lsrPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/profilerPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/poessoundingPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/gpdPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/mcidasPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/acarssoundingPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ffgPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/sgwhvPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/intlsigmetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/aggregatePurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/radarPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/idftPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/wcpPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/warningPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/satellitePurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/sshaPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ldadhydroPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ldadmesonetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ccfpPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/atcfPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrssmiPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/sgwhPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/geomagPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/modelsoundingPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosAVNPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ncuairPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/acarsPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/airepPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrquikscatPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/cwatPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/nonconvsigmetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/pirepPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/statsPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmthdwPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrascatPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/gridPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosGFSPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ncpafmPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrsigwxPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/pgenPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/dmwPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosMRFPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosETAPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/sfcobsPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/modisPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/awwPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/defaultPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ldadprofilerPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosNGMPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/madisPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/convsigmetPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/redbookPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrncwfPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/tcmPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/bufrmosLAMPPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/goessoundingPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/binlightningPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/stormTrackPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ghcdPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ldadmanualPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/ntransPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/mosaicPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/regionalsatPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/nctafPurgeRules.xml
    /awips2/edex/data/utility/common_static/base/purge/solarimagePurgeRules.xml
    /awips2/edex/data/utility/common_static/site/AFC/purge/regionalsatPurgeRules.xml
